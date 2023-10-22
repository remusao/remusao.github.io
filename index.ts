import { readFileSync, readdirSync, promises as fs } from 'fs';
import { join, parse } from 'path';

import sharp from 'sharp';
import got from 'got';
import { Octokit } from '@octokit/rest';
import chokidar from 'chokidar';
import compression from 'compression';
import csso from 'csso';
import createDOMPurify from 'dompurify';
import express from 'express';
import glob from 'glob';
import hljs from 'highlight.js';
import { minify } from 'html-minifier';
import { JSDOM } from 'jsdom';
import moment from 'moment';
import rimraf from 'rimraf';
import serveStatic from 'serve-static';
import TreeSync from 'tree-sync';
import { Feed } from "feed";

import markdown from 'markdown-it';
// @ts-ignore
import markdownFootnote from 'markdown-it-footnote';
// @ts-ignore
import markdownAttributes from 'markdown-it-attrs';
// @ts-ignore
import markdownLinkAttributes from 'markdown-it-link-attributes';
// @ts-ignore
import markdownImplicitFigures from 'markdown-it-implicit-figures';
import markdownKatex from '@traptitech/markdown-it-katex';

const md: any = markdown({
  html: true,
  linkify: true,
  typographer: true,
  highlight(code, language) {
    if (language && hljs.getLanguage(language)) {
      try {
        return `<pre class="code" data-lang="${language}"><code>${
          hljs.highlight(code, { language, ignoreIllegals: true }).value
        }</code></pre>`;
      } catch (ex) {
        /* Empty */
      }
    }

    return `<pre class="code" data-lang="${language}"><code>${md.utils.escapeHtml(code)}</code></pre>`;
  },
})
  .use(markdownFootnote)
  .use(markdownAttributes)
  .use(markdownLinkAttributes, {
    pattern: /^(?:dat:|http:|https:)?\/\//,
    attrs: {
      target: '_blank',
      rel: 'noopener noreferrer',
    },
  })
  .use(markdownImplicitFigures, {
    figcaption: true,
    link: true,
  })
  .use(markdownKatex);

import emojis from './emojis';

const ci = process.argv[process.argv.length - 1] === '--ci';

const blogDomain = 'https://remusao.github.io';
const blogTitle = 'Simplex Sigillum Veri';
const blogDescription = blogTitle;

function escape(s: string): string {
  return `(?:${s.replace(/[-/\\^$*+?.()|[\]{}]/g, '\\$&')})`;
}

async function addEmojis(body: string): Promise<string> {
  const emojiPath = join('images', 'comments', 'emoji');
  await fs.mkdir(join(__dirname, '_site', emojiPath), { recursive: true });
  for (const [gh, hex] of emojis) {
    const tag = `${gh}:`;
    if (body.includes(tag)) {
      console.log('>>', tag, hex);

      const path = `${emojiPath}/${hex.toLowerCase()}.png`;
      const url = `https://github.githubassets.com/images/icons/emoji/unicode/${hex.toLowerCase()}.png`;
      await sharp(await got(url).buffer())
        .resize(64, 64)
        .toFile(join(__dirname, '_site', path));

      body = body.replace(
        new RegExp(escape(tag), 'g'),
        `<img loading="lazy" class="emoji" alt="${tag}" src="/${path}"/>`,
      );
    }
  }

  return body;
}

interface Comment {
  author: string;
  avatar: string;
  body: string; // raw markdown
  date: string;
  humanized: string;
  profile: string;
  url: string;
}

function createButtons(title: string, url: string): string {
  return `<ul>
  ${[
    {
      alt: 'Share on Facebook',
      href: `https://www.facebook.com/sharer/sharer.php?u=${url}&t=${title}`,
      img: 'facebook',
    },
    {
      alt: 'Tweet',
      href: `https://twitter.com/share?url=${url}&text=${title}&via=Pythux`,
      img: 'twitter',
    },
    {
      alt: 'Add to Pocket',
      href: `https://getpocket.com/save?url=${url}&title=${title}`,
      img: 'pocket',
    },
    {
      alt: 'Submit to Hacker News',
      href: `https://news.ycombinator.com/submitlink?u=${url}&t=${title}`,
      img: 'hackernews',
    },
    {
      alt: 'Submit to Reddit',
      href: `https://www.reddit.com/submit?url=${url}&title=${title}`,
      img: 'reddit',
    },
  ]
    .map(
      ({ href, alt, img }) => `
          <li>
        <a href="${href}" title="${alt}" target="_blank" rel="noopener noreferrer">
            <div class="sharing sharing-${img}"></div>
        </a>
          </li>
      `,
    )
    .join('\n')}
  </ul>`;
}

const LOGOS = (() => {
  const logos: { [name: string]: string } = {};
  const logoDirectory = './images/logos/';
  for (const file of readdirSync(logoDirectory)) {
    const { name } = parse(file);
    logos[name] = join(logoDirectory, file);
  }
  return logos;
})();

function getSvgAsDataUrl(path: string): string {
  if (path.endsWith('.svg') === false) {
    throw new Error(`Logo should be svg, got: ${path}`);
  }

  return `data:image/svg+xml;utf-8,${encodeURIComponent(readFileSync(path, 'utf-8'))}`;
}

function getApproxReadingTime(content: string): number {
  const textLength = content.split(/\s+/g).length;
  return Math.floor(Math.max(1, textLength / 150));
}

function getFileName(path: string): string {
  const lastSlashIndex = path.lastIndexOf('/');
  if (lastSlashIndex === -1) {
    return path;
  }
  return path.slice(lastSlashIndex + 1);
}

function getBaseName(path: string): string {
  const filename = getFileName(path);
  const lastDotIndex = filename.lastIndexOf('.');
  if (lastDotIndex === -1) {
    return filename;
  }

  return filename.slice(0, lastDotIndex);
}

function formatDate(date: Date): string {
  const formatTwoDigits = (n: number) => ('0' + n).slice(-2);
  return `${date.getFullYear()}-${formatTwoDigits(date.getUTCMonth() + 1)}-${formatTwoDigits(
    date.getDate(),
  )}`;
}

type PostMetadata = Map<string, string>;

interface Post {
  comments: string; // html
  content: string;
  date: Date;
  html: string; // html
  logo: string;
  name: string;
  readingTime: number;
  title: string;
  url: string;
  lang: string;
}

class Generator {
  private readonly purifyDOM: (_: string) => string;
  private readonly octokit: Octokit;
  private readonly posts: Map<string, Post>;

  private indexCSS!: string;
  private articleCSS!: string;
  private articleKatexCSS!: string;

  constructor() {
    // Initialize posts
    this.posts = new Map();

    // Initialize DOM sanitizer
    // @ts-ignore
    const DOMPurify = createDOMPurify(new JSDOM('').window);
    this.purifyDOM = (html) => DOMPurify.sanitize(html);

    // Initialize octokit for GitHub APIs
    const token = process.env['GITHUB_TOKEN'];
    this.octokit = new Octokit({
      auth: token,
    });
  }

  public async init(): Promise<void> {
    const [index, articleKatex, article] = await Promise.all([
      this.generateIndexCSS(),
      this.generateArticleCSS({ katex: true }),
      this.generateArticleCSS({ katex: false }),
    ]);

    this.indexCSS = index;
    this.articleKatexCSS = articleKatex;
    this.articleCSS = article;
  }

  public async generate(path: string): Promise<void> {
    const name = getBaseName(path);
    console.log(`Generating ${name}...`);

    try {
      const content = await fs.readFile(path, { encoding: 'utf-8' });
      const post = await this.parsePost(name, content);
      this.posts.set(name, post);

      const html = this.renderPost(post);
      const outputFilePath = `./_site/posts/${name}.html`;
      const outputFilePathLegacy = `./_site/posts/${post.date
        .toISOString()
        .slice(0, 10)}-${name}.html`;

      await Promise.all([
        fs.writeFile(outputFilePath, html, { encoding: 'utf-8' }),
        fs.writeFile(outputFilePathLegacy, html, { encoding: 'utf-8' }),
      ]);
    } catch (ex) {
      console.log('could not generate', path, ex);
    }
  }

  public async generateIndex(): Promise<void> {
    console.log('generating index.html');

    const indexEntry = (post: Post): string =>
      [
        `<a href="${post.url}">`,
        '  <li>',
        `    <div class="logo logo-${post.logo}"></div>`,
        `    ${post.title}`,
        '  </li>',
        '</a>',
      ].join('\n');

    const indexYear = (year: number, sameYearPosts: Post[]): string =>
      [
        '<div>',
        `  <h2 class="year">${year}</h2>`,
        '  <ul class="index">',
        ...sameYearPosts.map(indexEntry),
        '  </ul>',
        '</div>',
      ].join('\n');

    const posts = Array.from(this.posts.values()).sort(
      (p1: Post, p2: Post) => p2.date.getTime() - p1.date.getTime(),
    );

    const postsByYear: Map<number, Post[]> = new Map();
    for (const post of posts.values()) {
      const year = post.date.getFullYear();
      let postsForThisYear = postsByYear.get(year);
      if (postsForThisYear === undefined) {
        postsForThisYear = [];
        postsByYear.set(year, postsForThisYear);
      }
      postsForThisYear.push(post);
    }

    const postsSortedByYear = Array.from(postsByYear.entries()).sort(
      ([year1], [year2]) => year2 - year1,
    );

    const index = this.wrapHtml(
      'en',
      'index.html',
      'Simplex Sigillum Veri',
      ['<div>', ...postsSortedByYear.map((group) => indexYear(group[0], group[1])), '</div>'].join(
        '\n',
      ),
      this.indexCSS,
    );

    await fs.writeFile('./_site/index.html', index, { encoding: 'utf-8' });
  }

  public async generateRSS(): Promise<void> {
    const feed = new Feed({
      title: blogTitle,
      description: blogDescription,
      id: blogDomain,
      link: blogDomain,
      copyright: "All rights reserved 2023, Rémi Berson",
      feedLinks: {
        json: `${blogDomain}/rss.xml`,
        atom: `${blogDomain}/atom.xml`
      },
      author: {
        name: "Rémi Berson",
        link: blogDomain
      }
    });

    const posts = Array.from(this.posts.values()).sort(
      (p1: Post, p2: Post) => p2.date.getTime() - p1.date.getTime(),
    );
    
    posts.forEach(post => {
      feed.addItem({
        title: post.title,
        id: `${blogDomain}/${post.url}`,
        link: `${blogDomain}/${post.url}`,
        content: post.html,
        date: post.date,
      });
    });
    
    await Promise.all([
      fs.writeFile('./_site/rss.xml', feed.rss2(), { encoding: 'utf-8' }),
      fs.writeFile('./_site/atom.xml', feed.atom1(), { encoding: 'utf-8' }),
    ]);
  }

  private async generateIndexCSS(): Promise<string> {
    const logos = [];
    for (const [name, path] of Object.entries(LOGOS)) {
      logos.push(
        [
          `.logo-${name} {`,
          `  background-image: url('${getSvgAsDataUrl(path)}');`,
          '  background-repeat: no-repeat;',
          '}',
        ].join('\n'),
      );
    }

    const stylesheets = [
      logos.join('\n'),
      ...['./styles/style.css', './styles/index.css'].map((path) => readFileSync(path, 'utf8')),
    ];

    return csso.minify(stylesheets.join('\n')).css;
  }

  private async generateArticleCSS({ katex }: { katex: boolean }): Promise<string> {
    const stylesheets = [
      'styles/style.css',
      'styles/article.css',
      'styles/code.css',
      'styles/comments.css',
      'styles/table.css',
      'styles/footer.css',
      'styles/sharing.css',
      'styles/header.css',
      'styles/github.css',
    ];

    const sharing = [];
    for (const [name, path] of [
      ['facebook', 'images/social_flat_rounded_rects_svg/Facebook.svg'],
      ['twitter', 'images/social_flat_rounded_rects_svg/Twitter.svg'],
      ['pocket', 'images/social_flat_rounded_rects_svg/Pocket.svg'],
      ['hackernews', 'images/social_flat_rounded_rects_svg/HackerNews.svg'],
      ['reddit', 'images/social_flat_rounded_rects_svg/Reddit.svg'],
    ] as const) {
      sharing.push(
        [
          `.sharing-${name} {`,
          `  background-image: url('${getSvgAsDataUrl(path)}');`,
          '  background-repeat: no-repeat;',
          '}',
        ].join('\n'),
      );
    }

    if (katex) {
      stylesheets.push('styles/katex.css');
    }

    return csso.minify(
      [
        sharing.join('\n'),
        ...(await Promise.all(
          stylesheets.map((path) => fs.readFile(join(__dirname, path), 'utf8')),
        )),
      ].join('\n'),
    ).css;
  }

  private wrapHtml(lang: string, path: string, title: string, html: string, css: string): string {
    return minify(
      `
<!DOCTYPE html>
<html lang="${lang}" dir="ltr"> <!-- HTML5 -->
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1, viewport-fit=cover">

  <title>${title}</title>
  <meta name="description" content="${blogDescription}">

  <meta name="apple-mobile-web-app-capable" content="yes">
  <meta name="apple-mobile-web-app-status-bar-style" content="black">

  <link rel="canonical" href="https://remusao.github.io/${path}">

  <link rel="icon" type="image/x-icon" href="/images/favicon.ico"></link>

  <style type="text/css">${css}</style>
  </head>
  <body>
    <main>${html}</main>
  </body>
</html>`,
      {
        collapseWhitespace: true,
        minifyCSS: false,
        minifyJS: false,
        removeComments: true,
        removeOptionalTags: false,
        removeRedundantAttributes: true,
        removeScriptTypeAttributes: false,
        removeTagWhitespace: false,
        useShortDoctype: false,
      },
    );
  }

  private renderPost(post: Post): string {
    return this.wrapHtml(
      post.lang,
      `posts/${post.name}.html`,
      post.title,
      `
<header>
  <h1>
    <a href="../"><span>../</span></a>
  </h1>
</header>

<h1>${post.title}</h1>
<section class="header">
  <div>└─ ${formatDate(post.date)} • <em>Reading time: ~${post.readingTime} minutes</em></div>
</section>
<article>
  <div class="main">
    ${post.html}
  </div>
  <div class="comments">
    ${post.comments}
  </div>
</article>
<footer>
  <div class="share">
    ${createButtons(post.title, `${blogDomain}${post.url}`)}
  </div>
</footer>
`,
      post.html.includes('class="katex"') ? this.articleKatexCSS : this.articleCSS,
    );
  }

  private async parsePost(name: string, content: string): Promise<Post> {
    if (content.startsWith('---') === false) {
      throw new Error(`no metadata block:\n${content}`);
    }

    const endOfMetadata = content.indexOf('---', 3);
    if (endOfMetadata === -1) {
      throw new Error(`end of metadata block not found:\n${content}`);
    }

    const metadata: PostMetadata = new Map();
    const lines = content
      .slice(3, endOfMetadata)
      .trim()
      .split(/[\n\r]+/);

    for (const line of lines) {
      const indexOfColon = line.indexOf(':');
      if (indexOfColon === -1) {
        throw new Error(`end of metadata entry not found: ${line}`);
      }

      const prop = line.slice(0, indexOfColon);
      const value = line.slice(indexOfColon + 1).trim();
      metadata.set(prop, value);
    }

    // Generate HTML from markdown
    const html = md.render(content.slice(endOfMetadata + 3));

    const issue = metadata.get('issue');
    const comments = ci === false || issue === undefined ? '' : await this.createComments(issue);
    // const comments = issue === undefined ? '' : await this.createComments(issue);

    const rawDate = metadata.get('date');
    const date = rawDate === undefined ? new Date() : new Date(rawDate);

    const logo = metadata.get('logo');
    if (logo === undefined) {
      throw new Error('Logo is missing');
    }

    if (LOGOS[logo] === undefined) {
      throw new Error(`Unknown logo: ${logo}`);
    }

    return {
      comments,
      content,
      date,
      html,
      lang: metadata.get('lang') || 'en',
      logo,
      name,
      readingTime: getApproxReadingTime(content),
      title: metadata.get('title') || '',
      url: `/posts/${name}.html`,
    };
  }

  /**
   * Ideas:
   *   - allow emojis like on GitHub
   *   - make mentions to users clickable
   *   - make sure content style is consistent with article bodies (modulo background color)
   *   - show reactions to comments as well
   *   - apply a bit of styling to make it cleaner
   */
  private async createComments(issue: string): Promise<string> {
    const issueLink = `https://github.com/remusao/remusao.github.io/issues/${issue}`;
    const { status, data } = await this.octokit.issues.listComments({
      issue_number: Number(issue),
      owner: 'remusao',
      repo: 'remusao.github.io',
    });

    if (status !== 200) {
      throw new Error(`could not fetch comments from issue ${issue}: ${status}`);
    }

    const comments: Comment[] = [];
    for (const { body, created_at, html_url, user } of data){
      if (user === null || body === undefined) {
        continue;
      }

      comments.push({
        author: user.login,
        avatar: user.avatar_url,
        body,
        date: created_at,
        humanized: moment(created_at).from(moment()),
        profile: user.html_url,
        url: html_url,
      });
    }

    const avatarPath = join('images', 'comments', 'avatar');
    await fs.mkdir(join(__dirname, '_site', avatarPath), { recursive: true });

    return `
<span>
  <a class="leave-comment-btn" href="${issueLink}" title="${issueLink}" target="_blank" rel="noopener noreferrer">Leave a comment on GitHub</a>
</span>
${
  comments.length === 0
    ? ''
    : `
<details>
  <summary>${comments.length === 1 ? '1 comment' : `${comments.length} comments`}</summary>
  <ul class="comments-list">${(
    await Promise.all(
      comments.map(async ({ author, body, profile, avatar, url, humanized }) => {
        const path = join(
          avatarPath,
          `${avatar.slice(avatar.lastIndexOf('/') + 1, avatar.lastIndexOf('?'))}.jpg`,
        );

        await sharp(await got(avatar).buffer())
          .resize(80, 80)
          .toFile(join(__dirname, '_site', path));

        return `
      <li>
        <div class="comment">
          <div class="meta">
            <img loading="lazy" class="avatar" alt="GitHub avatar from user ${author}" src="/${path}" width="40" height="40"/>
            <a class="author" href="${profile}" title="${author}" target="_blank" rel="noopener noreferrer">${author}</a>
            <span> commented </span>
            <a class="date" href="${url}" title="${url}" target="_blank" rel="noopener noreferrer">${humanized}</a>
          </div>

          <div class="content">${await addEmojis(this.purifyDOM(md.render(body)))}</div>
        </div>
      </li>
    `;
      }),
    )
  ).join('\n')}</ul>
</details>
  `
}
`;
  }
}

(async () => {
  const generator = new Generator();
  await generator.init();

  // Clean-up
  rimraf.sync('./_site');
  await fs.mkdir('./_site/posts', { recursive: true });

  // Copy assets to output folder
  new TreeSync('images', '_site/images').sync();
  new TreeSync('snippets', '_site/snippets').sync();
  new TreeSync('experiments', '_site/experiments').sync();
  new TreeSync('node_modules/katex/dist', '_site/katex').sync();

  // Generate all posts + index.html + rss feeds
  await Promise.all(glob.sync('./posts/*.md').map((path) => generator.generate(path))).then(() =>
    Promise.all([
      generator.generateIndex(),
      generator.generateRSS(),
    ])
  );

  if (ci === false) {
    // Start watching for changes generating
    chokidar.watch('./posts/*.md', { persistent: ci === false }).on('change', async (path) => {
      await generator.generate(path);
      await generator.generateIndex();
      await generator.generateRSS();
    });

    // Start serving site locally
    const app = express();
    app.use(compression());
    app.use(serveStatic('./_site'));
    console.log('serving http://localhost:8080');
    app.listen(8080);
  }
})();
