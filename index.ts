import fs from 'fs';
import { join } from 'path';

import Octokit from '@octokit/rest';
import chokidar from 'chokidar';
import compression from 'compression';
import createDOMPurify from 'dompurify';
import express from 'express';
import glob from 'glob';
import hljs from 'highlight.js';
import { minify } from 'html-minifier';
import { JSDOM } from 'jsdom';
import marked from 'marked';
import serveStatic from 'serve-static';

const ci = process.argv[process.argv.length - 1] === '--ci';

const blogDomain = 'https://remusao.github.io/';
const blogTitle = 'Simplex Sigillum Veri';
const blogDescription = blogTitle;

// TODO - fix + cleanup css
// TODO - re-architecture source code
// TODO - support KaTex?
// TODO - consider using JSX for HTML

interface Comment {
  author: string;
  body: string; // raw markdown
  date: string;
  url: string;
}

function createButtons(title: string, url: string): string {
  return `<ul>
  ${[
    {
      alt: 'Share on Facebook',
      href: `https://www.facebook.com/sharer/sharer.php?u=${url}&t=${title}`,
      img: 'Facebook.svg',
    },
    {
      alt: 'Tweet',
      href: `https://twitter.com/share?url=${url}&text=${title}&via=Pythux`,
      img: 'Twitter.svg',
    },
    {
      alt: 'Add to Pocket',
      href: `https://getpocket.com/save?url=${url}&title=${title}`,
      img: 'Pocket.svg',
    },
    {
      alt: 'Submit to Hacker News',
      href: `https://news.ycombinator.com/submitlink?u=${url}&t=${title}`,
      img: 'HackerNews.svg',
    },
    {
      alt: 'Submit to Reddit',
      href: `https://www.reddit.com/submit?url=${url}&title=${title}`,
      img: 'Reddit.svg',
    },
  ]
    .map(({ href, alt, img }) => {
      return `
        <li>
          <a href="${href}" title="${alt}" target="_blang" rel="noopener noreferrer">
            <img alt="${alt}" src="/images/social_flat_rounded_rects_svg/${img}"></img>
          <a/>
        </li>
      `;
    })
    .join('\n')}
  </ul>`;
}

const LOGOS: {
  [name: string]: string;
} = {
  adblock: '/images/adblocking.png',
  aws: '/images/aws.png',
  ccc: '/images/ccc.png',
  cliqz: '/images/cliqz.png',
  cpp: '/images/cpp.png',
  hashcode: '/images/hashcode.png',
  haskell: '/images/haskell.png',
  html5: '/images/html5.png',
  julia: '/images/julia.png',
  learning: '/images/learning.png',
  linux: '/images/tux-logo.png',
  ocaml: '/images/ocaml.png',
  pi: '/images/pi.png',
  python: '/images/python.png',
  raspberry: '/images/raspberry-pi.png',
  synacor: '/images/synacor.png',
  v8: '/images/v8.png',
  xmonad: '/images/xmonad.png',
};

function getLogo(name: string | undefined): string {
  const defaultLogo = '/images/favicon.ico';
  if (name === undefined) {
    return defaultLogo;
  }

  return LOGOS[name] || defaultLogo;
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
}

class Generator {
  private readonly purifyDOM: (_: string) => string;
  private readonly styles: string;
  private readonly octokit: Octokit;
  private readonly posts: Map<string, Post>;

  private indexGenerationTimer: NodeJS.Timeout = setTimeout(() => 0, 0);

  constructor() {
    // Initialize posts
    this.posts = new Map();

    // Initialize markdown to HTML converter
    marked.setOptions({
      breaks: false,
      gfm: true,
      smartLists: true,
      smartypants: true,
      highlight(code, lang) {
        return hljs.highlight(lang, code).value;
      },
    });

    // Initialize DOM sanitizer
    const DOMPurify = createDOMPurify(new JSDOM('').window);
    this.purifyDOM = (html) => DOMPurify.sanitize(html);

    // Initialize stylesheets
    this.styles = [
      // TODO - replace use of `glob` by static list?
      ...glob.sync('./styles/*.css').map((path) => fs.readFileSync(path, { encoding: 'utf-8' })),
      fs.readFileSync(join(__dirname, 'node_modules/highlight.js/styles/github.css'), {
        encoding: 'utf-8',
      }),
    ].join('\n\n');

    // Initialize octokit for GitHub APIs
    const token = process.env.GITHUB_TOKEN;
    this.octokit = new Octokit({
      auth: token,
    });
  }

  public async generate(path: string): Promise<void> {
    const name = getBaseName(path);
    console.log(`Generating ${name}...`);

    try {
      const content = await fs.promises.readFile(path, { encoding: 'utf-8' });
      const post = await this.parsePost(name, content);
      this.posts.set(name, post);

      const html = this.renderPost(post);
      const outputFilePath = `posts/${name}.html`;
      const outputFilePathLegacy = `posts/${post.date}-${name}.html`;

      await Promise.all([
        fs.promises.writeFile(outputFilePath, html, { encoding: 'utf-8' }),
        fs.promises.writeFile(outputFilePathLegacy, html, { encoding: 'utf-8' }),
      ]);

      // Make sure we throttle calls to `generateIndex(...)`
      clearTimeout(this.indexGenerationTimer);
      this.indexGenerationTimer = setTimeout(() => this.generateIndex(), 500);
    } catch (ex) {
      console.log('could not generate', path, ex);
    }
  }

  private async generateIndex(): Promise<void> {
    console.log('generating index.html');

    const indexEntry = (post: Post): string => `
    <li>
    <img class="logo" src="${post.logo}"></img>
    <a href="${post.url}">${post.title}</a>
    <span> - ${formatDate(post.date)}</span>
    </li>
    `;

    const indexYear = (year: number, sameYearPosts: Post[]): string => `
    <div>
    <h2>${year}</h2>
    <ul class="index">
    ${sameYearPosts.map(indexEntry).join('\n')}
    </ul>
    </div>
    `;

    // @ts-ignore
    const posts = Array.from(this.posts.values()).sort((p1: Post, p2: Post) => p2.date - p1.date);

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
      'Posts',
      `
        <div class="blogIndex">
        ${postsSortedByYear.map((group) => indexYear(group[0], group[1])).join('\n')}
        </div>
        `,
    );

    await fs.promises.writeFile('index.html', index, { encoding: 'utf-8' });
  }

  private wrapHtml(title: string, html: string): string {
    return minify(
      `
<!DOCTYPE html lang="en" dir="ltr">
<head>
  <meta description="${blogDescription}">
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">
  <meta httpEquiv="x-ua-compatible" content="ie=edge">

  <link rel="apple-touch-icon" href="/images/favicon.ico"></link>
  <link rel="icon" type="image/x-icon" href="/images/favicon.ico"></link>

  <title>${title}</title>
  <style>${this.styles}</style>
  </head>
  <body>
    <header>
      <h1>
        <a href="../"><span>Simplex Sigillum Veri</span></a>
      </h1>
    </header>
    <main>${html}</main>
  </body>
</html>`,
      {
        collapseWhitespace: true,
        minifyCSS: true,
        minifyJS: true,
        removeComments: true,
        removeOptionalTags: false,
        removeRedundantAttributes: true,
        removeScriptTypeAttributes: false,
        removeTagWhitespace: true,
        useShortDoctype: true,
      },
    );
  }

  private renderPost(post: Post): string {
    return this.wrapHtml(
      post.title,
      `
<h1>${post.title}</h1>
<section class="header">
  <div>${formatDate(post.date)} | <em>Reading time: ~${post.readingTime} minutes</em></div>
</section>
<article>
  ${post.html}
  ${post.comments}
</article>
<footer>
  <div class="share">
    ${createButtons(post.title, `${blogDomain}${post.url}`)}
  </div>
</footer>
`,
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
    const html = marked(content.slice(endOfMetadata + 3));

    const issue = metadata.get('issue');
    const comments = issue === undefined ? '' : await this.createComments(issue);

    const rawDate = metadata.get('date');
    const date = rawDate === undefined ? new Date() : new Date(rawDate);

    return {
      comments,
      content,
      date,
      html,
      logo: getLogo(metadata.get('logo')),
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

    const comments: Comment[] = data.map(({ body, created_at, url, user }) => {
      return {
        author: user.login,
        body,
        date: created_at,
        url,
      };
    });

    return `
  <div class="comments">
    <span class="commentHeader">
      <button id="showCommentsButton" class="showComments">${
        comments.length === 1 ? '1 comment' : `${comments.length} comments`
      }</button>
      <script>
        var button = document.getElementById('showCommentsButton');
        button.onclick = function() {
          var div = document.getElementById('commentsList');
          if (div.style.display !== 'none') {
            div.style.display = 'none';
          }
          else {
            div.style.display = 'block';
          }
        };
      </script>
    </span>

    <span class="leaveComment">
      <a href="${issueLink}" title="${issueLink}" target="_blank" rel="noopener noreferrer">Leave a comment on Github</a>
    </span>

    <ul id="commentsList" style="display: none;">${comments
      .map(
        ({ author, body, date, url }) => `
        <li class="comment">
          <div>
            <span class="meta">
              <span class="author">${author}</span>
              <span> - </span>
              <a href="${url}" title="${url}" target="_blank" rel="noopener noreferrer">${date}</a>
            </span>

            <div class="content">${this.purifyDOM(marked(body))}</div>
          </div>
        </li>
      `,
      )
      .join('\n')}</ul>
  </div>
  `;
  }
}

const generator = new Generator();

// Start generating
chokidar.watch('./posts/*.md', { persistent: ci === false }).on('all', (event, path) => {
  if (event === 'add') {
    generator.generate(path);
  }
});

// Serve current folder
if (process.env.SERVE) {
  const app = express();
  app.use(compression());
  app.use(serveStatic('.'));
  console.log('serving http://localhost:8080');
  app.listen(8080);
}
