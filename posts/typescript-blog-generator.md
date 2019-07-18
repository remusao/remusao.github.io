---
title: Static Blog Generator in TypeScript
date: 2019-07-19
logo: typescript
lang: en
---

> Oups, I did it again...

A while ago I was joking with [@sebadzia](https://twitter.com/sebadzia) about
the fact that if you are not re-writting your blog generation mechanism from
scratch every *N* posts, then you're doing it wrong. It appears that I'm
actually living by these words and have been re-implementing my own blog over
and over again; maybe in the hope that it would motivate me to write more. This
blog *became an end instead of a mean* to communicate things.

Here are the 6 engines I've used over the years (and this does not even account
for the different themes that I used with each of them):

* [WordPress](https://wordpress.org/)
* [Jekyll](https://jekyllrb.com/)
* [Pelican](https://blog.getpelican.com/)
* [Hakyll](https://jaspervdj.be/hakyll/)
* [Custom generator in Haskell](https://github.com/remusao/remusao.github.io/tree/diy)
* [**Custom generator in TypeScript**](https://github.com/remusao/remusao.github.io/tree/diy-ts)

This means that the blog engine has been changed on average every *8.5* posts.
Not too bad! Maybe the next one will be in Rust?

I think what made me switch this time is the fact that I'm actually enjoying
TypeScript very much lately (I find the type system super neat, and already
liked JavaScript tooling a lot); and I was a bit fed up by the build time of
the Haskell version. The way I did it was most probably sub-optimal as I am by
no mean an experienced Haskell developer. Last but not least, I was on vacation
and had some time to waste...

The TypeScript generator is still rough around the edges but it's working and
allows me to build the site, watch changes, and serve locally in development
mode.

I rely on the following packages to do the heavy lifting:

* [@octokit/rest](https://www.npmjs.com/package/@octokit/rest) to interact with GitHub APIs (used to fetch static comments)
* [chokidar](https://www.npmjs.com/package/chokidar) to watch for files changes
* [express](https://www.npmjs.com/package/express) to serve blog locally
* [dompurify](https://www.npmjs.com/package/dompurify) to sanitize final HTML
* [highlight.js](https://www.npmjs.com/package/highlight.js) to provide syntactic coloration for code blocks in posts
* [html-minifier](https://www.npmjs.com/package/html-minifier) to minimize HTML
* [marked](https://www.npmjs.com/package/marked) to convert markdown to HTML

Overall I'm fairly happy with this setup. The generator currently fits into a
single source file (which should probably be split at some point) and generation
is pretty fast: *~50* seconds from scratch (which includes installing all npm
dependencies, generating posts and fetching comments from GitHub). Generating
posts only takes less than 10 seconds, and most of the time is spent waiting
for comments to be fetched from GitHub issues.

There are a few things I'd like to try or investigate in the future to improve:

* consider using JSX (with React in pure server-side rendering mode)
* consider writing CSS in TypeScript as well with typing
* add support for inlined math using KaTex (which I do not need at the moment)
* improve comments display with better rendering and support for reactions and emojis

And of course, I will try my best to write more content here in the future!
