---
title: Fully Static Comments
date: 2017-11-19
lang: en
logo: html5
issue: 3
---

I'd like to give a quick update about the comments
on this blog. I already mentioned in a [previous post](/posts/state-of-static-blogs.html)
that I wanted to experiment with fully static comments for my blog. By
fully static I mean that comments are directly integrated in the HTML of
each page, at generation time. It requires to generate pages with new
comments regularly but I think it's a good trade-off.

I'd like to give an overview of how it works, and how it looks in practice.

## How does it Work?

1. Comments will be posted and hosted on *Github Issues*.
2. Each `post`'s original `Markdown` document has an optional `issue`
   attribute in its metadata; when it is specified, the static blog
   generator will fetch the comments of the specified issue every time we
   build the site.
3. Issues are directly integrated in the HTML of the page (at the bottom) and
   are collapsed by default.

## Pros

I see several advantages with this system:

1. It's fast because it's part of the page's HTML.
2. The look and feel of the page is consistent.
3. It does not require any third-party Javascript to work (so no tracking...).
4. It leverages Github, which is already used to host the blog anyway, so why
   not use the `issues` as well.

## Cons

Obviously it's not perfect because:

1. People need a Github account to post comments: but I expect a lot of people
   do, and if they don't, there are a lot of ways to discuss blog posts on the
   Internet: Reddit, HackerNews, Twitter; and they all have their sharing button
   at the bottom of the page!
2. It takes a bit of time to refresh the comments (currently `Travis` will
   deploy the posts at least once a day + every time I do a commit), but they
   can always be seen in real time on the `issue` itself.
3. It might not scale very well with a lot of comments, but I will be happy to
   figure out a solution for this problem when it happens with this blog!

## How it Looks

This post is already equipped with the static comments. I created a few of them
to demonstrate the look and feel of them. You can expend the comments by
clicking the first link on the left `N comments`, or post a new one by clicking
on `Leave a comment on Github` (you will be redirected on the correct `issue`).

Feel free to add more! I'd love to get some feedback on this.
