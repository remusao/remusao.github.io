---
title: The State of Statically Generated Blogs
---

I woke up one morning and realized that my "static blog" (hosted on
Github pages), contained around **14 trackers** on every page. By
tracker I mean a third-party company having some piece of `javascript`
injected on the page. Each tracker would also try to send unsafe data
to their backend (You can observe this behavior using any privacy
extension: Ghostery or Cliqz for example).

The truth is, most static blog embed *Google* for analytics, *Disqus*
for comments, *Mathjax* for Latex rendering, extra fonts, CSS, etc.

This is nice, and it brings many benefits to your blog, but people are
probably not aware of what the consequences are for their readers.
Instead of having a nice static html page containing only the content
you'd like to read, you get third-party scripts tracking you on every
page you visit (my content blocker extension even blocked a few advertising
requests).

As an example, here are some of the advertising companies present on one
page of my previous blog:

* `LiveRamp`
* `Disqus`
* `Facebook`
* `Google`
* `Cloudflare`
* `Narrative.io`
* `BlueKai`
* `Turn Inc.`
* `AppNexus`
* `PubMatic`

If you wonder why there are so many, let's just say that once you allow
a third-party script to execute on your page, you lost control over what
happens next. It might be that any script you include on your page,
will also inject a script from another company, which might make a few
requests as well, etc. If you want to see for yourself, go on a random site
having Disqus and Google Analytics (or your own blog if you have one), and open
the *Network Monitor* while loading the page. You will see all the resources
fetched (which can be scary...).

And they all sent unsafe data (unique identifiers) back! I agree that
features like comments and math rendering are sometimes unavoidable, but
there should be a way to do it without all the tracking.

Unfortunately, the alternatives (if there is one at all), represent more
work to integrate and use.

For my own blog I decided to first strip all superfluous external
dependencies (meaning basically anything but the content of the blog),
and then find a way to introduce extra features one by one if needed.

It means that currently, there are no comments and no analytics report
on the number of readers/visits. But there are some promising solutions.

### Comments

That is one of the most wanted features on a blog, and yet it requires having a
central server to store the comments. *Disqus* comes pretty handy here, with
only one small line of javascript to include on the page to load the entire
commenting system (but it comes with a high price, as we saw above).

Some possible alternatives are:

* Using *Github Issues* to host the comments. Here are a few posts about how that
  can work for you:
    * [Going Static: Episode II — Attack of the Comments](https://mademistakes.com/articles/jekyll-static-comments/)
    * [GitHub hosted comments for GitHub hosted blogs](http://ivanzuzak.info/2011/02/18/github-hosted-comments-for-github-hosted-blogs.html)
    * [Using GitHub Issues for Blog Comments](http://artsy.github.io/blog/2017/07/15/Comments-are-on/)
    * [Hosting comments within issues on Github Pages](http://sean.lane.sh/blog/2016/Hosting_comments_within_issues_on_Github_Pages)
* There is also the [Mozilla Coral Project](https://coralproject.net/about.html), Talk. I'm not sure if it can be used on any static website, but it might be something worth investigating.

That does not seem to exist yet, but a totally distributed commenting
system would be an ideal fit here (using something like [IPFS](https://ipfs.io/)). There are
some discussions about that on this
[reddit thread](https://www.reddit.com/r/ipfs/comments/4om8c0/how_to_create_a_fairly_decentralized_commenting/).

I'm not sure yet if I will add one of those on this blog, since I don't
really have enough readers to justify the extra complexity (and it's
still possible to leave some feedback on Github or Twitter if you really
want to). But if I were to choose one solution, I would probably go for
the Github Issues solution, which seems to fit nicely with a blog hosted
on Github Pages.

### Math Rendering

One solution for privacy-preserving math rendering is to render the
equations at build-time and then embed them in the html as data urls.
That might not be the perfect solution, but it actually works pretty
well, and does not require any third-party resource. I used the
[latex-formulae](https://github.com/liamoc/latex-formulae) plugin of
Hakyll, which worked pretty smoothly!

For example:

$$ \sum^{42}_{i=0} i^2 $$

But it can also be inlined like that $\sum^{42}_{i=0} i^2$.

### Analytics

I have to admit that I totally dropped this feature for now. Ideally,
Github would provide a very basic visit counter for each page of your
blog by default.

Otherwise, there are some nicer alternatives to *Google Analytics* such as:

* [Piwik](https://piwik.org/) -- probably the most serious alternative
* [Isso](https://posativ.org/isso/) -- which requires self-hosting
* [Green Tracker](https://github.com/cliqz-oss/green-analytics) -- still experimental but demonstrates some very promising capabilities

And probably more, although none of them will be as convenient/powerful as
Google Analytics (for now at least...), but do you need as much?

### Fonts and styling

This is obviously a very personal choice here, and most people will go with
pre-existing templates or frameworks such as *Bootstrap*.

I took the path of more simplicity and tried to roll-out my own super minimal
template. I also tried to use only fonts available widely (and provide
fall-backs in case a font is not present on the system).

### Conclusion

At the end, I think it's possible to get a mostly static site, with no or very
few external dependencies. This page was probably loaded using only one request
(the main document), which includes everything needed. Yes it's super minimal,
and it could look better, and it could use some comments, but most personal
static blogs don't need as much, and if they do, and you're willing to spend
the extra time -- having totally private analytics, comments or math rendering,
could make a very interesting and rewarding side-project!
