---
title: State of Privacy in Statically Generated Blogs
date: 2017-10-08
logo: html5
lang: en
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

<figure>
<a href="/images/static-site-ghostery-trackers.png">
![Network Monitor](/images/static-site-ghostery-trackers.png){width=60%}
</a>
<figcaption>Advertisers joined the party...</figcaption>
</figure>

If you wonder why there are so many, let's just say that once you allow
a third-party script to execute on your page, you loose control over what
happens next. It might be that any script you include on your page,
will also inject a script from another company, which might make a few
requests as well, etc. If you want to see for yourself, go on a random site
having Disqus and Google Analytics (or your own blog if you have one), and open
the *Network Monitor* while loading the page. You will see all the resources
fetched (which can be scary...).

<figure>
<a href="/images/network-monitor-tracking.png">
![Network Monitor](/images/network-monitor-tracking.png){width=70%}
</a>
<figcaption>74 requests seems like a lot for a static page.</figcaption>
</figure>

And they all send unsafe data (unique identifiers) back! I agree that
features like comments and math rendering are sometimes unavoidable, but
there should be a way to do it without all the tracking.

Unfortunately, the alternatives (if there is one at all), represent more
work to integrate and use.

For my own blog I decided to first strip all superfluous external
dependencies (meaning basically anything but the content of the blog),
and then find a way to introduce extra features one by one if needed.

It means that currently, there are no comments and no analytics report
on the number of readers/visits. But there are some promising solutions.

## Comments

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
    * Last but not least, I seriously consider *embedding the comments
    statically in the page* (since Github's API returns the markdown, it
    would allow to style the comments like the rest of your blog). That
    would require you to fetch all the comments when you generate your
    page (yes it takes extra time, but I would rather pay this price
    once, instead of making users do it at every page load). Then there
    could be some automated task scheduled using Travis to re-generate
    the blog (or only the pages with new comments) every time there is a
    new comment. I admit that this does not scale for a popular blog
    with a lot of comments, but most of the times comments are scarce,
    so this solution would work pretty well!
* [Isso](https://posativ.org/isso/), which requires self-hosting.
* There is also *Talk*, from the [Mozilla Coral Project](https://coralproject.net/about.html). I'm not sure if it can be used on any static website, but it might be something worth investigating.


That does not seem to exist yet, but a totally distributed
commenting system would be an ideal fit here (using something like
[IPFS](https://ipfs.io/)). There are some discussions about that on this
[reddit thread](https://www.reddit.com/r/ipfs/comments/4om8c0/how_to_create_a_fairly_decentralized_commenting/).

I'm not sure yet if I will add one of those on this blog, since I don't
really have enough readers to justify the extra complexity (and it's
still possible to leave some feedback on Github or Twitter if you really
want to). But if I were to choose one solution, I would probably go for
the Github Issues solution, which seems to fit nicely with a blog hosted
on Github Pages.

## Math Rendering

One solution for privacy-preserving math rendering is to render the
equations at build-time and then embed them in the html as data urls.
That might not be the perfect solution, but it actually works pretty
well, and does not require any third-party resource. I used the
[latex-formulae](https://github.com/liamoc/latex-formulae) plugin of
Hakyll, which worked pretty smoothly!

For example:

$$ \sum^{42}_{i=0} i^2 $$

But it can also be inlined like that $\sum^{42}_{i=0} i^2$.

## Analytics

I have to admit that I totally dropped this feature for now. Ideally,
Github would provide a very basic visit counter for each page of your
blog by default.

Otherwise, there are some nicer alternatives to *Google Analytics* such as:

* [Piwik](https://piwik.org/) -- probably the most serious alternative.
* [Green Tracker](https://github.com/cliqz-oss/green-analytics) -- still experimental but demonstrates some very promising capabilities, and it requires self-hosting as well.

And probably more, although none of them will be as convenient/powerful as
Google Analytics (for now at least...), but do you need as much?

## Fonts and styling

This is obviously a very personal choice here, and most people will go with
pre-existing templates or frameworks such as *Bootstrap*.

I took the path of more simplicity and tried to roll-out my own super minimal
template. I also tried to use only fonts available widely (and provide
fall-backs in case a font is not present on the system).

## Social sharing

It's nice to have a few buttons to share an article quickly on Twitter,
Facebook or other social network using the social sharing widgets. The
downside is that most of the time, you're also including trackers on
your page! (e.g.: Facebook will be able to track users reading your
page).

One option is to insert static buttons on your page, instead of the official
widgets. More information can be found there:

* [Creating static social share buttons](https://www.savjee.be/2015/01/Creating-static-social-share-buttons/)

It looks something like that:


<div style="width:70px; height:55px; background-color:#5BC2F0;display:inline-block;margin:5px;text-align:center;box-sizing:border-box">
<a target="_blang" href="https://twitter.com/intent/tweet?text=State%20of%20Privacy%20in%20Statically%20Generated%20Blogs&url=https%3A%2F%2Fremusao.github.io%2Fposts%2F2017-10-08-the-state-of-static-blogs.html">
<p>
<img class="icon" src="/images/static/social-twitter.svg" style="height:25px">
</p>
</a>
</div>

## Conclusion

The main down-side of this path, is that most of the time you will be on your
own. You might get lucky and find a plugin for your favorite static blog
generator (Jekyll, Pelican, Hugo, Hakyll), but if there is nothing, you will
have to come up with a solution from scratch.

At the end, I think it's possible to get a mostly static site, with
no or very few external dependencies. This post was probably loaded
using only one request (the main document + the example images), which
includes everything needed. Yes it's super minimal, and it could look
better, and it could use some comments, but most personal static
blogs don't need as much, and if they do, and you're willing to spend
the extra time -- having totally private analytics, comments or math
rendering, could make for a very interesting and rewarding side-project!
