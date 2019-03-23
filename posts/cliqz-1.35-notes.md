---
title: What's Exciting in Cliqz 1.35
date: 2019-03-22
logo: cliqz
lang: en
---

[Cliqz 1.35](https://cliqz.com/en/download) is just being released as I write
these lines and there is a lot to talk about in there: performance, more
privacy, more polish. Let me share a few of the things that make me excited!

## Privacy

There is continuous work to make sure Cliqz users always get the best privacy
protection. In this release, there were three major improvements when it comes
to privacy: human proxy network, a more efficient adblocker and better cookie
retention policies.

* **Human Proxy Network** (or, HPN), allows our users to stay anonymous when
  the browser contacts our backend (e.g.: for search or telemetry). This is the
  heart of our *privacy by design approach*, where you don't even need to trust
  us to not do bad things: we simply don't know anything personal about you. In
  this case, we now use a *third-party VPN provider to also hide any
  network-level identifiers like IPs*. We were already providing this
  protection before but we were operating the network of proxies ourselves.
  What I'm especially excited about is that now, there is *no way Cliqz can
  learn anything about who you are using your IP* (because of the third-party
  VPN relaying the messages); and there is also no way the third-party VPN
  provider can learn anything about you, because all messages are encrypted
  client-side!

* **Adblocker**. We wrote about it recently in our [performance study](https://whotracks.me/blog/adblockers_performance_study.html)
  in the context of Manifest v3; *Cliqz has the fastest and most memory
  efficient adblocker around* and we are very proud of it. In this release we
  made sure that the latest improvements are included in the browser: blocking ads
  was never faster!

<figure>
<a href="/images/cliqz_1.35/adblockerSpeed.png">
![Adblock Speed](/images/cliqz_1.35/adblockerSpeed.png){width=100%}
</a>
<figcaption>Adblocker Speed</figcaption>
</figure>

* **Better cookies retention policies**. We're constantly fine-tuning our
  Anti-tracking as well as cookies retention policies. In a nutshell, we make
  sure that cookies cannot be used to track you by limiting their life-time
  depending on if they are needed for a service/website you are using or not.

<figure>
<a href="/images/cliqz_1.35/cookies.jpg">
![Cookies Retention](/images/cliqz_1.35/cookies.jpg){width=70%}
</a>
<figcaption>Cookies Retention</figcaption>
</figure>

## Performance

We're pushing super hard for our mobile products to offer the best possible
experience even on slow devices. This means that usability, privacy protection
and performance need to be great. There was a lot of work on these aspects in
the last few months and because most of our core features share the same
codebase across products, this impacts positively Cliqz desktop browser as
well.

* **Faster URL parsing**. To make sure no identifiers or ads slip through when
  you browser the web, Cliqz needs to analyze and filter *a lot* of URLs (to
  load a single page, it can go up to hundreds of network requests). Because
  this represents a lot of work, it was a good candidate for optimization. My
  co-worker [sammacbeth](https://twitter.com/sammacbeth) did a fantastic job
  and was able to replicate the API of the native URL parser provided by the
  browser: except it's now *4-5x faster*. This means users get the same great
  privacy protection, but the browser sweats less for it!

* Along those lines, we made sure that the processing of requests in the
  extension (not just parsing the URLs) is as fast as possible. We removed lots
  of friction so that *when an ad needs to get blocked, or a fingerprint needs
  to be removed, it happens as fast as possible* and more resources can be
  dedicated to load pages faster. This also greatly improves the experience on
  mobile!

## What Else?

There were a lot of other changes to polish the overall Cliqz experience. For
example: the weather smart Cliqz is now... much smarter and will display very
detailed information about the forcasts (temperatures, wind, etc.)!

<figure>
<a href="/images/cliqz_1.35/weather.png">
![Weather Smart Cliqz](/images/cliqz_1.35/weather.png){width=100%}
</a>
<figcaption>Advanced Weather Smart Cliqz</figcaption>
</figure>

The estimation of time saved on Freshtab now takes into account the time saved
when using Cliqz results directly in the dropdown instead of going to a search
result page. This comes as an addition to the time saved by blocking ads or
trackers.

<figure>
<a href="/images/cliqz_1.35/timeSaved.png">
![Weather Smart Cliqz](/images/cliqz_1.35/timeSaved.png){width=100%}
</a>
<figcaption>Updated Time Saved on Freshtab</figcaption>
</figure>

## What's Next?

I am also super excited with the changes coming to mobile. We've been doing *a
lot* of performance work to ensure that our mobile browser can run at full
speed while enabling all privacy protections, even on very slow devices (in
fact, we're even more aggressive when blocking ads and trackers!).

In the next release, we have more coming: the infrastructure distributing
adblocker rules has been revamped and allows most of the heavy lifting of
parsing the rules and initializing the engine to be performed on the backend,
which means much faster updates for clients as well as drastically reduced data
usage (we're talking of at least one order of magnitude less)! Stay tuned.
