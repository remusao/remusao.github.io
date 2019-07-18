---
title: Monitoring your Internet Bandwidth with a Raspberry Pi
date: 2018-01-13
logo: raspberry
lang: en
---

After having switched to a new Internet Provider a few months ago, I
was often disappointed by the bandwidth I got (or perceived). It's not
always easy to know if your connection is the problem, or is it Wi-Fi
connectivity? Or the website you are trying to connect to, etc. So to
get a better idea, I decided to create a simple setup to continuously
monitor my connection, using two independent methods that I will
describe below. Before digging into the details, here is how it currently looks
like:

<script>
  function resizeIframe(obj) {
    obj.style.height = obj.contentWindow.document.body.scrollHeight + 'px';
  }
</script>
<iframe
  frameborder="0"
  onload="resizeIframe(this)"
  scrolling="no"
  style="width:100%;height:100%;overflow:hidden;display:block"
  src='../images/bandwidth.html'>
</iframe>

## Setup

* RaspberryPi 3.
* Latest version of Raspbian.
* Direct Ethernet connection to the router.

## Measurements

To get a better idea of the bandwidth (I also measure upload and ping, but that
is not the most important for me, and I'm not sure how robust the measurements
are), I decided to implement two separate methods:

1. The first one is using [speedtest-cli](https://github.com/sivel/speedtest-cli),
  a Python project which allows you to measure you connection using servers from
  [speedtest.net](http://www.speedtest.net/). It is very simple to use and the
  project seems to be pretty mature.
2. For the second measurement, I wanted to use [fast.com](https://fast.com/), a
   service provided by Netflix. I could not find a way to use it easily
   from the command line (and I could not find an easy way to get a
   driver for Selenium for either Firefox or Chromium), but I managed
   to get the measurements using `chromium` headless mode, which is
   available on `raspbian`, so no extra dependency is required!

Here is the current way measurements are performed:

1. The script `speed.py` is triggered from a `watch` command every 30 minutes (it
  runs in a detached `screen`).
2. `speedtest-cli` measurement is triggered.
3. Then we wait for 15 seconds.
4. `fast.com` measurement is triggered.
5. Results are persisted into a local sqlite3 database.

In parallel, the `app.py` runs in another `screen` to allow visualizing
the results stored in the database. This is not mandatory, but it's nice
to see the bandwidth over time (the fluctuations reminds me of Bitcoin
pricing...).

## Results

I have uploaded the scripts I'm using on [GitHub](https://github.com/remusao/bandwidth-monitor)
so that it can be re-used or modified.

It should be noted that I now have more measurements and that both
methods (speedtest and fast.com) yield similar results. Another
interesting thing is that the bandwidth is usually higher than what I
perceive in normal usage. I have currently two hypotheses:

1. It could be that the provider is playing nice with servers used to measure
   bandwidth and artificially increases the bandwidth for those?
2. My Wi-Fi setup might not be optimal, and I need to investigate if changing
   the channel or other settings would improve the situation.
