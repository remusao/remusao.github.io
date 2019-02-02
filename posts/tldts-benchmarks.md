---
title: Fast Domains and URLs Parsing With Tldts
date: 2019-02-02
logo: v8
lang: en
---

For quite some time now I've been working on
[**tldts**](https://github.com/remusao/tldts/) ([**npm**](https://www.npmjs.com/package/tldts)), an *extemely fast*,
*feature-full*, *battle-tested* JavaScript library for domain parsing. It
allows to answer questions such has:
- What's the hostname of an URL?
- What's the registrable domain of an URL, a hostname or an email address?
- What's the sub-domain of a domain?
- etc.

What puts [**tldts**](https://github.com/remusao/tldts/) appart from other libraries is:

* It is **much faster** than alternatives, allowing to parse between
  **1 and 2 million** domains per second (that's **up to 1000 times** fastest
  than other popular libraries).
* It is more feature-full, supporting *IPs detection*, *domain validation* and *complex URLs parsing*.
* It offers the smallest bundles, in both `cjs`, `esm` and `umd` formats; it runs *anywhere*.
* It is written in *TypeScript* and benefits from 100% test coverage.


Most of the features are made possible by the [public suffix
list](https://publicsuffix.org/) project. But
[**tldts**](https://github.com/remusao/tldts/) offers some bells and whistle on
top. One of the goals is to be *conveniant* to use and as fast as it gets. Some
libraries require you to provide already-valid hostnames, but
[**tldts**](https://github.com/remusao/tldts/) has no such constraints and will
happily parse complex URLs, as well as already-extracted hostnames. The best
part is that this does not come with any over-head!

```javascript
const tldts = require('tldts');

// Retrieving hostname related informations of a given URL
tldts.parse('https://remusao.github.io/posts/tldts-benchmarks.html');
// { domain: 'github.io',
//   hostname: 'remusao.github.io',
//   isIcann: true,
//   isIp: false,
//   isPrivate: false,
//   publicSuffix: 'io',
//   subdomain: 'remusao' }
```

In this post I'd like to share some results of a performance comparison against
other available JavaScript libraries offering the same kind of features. We
will see that they vary greatly in terms of performance, ease of use or
features. In fact, we observed that `tldts` is up to **1000 times** faster than
some of the other libraries!

Before presenting the results, a few words about the instrumentation and what
was measured. We aim at comparing the libraries in the following aspects:
1. Features
2. Performance (in terms of operations per second)
3. Loading time (time it took V8 to load the bundle)
4. Memory used

All the measurements were performed in the following environment:
* `Node.js` version **11.6.0**
* Hardware: X1 Carbon 4th with i7-6600U CPU and 16GB of RAM

And now the list of the contenders:

  * [**tldts**](https://www.npmjs.com/package/tldts)
  * [psl](https://www.npmjs.com/package/psl)
  * [tld.js](https://www.npmjs.com/package/tldjs)
  * [parse-domain](https://www.npmjs.com/package/parse-domain)
  * [haraka-tld](https://www.npmjs.com/package/haraka-tld)
  * [uBlock's publicsuffixlist.js](https://github.com/gorhill/uBlock/blob/master/src/lib/publicsuffixlist.js)

In the results you will also see `tldts-experimental` mentioned. It is a
probabilistic data-structure implementing the exact same features as `tldts`
but using *much less memory*, *loading instantly* and offering *even higher
performances*. It can be used in contexts with very constrained hardware
capabilities such as mobiles.

Now let's now dig into the results!

## Feature Matrix

The following features are considered:
* Is there `IDNA` support? Does the library support inputs with unicode such as *中国*?
* Does the library accept complex `URLs` as input, or is it necessary to extract the hostname before-hand?
* Is the library able to detect if the given input is an `IP` address? This is important, otherwise `getPublicSuffix('192.168.0.1')` would return `1`!
* Does the library allow you to extract `domain` and `public suffix`?
* Does the library support ICANN/Private sections of the public suffix list? Can they be disabled individually?
* Will the public suffix rules be `shipped` with the library, or do they need to be fetched separately?


| Library                 | IDNA | URLs | IPs | getDomain | getPublicSuffix | ICANN/Private | Ships lists |
|:----------------------- | ----:| ----:| ---:| ---------:| ---------------:| -------------:| -----------:|
| **tldts**               |    X |    X |   X |         X |               X |             X |           X |
| tld.js                  |    X |    X |   X |         X |               X |             X |           X |
| psl                     |    X |      |     |         X |               X |               |           X |
| parse-domain            |    X |    X |     |         X |               X |             X |           X |
| haraka-tld              |    X |    X |     |         X |               X |               |           X |
| uBlock publicsuffixlist |    ? |      |     |         X |               X |               |             |

# Performance

Here we measure the performance of three common operations offered by domain
parsing library: *getting the public suffix* of a hostname, *getting the
domain* (tld + sld) and *getting the subdomain*.

A few notes about this benchmark:
* The inputs used are always already valid hostnames (no URLs, although some
  libraries like tldts support it). You can find the list of inputs there:
* The selection of hostnames can be seen in [bench_performance.js](https://github.com/remusao/tldts/blob/master/comparison/bench_performance.js#L12)
  and was selected to contain a mix of non-existing suffixes, ICANN rules,
  private rules as well as wildcards and exceptions.
* All hostnames were ASCII (puny-encoded if needed before-hand)
* All libraries were used in their default setup (no option given, with the
  exception of `tldts-no-parse` which runs `tldts` disabling the parsing phase
  and assuming that the input is already a valid hostname, to match the
  behavior of other libraries).

The results are expressed in terms of operations per second (where each
operation is calling the function once on a hostname).

| Library                | getPublicSuffix | getDomain     | getSubdomain |
|:---------------------- | ---------------:| -------------:| ------------:|
| **tldts-experimental** |       1 898 446 | 1 690 572     |    1 615 166 |
| tldts no parsing       |       1 780 469 | 1 515 703     |    1 502 692 |
| **tldts**              |   **1 280 063** | **1 134 956** |    1 125 362 |
| tld.js                 |       1 141 414 | 1 049 180     |    1 125 362 |
| ublock publicsuffix    |         620 816 |   567 664     |            ? |
| parse-domain           |         554 355 |   528 217     |      551 008 |
| haraka-tld             |               ? |   105 321     |            ? |
| psl                    |           1 654 |     1 693     |        1 673 |

Here we see that the performance varies a lot between libraries, for the same
operations. `tldts` is **1000** faster than `psl`, which is the most popular
library.



# Memory Usage

Here we estimate the memory used by each library. The measurements are done
using the [bench_memory.js](https://github.com/remusao/tldts/blob/master/comparison/bench_memory.js)
script, which loads each file ten times and measure the average memory usage
before and after GC using `process.memoryUsage()`. The result are then compared
to a reference memory usage computed in the same way using `noop_test.js` which
does not import anything.

| Library                | Before GC  | After GC   |
|:---------------------- | ----------:| ----------:|
| **tldts-experimental** |  **461KB** | **229 KB** |
| parse-domain           |   2.579 MB |    1.310MB |
| psl                    |   2.199 MB |    1.537MB |
| tldjs                  |   2.621 MB |    1.714MB |
| tldts                  |   3.094 MB |    1.792MB |
| UBlock publicsuffix    |   4.529 MB*|    2.399MB |
| haraka-tld             |   4.405 MB |    2.595MB |

(*) The memory of uBlock cannot be estimated correctly as for this
benchmarks the lists were inlined in the source code, which is not how
it's used in production.

# Loading Time

One point of comparison which can make a difference in some contexts (e.g.:
mobile or if the library is embedded in a website) is the loading time of the
bundle itself (or time it takes to parse the code and initialize it). It can
have a big impact if you use the library on very slow devices (like mobiles)
and here again, not all the libraries are equal.

The benchmark code can be found in [bench_startup.sh](https://github.com/remusao/tldts/blob/master/comparison/bench_startup.sh).
It measures the time it takes to import each of the libraries. The measurements
are performed using the [bench](https://hackage.haskell.org/package/bench) CLI,
looking at the `mean` time returned for each.

Note that this benchmark was performed using the cjs bundle. The performance
might be different in another environment or different bundle (e.g.: UMD in a
browser).

| Library                | Mean (ms) |
|:---------------------- | ---------:|
| Ref (no `require`)     |     48.21 |
| **tldts-experimental** | **47.93** |
| psl                    |     53.77 |
| tld.js                 |     58.74 |
| parse-domain           |     61.96 |
| tldts                  |     64.48 |
| ublock                 |     78.05 |
| haraka-tld             |     84.93 |

Note that some libraries like `ublock` or `haraka-tld` perform some form of parsing
of the rules at loading-time, which incurs an initial cost when importing the
library.

# Bundles

Comparison of bundle sizes, when applicable (not all libraries provide bundles):

| Library                | Normal    | Minified  | Gzipped |
|:---------------------- | ---------:| ---------:| -------:|
| **tldts-experimental** | **100KB** |  **94KB** |    38KB |
| tldts                  |  140KB    |  **95KB** |    37KB |
| psl                    |  138KB    |    122KB  |    39KB |
| tld.js                 |  209KB    |    141KB  |    40KB |
| parse-domain           |      ?    |        ?  |       ? |
| ublock                 |      ?    |        ?  |       ? |
| haraka-tld             |      ?    |        ?  |       ? |

# Dependencies

Here is a comparison of dependencies for each library:

| Library             | Dependencies             |
|:------------------- |:------------------------ |
| **tldts**           | **(none)**               |
| psl                 | punycode                 |
| tld.js              | punycode                 |
| ublock              | punycode                 |
| haraka-tld          | punycode                 |
| parse-domain        | ?                        |


# Conclusion

=> [**Tldts GitHub repository**](https://github.com/remusao/tldts)

=> [**Tldts on NPM**](https://www.npmjs.com/package/tldts)

```sh
npm install tldts
```

If that sounds appealing to you, give it a shot and do not hesite to open
issues for any feedback you might have!
