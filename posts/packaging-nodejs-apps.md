---
title: Packaging Node.js apps the easy way
date: 2018-03-04
logo: v8
lang: en
issue: 10
---

*TL;DR* in this article we demonstrate how to:

* Create a minified bundle for a Node.js application (using Webpack 4).
* Create a self-contained executable (using Nexe).
* Create a Docker containers to run the application.

...without any configuration file!

Packaging, bundling, deploying Node.js applications can get very tricky
when you are faced with the diversity of tools that can be used. Even
when tools have been chosen, configuration is often non-trivial.

In this post we will see that packaging a Node.js application does not have to
be difficult or confusing. By sticking to a minimal set of tools and using just
the necessary configuration, we will be able to create the following artifacts:
minified bundle, self-contained executable, Docker image.

All the code from this article can be found [on GitHub](https://github.com/remusao/node-app-packaging-template).


## The Application

Our demonstration application is performing a bandwidth test with
`fast.com`, using `jsdom` to avoid having to resort to a headless
browser. The output looks like the following:

```js
$ node index.js
Starting bandwidth test...
~ Speed 0
~ Speed 7.9 Mbps
~ Speed 35 Mbps
~ Speed 60 Mbps
~ Speed 60 Mbps
~ Speed 64 Mbps
~ Speed 65 Mbps
~ Speed 67 Mbps

= Speed 66 Mbps
```

The implementation details are not really relevant, but long story
short; `jsdom` is loading the URL `https://fast.com`. We then display the
bandwidth estimation every second until the test is completed.

## Bundling with Webpack

In the past, I found Webpack to be a bit cumbersome to use, even for
simple use-cases. With version 4, I was delighted to see that they now
provide meaningful defaults and simple options. In our case, it is not
even necessary to have a configuration file, which is great!

Producing a minified bundle is as simple as:
```sh
$ webpack index.js --output bundle.min.js --mode production --target node
```

That's it! This command will create a new file: `bundle.min.js` in the
current directory. Since the `--target` is Node.js, we can invoke it
directly: `node bundle.min.js`.

If you want faster builds as well as a watch-mode, you can use the following
variations of the previous command:

```sh
$ webpack index.js --output bundle.min.js --mode development --target node
$ webpack index.js --output bundle.min.js --mode development --watch --target node
```

Yes, it's *that easy*!

## Compiling into a Single Executable

We could have stopped at our minified bundle, but what if we could get
a self-contained executable packaging our application as well as all
dependencies? And by dependencies, we really mean: Node.js itself and
everything needed for the runtime.

It's possible thanks to [Nexe](https://github.com/nexe/nexe), which
provides a way to *compile* your Javascript application along with a
Node.js runtime in a single executable. Neat.

With our already existing bundle `bundle.min.js`, a single command is needed to
create the executable:
```sh
$ nexe bundle.min.js -t alpine-x64-8.9.3 -o app
```

What we're saying here is that our `bundle.min.js` should be compiled with a
Node.js runtime version 8.9.3 compiled for Alpine Linux x64 (which will be
useful in a moment to create a Docker image). You can also check the [exhaustive list of all
possible targets](https://github.com/nexe/nexe/releases/tag/v2.0.0-rc.17)

You should now have a new executable in your current folder: `app`. It
can be started by invoking it like any other command:

```sh
$ ./app
```

## Building Docker Images

And now the cherry on the cake! Although you can already easily ship your
self-contained application created using Nexe, why not create a Docker
container out of it? This can be achieved with the following `Dockerfile`:

```sh
FROM alpine:3.7
COPY ./app .
```

Building:
```sh
$ docker build -t app .
```

The resulting image should weigh around 44MB. And here is how to run the
application:
```sh
$ docker run app ./app
Starting bandwidth test...
...

= Speed 66 Mbps
```

## Final Thoughts

I will not pretend that the tools described here are able to handle
all use-cases (or that they are the only possible tools; there are a
plethora!), far from it. It is mostly restricted to simple Node.js
applications (with dependencies). But this is probably a nice starting
point for more complex situations, for which you might need to create a
proper configuration file with more bells and whistles.

The ease with which complex tasks can be handled with simple commands and
almost no configuration also shows that the Node.js/Javascript ecosystem is
maturing! Although there is still a lot of fragmentation, it's really nice to
observe and benefit from all these improvements.
