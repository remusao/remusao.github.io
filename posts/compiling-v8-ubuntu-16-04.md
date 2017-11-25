---
title: Compiling V8 on Ubuntu 16.04
lang: en
logo: v8
date: 2017-11-25
issue: 5
---

While preparing another article about fast hashing and how to use it for very
fast fuzzy matching and approximate set membership tests, I found myself in a
situation where I needed more insight about what optimizations `V8` was
performing under the hood. I wanted to see what `d8` offers in this regard so I
compiled `v8` from source. It was relatively easy, but there are a few things to
consider!

This article assumes you're using
`Ubuntu`, but it should work as well for other platforms. You can check for any
extra steps from the [official wiki](https://github.com/v8/v8/wiki/Building-from-Source).
During the redaction of this article, I also found this [other blog post](http://www.mattzeunert.com/2015/08/19/viewing-assembly-code-generated-by-v8.html)
very useful!

# Building V8 From Source

## Installing depot_tools

The `V8` toolchain requires scripts from [depot_tools](https://www.chromium.org/developers/how-tos/install-depot-tools).

```sh
$ git clone https://chromium.googlesource.com/chromium/tools/depot_tools.git
$ export PATH=`pwd`/depot_tools:"$PATH"
```

## Compiling V8

Now that everything is available in `PATH`, let's clone and compile `V8`:

```sh
$ fetch v8
```

This might take a while. It will create a new `v8` folder containing the latest
version of the sources. It will also configure everything needed for the build.

```sh
$ ./tools/dev/v8gen.py x64.release
$ ninja -C out.gn/x64.release
```

Have a tea or a coffee, then come back and check if the compiled `v8` works as
intended by running the tests:

```sh
$ tools/run-tests.py --gn
```

If everything went well, this file should exist: `./out.gn/x64.release/d8`.
