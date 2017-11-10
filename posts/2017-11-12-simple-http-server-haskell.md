---
title: A Simple HTTP Server in Haskell
date: 2017-11-12
logo: haskell
---

I was recently looking for a way to create a very simple `HTTP` Server to serve
static files in the current directory, in Haskell. The way we would do it in
Python:

```sh
$ python -m SimpleHTTPServer 8000
```

That's dead simple, exactly what I needed. It took me some time, but I finally
found a very simple (and efficient) solution:

```haskell
#! /usr/bin/env stack
{-
  stack --resolver lts-9.12
        --install-ghc runghc
        --package wai-app-static
-}

main :: IO ()
main = run 8000 (staticApp (defaultFileServerSettings "."))
```

That's it! And it's using the super efficient [warp](http://www.aosabook.org/en/posa/warp.html)
HTTP server.
