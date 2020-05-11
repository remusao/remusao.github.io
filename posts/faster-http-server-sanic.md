---
title: simple-sanic—A blazing fast HTTP server using Sanic
date: 2017-11-26
lang: en
logo: python
issue: 6
---

I recently [showed how](/posts/simple-http-server-haskell.html) one can
implement something equivalent to `http.server` from `Python` in `Haskell`. The
solution turned out to be reasonably simple, and *surprisingly fast*.

Unfortunately, we cannot say that `http.server` is fast... It works fine
to serve simple files and is very convenient to use, but will quickly
reach its limit when more files need to be served. To be clear, *you should not
use* `sanic` or `http.server` in production to serve static files; [nginx](https://www.nginx.com/resources/admin-guide/serving-static-content/)
is a much more robust solution for that. But sometimes you need to work
locally on some static files, and it makes sense to have a reasonably fast and
easy way to achieve that!

I decided to go with [sanic](https://github.com/channelcat/sanic), an "Async
Python 3.5+ web server that's written to go fast". It can be configured to
behave like `http.server`, except much faster...

```python
from sanic import Sanic

def main():
    app = Sanic(__name__, log_config=None)
    app.static('/', './index.html')
    app.static('/', './')
    app.run(host="0.0.0.0")

if __name__ == '__main__':
    main()
```

To make it even easier to use, I bundled this code into a package: [simple-sanic](https://github.com/remusao/simple-sanic),
which is a drop-in replacement for `http.server`. It allows to serve the content
of the current directory over http with a single command:

```sh
$ python -m simple-sanic
```

The package is available on [Pypi](https://pypi.python.org/pypi/simple-sanic)
and [Github](https://github.com/remusao/simple-sanic). To install:

```sh
$ pip install simple-sanic
```

You can customize the `host` and `port` as well:

```sh
$ python -m simple-sanic --help
Usage:
    simple-sanic [options]
    simple-sanic -h | --help

Options:
    -p --port PORT      Specify alternate port [default: 8000]
    -b --bind ADDRESS   Specify alternate bind address [default: 0.0.0.0]
    -h, --help          Show this help message and exit.
```
