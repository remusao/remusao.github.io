---
title: Install IHaskell on Ubuntu 16.04 with Stack
---

Last year I wrote a tutorial to install iHaskell on Ubuntu 14.04. Since it's a
bit out-dated now, but I still think iHaskell can be super useful, I decided to
update it for Ubuntu 16.04.

Before we start, please be aware that you can use IHaskell directly in your
browser by visiting this website: [try.jupyter.org](https://try.jupyter.org/).
The following tutorial is only meant for those who want IHaskell to run locally.

## Install Stack

Compared to Ubuntu 14.04, it is now easier to install `stack` thanks to the
`haskell-stack` package:

```sh
$ sudo apt-get install haskell-stack
```

Now let's make sure we have the latest version of stack, as well as the latest
snapshot from [stackage](stackage.org/lts):

```sh
$ stack upgrade
$ stack update
$ stack setup
```

## Dependencies

Some [dependencies](https://github.com/gibiansky/IHaskell#linux) are
required before building the IHaskell kernel, let's install them:

```sh
$ sudo apt-get install -y \
    python3-pip \
    git \
    libtinfo-dev \
    libzmq3-dev \
    libcairo2-dev \
    libpango1.0-dev \
    libmagic-dev \
    libblas-dev \
    liblapack-dev
```

## Install Jupyter

IHaskell requires a recent version of *Jupyter*, so we need to install
it ourselves. There are several options:

1. Use pip and install it globally (`pip install jupyter`)
2. **Use pip and install it in a [`virtualenv`](https://virtualenv.pypa.io/en/stable/)**
3. Use [nix](https://nixos.org/nix/) *(You're on your own)*
4. Use [conda](https://www.continuum.io/downloads) (`conda update jupyter`)

Let's go with option `2.`. I usually never install python packages globally
using `sudo pip`, and prefer to create a fresh [`virtualenv`](https://virtualenv.pypa.io/en/stable/) for each project.
Let's proceed!

If you're on a fresh install, we need to install virtualenv first:

```sh
$ sudo apt-get install virtualenv python3-dev ncurses-base
```

Then let's install `jupyter` inside of our virtualenv `virtualenv`:
```sh
$ virtualenv venv-ihaskell -p /usr/bin/python3 # Create a virtualenv
$ source venv-ihaskell/bin/activate # Active it
$ pip install jupyter # Install jupyter
```

## Install IHaskell

Since IHaskell is not available in the latest Stackage LTS snapshot, we have two
options to install it:

* Compile from source
* Use the latest snapshot supporting IHaskell (`lts-6.35`)

I will present both methods.

### From source

The instructions can be found in the github [repository](https://github.com/gibiansky/IHaskell#linux):

> ```sh
> $ source ihaskell-venv/bin/activate
> $ git clone git@github.com:gibiansky/IHaskell.git
> $ cd IHaskell
> $ pip install -r requirements.txt
> $ stack setup
> $ stack install gtk2hs-buildtools
> $ stack install --fast
> $ stack exec ihaskell -- install --stack
> ```

Then it can be started using:
```sh
$ stack exec jupyter -- notebook
```

### From Stack LTS

Unfortunately, it is not possible to build IHaskell using the latest snapshot,
as it does not seem to work with GHC 8.x. The latest supported LTS snapshot is
`6.35`, so we will have to specify it explicitly:

```sh
$ stack --resolver lts-6.35 setup --install-ghc
$ stack --resolver lts-6.35 install ihaskell
$ stack --resolver lts-6.35 exec ihaskell -- install --stack
```

You may want to install extra packages to enhance *IHaskell*'s capabilities. Here are the ones supported by Stackage:

* `ihaskell-aeson`
* `ihaskell-basic`
* `ihaskell-blaze`
* `ihaskell-charts`
* `ihaskell-diagrams`
* `ihaskell-hatex`
* `ihaskell-inline-r`
* `ihaskell-juicypixels`
* `ihaskell-magic`
* `ihaskell-rlangqq`

Some others are not in the snapshot, but could be very useful and can probably
be installed from source:

* `ihaskell-widgets`
* `ihaskell-parsec`
* `ihaskell-plot`

## What's next

The installation of IHaskell on Ubuntu 16.04 is a bit easier than on 14.04
thanks to more up-to-date dependencies. Unfortunately, the latest IHaskell is
not available on Stackage LTS, which requires to use an older snapshot (`6.35`).
It is still possible to build IHaskell from source, which allows you to
run the latest version of the code! It's up to you to choose the method you prefer.
