---
title: Install IHaskell on Ubuntu 14.04 with Stack
---



In this post, we're going through the installation of IHaskell on GNU/Linux (Ubuntu 14.04 in my case, though it should be pretty similar on other ditributions) step-by-step. We'll see how the use of *Stack* will simplify the whole process, and how to get all the dependencies right!


## Install Stack

From *Stack* official [documentation](http://docs.haskellstack.org/en/stable/install_and_upgrade.html), for Ubuntu 14.04:

```sh
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 575159689BEFB442
echo 'deb http://download.fpcomplete.com/ubuntu trusty main' \
    |sudo tee /etc/apt/sources.list.d/fpco.list
sudo apt-get update && sudo apt-get install stack -y
```

You should now be able to run the `stack` executable in a shell:
```sh
stack --help
```

Being up-to-date is important, so setup your *Stack* global configuration `~/.stack/global/stack.yaml`:

```yaml
flags: {}
resolver: lts-4.0
packages: []
extra-deps: []
```

Tell *Stack* to setup everything (download package list, install latest *GHC* release, etc.):

```sh
stack setup
```

Note that you can lookup the latest release of stackage at anytime on this [page](http://www.stackage.org/lts).

## Install ZeroMQ latest version

If you try to install IHaskell directly, you'll get an error since Ubuntu 14.04 ships with an older version of ZeroMQ. We need a more recent release. There are several ways to install it:

1. Compile it from source
2. Use Nix

We're going to compile it from source. I'll follow the way of the official [README](https://github.com/gibiansky/IHaskell#install-zeromq):

```sh
# Compiling from source:
git clone git@github.com:zeromq/zeromq4-x.git libzmq
cd libzmq
./autogen.sh && ./configure && make
sudo make install
sudo ldconfig
```

That will do the trick! If you don't like to install packages in global scope, feel free to install it in a user folder (Add the `--prefix=LOCATION` directive to `./configure`, and change `LD_LIBRARY_PATH` and `PATH` in your shell configuration accordingly).

## Install <s>IPython</s> Jupyter

**EDIT**: Following Florian's comment, it appears that *IPython* is now officialy named *Jupyter*, so we might as well install it in our virtualenv. The only change in the instructions is to replace `pip install ipython` by `pip install jupyter` (Note that *IPython* is a dependency of *Jupyter*).

IHaskell requires a <s>version of *IPython > 3.0.0 or up*</s> recent version of *Jupyter*, so we need to install it ourselves. There are several options:

1. Use pip and install it globaly (<s>*pip install ipython*</s>`pip install jupyter`)
2. Use pip and install it in a virtualenv (**This is what we will do here**)
3. Use [nix](https://nixos.org/nix/) *(You're on your own)*
4. Use [conda](https://www.continuum.io/downloads) (<s>*conda update ipython*</s>`conda update jupyter`)

If you're on a fresh install: `sudo apt-get install python-virtualenv python-dev ncurses-base`

With `virtualenv`:
```sh
virtualenv venv-ihaskell
source venv-ihaskell/bin/activate
```

With `virtualenvwrapper`:
```sh
mkvirtualenv ihaskell
workon ihaskell
```

Now we install <s>*ipython*</s> *Jupyter*:
```sh
pip install jupyter
```



## Install IHaskell

Nothing simpler, just use *Stack*:

```sh
stack build ihaskell
```

You may want to install extra packages to enhance *IHaskell* capabilities. Here are the ones supported by Stackage:

* ihaskell-aeson
* ihaskell-blaze
* ihaskell-charts
* ihaskell-diagrams
* ihaskell-rlangqq
* ihaskell-magic
* ihaskell-juicypixels
* ihaskell-hatex
* ihaskell-basic

Some others are not in the LTS snapshot of Stackage, but could be useful in the future. I don't know about the current maturity of these packages:

* ihaskell-widgets (conflict with current version of singletons)
* ihaskell-parsec (conflict with current version of aeson)
* ihaskell-plot


## Setup IHaskell

If everything went well, you should have *Jupyter* (latest version) and *ihaskell* installed properly. The last step is to install the *IHaskell Kernel* into *Jupyter*.

```sh
stack exec ihaskell -- install
```

Now run the notebook server and enjoy:

```sh
stack exec jupyter -- notebook
```

Open your browser and go to: `http://localhost:8000`


## What's next

Thanks to Stack, the install process went very smoothely (appart from some dependency we had to install ourselves).
Using IPython with a Haskell Kernel I believe writing about Haskell will be a much pleasant experience since it's really easy
to export a notebook either in HTML or Markdown.
