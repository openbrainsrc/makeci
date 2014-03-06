makeci
=========

Lightweight Make-based continuous integration, written in Haskell
using [Spock](http://hackage.haskell.org/package/Spock).

## Assumptions

* Your code is hosted on [GitHub](https://github.com)

* You repository has a Makefile in its root directory 
  with the rules `cibuild` and `citest`.

## Install

* Install the [Haskell Platform](http://www.haskell.org/platform/)
* `git clone https://github.com/openbrainsrc/makeci.git`
* `cabal install`

## Configuration

You need to tell makeci which repositories you want to build in the
configuration file. This file has one line per repository, in
the format {GitHub username}/{repository name}. Example:

```
glutamate/probably-base
glutamate/matio
glutamate/baysig-platform
openbrainsrc/debcd
```

You then run makeci (after you have made sure that wherever cabal puts
your binaries is in your PATH) with

```
makeci {configuration_file}
```

if makeci is invoked with no arguments, it will look for a
configuration file in `/etc/makeci-repos`.

Makeci will try to clone these repositories with 

```
git clone git@github.com:{user_name}/{repository_name}.git
```

Thus, you will need to have SSH keys in place to access GitHub in this
way.

## Build configuration

Makeci will try to build you repositories by running

```
make cibuild && make citest
```

if the output contains a line that starts with `file://` then makeci
will splice the contents of that file into your job output.

## GitHub webhook

Set this as your webhook in GitHub:

```
http://{server_url}:3001/github-webhook/{repository_name}
```

## But I am not using Make as a build system!

Great, just create a Makefile to invoke whichever build system you are
using.

For instance, here is an example Makefile for a Cabal-based project:

```
cibuild:
	cabal sandbox init
	cabal install --enable-tests

citest:
```