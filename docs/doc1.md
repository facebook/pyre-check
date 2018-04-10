---
id: doc1
title: Installing Pyre-Check
sidebar_label: Installation
---

The currently-recommended way to get the latest version of Pyre-Check
is to build it from source.

# Building from Source

These instructions are known to work on Mac OS X (tested on High
Sierra) and Linux (tested on Ubuntu 16.04 LTS and CentOS 7).

## Getting the source

This is the currently-recommended way of installing
Pyre-Check. First, clone the repo:

```bash
  $ git clone https://github.com/facebook/pyre-check
  $ cd pyre-check
```

You can find the most recent tagged release by
running `git tag -l`. We recommend using a tagged
release unless you specifically need the latest
hotness from master. If the latest release is v1.0.0:

```bash
  $ git checkout v1.0.0
  You are in a 'detached HEAD' state. You can look # [...]
```

## Installing OPAM

The easiest way to acquire the correct version of OCaml and the
related libraries is to use [OPAM](https://opam.ocaml.org/). Use your
system's package manager to install it; then configure it to use
the OCaml v4.06.0 toolchain (note that this may take a while):

```bash
  $ sudo apt install opam
  $ opam init
  $ opam switch 4.06.0 # compiles the ocaml toolchain -> long
  $ opam config env
```

At this point, you should be able to run the Pyre-Check setup script,
which builds the typechecker binary and runs the unit tests:

```bash
  $ ./scripts/setup.sh --local
```

## Getting the type stubs from Typeshed

To reliably type check calls to the Python standard library and Python
builtins, stub annotations from the [`typeshed`
project](https://github.com/python/typeshed) are needed. These
annotations are not distributed with this source code, and are
downloaded separately.

If you have installed this package through `pip`, the annotations have
been installed automatically as a dependency, and you do not need
anything else.

If you are installing this package from source, you can install the
stubs through `pip`:

```bash
  $ pip install typeshed
```

This way, Pyre-Check will be able to find the annotations automatically.

Alternatively, for the latest version of `typeshed`, you can clone the
Github repository:

```bash
  $ git clone https://github.com/python/typeshed.git
```

In this case, the location of the downloaded stubs can be passed to
Pyre-Check via the `--typeshed` commandline parameter.

If you install the annotations separately, please note, however, that
`typeshed` might have introduced new features not yet recognized by
Pyre-Check, so this workflow is not officially supported. You are
encouraged to open a pull request with a patch though! :)
