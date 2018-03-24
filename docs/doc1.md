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
