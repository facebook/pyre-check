---
id: installation
title: Installing Pyre
sidebar_label: Installation
---

You can install Pyre via `pip` or build it yourself from the source.

# Pypi Package (recommended)
You will need to have a working *Python (version 3.5 or later)* on your machine. Running

```bash
$ pip install pyre-check
```

should take care of installing Pyre on your system. See our [Guided Tour](guided_tour.md) for how
to use Pyre.

Note that
- on *MacOS* you might have to invoke `pip36` explicitly to use a current version,
- we currently only provide *linux/AMD64* and *OSX 10.13/AMD64* binaries. If you need support for a
different architecture, feel free to reach out to us.


# Building from Source

These instructions are known to work on Mac OS X (tested on High
Sierra - OSX 10.13) and Linux (tested on Ubuntu 16.04 LTS and CentOS 7).

## Prerequisites
### Opam
Before we can build Pyre, we need to make sure that we have a current OCaml compiler. We use
[Opam](https://opam.ocaml.org/) to manage our compiler and libraries. You can get Opam via various
package management systems. Please follow their instructions for your particular operating system.

Once you have Opam on your system, switch to a current compiler with

```bash
$ opam switch 4.06.0
```

This will compile the compiler from scratch and is likely going to take some time on your system.

### Typeshed
Although not strictly required, we recommend that you get a version of typeshed to test your local
changes with. These stubs provide definitions for most of the standard library functions.
 You can get a current version from GitHub with
```bash
$ git clone https://github.com/python/typeshed.git
```

You can pass the location of typeshed to Pyre (once you have it set up) with a `--typeshed`
parameter.


## Getting the Source
With a working OCaml, you can clone the source from GitHub with
```bash
$ git clone https://github.com/facebook/pyre-check
```

You can complete the setup of your development environment with

```bash
$ cd pyre-check
$ ./scripts/setup.sh --local
```

This will generate a `Makefile` in your checkout directory. You can subsequently build and test
Pyre with

```bash
$ make
$ make test
$ make python_tests
```
