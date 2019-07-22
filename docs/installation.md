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
- we currently only provide *linux/AMD64* and *OSX/AMD64* binaries. If you need support for a
different architecture, feel free to reach out to us via a GitHub Issue.
- Please see below for a unsupported workaround to get `pyre` working on Windows via
[WSL](https://en.wikipedia.org/wiki/Windows_Subsystem_for_Linux)


# Building from Source

These instructions are known to work on Mac OS X (tested on High
Sierra - OSX 10.13 - even though binaries are compatible with versions
as old as 10.11) and Linux (tested on Ubuntu 16.04 LTS and CentOS 7).

## Prerequisites
### Opam
Before we can build Pyre, we need to make sure that we have a current OCaml compiler. We use
[Opam](https://opam.ocaml.org/) to manage our compiler and libraries. You can get Opam via various
package management systems. Please follow their instructions for your particular operating system.

Once you have Opam on your system, switch to a current compiler with

```bash
$ opam switch 4.08.0
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

## Windows Subsystem for Linux (WSL) Install

On *x86_64* Windows `pyre` can run via Linux using [WSL](https://en.wikipedia.org/wiki/Windows_Subsystem_for_Linux).
A brief summary to get this running on Ubuntu please follow:
- [Install WSL](https://docs.microsoft.com/en-us/windows/wsl/install-win10) *(external Microsoft Documentation)*
- Once you have a login to your Linux of choice:
  - Optionally: Install build environment (some dependencies of `pyre` might need to be built)
  - Use `pip` as described above or via a [Python Virtual Environment](https://docs.python.org/3/tutorial/venv.html)
- Here is an example using [Ubuntu](https://www.ubuntu.com/) with a [venv](https://docs.python.org/3/tutorial/venv.html):

```bash
sudo apt install python3-venv build-essential python3-dev libpython3-dev
python3 -m venv /tmp/tp
/tmp/tp/bin/pip install --update pip setuptools wheel
/tmp/tp/bin/pip install pyre-check
source /tmp/tp/bin/activate
cd /mnt/c/path/to/repo
pyre --source-directory . check

(tp) cooper@TESTFAC-1FMHLI2:/mnt/c/path/to/repo$ pyre --source-directory . check
 ƛ Setting up a `.pyre_configuration` with `pyre init` may reduce overhead.
 ƛ No type errors found
```
