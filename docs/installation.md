---
id: installation
title: Installation
sidebar_label: Installation
---

We recommend that you use our binary distribution through [pypi](https://pypi.org/) inside of a virtual environment and support both *MacOs* and *Linux*. On *Windows* we have successfully got `pyre` to work through [WSL](https://en.wikipedia.org/wiki/Windows_Subsystem_for_Linux) but do not officially support it.

## Binary Distribution
Please refer to our [Getting Started](getting_started.md) section for instructions to get the latest version of `pyre` inside your virtual environment.

## Building from Source
These instructions are known to work on Mac OS X (tested on High
Sierra - OSX 10.13 - even though binaries are compatible with versions
as old as 10.11) and Linux (tested on Ubuntu 16.04 LTS and CentOS 7).

### Requirements
In addition to [Python and watchman](getting-started#requirements), we need a working *OCaml* compiler. We use
[Opam](https://opam.ocaml.org/) to manage our compiler and libraries. You can get Opam via various
package management systems. Please follow their instructions for your particular operating system.

Once you have Opam on your system, switch to a current compiler with

```bash
$ opam switch 4.08.0
```

This will compile the compiler from scratch and is likely going to take some time on your system.

### Getting the Source
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
`pyre` with

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
$ sudo apt install python3-venv build-essential python3-dev libpython3-dev
$ python3 -m venv /tmp/tp
$ /tmp/tp/bin/pip install --update pip setuptools wheel
$ /tmp/tp/bin/pip install pyre-check
$ source /tmp/tp/bin/activate
$ cd /mnt/c/path/to/repo
$ pyre --source-directory . check

$ (tp) cooper@TESTFAC-1FMHLI2:/mnt/c/path/to/repo$ pyre --source-directory . check
 ƛ Setting up a `.pyre_configuration` with `pyre init` may reduce overhead.
 ƛ No type errors found
```
