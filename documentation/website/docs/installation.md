---
id: installation
title: Installation
sidebar_label: Installation
---

We recommend that you use our binary distribution through [pypi](https://pypi.org/) inside of a virtual environment, which supports both *MacOs* and *Linux*. On *Windows* we have successfully gotten `pyre` to work through [WSL](https://en.wikipedia.org/wiki/Windows_Subsystem_for_Linux), but do not officially support it.

## Binary Distribution
You can get Pyre through [pypi](https://pypi.org/) by running:
```bash
$ (venv) $ pip install pyre-check
```
See our [Getting Started](getting-started) section for a more detailed example, including setup for a virtual environment.

## IDE Integration
Pyre supports the [Language Server Protocol](https://en.wikipedia.org/wiki/Language_Server_Protocol). We provide an [extension for *VSCode*](https://marketplace.visualstudio.com/items?itemName=fb-pyre-check.pyre-vscode) that will automatically try to connect to a running server. You can also directly interact with the LSP by piping the appropriate `JSON` into `pyre persistent`.

## Building from Source
These instructions are known to work on *Mac OS X* (tested on *High
Sierra* through *OSX 10.13* - even though binaries are compatible with versions
as old as *10.11*) and *Linux* (tested on *Ubuntu 16.04 LTS* and *CentOS 7*).

### Requirements
In addition to [Python and watchman](getting-started#requirements), we need a working *OCaml* compiler. We use
[Opam](https://opam.ocaml.org/) to manage our compiler and libraries. You can get Opam via various
package management systems. Please follow their instructions for your particular operating system.

Once you have Opam on your system, switch to a current compiler with

```bash
$ opam switch 4.10.2
```

This will compile the compiler from scratch and is likely going to take some time on your system.

### Building OCaml changes
With a working OCaml, you can clone the source from [GitHub](https://github.com/facebook/pyre-check) with
```bash
$ git clone https://github.com/facebook/pyre-check
```

You can complete the setup of your development environment with

```bash
$ cd pyre-check
$ ./scripts/setup.sh --local
```

This will generate a `Makefile` in the `source` directory. You can subsequently build and test
`pyre` with

```bash
$ make
$ make test
```
### Testing changes to the Python Client
In a virtualenv, install dependencies with `requirements.txt` and run python tests to make sure everything is set up correctly

```bash
$ cd /path/to/pyre-check
$ pip install -r requirements.txt
$ ./scripts/run-python-tests.sh
```
When installing and running `pyre` from PyPi, the entry point to the executable is actually `client/pyre.py`. To be able to run this file from anywhere, add the `pyre-check` directory to `PYTHONPATH` and subsequently assign `pyre` and an alias for `client.pyre`. For the `pyre` command to correctly point to the compiled binary, also set the environment variable `PYRE_BINARY` to `source/build/default/main.exe`.

```bash
$ echo "alias pyre='PYTHONPATH=\"/path/to/pyre-check:\$PYTHONPATH\" python -m client.pyre'" >> ~/.bashrc
$ echo "export PYRE_BINARY=/path/to/pyre-check/source/_build/default/main.exe" >> ~/.bashrc
$ source ~/.bashrc
```
You should be able to open a new shell and run `pyre -h` now, confirming `pyre` was set-up correctly. Any changes made to the Pyre Python client code should be immediately observable the next time you invoke `pyre`

## Windows Subsystem for Linux (WSL) Install

On *x86_64* Windows `pyre` can run via *Linux* using [WSL](https://en.wikipedia.org/wiki/Windows_Subsystem_for_Linux).
A brief summary to get this running on *Ubuntu* please follow:
- [Install WSL](https://docs.microsoft.com/en-us/windows/wsl/install-win10) *(external Microsoft Documentation)*
- Once you have a login to your Linux of choice:
  - Optionally: Install build environment (some dependencies of `pyre` might need to be built)
  - Use `pip` as described above or via a [Python Virtual Environment](https://docs.python.org/3/tutorial/venv.html)
- Here is an example using [Ubuntu](https://www.ubuntu.com/) with a [venv](https://docs.python.org/3/tutorial/venv.html):

```bash
$ sudo apt install python3-venv build-essential python3-dev libpython3-dev
$ python3 -m venv /tmp/tp
$ /tmp/tp/bin/pip install --upgrade pip setuptools wheel
$ /tmp/tp/bin/pip install pyre-check
$ source /tmp/tp/bin/activate
$ cd /mnt/c/path/to/repo
$ pyre --source-directory . check

$ (tp) cooper@TESTFAC-1FMHLI2:/mnt/c/path/to/repo$ pyre --source-directory . check
 ƛ Setting up a `.pyre_configuration` with `pyre init` may reduce overhead.
 ƛ No type errors found
```
