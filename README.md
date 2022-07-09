[![lint](https://github.com/facebook/pyre-check/workflows/lint/badge.svg)](https://github.com/facebook/pyre-check/actions/workflows/lint.yml)
[![tests](https://github.com/facebook/pyre-check/workflows/tests/badge.svg)](https://github.com/facebook/pyre-check/actions/workflows/tests.yml)
[![pyre](https://github.com/facebook/pyre-check/workflows/pyre/badge.svg)](https://github.com/facebook/pyre-check/actions/workflows/pyre.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

<p align="center">
  <img src="https://raw.githubusercontent.com/facebook/pyre-check/main/logo.png">
</p>

Pyre is a performant type checker for Python compliant with [PEP 484](https://www.python.org/dev/peps/pep-0484/). Pyre can analyze codebases with millions of lines of code incrementally – providing instantaneous feedback to developers as they write code. You can try it out on examples in [the Pyre Playground](https://pyre-check.org/play).

Pyre ships with **Pysa**, a security focused static analysis tool we've built on top of Pyre that reasons about data flows in Python applications. Please refer to our [documentation](https://pyre-check.org/docs/pysa-quickstart) to get started with our security analysis.

Pysa is also available on the [GitHub Marketplace as a Github Action](https://github.com/marketplace/actions/pysa-action)

## Requirements
To get started, you need [Python 3.6 or later](https://www.python.org/getit/) and [watchman](https://facebook.github.io/watchman/) working on your system. On *MacOS* you can get everything with [homebrew](https://brew.sh/):
```bash
$ brew install python3 watchman
```
On *Ubuntu*, *Mint*, or *Debian*; use `apt-get` and [homebrew](https://brew.sh/):
```bash
$ sudo apt-get install python3 python3-pip python3-venv
$ brew install watchman
```
We tested Pyre on *Ubuntu 18.04.5 LTS*, *CentOS 7*, as well as *OSX 10.11* and later.

## Setting up a Project
We start by creating an empty project directory and setting up a virtual environment:

```bash
$ mkdir my_project && cd my_project
$ python3 -m venv ~/.venvs/venv
$ source ~/.venvs/venv/bin/activate
(venv) $ pip install pyre-check
```

Next, we teach Pyre about our new project:
```bash
(venv) $ pyre init
```
This command will set up a configuration for Pyre (`.pyre_configuration`) as well as watchman (`.watchmanconfig`) in your project's directory. Accept the defaults for now – you can change them later if necessary.

## Running Pyre
We are now ready to run Pyre:
```bash
(venv) $ echo "i: int = 'string'" > test.py
(venv) $ pyre
 ƛ Found 1 type error!
test.py:1:0 Incompatible variable type [9]: i is declared to have type `int` but is used as type `str`.
```
This first invocation will start a daemon listening for filesystem changes – type checking your project incrementally as you make edits to the code. You will notice that subsequent invocations of `pyre` will be faster than the first one.

For more detailed documentation, see https://pyre-check.org.

## Join the Pyre community

See [CONTRIBUTING.md](CONTRIBUTING.md) for how to help out.

## License

Pyre is licensed under the MIT license.
