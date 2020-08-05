---
id: getting-started
title: Getting Started
sidebar_label: Getting Started
---

## Requirements
You need a working [Python 3.6 or later](https://www.python.org/getit/) environment to run Pyre. We highly recommend that you install [watchman](https://facebook.github.io/watchman/) to get the most out of Pyre but it's not strictly necessary.

On MacOS you can get everything with [homebrew](https://brew.sh/):
```bash
$ brew install python3 watchman
```
In Ubuntu, Mint and Debian use `apt-get`:
```bash
$ sudo apt-get install python3 python3-pip watchman
```
We tested Pyre on **Ubuntu 16.04 LTS**, **CentOS 7**, as well as **OSX 10.11** and later.

## Setting up a Project
These instructions assume you're working with a virtual environment set up inside of your project directory as follows.

```bash
$ cd your_project
$ python3 -m venv venv
(venv) $ pip install pyre-check
```

We now need to tell Pyre what to check by running
```bash
(venv) $ pyre init
```
By default, this command will set up a configuration for Pyre (`.pyre_configuration`) as well as watchman (`.watchmanconfig`) in your project's directory.

Note that if you do have your virtual environment inside your project directory you will want to tell Pyre not to check the contents of it by adding the following line to your `.pyre_configuration` file:

```json
"ignore_all_errors": [
  "<absolute path to the virtual environment>"
],
```

If you're using watchman, you need to make sure we have watchman listening to file changes in your project directory:

```bash
(venv) $ touch .watchmanconfig
```

We are now ready to start Pyre:
```bash
(venv) $ echo "i: int = 'string'" > test.py
(venv) $ pyre
 ƛ Found 1 type error!
test.py:1:0 Incompatible variable type [9]: i is declared to have type `int` but is used as type `str`.
```
Note that the first invocation initializes Pyre's server that handles incremental updates and will be slower than subsequent invocations – you can easily see this by invoking `pyre` again and observe the same result instantaneously.
