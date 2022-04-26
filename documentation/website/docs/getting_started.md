---
id: getting-started
title: Getting Started
sidebar_label: Getting Started
---
import {OssOnly, FbInternalOnly} from 'internaldocs-fb-helpers';
import FbGettingStarted from './fb/getting_started.md';
import YouTube from 'react-youtube';

<FbInternalOnly>

<FbGettingStarted />

</FbInternalOnly>

<OssOnly>

Welcome to the wonderful world of static typing! This guide will get you from zero to a simple project that is type checked with Pyre.

## Requirements
To get started, you need [Python 3.6 or later](https://www.python.org/getit/) and [watchman](https://facebook.github.io/watchman/) working on your system. On *MacOS* you can get everything with [homebrew](https://brew.sh/):
```bash
$ brew install python3 watchman
```
On *Ubuntu*, *Mint*, or *Debian*; use `apt-get` and [homebrew](https://brew.sh/):
```bash
$ sudo apt-get install python3 python3-pip python3-venv watchman
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

</OssOnly>

## Introductory Video

<YouTube videoId="k_xElpxw9aY" />
