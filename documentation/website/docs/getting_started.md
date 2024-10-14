---
id: getting-started
title: Getting Started with Pyre
sidebar_label: Getting Started
---
import {OssOnly, FbInternalOnly} from 'docusaurus-plugin-internaldocs-fb/internal';
import FbGettingStarted from './fb/getting_started.md';
import YouTube from 'react-youtube';

<FbInternalOnly>

<FbGettingStarted />

</FbInternalOnly>

<OssOnly>

Welcome to the wonderful world of static typing! This guide will get you from zero to a simple project that is type checked with Pyre.

## Requirements
To get started, you need [Python 3.8 or later](https://www.python.org/getit/) and [watchman](https://facebook.github.io/watchman/) working on your system. On *MacOS* you can get everything with [homebrew](https://brew.sh/):
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

## Introductory Video

<YouTube videoId="k_xElpxw9aY" />

</OssOnly>

## Further Reading

This page should contain all of the basic information you need to get started with type checking your own project.

If you are new to the type system, the [introduction to types in Python](gradual_typing.md) is recommended reading to familiarize with the type system, gradual typing, and common type errors.

<FbInternalOnly>

If you are looking for more options to configure your type checking experience, the [configuration](fb/configuration.md) page explores command line and configuration file settings.

</FbInternalOnly>

<OssOnly>

If you are looking for more options to configure your type checking experience, the [configuration](configuration.md) page explores command line and configuration file settings.

</OssOnly>

<FbInternalOnly>

For questions and support, please use our [Pyre Q&A](https://fb.workplace.com/groups/pyreqa) workplace group. For more insight into our roadmap, new features, and progress, please read our [Pyre FYI](https://fb.workplace.com/groups/295311271085134) workplace group.

</FbInternalOnly>
