---
id: pysa-quickstart
title: Quickstart
description: Quickstart guide to get Pysa and SAPP running on your project
sidebar_label: Quickstart
---

import useBaseUrl from '@docusaurus/useBaseUrl';
import Tabs from '@theme/Tabs';
import TabItem from '@theme/TabItem';

## Overview

By the end of this guide, you will be able to run Pysa to find bugs on most Python webservers, and view those bugs on [Static Analysis Post Processor (SAPP)](https://github.com/facebook/sapp).

This guide was tested on _Ubuntu 18.04.5 LTS_ and _macOS 10.15.7_

If you run into any issues along the way, please refer to the [common issues](#common-issues) section to help you debug

## Installing dependencies

<Tabs
  groupId="os"
  defaultValue="mac"
  values={[
    {label: 'Ubuntu', value: 'ubuntu'},
    {label: 'macOS', value: 'mac'},
  ]
}>
<TabItem value="mac">

<!-- TODO(T88801567) Remove all steps related to setuptools and wheel packages once we resolve T88801567 -->
Pysa runs on Pyre, so we require the [same dependencies as Pyre](getting_started.md#requirements). You will need Python 3.7 or later, and `setuptools` and `wheel` Python packages.
```bash
$ python3.8 -m pip install --upgrade setuptools
$ pip3 install wheel
```

</TabItem>

<TabItem value="ubuntu">

Since the Python version included in Ubuntu 18.04.5 is Python 3.6.9, we will need to update Python to a version later than Python 3.7
```bash
$ sudo apt-get update
$ sudo apt install python3.8 python3.8-venv python3.8-dev python3-pip -y
```

Next we need to install `setuptools` and `wheel` python packages:
```bash
$ python3.8 -m pip install --upgrade setuptools
$ pip3 install wheel
```

</TabItem>
</Tabs>

## Initial configuration

1. Setting up virtual environment:
```bash
$ python3.8 -m venv ~/.venvs/pysa
$ source ~/.venvs/pysa/bin/activate
```

2. Installing Pyre and SAPP in our virtual environment:
```bash
(pysa) $ pip install pyre-check fb-sapp
```

3. Install dependencies for your project

If you already have dependencies installed for your project in your virtual environment, you can skip this step. If not, usually Python projects will list their dependencies in a `requirements.txt` file. If your project has a `requirements.txt` file, you can install dependencies by:
```
(pysa) $ pip install -r requirements.txt
```

Installing your project dependencies in your virtual environment will allow Pysa to provide you better results, because it allows Pysa to use [models](pysa_basics.md#model-files) that correspond to those dependencies and detect [sources](pysa_basics.md#sources) and [sinks](pysa_basics.md#sinks) that might be hidden in your dependencies' code

4. Create Pyre configuration file in the project directory you want to run Pysa on:

`pyre init` will set up a [configuration file for Pyre (`.pyre_configuration`)](configuration.md) in your project's directory. The defaults for `pyre init` should cover most projects, but if you want more granular control over which files you want to run Pysa on, you can create and tweak [local configurations](configuration.md#local-configurations) to your needs.
<!-- TODO(edq) excludes key in pyre config means don't include the files in analysis while ignore-all-errors will include files, but not show you errors. Is this distinction worth specifying for "advanced" quickstart users who want to set up pyre local configuration? -->
```bash
(pysa) $ cd /path/to/your/repo
(pysa) $ pyre init
```

If your project uses any Python packages that you haven't installed with `pip` in step 3, you will need to add them to `.pyre_configuration` with the `search_path`. Eventually, your (`.pyre_configuration`) should look something like this:
```
{
  "source_directories": [
    "."
  ],
  "search_path": [
    "path/to/external/library"
  ],
  "taint_models_path": "~/.venvs/pysa/lib",
}
```

Also ensure the path corresponding to `taint_models_path` in `.pyre_configuration` contains the remaining Python packages you use in your project:
```
(pysa) $ ls ~/.venvs/pysa/lib/python3.8/site-packages
```
If you are missing any packages, refer back to step 3.

If you have any package-level imports, you may need to specify that in your `.pyre_configuration` or Pysa may be unable to analyze those packages. For example, if you have import statements, like `from package_name.a.b import X`, you will need to add the following to your `.pyre_configuration`:
```
{
  "source_directories": [
    {
      "root": ".",
      "subdirectory": "package_name"
    }
  ],
  "search_path": [
    "path/to/external/library"
  ],
  "taint_models_path": "~/.venvs/pysa/lib",
}
```

<!-- TODO(T84589597) @szhu mentioned she would like to get this done before Pycon. If it is fixed before then, we will include instructions from that release -->

5. Optional, but recommended - Adding type annotations automatically to your project:

If your project isn't type annotated, running Pyre's type inference might improve your Pysa results. Note this command will modify your code, but don't worry the type annotations won't affect your code at runtime.
```
(pysa) $ pyre -l . infer -r -i
```

<!-- 5. Optional - TODO ask @szhu about how to support users who want to change `--source-directory` in SAPP and `source_directory` in pyre configuration -->
<!-- TODO Users will need to specific source directory when they run pysa and sapp from outside their project folder -->

6. Setting up SAPP with some high signal filters
Run the following command to set up SAPP and import a list of filters to help you start looking into issues once Pysa has found them:

<!-- TODO(T88316513) Update filters directory once we figure out how to ship json blobs with `pyre-check` - `sapp import-filter ./filters-to-import/*`-->
```
sapp import-filter filter-filename.json
```

## Running Pysa
Now that Pysa is set up to run on your project, you won't need to repeat any of the earlier steps to run Pysa on subsequent runs. You can run Pysa with `pyre analyze`.

```bash
pyre analyze --no-verify --save-results-to ./pysa-runs
```

This command will run Pysa and store the results of the Pysa run into a `pysa-runs` folder in your project directory. The output of Pysa runs will be used later by SAPP to help you filter and refine the results Pysa provides you.

## Running SAPP
After you have the results from your Pysa run stored in `pysa-runs`, we will ingest those runs into SAPP for analysis with the following command:

```bash
sapp analyze ./pysa-runs/taint-output.json
```

We imported some filters into SAPP earlier and now that SAPP has processed your latest Pysa run, you can run the following command to open the [SAPP Web UI](http://localhost:5000), where you can filter and view the issues from Pysa at [http://localhost:5000](http://localhost:5000):

```
sapp server
```

<img alt="SAPP Web UI for filtering Pysa issues" src={useBaseUrl('img/sapp_web_ui.png')} />

## Going beyond the basics
We've provided you with some filters in SAPP to help you find a small subset of the bugs Pysa is able to catch. If you want to use Pysa to its full potential and find more bugs than the ones in your current Pysa run, you can learn how to write your own filters and tailor Pysa for your codebase below:

 - [Introduction to Pysa](pysa_basics.md)
 - [DEF CON 28 Pysa Tutorial](https://www.youtube.com/watch?v=8I3zlvtpOww)
 - [Pysa Debugging Tips](pysa_false_negatives.md)
 - [SAPP Documentation](static_analysis_post_processor.md)
<!-- TODO(edq) update SAPP docs after [features and bug fixes](https://www.internalfb.com/tasks?q=358499508817549) ship -->

## Common Issues
<!-- TODO(edq) SAPP traces don't show the corresponding code for my project -> you likely need to use --source-directory flag -->
**Problem**: Running Pysa results in `ƛ Error: Could not find a pyre client.`

**Solution**: Some shells (e.g. `zsh`) require a restart to pick up updated PATH variables.

For example:
```zsh
(pysa) $ pip install pyre-check
(pysa) $ cd demo-project
(pysa) $ pyre analyze --no-verify
ƛ Error: Could not find a pyre client.
(pysa) $ which pyre
/usr/local/bin/pyre
```
`which pyre` points to a path not within the virtual environment (e.g. `/usr/local/bin`)

```zsh
(pysa) $ deactivate
(pysa) $ zsh
$ source ~/.venvs/pysa/bin/activate
(pysa) $ which pyre
/Users/unixname/.venvs/pysa/bin/
```

`which pyre` now points to a path in the virtual environment (e.g. `~/.venvs/pysa/bin/`)

Ideally, if you have a local installation of Pyre, it would be best to remove the installation and only install Pyre in a virtual environment.

----
**Problem**: `pip install fb-sapp` or `pip install pyre-check` results in `ERROR: Could not install packages due to an OSError: [Error 13] Permission denied`

**Solution**: Likely you are not installing `fb-sapp` or `pyre-check` in the [virtual environment we created earlier](#initial-configuration). Try running `source ~/.venvs/pysa/bin/activate` before running `pip install`.
```bash
$ source ~/.venvs/pysa/bin/activate
(pysa) $ pip install pyre-check
```
----
**Problem**: `pip install fb-sapp` or `pip install pyre-check` fails, because it has multiple compilation errors like
```
fatal error: Python.h: No such file or directory
#include <Python.h>
^~~~~~~~~~
compilation terminated.
error: command 'x86_64-linux-gnu-gcc' failed with exit status
```
**Solution**: You are missing `setuptools` python package or `python3.8-dev`
```
$ sudo apt install python3.8-dev -y
$ source ~/.venvs/pysa/bin/activate
$ (pysa) python3.8 -m pip install --upgrade setuptools
```
---
**Problem**: Attempting to create a virtual environment with `python3.8 -m venv ~/.venvs/pysa` results in `The virtual environment was not created successfully because ensurepip is not available.`

**Solution**: You are either missing `python3.8-venv` or `python3.8-dev` package. Make sure you delete the directory created by the failed virtual environment command earlier.
```bash
$ sudo apt install python3.8-venv python3.8-dev -y
$ rm -rf ~/.venvs/pysa
$ python3.8 -m venv ~/.venvs/pysa
```
----
**Problem**: Running `pip install fb-sapp` or `pip install pyre-check` fails to install, because of a series of `error: invalid command 'bdist_wheel'` errors

**Solution**: You are missing either `wheel` or `setuptools` package
```bash
$ (pysa) pip3 install wheel
$ (pysa) python3.8 -m pip install --upgrade setuptools
```
----
**Problem**: Running `pyre analyze` results in a bunch of errors and Pysa stops running

**Solution**: Run `pyre analyze --no-verify` to skip model validation.

----
**Problem**: Running `pyre analyze --no-verify` will freeze when parsing stubs and sources or processing functions, but the seconds runtime timer is still increasing

**Solution**: Unfortunately, it is likely the case that your machine doesn't have enough memory to run Pysa on projects with similar size to yours.

---
**Problem**: Running any `sapp` command results in `SyntaxError: future feature annotations is not defined`

**Solution**: SAPP requires Python 3.7. Ensure you are running a Python version later than Python 3.7
```bash
$ python3 --version
```
---
**Problem**: I can't connect to the Web UI and it displays an error related to SSL. The SAPP server log displays a bunch of `400 Bad Request` error codes

**Solution**: Make sure you are visiting [`http://localhost:5000`](http://localhost:5000) and not `https://localhost:5000`

---
**Problem**: I'm seeing a bunch of errors like `~/.venvs/pysa/lib/pyre_check/taint/filename.pysa: module.path.function_name is not part of the environment!`

**Solution**: If you don't use the `module.path.function_name` mentioned, in your project, you can ignore them. Pysa ships with many taint models for code that isn't present in all projects. The errors you are seeing is Pysa informing you that Pysa hasn't found the source code for that particular function in your project.

----
**Problem**: If your SAPP server shows `404 Not found` and the webpage shows `The requested URL was not found on the server. If you entered the URL manually please check your spelling and try again.`

**Solution**: The SAPP Web UI will show this error when there are no issues imported during `sapp analyze`. Check if Pysa has found any issues with your project and if SAPP has imported any of those issues.

Checking if Pysa found any issues - you should expect the following command return at least one line:
```
(pysa) $ cat taint-output.json | grep "issue"
```

Checking which issues SAPP imported - you should expect the following lines to appear after running `sapp analyze`:
```
(pysa) $ sapp analyze
[INFO] Parsing analysis output...
[INFO] Generating issues and traces
[INFO] Run starting...
[INFO] Preparing bulk save.
[INFO] Dropped 0 unused TraceKind.precondition, 0 are missing
[INFO] Dropped 6 unused TraceKind.postcondition, 0 are missing
[INFO] Saving 10 issues, 21 trace frames, 0 trace annotations, 50 trace frame leaf assocs
...
[INFO] Run finished (0:00:00)
```

----
**Problem**: Pysa still doesn't work despite trying everything above

**Solution**: Ensure you are not running a nightly version of Pyre
```bash
(pysa) $ pip uninstall pyre-check-nightly
(pysa) $ pip install pyre-check
```

Some shells (e.g. zsh) will require extra steps:
```zsh
(pysa) $ pip uninstall pyre-check-nightly
(pysa) $ deactivate
$ zsh
(pysa) $ source ~/.venvs/pysa/bin/activate
(pysa) $ pip install pyre-check
```

If you are still unable to get Pysa to run, please [file an issue on pyre-check GitHub repo](https://github.com/facebook/pyre-check/issues) with some information about what OS you are running on, what error you are running into and how to reproduce the error
