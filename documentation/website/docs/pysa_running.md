---
id: pysa-running
title: Running Pysa
sidebar_label: Running Pysa
---

This page walks you through the basics of running Pysa. If you want exercises to
walk you through using Pysa's more advanced features, check out [this
tutorial](https://github.com/facebook/pyre-check/tree/main/documentation/pysa_tutorial).

## Setup

The setup requires the following 4 types of files.

1. **Source Code** (`*.py`): This is your application's code.
2. **Taint Config** (`taint.config`): This file declares sources, sinks,
   features, and rules.
3. **Taint Models** (`.pysa`): These files link together the information in your
   source code and `taint.config`. They tell Pysa where in our code there
   exist sources and sinks.
4. **Pysa Configuration** (`.pyre_configuration`): Parts of this file are
   critical to using Pysa. `source_directories` tells Pysa
   the directory containing the source code you want to analyze.
   `taint_models_path` tells Pysa where to find the config and model files.

## Example

Let's look at a simple taint analysis example. To follow along, create a
directory `static_analysis_example` and navigate to it. Paste the code snippets
into the appropriately named files.

### 1. Source Code

```python
# static_analysis_example/source.py

import os

def get_image(url):
    command = "wget -q https:{}".format(url)
    return os.system(command)

def convert():
    image_link = input("image link: ")
    image = get_image(image_link)
```

Notice the following:
* The `input` function is a taint source since it gets input directly from
  the user.
* The `os.system` function is a taint sink, since we do not want user-controlled
  values to flow into it.
* The return value of `input` is used as the URL for a `wget` call, which is
  executed by `os.system`. The `wget` can therefore be doing anything, out of
  the programmer's control.
* This data flow should be identified as a potential security issue.

### 2. Taint Config

```python
# static_analysis_example/stubs/taint/taint.config

{
  sources: [
    {
      name: "UserControlled",
      comment: "use to annotate user input"
    }
  ],

  sinks: [
    {
      name: "RemoteCodeExecution",
      comment: "use to annotate execution of code"
    }
  ],

  features: [],

  rules: [
    {
      name: "Possible shell injection",
      code: 5001,
      sources: [ "UserControlled" ],
      sinks: [ "RemoteCodeExecution" ],
      message_format: "Data from [{$sources}] source(s) may reach [{$sinks}] sink(s)"
    }
  ]
}
```

This declares the valid sources and sinks that Pysa should recognize. We
also tell Pysa that data flowing from a `UserControlled` source to a
`RemoteCodeExecution` sink is a possible shell injection.

### 3. Taint Models

```python
# static_analysis_example/stubs/taint/general.pysa

# model for raw_input
def input(__prompt) -> TaintSource[UserControlled]: ...

# model for os.system
def os.system(command: TaintSink[RemoteCodeExecution]): ...
```

This file links together the information in `source.py` and `taint.config`. We
use it to tell Pysa where in our code there exist sources and sinks.

### 4. Pysa Configuration

```python
# static_analysis_example/.pyre_configuration

{
  "source_directories": ["."],
  "taint_models_path": "stubs/taint"
}
```

Pysa needs to know what directory to analyze, as well as where to find the config
and model files.

### Analysis

Now let's run the static analysis:

```shell
[~/static_analysis_example] $ pyre analyze
 ƛ Fixpoint iterations: 2
[
  {
    "line": 9,
    "column": 22,
    "path": "source.py",
    "code": 5001,
    "name": "Possible shell injection",
    "description":
      "Possible shell injection [5001]: Data from [UserControlled] source(s) may reach [RemoteCodeExecution] sink(s)",
    "long_description":
      "Possible shell injection [5001]: Data from [UserControlled] source(s) may reach [RemoteCodeExecution] sink(s)",
    "concise_description":
      "Possible shell injection [5001]: Data from [UserControlled] source(s) may reach [RemoteCodeExecution] sink(s)",
    "define": "source.convert"
  }
]
```

Looking at the output, we see that pyre surfaces the tainted data flow that we
expected.

Let's run it again and save the results:

```shell
[~/static_analysis_example] $ pyre analyze --save-results-to ./
```

The `--save-results-to` option will save more detailed results to
`./taint-output.json`.

### Understanding the results

See [Static Analysis Post Processor](static_analysis_post_processor.md).
