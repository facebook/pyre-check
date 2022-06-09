---
id: pysa-explore
title: Exploring Taint Models Interactively
sidebar_label: Exploring Taint Models Interactively
---

## Overview

After Pysa's analysis is complete, the tool will output a detailed JSON with its final view of the taint of callables in addition to the issues it finds. We provide a script to explore these taint models called `explore_pysa_models.py`, which can give you insight into why Pysa thinks there might or might not be taint for a given callable.

## Basic Usage

Before using the explore script, you should already have run Pysa on your codebase. For the purposes of this page, we will assume you stored it in `/tmp/output_dir`, e.g.

```bash
$ pyre analyze --save-results-to /tmp/output_dir
```

After the analysis succeeds, Pysa will write a file, `/tmp/output_dir/taint-output.json`, containing the taint of each callable in addition to the issues found. Let's load this JSON into our explore script:

```bash
$ python3 -i ~/fbsource/fbcode/tools/pyre/scripts/explore_pysa_models.py
# Pysa Model Explorer
Available commands:
  index('taint-output.json')      Index all available models in the given taint output file or directory.
  callables_containing('foo.bar') Find all callables containing the given string.
  callables_matching(r'foo\..*')  Find all callables matching the given regular expression.
  get_model('foo.bar')            Get the model for the given callable.
  print_model('foo.bar')          Pretty print the model for the given callable.
                                  Optional parameters:
                                    kind='UserControlled'      Filter by taint kind.
                                    caller_port='result'       Filter by caller port.
                                    remove_sources=False
                                    remove_sinks=False
                                    remove_tito=False
                                    remove_tito_positions=True
                                    remove_features=True
                                    remove_leaf_names=True
  get_issues('foo.bar')           Get all issues within the given callable.
  print_issues('foo.bar')         Pretty print the issues within the given callable.
  print_json({'a': 'b'})          Pretty print json objects with syntax highlighting.
```
```python
>>> index('/tmp/output_dir')
Indexing `/tmp/output_dir`
Indexed 307120 models and 35357 issues
```

Once we've indexed our taint JSON, we're good to go! Let's investigate what models Pysa finds for HttpRequest. First, we'll need to get the full name of the relevant callables:


```python
>>> callables_containing('HttpRequest')
['django.http.request.HttpRequest.__init__', 'django.http.request.HttpRequest.body', ...]
>>> get_model('django.http.request.HttpRequest.__init_')
{'callable': 'django.http.request.HttpRequest.__init__', 'sources': [], 'sinks': [], 'tito': [{'port': 'formal(self)', 'taint': [{'decl': None, 'leaves': [{'kind': 'LocalReturn', 'name': ''}]}]}]}
```

This (hard-to-parse) JSON is all that Pysa knows about the `HttpRequest.__init__` function. If you squint, you'll see that the model doesn't introduce any sources or sinks (as expected), but has taint-in-taint-out for the `self` parameter.

Let's take a look at `body`, a slightly more interesting function. We'll also swap to using the `print_model()` function which will pretty print the output:

```python
>>> print_model('django.http.request.HttpRequest.body')
{
  "callable": "django.http.request.HttpRequest.body",
  "sources": [
    {
      "port": "result",
      "taint": [
        {
          "decl": null,
          "kinds": [
            {
              "kind": "UserControlled"
            }
          ]
        },
      ]
    }
  ],
  "sinks": [],
  ...
```

Much easier to read! This model shows that the `body` property of HttpRequests returns a UserControlled source.

You can also use the `get_issues`, and corresponding pretty-printing `print_issues` functions to see all issues in a given callable.

Note that the `get_issues` and `get_models` functions return Python objects that you can manipulate:

```python
>>> print_json(get_issues('foo.bar.log_errors')[0]) # This is valid, will print first issue!
...
>>> print_json(get_model('django.http.request.HttpRequest.body')["sources"]) # Pretty print only the sources.
[
  {
    "port": "result",
    "taint": [
      {
        "decl": null,
        "kinds": [
          {
            "kind": "UserControlled"
          }
        ]
      },
    ]
  }
]
```
