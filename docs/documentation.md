---
id: documentation
title: Pyre Documentation
sidebar_label: Overview
---

## Why Types?

We believe statically typing what are essentially fully dynamic languages gives our code
stability and improves developer productivity.


## Try It Yourself

See [Getting Started](installation.md) for Pyre installation steps.

## Configurations

Pyre's configuration typically sits at the root of the repository in `.pyre_configuration`.
You can override that configuration locally to set up your own default target to check.

Running
```
pyre init
```
from your project root will set up your `.pyre_configuration`, but you may want to override
the source-directories field in projects within that root, so you can easily run pyre on
different sets of default paths.
