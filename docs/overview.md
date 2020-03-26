---
id: overview
title: Overview
sidebar_label: Overview
---

Pyre is a performant type checker for Python. Statically typing what are 
essentially fully dynamic languages has a long tradition at Facebook. 
We've done this for PHP with [Hack](https://hacklang.org/) and for Javascript 
with [Flow](https://flow.org/). 

## Why Types?
We believe statically typing what are essentially fully dynamic languages 
gives our code stability and improves developer productivity. Type errors are 
dangerous and sometimes difficult to track down, depending on the complexity 
of the program. Pyre allows us to check the source before we run the program 
and to fix our code accordingly.


## Getting Started
See the [Installation](installation.md) section for how to set up Pyre on your 
machine. If you already have a Pyre installation, you can take the 
[Guided Tour](guided_tour.md) to familiarize yourself the basic workflows 
that we recommend.

The [Configuring Pyre](http://configuration.html/) section contains information 
on how to customize Pyre to work with your own projects. You can tune Pyre in 
a number of different ways, depending on how strict you want to be with your 
types, and how performant you want Pyre to be when run against your source base. 

Pyre includes a number of key features, described in these pages:

* [Gradual Typing](gradual-typing.html) - Not every expression is typed. Pyre 
   allows users to explicitly specify how strict the type checking should be 
   on a per-file basis.
* [Error Suppression](error-suppression.html) - Pyre will suppress specified 
   errors, depending on configuration. 
* [Error Types](error-types.html) - Pyre identifies a number of different error 
   types including incompatible variables, behavioral subtyping, missing 
   attributes, and much more. 
* [Editor Integration](lsp-integration.html) - Pyre integrates with VS Code and 
   Nuclide. 
* [Watchman Integration](watchman-integration.html) - Allows for incremental 
   typechecking using terminal editors such as Vim or Emacs.
