---
id: pysa-coverage
title: Coverage Increasing Strategies
sidebar_label: Coverage Increasing Strategies
---

Pysa relies on good type information and compete models in order to accurately analyze code. This page describes a number of strategies for increasing typing and model coverage to help Pysa. These strategies can be used once, with the results committed to your repository, or run as an ephemeral step in your analysis process prior to running `pyre analyze`.

## `pyre infer`

Pyre comes with a built-in type inference feature. From the root of your project, run `pyre infer -r -i` to make recursive in-place edits to add type information.

Note: There is currently a bug with using the above arguments with the infer feature. The workaround is to break it into two commands:
```
pyre infer
pyre infer -i --annotate-from-existing-stubs
```

## Preprocessors

Pysa comes with a number of preprocessors intended to dynamically generate models for a project. Read [this page](pysa_precollectors.md) to learn about the preprocessors that are currently available, and how to write your own. For best results, every entry point to your application (eg. view function for a Django web server) should have a hand written or preprocessor-generated model.

## Quick and Dirty Scripts

Sometimes, code has conventions that convey typing/model information that just needs to be translated to a form Pysa understands. Don't be afraid of quick and dirty scripts to encode that information in a meaningful way.

For example, do all your Django view functions live in a file called `views.py` and have an untyped `request` argument as the first argument? Can you just do a quick `grep` + `sed` to add the `HttpRequest` type annotation to all of those parameters? Pysa has a ton of pre-written models for `HttpRequest`, so a small typing change like that can cover a ton of entry points.

## Hand Crafted Models

Consider all the ways that your application takes user input, and check to see if Pysa already has models written or not. For example, if you're using Flask as your web server rather than Django, you'll find Pysa doesn't currently have any pre-written models for Flask. You'll need to write some models following the instructions in the [previous pages of our docs](pysa_basics.md). If the models are generally useful to others, please consider putting up a pull request to contribute them back to Pysa.
