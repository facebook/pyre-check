# Model Generators
## Overview
The purpose of this exercise is to learn how to dynamically generate models for sources and sinks. This is useful when there are too many sources/sinks to write by hand, or they change too quickly to be maintainable.

The functions in `views.py` have been ported to use two different ways of passing data into a Django-like API. In the process, they were made vulnerable to RCE again, but Pysa can't catch the issue yet. The goal of this exercise is the make changes to `generate_models.py` so that it can dynamically generate source annotations for `views.py`, allowing Pysa to catch the RCE vulnerabilities.

**If you directly modify any file other than `generate_models.py` in this exercise, you are doing something wrong**

## What you need to know
### Dynamic Model Generators
Dynamic model generators run before Pysa, and generate `.pysa` files to identify sources and sinks that are too difficult to write and maintain by hand. The selection is always expanding, and they live in `pyre-check/tools/generate_taint_models/get_*.py`. A summary of ones that will be useful in this exercise is available in the [public docs](https://pyre-check.org/docs/pysa-model-generators.html#example-model-generators).

## Instructions

1. `operate_on_twos` accepts a raw string in its interface, which is extracted from the request URL, according to the pattern specified in `urls.py`. This string is user-controlled, but Pysa doesn't know that. Update `generate_models.py` to correctly generate a `.pysa` file, which will tell Pysa that the `operator` argument to `operate_on_twos` is user-controlled. _Note that this `urls.py` + `views.py` pattern is designed to closely resemble how Django dispatches incoming requests._

   1. Replace the TODO comment sections in `generate_models.py` with the correct dynamic model generator to taint a Django-style API

   1. Run `python3 generate_models.py --output-directory .` from this directory

   1. Verify that you have a `.pysa` file generated for you, and that the file correctly declares the `operator` argument to `operate_on_twos` as `TaintSource[UserControlled]`

   1. Run `pyre analyze`, and verify you see **1 issue** in the output.

1. `operate_on_threes` uses a custom decorator which extracts user-controlled data from the request and passes it in as arguments. Pysa doesn't have good insight into decorators, so it misses this taint pattern. Update `generate_models.py` to correctly generate a `.pysa` file, which will tell Pysa that the `operator` argument to `operate_on_threes` is user-controlled.

   1. Look through the available dynamic model generators in in `pyre-check/tools/generate_taint_models/`, or read a summary of a few in the [public docs](https://pyre-check.org/docs/pysa-model-generators.html#example-model-generators). Identify which one would be useful in this scenario

   1. Update `generate_models.py` to also call the generator you identified.

   1. Run `python3 generate_models.py --output-directory .` from this directory

   1. Verify that you have a new `.pysa` file generated, in addition to the one from the previous section. Check that this file is correctly declaring the `operator` argument to `operate_on_threes` as `TaintSource[UserControlled]`

   1. Run `pyre analyze`, and verify you see **2 issues** in the output.
