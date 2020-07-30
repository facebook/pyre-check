# Pysa Tutorial: Model Generators

The purpose of this exercise is to learn how to dynamically generate _models_
for sources and sinks. This is useful when there are too many sources/sinks to
write by hand, or they change too quickly to be maintainable.

The functions in `views.py` have been ported to use two different ways of
passing data into a Django-like API. In the process, they were made vulnerable
to RCE again, but Pysa cannot catch the issues yet. The goal of this exercise is
the make changes to `generate_models.py` so that it can dynamically generate
source annotations for `views.py`, allowing Pysa to catch the RCE
vulnerabilities.

**You do not need to directly modify any file other than `generate_models.py` in
this exercise.**

## What you need to know

### Dynamic Model Generators

Dynamic model generators run before Pysa, and generate `.pysa` files to identify
sources and sinks that are too difficult to write or maintain by hand. The
selection of model generators is always expanding, and they live in the
[`pyre-check`](https://github.com/facebook/pyre-check) repository within
`tools/generate_taint_models/get_*.py`. A summary of generators is available in
the [public
docs](https://pyre-check.org/docs/pysa-model-generators.html#example-model-generators).

## Instructions

### Use `generate_models.py`

The `operate_on_twos` function accepts a raw string in its interface, which is
extracted from the request URL, according to the pattern specified in `urls.py`.
This string is user-controlled, but Pysa does not know that. Update
`generate_models.py` to correctly generate a `.pysa` file, which will tell Pysa
that the `operator` argument to `operate_on_twos` is user-controlled.

_Note that this `urls.py` + `views.py` pattern is designed to closely resemble
how Django dispatches incoming requests._

1. Run `python3 generate_models.py --output-directory .` from this directory

1. Verify that you have a `.pysa` file called
   `generated_django_path_params.pysa`, and that the file correctly declares the
   `operator` argument to `operate_on_twos` as `TaintSource[UserControlled]`

1. Run `pyre analyze`, and verify you see **one issue** within a JSON list in
   the output.

### Extend `generate_models.py`

The `operate_on_threes` function uses a custom decorator which extracts
user-controlled data from the request and passes it in as arguments. Pysa
doesn't have good insight into decorators, so it misses this taint pattern.
Update `generate_models.py` to correctly generate a `.pysa` file, which will
tell Pysa that the `operator` argument to `operate_on_threes` is
user-controlled.

1. Read a summary of available model generators in the
   [docs](https://pyre-check.org/docs/pysa-model-generators.html#example-model-generators),
   and identify which one would be useful in this scenario.

   _Note that the full set of available dynamic model generators can be found by
   browsing `pyre-check/tools/generate_taint_models/`_

1. Uncomment the commented out code in `generate_models.py` and fill in the
   missing pieces to start using the generator you identified.

1. Run `python3 generate_models.py --output-directory .` from this directory

1. Verify that you have a new `.pysa` file called
   `generated_decorator_extracted_params.pysa`, in addition to the one from the
   previous section. Check that this file is correctly declaring the `operator`
   argument to `operate_on_threes` as `TaintSource[UserControlled]`

1. Run `pyre analyze`, and verify you see **two issues** within a JSON list in
   the output.
