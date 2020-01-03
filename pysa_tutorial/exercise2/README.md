# Writing sources, sinks, and rules
## Overview
The purpose of this exercise is to apply the learnings from Exercise 1 by writing new _sources_, _sinks_ and _rules_.

The three functions in `views.py` all contain vulnerabilities, but none of them will be caught initially when you run `pyre analyze`. The goal of this exercise is the make the changes to `sources_sinks.pysa` and `taint.config` required to catch the vulnerabilities.

## What you need to know
### Stubs
The `sources_sinks.pysa` file contains stubs with annotations, which have to match the stub file that Pyre uses. Pyre's primary set of stubs come from [typeshed](https://github.com/python/typeshed). Pyre ships with it's own copy of typeshed, and you can find that copy in `$VIRTUAL_ENV/lib/pyre_check/typeshed/`.

Exercise 1 already alluded to another set of stubs. That exercise mentioned `search_path` in `.pyre_configuration`, which pointed to `../../stubs/`. This is where the stubs that were written just for Pysa are stored. These stubs cover third party libraries such as Django, which aren't in typeshed.

## Instructions

1. `operate_on_twos` has changed slightly from Exercise 1. It now takes data from `request.POST` rather than `request.GET`. Add a new line to `sources_sinks.pysa` to teach Pysa that `request.POST` is also user controlled. Don't forget it needs to match the stubs described above

   You will know you are done this step when you run `pyre analyze`, and see **1 issue** in the output.

1. `operate_on_threes` is similar to `operate_on_twos`, but it now uses `exec` rather than `eval`. Add a new line to `sources_sinks.pysa` to teach Pysa that `exec` also executes code. Don't forget it needs to match the stubs described above

   You will know you are done this step when you run `pyre analyze`, and see **2 issues** in the output.

1. `operate_on_fours` is similar to the previous two functions, but it uses a sink that executes shell code, rather than python code. The sink is already described in `sources_sinks.pysa`. Add a rule to `taint.config` to teach Pysa that it is also dangerous for user controlled data to be a part of shell code execution.

   You will know you are done this step when you run `pyre analyze`, and see **3 issues** in the output.
