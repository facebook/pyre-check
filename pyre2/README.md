# Pyre2

NOT INTENDED FOR EXTERNAL USE YET. INCOMPLETE AND IN DEVELOPMENT.

We are building a new version of Pyre (the Python type checker), named Pyre2, to increase our internal
velocity and enable new features such as producing typed ASTs. We aim to fully replace the existing
Pyre by the end of 2025.

## Developer cheat sheet

From this directory, if you are inside Meta, you can run:

* Check things are plausible: `./test.py` (runs the basic tests and linter)
* Run a command: `buck2 run pyre2 -- COMMAND_LINE_ARGUMENTS`
  * For example, run on a single file: `buck2 run pyre2 -- check test.py`
* Run a single test: `buck2 test pyre2 -- NAME_OF_THE_TEST`
* Run `arc pyre` (a.k.a. per-target type checking) with `minipyre`:
  `arc pyre check <targets_to_check> -c python.type_checker=fbcode//tools/pyre/pyre2:minipyre_for_buck`

## Coding conventions

We follow the [Buck2 coding conventions](https://github.com/facebook/buck2/blob/main/HACKING.md#coding-conventions),
with the caveat that we use our internal error framework for errors reported by the type checker.
