# Pysa Tutorial: Writing Sources, Sinks, and Rules

The purpose of this exercise is to apply the learnings from Exercise 1 by
writing new _sources_, _sinks_ and _rules_.

The three functions in `views.py` all contain vulnerabilities, but none of them
will be caught initially when you run `pyre analyze`. The goal of this exercise
is the make the changes to `sources_sinks.pysa` and `taint.config` required to
catch the vulnerabilities.

## What you need to know

### Stubs

The `sources_sinks.pysa` file contains models with taint annotations. These
models have to match the stubs in `.pyi` stub file, if present, or the original
source code, otherwise. When translating a `.pyi` stub or source code to a
model, make sure:

1. The function name is unchanged
1. The argument names are unchanged
1. Type annotations are removed
1. The function or attribute is [fully
   qualified](https://pyre-check.org/docs/pysa-basics.html#fully-qualified-names)

Pyre's primary set of _stubs_ come from
[typeshed](https://github.com/python/typeshed). Pyre ships with its own copy of
typeshed, and you can find that copy in `$VIRTUAL_ENV/lib/pyre_check/typeshed`.

Exercise 1 already alluded to another set of stubs. That exercise mentioned
`search_path` in `.pyre_configuration`, which pointed to `../../stubs`. This is
where the stubs that were written just for Pysa are stored. These stubs cover
third party libraries such as Django, which are not in typeshed.

## Instructions

1. The `operate_on_twos` function in `views.py` has changed slightly from
   Exercise 1. It now takes data from `request.POST` rather than `request.GET`.
   Add a new line to `sources_sinks.pysa` to teach Pysa that `request.POST` is
   also user controlled. Don't forget it needs to match the stubs described
   above

   You will know you are done this step when you run `pyre analyze`, and see
   **one issue** within a JSON list in the output.

1. The `operate_on_threes` function is similar to `operate_on_twos`, but it uses
   `exec` rather than `eval`. Add a new line to `sources_sinks.pysa` to teach
   Pysa that `exec` also executes code. Don't forget it needs to exactly match
   the [`exec` stub provided by
   typeshed.](https://github.com/python/typeshed/blob/83e955b52ff0379538441df2cfd0b0e1b4aa5f7e/stdlib/2and3/builtins.pyi#L1249)

   You will know you are done this step when you run `pyre analyze`, and see
   **two issues** within a JSON list in the output.

1. The `operate_on_fours` function is similar to the previous two functions, but
   it uses a sink that executes shell code, rather than python code. The sink is
   already described in `sources_sinks.pysa`. Add a rule to `taint.config` to
   teach Pysa that it is also dangerous for user controlled data to be a part of
   shell code execution.

   You will know you are done this step when you run `pyre analyze`, and see
   **three issues** within a JSON list in the output.

## Debugging Tips

- `pyre analyze` erroring out? Make sure type annotations didn't sneak into your
  `.pysa` model files. The only annotations you should have should be taint
  annotations: `__source: TaintSink[CodeExecution]`. Make sure you remove all
  type annotations such as `__source: Union[str, bytes, CodeType]` from your
  `.pysa` files.
