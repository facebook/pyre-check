# Pysa Tutorial: Features and SAPP

The purpose of this exercise is to learn how to add _features_ to issues, so
that we can filter down to only the interesting issues with the _Static Analysis
Post Processor (SAPP)_.

Like the previous exercise, none of the functions in `views.py` are vulnerable,
but they will all show up in `pyre analyze` as false positives. The goal of this
exercise is to make changes to `features.pysa`, `taint.config`, and `views.py`
in order to add features that will allow the false positives to be filtered out
when browsing via SAPP.

## What you need to know

### Features

[_Features_](https://pyre-check.org/docs/pysa-features.html) are declared in
`taint.config` files and used in `.pysa` files, just like sources and sinks. An
example of both the declaration and usage are provided in `taint.config` and
`features.pysa` respectively. You will need to copy these examples to add a new
feature of your own.

### Static Analysis Post Processor (SAPP)

The [_Static Analysis Post Processor
(SAPP)_](https://pyre-check.org/docs/static-analysis-post-processor.html) ships
in a separate package from Pysa, which you should have already installed during
the initial setup of this tutorial. For projects larger than the toy examples in
these tutorials, SAPP is the preferred way to browse issues.

## Instructions

1. The numeric operations from the previous tutorial have been replaced with a
   set of `and` and `or` operations in this tutorial. These operations have been
   crafted carefully to ensure that they are not exploitable for RCE, however,
   they still trigger Pysa issues. Your goal is to use _features_ to filter
   these issues out while browsing results in SAPP.

   To start, run Pysa and open the results in SAPP:

   ```
   pyre analyze --save-results-to .
   sapp analyze taint-output.json
   sapp explore
   ```

   Within the SAPP prompt (you should see `>>>`), type `issues` and observe the
   output:

   ```
   ====================================================
   Interactive issue exploration. Type 'help' for help.
   ====================================================

   [ run 1 ]
   >>> issues
   ```

   You will know you are done this step when you see **2 issues** displayed.

1. `do_and` ensures RCE is not possible by converting both user controlled
   arguments to `bool`. Try to identify an _automatically added feature_ that
   indicates `bool` was used, and use that feature to filter out `do_and` from
   your results.

   1. Find the issue representing `do_and` in the issues output you got in the
      previous step; try looking for `Callable: views.do_and` in the output.

   1. Look at the features listed for this issue, and identify one which looks
      like it indicates all flows were converted to `bool`.

   1. Filter out this issue within the SAPP prompt:

      ```
      >>> issues(exclude_features=["<REPLACE_ME>"])
      ```

      You will know you are done this section when you see **one issue**
      displayed.

1. `do_or` ensures RCE is not possible by asserting that both user controlled
   arguments are numeric. Try to add a feauture to all arguments passed to
   `assert_numeric`, and use that feature to filter out `do_or` from your
   results.

   1. Update `taint.config` to contain a new entry for the feature you're going
      to add.

   1. Update `features.pysa` to contain a model that will add a feature to all
      arguments passed into `assert_numeric`

   1. Re-run pysa and open the new results in SAPP

      ```
      pyre analyze --save-results-to .
      sapp analyze taint-output.json
      sapp explore
      ```

   1. Query issues with the `issues(exclude_features=["<REPLACE_ME>"])`
      query you wrote in the previous step. The issue representing `do_or`
      should be the only remaining issue you see in your SAPP output; ensure you
      see `Callable: views.do_or` the issue output.

   1. Identify the featue you added in the SAPP issue output, and update your
      query to filter out this issue:

      ```
      >>> issues(exclude_features=["<REPLACE_ME>", "<AND_REPLACE_ME>"])
      ```

      You will know you are done this exercise when you see **zero issue**
      displayed.

## Debugging Tips

- `pyre analyze` erroring out? Try these strategies:
  -  Make sure type annotations didn't sneak into your `.pysa` model files. The
     only annotations you should have should be feature annoations:
     `AddFeatureToArgument[Via[FEATURE_NAME]]`. Make sure you remove all type
     annotations such as `-> None` from your `.pysa` files.
  - Make sure you're using **fully qualified names**. All of your models for
    functions in this file should look like `def
    FILE_NAME.function_name(argument: ANNOTATION): ...`
- `sapp` erroring out? Try these strategies:
  - Make sure you installed the necessary dependencies described in
    `pyre-check/pysa_tutorial/README.md`
