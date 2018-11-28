---
id: error-suppression
title: Error Suppression
sidebar_label: Error Suppression
---

Pyre throws errors on inferred or explicitly conflicting type annotations, and on type-incompatible usage and access.

In default mode, pyre **will not** throw errors error on:

1. `Any` types
2. Missing annotations
3. Code within functions whose return type is not explicitly annotated

Strict mode (`# pyre-strict`) will not suppress any of the above errors in a file.

Declare mode (`# pyre-ignore-all-errors`) will suppress all errors in a file.

## Explicitly Suppressing Errors

Pyre will ignore errors on lines marked with `# pyre-ignore`,
`# pyre-fixme`, or `# type: ignore`.

You may also ignore pyre errors on line X by adding an ignore comment to line X-1,
as long as nothing else is on line X-1.

To only ignore a specific kind of error (denoted in the pyre error message),
you can add the error code to the end of the ignore: i.e. `# pyre-ignore[7]`.

Examples:

```
  # same-line ignore
  def foo() -> int:
     return "string"  # pyre-ignore

  # previous-line ignore
  def foo() -> int:
     # pyre-ignore
     return "string"

  # only ignore return errors
  def foo() -> int:
     return a.undefined # pyre-ignore[7]
```

## Suppression Comment Types

`# pyre-fixme` suppresses a type error with the intention of coming back to fix it later.

`# pyre-ignore` signifies that there is something wrong with the type error,
and so no further work needs to be done to fix this error where it occurs.
Investigation should be done on the type checker side.*

`# type: ignore` is MyPy's error ignore syntax and is compatible with Pyre.

*Ensure your issue is addressed by the Pyre team by [opening an issue](https://github.com/facebook/pyre/issues)
describing a way to reproduce the problem you encountered.
