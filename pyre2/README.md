# Pyre2

NOT INTENDED FOR EXTERNAL USE YET. INCOMPLETE AND IN DEVELOPMENT.

We are building a new version of Pyre (the Python type checker), named Pyre2, to
increase our internal velocity and enable new features such as producing typed
ASTs. We aim to fully replace the existing Pyre by the end of 2025.

## Developer cheat sheet

### GitHub developers

`cd pyre2` then use the normal `cargo` commands (e.g. `cargo build`,
`cargo test`).

Typeshed can be fetched from upstream into the codebase using the following
command (assuming this is the current directory):
`python scripts/fetch_typeshed.py -o pyre2/third_party`

### Meta internal developers

From this directory, you can run:

- Check things are plausible: `./test.py` (runs the basic tests and linter)
- Run a command: `buck2 run pyre2 -- COMMAND_LINE_ARGUMENTS`
  - For example, run on a single file: `buck2 run pyre2 -- check test.py`
- Run a single test: `buck2 test pyre2:pyre2_library -- NAME_OF_THE_TEST`
- Run the end-to-end tests: `buck2 test test:`
- Run `arc pyre` (a.k.a. per-target type checking) with Pyre2:
  `arc pyre check <targets_to_check> -c python.type_checker=fbcode//tools/pyre/pyre2:pyre2_for_buck`
- Debug a file: `buck2 run pyre2 -- check <filename> --debug-info=debug.js`,
  then open `debug.html` in your browser
- Fetch Typeshed from upstream
  `HTTPS_PROXY=https://fwdproxy:8080 fbpython scripts/fetch_typeshed.py -o pyre2/third_party`

## Packaging

We use [maturin](https://github.com/PyO3/maturin) to build wheels and source
distributions. This also means that you can pip install `maturin` and use
`maturin build` and `maturin develop` for local development. `pip install .` in
the `pyre2/pyre2` directory works as well.

## Coding conventions

We follow the
[Buck2 coding conventions](https://github.com/facebook/buck2/blob/main/HACKING.md#coding-conventions),
with the caveat that we use our internal error framework for errors reported by
the type checker.

## Choices

There are a number of choices when writing a Python type checker. We are take
inspiration from Pyre1, Pyright and MyPy. Some notable choices:

- We infer types in most locations, apart from parameters to functions. We do
  infer types of variables and return types. As an example,
  `def foo(x): return True` would result in something equivalent to had you
  written `def foo(x: Any) -> bool: ...`.
- We attempt to infer the type of `[]` to however it is used first, then fix it
  after. For example `xs = []; xs.append(1); xs.append("")` will infer that
  `xs: List[int]` and then error on the final statement.
- We use flow types which refine static types, e.g. `x: int = 4` will both know
  that `x` has type `int`, but also that the immediately next usage of `x` will
  be aware the type is `Literal[4]`.
- We aim for large-scale incrementality (at the module level) and optimised
  checking with parallelism, aiming to use the advantages of Rust to keep the
  code a bit simpler.
- We expect large strongly connected components of modules, and do not attempt
  to take advantage of a DAG-shape in the source code.

## Design

There are many nuances of design that change on a regular basis. But the basic
substrate on which the checker is built involves three phases:

1. Figure out what each module exports. That requires solving all `import *`
   statements transitively.
2. For each module in isolation, convert it to bindings, dealing with all
   statements and scope information (both static and flow).
3. Solve those bindings, which may require the solutions of bindings in other
   modules.

If we encounter unknowable information (e.g. recursion) we use `Type::Var` to
insert placeholders which are filled in later.

### Example of bindings

Given the program:

```python
1: x: int = 4
2: print(x)
```

We might produce the bindings:

- `define int@0` = `from builtins import int`
- `define x@1` = `4: int@0`
- `use x@2` = `x@1`
- `anon @2` = `print(x@2)`
- `export x` = `x@2`

Of note:

- The keys are things like `define` (the definition of something), `use` (a
  usage of a thing) and `anon` (a statement we need to type check, but don't
  care about the result of).
- In many cases the value of a key refers to other keys.
- Some keys are imported from other modules, via `export` keys and `import`
  values.
- In order to disamiguate identifiers we use the textual position at which they
  occur (in the example I've used `@line`, but in reality its the byte offset in
  the file).

### Example of `Var`

Given the program:

```python
1: x = 1
2: while test():
3:     x = x
4: print(x)
```

We end up with the bindings:

- `x@1` = `1`
- `x@3` = `phi(x@1, x@3)`
- `x@4` = `phi(x@1, x@3)`

The expression `phi` is the join point of the two values, e.g. `phi(int, str)`
would be `int | str`. We skip the distinction between `define` and `use`, since
it is not necessary for this example.

When solving `x@3` we encounter recursion. Operationally:

- We start solving `x@3`.
- That requires us to solve `x@1`.
- We solve `x@1` to be `Literal[1]`
- We start solving `x@3`. But we are currently solving `x@3`, so we invent a
  fresh `Var` (let's call it `?1`) and return that.
- We conclude that `x@3` must be `Literal[1] | ?1`.
- Since `?1` was introduced by `x@3` we record that `?1 = Literal[1] | ?1`. We
  can take the upper reachable bound of that and conclude that
  `?1 = Literal[1]`.
- We simplify `x@3` to just `Literal[1]`.
