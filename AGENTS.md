# Guidance for Project Agents

## Project Overview

**Pyre** is Meta's performant type checker for Python (PEP 484 compliant).

It includes **Pysa**, a security-focused static analysis tool for taint analysis.

The core engine is written in OCaml (`source/`), with a Python CLI client (`client/`).
Note: [Pyrefly](https://github.com/facebook/pyrefly) is the next-generation replacement.

## Build and Test Commands

### Detecting Meta Internal vs Open Source

Assume the user is a Meta internal user by default. If the `facebook/` directory is missing from the root of the project, the user is an open source user. All `buck` and `arc` commands are only available to Meta internal users.

### Configure

If the file `source/dune` is missing, this must be a fresh repository clone.
In that case, run the following command:

```bash
# For Meta internal users (facebook/ directory exists)
./facebook/scripts/setup.sh --local
# For open source users
./scripts/setup.sh --local
```

### Building the OCaml binary

```bash
cd source && make
```

The built binary is at `source/_build/default/main.exe`.

### Running OCaml tests

```bash
# All OCaml tests
cd source && make test

# Specific OCaml test - e.g. analysis/test/integration/methodTest.ml:
cd source && dune exec analysis/test/integration/methodTest.exe
```

### Running Python tests

```bash
# For Meta internal users
buck test fbcode//tools/pyre/client/...
```

### Building the Python CLI (Buck)

```bash
# For Meta internal users
buck build fbcode//tools/pyre/client:pyre
```

### Linting and Formatting

**IMPORTANT: Always run linting and formatting after editing files.**

For Meta internal users:
```bash
# Formatting
arc f
# Linting
arc lint
```

### Testing against real code

Set `PYRE_BINARY` to your built binary to override the default:
```bash
export PYRE_BINARY=/path/to/source/_build/default/main.exe
pyre -n check  # Use one-off check mode, not the server, when recompiling frequently
```

### Pyrefly Integration

By default, Pysa uses Pyre as its type provider. It can also use [Pyrefly](https://github.com/facebook/pyrefly) instead.

Building the Pyrefly binary requires `buck2` (Meta internal only):

```bash
buck2 build --show-full-simple-output @fbcode//mode/opt fbcode//pyrefly/pyrefly:fbpyrefly
# This prints a path like: /data/users/<user>/.../__pyrefly__/fbpyrefly on stdout.
```

To run OCaml tests with Pyrefly, set the `PYREFLY_BINARY` environment variable:

```bash
cd source && PYREFLY_BINARY=<path-to-pyrefly-binary> make test
```

## Coding Style

- Avoid abbreviations
- Prefer `snake_case` for variables/functions, `CamelCase` for modules/classes
