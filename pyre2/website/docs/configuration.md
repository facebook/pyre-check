---
id: configuration
title: Configuration
slug: /configuration

description: Instructions for configuring Pyrefly
---

# Pyrefly Configuration

Pyrefly has a basic configuration that can (or will) allow you to customize your
Pyrefly runs without having to specify all of your arguments on the command
line.

NOTE: this is early in its development, so the options listed here are subject
to change in name, usage, type, quantity, and structure.

Configurations can be specified in a `pyre.toml` file, with all configuration
options in the top-level of the document. You can also specify a configuration
in a `pyproject.toml` under a `[tool.pyrefly]` section.

NOTE: At the moment, configuration finding is not supported. It will be
implemented in the near future. In the meantime, please explicitly specify your
configuration with the `-c <config>` or `--config <config>` flags when running
Pyrefly.

Both absolute and config-relative paths are supported.

## Precedence in Options

The following is the order in which configuration options are selected:

1. CLI flag
2. Environment variable override -- This is the same as `PYRE_<CLI flag name>`
3. Configuration option
4. Hard-coded default

# Configuration Options

- `project_includes`: the patterns used to describe which files to type check,
  typically understood as user-space files. This takes highest precedence in
  import resolution.
  - Type: list of Unix Glob patterns
  - Default: `["./**/*.py"]`
  - Flag equivalent: Equivalent to `FILES...`
  - Equivalent Configs: `include` in Pyright, `files`/`modules`/`packages` in
    MyPy
- `search_path`: a file path describing a root from which imports should be
  found and imported from (including modules in `project_includes`). This takes
  the highest precedence in import order, before `typeshed` and
  `site_package_path`. When a `project_includes` type checked file is imported
  by another type checked file, we check all search roots to determine how to
  import it.
  - Type: list of directories
  - Default: `["."]`
  - Flag equivalent: `--search-path`
  - ENV equivalent: `PYRE_SEARCH_PATH`
  - Equivalent Configs: `extraPaths` in Pyright, `mypy_path` in MyPy
  - NOTE: we automatically append `"."` (the directory containing the
    configuration file) to the `search_roots` when type checking as a sensible
    default and last attempt at an import.
- `site_package_path`: a file path describing a root from which imports should
  be found and imported from. This takes the lowest priority in import
  resolution, after `project_includes`, `typeshed`, and `search_roots`.
  - Type: list of directories
  - Default: `["."]`
  - Flag equivalent: `--site-package-path`
  - ENV equivalent: `PYRE_SITE_PACKAGE_PATH`
  - Equivalent Configs: none
- `python_platform`: the value used with conditions based on type checking
  against
  [`sys.platform`](https://docs.python.org/3/library/sys.html#sys.platform)
  values.
  - Type: string
  - Default: `"linux"`
  - Flag equivalent: `--python-platform`
  - ENV equivalent: `PYRE_PYTHON_PLATFORM`
  - Equivalent Configs: `pythonPlatform` in Pyright, `platform` in MyPy
- `python_version`: the value used with conditions based on type checking
  against
  [`sys.version`](https://docs.python.org/3/library/sys.html#sys.version)
  values. The format should be `<major>[.<minor>[.<micro>]]`, where minor and
  micro can be omitted to take the default positional value.
  - Type: string
  - Default: `3.13.0`
  - Flag equivalent: `--python-version`
  - ENV equivalent: `PYRE_PYTHON_VERSION`
  - Equivalent Configs: `pythonVersion` in Pyright, `python_version` in MyPy
