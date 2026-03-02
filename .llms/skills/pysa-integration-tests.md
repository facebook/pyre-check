---
name: pysa-integration-tests
description: Use when running, debugging, updating, or creating Pysa end-to-end integration tests. Use when taint analysis tests fail, when expected output files need updating, or when working with .models, .cg, .hofcg, .overrides files under `source/interprocedural_analyses/taint/test/integration`.
---

# Pysa Integration Tests

## Overview

End-to-end integration tests for the Pysa taint analysis engine. Each test is a `.py` file under `source/interprocedural_analyses/taint/test/integration/`. Tests run the full taint analysis pipeline and compare output against expected files.

## Running Tests

All commands must be run from the `source/` directory.

```bash
cd source

# Run ALL tests (parallelized with 16 shards)
OUNIT_SHARDS=16 dune exec interprocedural_analyses/taint/test/integrationTest.exe

# Run a SINGLE test (e.g., format.py)
PYSA_INTEGRATION_TEST=format.py dune exec interprocedural_analyses/taint/test/integrationTest.exe
```

### With Pyrefly Frontend

By default, tests use Pyre as the type-checking frontend. To use Pyrefly instead:

```bash
# 1. Build pyrefly
buck2 build --show-full-simple-output @fbcode//mode/opt fbcode//pyrefly/pyrefly:pyrefly
# This prints a path like: /data/users/<user>/.../__pyrefly__/pyrefly on stdout.

# 2. Run tests with PYREFLY_BINARY set
PYREFLY_BINARY=<path-to-binary> PYSA_INTEGRATION_TEST=format.py dune exec interprocedural_analyses/taint/test/integrationTest.exe
```

## Test File Structure

Each test `<name>.py` may have these companion files:

| File | Required | Purpose |
|------|----------|---------|
| `<name>.py` | Yes | Python source code to analyze |
| `<name>.py.pysa` | No | Pysa model file: declares sources, sinks, TITO |
| `<name>.py.config` | No | Taint configuration: rules, sources, sinks, options |
| `<name>.py.models` | Yes | Expected output: taint models and issues (JSON) |
| `<name>.py.cg` | Yes | Expected output: call graph |
| `<name>.py.hofcg` | Yes | Expected output: higher-order call graph |
| `<name>.py.overrides` | Yes | Expected output: override graph |
| `<name>.py.pyrefly.models` | No | Expected Pyrefly output: models |
| `<name>.py.pyrefly.cg` | No | Expected Pyrefly output: call graph |
| `<name>.py.pyrefly.hofcg` | No | Expected Pyrefly output: higher-order call graph |
| `<name>.py.pyrefly.overrides` | No | Expected Pyrefly output: overrides |

**Default models:** When no `.pysa` and no `.config` file is present, the test runner automatically provides default test models (`_test_sink`, `_test_source`, etc.). When either file is present, the test must be self-contained.

## Debugging Test Failures

When a test fails because expected output doesn't match:

1. The test runner prints a diff (may be truncated for large diffs)
2. It creates `.actual` files for each mismatched output (e.g., `format.py.models.actual`)
3. Use `diff` to compare expected vs actual:

```bash
diff source/interprocedural_analyses/taint/test/integration/format.py.models \
     source/interprocedural_analyses/taint/test/integration/format.py.models.actual
```

If the test fails with type errors or analysis errors (not output mismatches), the issue is in the Python source or model definitions.

## Updating Expected Files

If the output changes are expected (e.g., you intentionally changed the analysis):

```bash
# Automatically moves all .actual files to their expected counterparts
facebook/scripts/in_path/pysa-update-expected
```

This replaces each `<name>.py.<ext>` with the corresponding `<name>.py.<ext>.actual`.

**Always review the diff before updating** — run `sl diff` after updating to verify changes are intentional.

## Creating a New Test

1. Create `<name>.py` under `source/interprocedural_analyses/taint/test/integration/`
2. Optionally create `<name>.py.pysa` (model file) and/or `<name>.py.config` (taint config)
3. Run the test — the runner auto-creates missing expected output files on the first run:

```bash
PYSA_INTEGRATION_TEST=<name>.py dune exec interprocedural_analyses/taint/test/integrationTest.exe
```

4. Review the generated `.models`, `.cg`, `.hofcg`, `.overrides` files
5. Commit all files together

## Common Mistakes

- **Wrong env var name**: It's `PYSA_INTEGRATION_TEST`, not `PYSA_TEST` or similar
- **Forgetting the `.py` extension**: Use `PYSA_INTEGRATION_TEST=format.py`, not `format`
- **Running from wrong directory**: `dune exec` must be run from `source/`
- **Guessing OUnit flags**: Don't use `-- format` to filter tests; use the `PYSA_INTEGRATION_TEST` env var
- **Manually editing expected files**: Use `pysa-update-expected` instead; expected files are `@generated`
- **Forgetting OUNIT_SHARDS**: Running all tests without `OUNIT_SHARDS=16` is much slower
