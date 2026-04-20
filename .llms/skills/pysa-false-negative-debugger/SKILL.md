---
name: pysa-false-negative-debugger
description: Use when debugging a Pysa false negative (missing taint issue), comparing two Pysa output directories, or finding where taint flow is lost.
oncalls: [pysa]
---

# Pysa Model Debugger

## Overview

Systematic workflow that identifies exactly where and why taint flow is lost by comparing two Pysa result directories. Accepts static analysis issue URLs (e.g., `https://www.internalfb.com/security/static_analysis/issue/<issue_instance_id>?database=<database>`).

**REQUIRED BACKGROUND:** Load the `pysa-json-models` skill to understand trace element syntax (ports, call info, kinds).

## When to Use

- User reports a Pysa false negative (issue found in run A, missing in run B)
- User provides a static analysis issue URL

## Prerequisites

The user must provide:
1. An issue URL: `https://www.internalfb.com/security/static_analysis/issue/<issue_instance_id>?database=<database>`
2. Two Pysa output directories: one where the issue IS found (FOUND), one where it is NOT (NOT-FOUND)

## Workflow

### Step 1: Extract Issue Metadata

From the URL, extract:
- **Issue instance ID**: the numeric ID in the path (e.g., `216172782209137158`)
- **Database**: the `database` query parameter (e.g., `xdb.pysa-instagram-sharded.1`)

Get the issue handle:
```bash
db <database> -e "SELECT handle FROM issues WHERE id=(SELECT issue_id FROM issue_instances WHERE id=<issue_instance_id>);"
```

If the query fails, ask the user for help.

### Step 2: Parse the Handle

Example handle:
```
accounts.service.Service.async_is_phone_suspicious:5120:0:Call|accounts.service.Service._async_helper|0|f:ec82abb59a9d0e207fe4f5acc361f0ad
```

Extract:
- **Root callable**: everything before the first `:` (e.g., `accounts.service.Service.async_is_phone_suspicious`)
- **Issue code**: the number after the first `:` (e.g., `5120`)

### Step 3: Explorer Tool

All commands below use `buck run` to invoke the explorer. The shorthand `<explorer>` means:
```bash
buck run fbcode//tools/pyre/tools/pysa_model_explorer_cli:pysa_model_explorer --
```

Run `<explorer> --help` for usage details.

**Suppressing build noise:** Always append `2>/dev/null` to explorer commands to suppress buck build output that clutters the results. Only retry *without* `2>/dev/null` if the command produces empty or unexpected output, so you can see the actual error message from buck.

### Step 4: Verify the Issue

Confirm the issue exists in FOUND:
```bash
<explorer> /tmp/FOUND get-issues <root-callable> --handle <handle>
```

If not found, ask the user for help.

Confirm it does NOT exist (or is very different) in NOT-FOUND:
```bash
<explorer> /tmp/NOT-FOUND get-issues <root-callable> --code <code>
```

The result should be empty or contain very different issues (different locations).

### Step 5: Investigate Where Taint Is Lost

Check these options **in order**: A → B → C.

#### Option A: Source Trace (Forward)

In the issue from FOUND, examine `"traces"` → entries with `"name": "forward"`.

Each root (trace element) may have multiple kinds with different `"length"` values. Pick the root whose minimum `"length"` across its kinds is smallest (missing length = 0, which is always shortest).

**If it's an origin**: the source comes from a user-annotated function. Check if at least one of the leaves have a model in NOT-FOUND:
```bash
<explorer> /tmp/NOT-FOUND get-model <leaf-callable> --show sources --kind <kind>
```
If none of the leaves have source taint in NOT-FOUND, taint is lost at one of these leaf callables. If at least one does, the source trace is intact — move to Option B.

**If it's a call** with `"resolves_to": [<callee-callable>, ...]`: check if those callables have source taint in NOT-FOUND. If there are multiple entries (overrides), check all of them — taint is lost only if ALL are missing:
```bash
<explorer> /tmp/NOT-FOUND get-model <callee-callable> --show sources --kind <kind>
```

Interpret the results:
- **No entries** → taint is lost here or deeper. Keep diving into that callable's model in FOUND and repeat.
- **Entries with wrong ports** (e.g., issue has `"port": "formal(a)"` in the `"call"` section, but model has sources for `"formal(b)"`) → these don't count. Taint is still lost.
- **Matching entries found** → source trace is intact. Move to Option B.

When you find a missing source, double-check it exists in FOUND:
```bash
<explorer> /tmp/FOUND get-model <leaf-or-callee-callable> --show sources --kind <kind>
```

#### Option B: Sink Trace (Backward)

Same process as Option A, but for `"name": "backward"` traces. Use `--show sinks` instead of `--show sources` with `get-model`.

#### Diving Deeper (Options A/B)

When Option A or B identifies a callee whose model is present in FOUND but missing in NOT-FOUND, you need to find exactly which function in the call chain lost the taint. Recurse into that callee's model:

1. **Examine the callee's model in FOUND**:
```bash
# For a missing source (Option A):
<explorer> /tmp/FOUND get-model <first-source-callee> --show sources --kind <kind>
# For a missing sink (Option B):
<explorer> /tmp/FOUND get-model <first-sink-callee> --show sinks --kind <kind>
```

2. **Pick the frame with the shortest length** (same strategy as in Option A/B).

3. **Identify the next callee**: if the frame is a `"call"`, use `"resolves_to"` to get the callee. If it's an `"origin"`, use the leaf names.

4. **Check the next callee's model in NOT-FOUND**:
```bash
<explorer> /tmp/NOT-FOUND get-model <next-callee> --show sources --kind <kind>
```

5. **Interpret**:
   - **Model exists in NOT-FOUND** → taint is lost within the previous callee (e.g., `<first-source-callee>`). Use the Option C strategy on that callable: read its source code, check its call graph and TITO models to find the break.
   - **Model missing in NOT-FOUND** → taint is lost deeper. Recurse: go back to step 1 with the next callee.

Continue until you find the function where taint is present in its callees but lost in its own model. Then apply Option C's strategy (call graph + TITO checks) to that function to pinpoint the root cause.

#### Option C: Taint Lost in Root Callable

If both forward and backward traces are correct, taint is lost within the root callable itself.

1. **Read the source code** using `"filename"` and location from the issue. Filenames are usually relative to `~/fbsource`.

2. **Check the call graph**:
```bash
<explorer> /tmp/NOT-FOUND get-call-graph <root-callable>
```
If you know the relevant line numbers (e.g., from the issue location or source code), use `--start-line` and `--end-line` to filter to that range:
```bash
<explorer> /tmp/NOT-FOUND get-call-graph <root-callable> --start-line <line> --end-line <line>
```
This avoids noise from irrelevant call graph edges in large functions.

Verify calls to source and sink are present at the right locations.

3. **Check intermediate calls**. For flows like:
```python
a = source()
b = foo(a)
sink(b)
```
`foo` is an intermediate call. Check:
- Is `foo` resolved in the call graph?
- If resolved (say, to `my_module.foo`), check its TITO model:
```bash
<explorer> /tmp/NOT-FOUND get-model my_module.foo --show tito
```
- Look at propagations. If TITO is missing, keep diving deeper: investigate `foo`'s call graph and models to find where propagation breaks.

### Step 6: Produce Summary

Your output must include:
1. **Issue description**: root callable, code, handle, filename
2. **Path to the lost taint**: chain of callables from root to where taint is lost (include all callable names and ports)
3. **Which trace**: source (forward) or sink (backward)
4. **Root cause**: why taint is lost and where exactly

## Quick Reference: Explorer Commands

| Command | Purpose |
|---------|---------|
| `<explorer> <dir> get-issues <callable> --handle <handle>` | Find specific issue |
| `<explorer> <dir> get-issues <callable> --code <code>` | Find issues by code |
| `<explorer> <dir> get-issues <callable> --show-leaf-names` | Show leaf callables |
| `<explorer> <dir> get-model <callable> --show sources --kind <kind>` | Check source taint |
| `<explorer> <dir> get-model <callable> --show sinks --kind <kind>` | Check sink taint |
| `<explorer> <dir> get-model <callable> --show tito` | Check TITO propagation |
| `<explorer> <dir> get-call-graph <callable>` | Check call resolution (`--start-line`, `--end-line` to filter by line range) |
| `<explorer> <dir> get-overrides <callable>` | List all overrides for a method |
| `<explorer> <dir> search <regex>` | Search for callables |

`<explorer>` is shorthand for `buck run fbcode//tools/pyre/tools/pysa_model_explorer_cli:pysa_model_explorer --`.

Additional flags: `--show-features`, `--show-tito-positions`, `--show-class-intervals`, `--format text`.

## Common Mistakes

| Mistake | Fix |
|---------|-----|
| Searching the wrong trace first | Always check forward (A) → backward (B) → local (C) in order |
| Ignoring port mismatches | `formal(a)` ≠ `formal(b)` — matching kind with wrong port means taint is still lost |
| Not cross-checking against FOUND | Always confirm the model exists in FOUND before concluding it's missing in NOT-FOUND |
| Skipping call graph check | Unresolved calls are a common root cause for lost taint |
| Stopping at first missing model | The missing model might itself be caused by a deeper missing propagation — keep diving |
| Checking only one `resolves_to` entry | When there are multiple entries (overrides), check ALL of them — taint flows if any has the model |
| Using `get-model` on `Overrides{module.Class.foo}` | Override targets don't have models. Use `get-overrides module.Class.foo` to list all overrides, then `get-model` on each override |
| Using grep/jq on raw JSON files | Always use `pysa_model_explorer` — it handles indexing and filtering efficiently |
| Forgetting `--show-leaf-names` on origins | Without this flag, you can't see which callables originated the taint |
| Forgetting `2>/dev/null` on explorer commands | Buck build output clutters results — always append `2>/dev/null` and only remove it to diagnose errors |
| Using knowledge_load | Avoid using knowledge_load on the given issue URL, prefer local results |
