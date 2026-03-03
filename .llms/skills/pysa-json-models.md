---
name: pysa-json-models
description: Use when reading, writing, or debugging Pysa JSON model output (.models files). Use when working with taint models that describe sources, sinks, TITO, sanitizers, or issues in JSON format.
oncalls: [pysa]
---

# Pysa JSON Model Syntax

## Overview

Pysa outputs analysis results as **newline-delimited JSON** (NDJSON). Each line is a self-contained JSON object with `"kind"` and `"data"` fields. There are two kinds: `"model"` (a callable's taint behavior) and `"issue"` (a detected vulnerability).

## Top-Level Wrapper

Every JSON object is wrapped as:
```json
{"kind": "model", "data": { ... }}
{"kind": "issue", "data": { ... }}
```

Models and issues are **separate top-level objects**. Issues are never nested inside models.

## Model Structure

```json
{
  "callable": "module.function_name",
  "filename": "module.py",
  "callable_line": 42,
  "sources": [ ... ],
  "sinks": [ ... ],
  "tito": [ ... ],
  "parameter_sources": [ ... ],
  "global_sanitizer": { ... },
  "parameters_sanitizer": { ... },
  "sanitizers": [ ... ],
  "modes": [ "Obscure", ... ]
}
```

**All fields except `callable` are optional. Empty fields are omitted entirely** — never include empty arrays `[]` or objects `{}`.

Note: `"sources"` represents *generations* (taint produced by the function). This is distinct from `"parameter_sources"` which represents sources on parameters.

### Callables

- Regular functions: `"module.function_name"`
- Methods: `"module.ClassName.method_name"`
- Attribute models: `"Obj{module.ClassName.attribute_name}"`

### Modes

Available modes: `Obscure`, `SkipObscure`, `SkipAnalysis`, `SkipOverrides`, `AnalyzeAllOverrides`, `Entrypoint`, `IgnoreDecorator`, `SkipModelBroadening`, `InferSelfTito`, `InferArgumentTito`.

## Port Entries (sources/sinks/tito/parameter_sources)

Each of `sources`, `sinks`, `tito`, and `parameter_sources` is a list of port entries:

```json
{
  "port": "<port>",
  "taint": [ <trace_element>, ... ]
}
```

### Port Formats

| Port | Meaning |
|------|---------|
| `result` | Return value |
| `result[field]` | Return value with access path |
| `formal(name, position=N)` | Named parameter at position N |
| `formal(name, position=N, positional_only)` | Positional-only parameter |
| `formal(name, position=N)[field][subfield]` | Parameter with access path |
| `formal(*args, position=N)` | Variadic positional args |
| `formal(**kwargs)` | Variadic keyword args |
| `formal(**kwargs, excluded=[x])` | Kwargs excluding named params |
| `formal($global, position=0)` | Global/attribute model port |

**Key rule: position is always included in `formal()` ports** (e.g., `formal(x, position=0)`, not `formal(x)`).

**For sources/parameter_sources**, the port indicates where taint is *produced* (typically `result` or `result[field]`).

**For sinks**, the port indicates where taint is *consumed* (typically `formal(...)` with optional access path).

**For tito**, the port indicates the *input* side (always `formal(...)`). The output side is encoded inside the taint entry via `return_paths`.

## Trace Elements (Call Info)

Each element in a `"taint"` list is a trace element keyed by its call info. There are four call info variants:

### 1. Declaration — user-specified taint from .pysa model files

```json
{
  "kinds": [ { "kind": "Test" } ],
  "declaration": null
}
```

This is the **leaf** of the trace — the taint is directly declared by the user, not propagated from another function.

### 2. Origin — taint at a direct call site (first hop)

```json
{
  "kinds": [ { "kind": "Test", "length": 0 } ],
  "origin": { "line": 16, "start": 15, "end": 20 }
}
```

Represents a call to a function with a user-declared model. The `origin` location is where the call happens. `"call_site"` may also be present (e.g., `"call_site": "16:4-16:21"`).

### 3. Call — taint propagated through a call chain

```json
{
  "kinds": [ { "kind": "Test", "length": 2 } ],
  "call": {
    "position": { "line": 20, "start": 8, "end": 22 },
    "resolves_to": [ "module.callee_function" ],
    "port": "result[a]"
  }
}
```

Represents a hop: taint flows from `callee_function` at the given port. `resolves_to` is a list (can have multiple callees for overrides). `"call_site"` may also appear inside the `"call"` object.

### 4. Tito — taint-in-taint-out propagation

```json
{
  "kinds": [ { "return_paths": { "": 0 }, "kind": "LocalReturn" } ],
  "tito": {}
}
```

Used in `"tito"` port entries. The `"tito"` call info is always `{}` (empty object, never `null`).

## Kind Entries

Each `"kinds"` list entry describes a taint kind with optional metadata:

```json
{
  "kind": "Test",
  "length": 2,
  "leaves": [ { "name": "pysa._test_source", "port": "leaf:return" } ],
  "features": [ { "always-via": "special_source" } ],
  "local_features": [ { "always-via": "special_sink" } ],
  "return_paths": { "": 0 }
}
```

| Field | Description |
|-------|-------------|
| `kind` | Source/sink kind name (e.g., `Test`, `UserControlled`, `SQL`, `RemoteCodeExecution`). For TITO: `LocalReturn` |
| `length` | Trace length (hops to closest leaf). Omitted when 0 |
| `leaves` | Set of leaf declarations this taint originates from. Each leaf: `{"name": "qualified.name", "port": "leaf:return"}` |
| `features` | Propagated breadcrumbs from callees |
| `local_features` | Breadcrumbs introduced in this function |
| `return_paths` | (TITO only) Maps access paths to collapse depths, e.g. `{"": 0}` (identity), `{"[field]": 3}` (into field) |

### Leaf Ports

- `leaf:return` — source on return value
- `leaf:arg` — sink on argument
- `leaf:arg[field]` — sink on argument field
- `producer:N:formal(M)` — cross-repository producer
- `anchor:formal(M)` — cross-repository anchor

## Trace-Level Local Features and Tito Positions

Local features and tito positions can appear at the trace element level (outside `kinds`):

```json
{
  "kinds": [ ... ],
  "local_features": [ { "has": "first-index" }, { "first-index": "<numeric>" } ],
  "tito_positions": [ { "line": 71, "start": 34, "end": 37 } ],
  "origin": { ... }
}
```

**`local_features`** (trace-level): Breadcrumbs introduced at this particular call site within the current function. These describe what happens to the data at this point in the flow — for example, `first-index` records the first dictionary key or list index used to access tainted data. Unlike kind-level `local_features` (which are specific to a taint kind), trace-level local features apply across all kinds in the trace element.

**`tito_positions`**: Locations in the current function where taint was propagated via taint-in-taint-out. Each entry is a source position (`line`, `start`, `end`) pointing to an expression where tainted data flowed through a TITO function call. This helps trace how data moved through the function before reaching a sink or being returned.

## Features (Breadcrumbs)

Features use a prefix to indicate whether they appear on all paths (`always-`) or some paths (no prefix):

```json
{ "always-via": "special_source" }
{ "via": "tito" }
{ "always-type": "bool" }
{ "has": "first-index" }
{ "first-index": "<numeric>" }
{ "first-index": "arg" }
{ "has": "first-field" }
{ "first-field": "attribute_name" }
```

## Sanitizers

### Global Sanitizer (applies to entire function)

```json
"global_sanitizer": {
  "sources": "All",
  "sinks": [ "SQL" ],
  "tito": { "sources": [ "UserControlled" ], "sinks": "All" }
}
```

Each of `sources`, `sinks`, `tito` is optional. Values are either `"All"` or a list of kind names. The `tito` sub-object has its own `sources`/`sinks`.

### Parameters Sanitizer (applies to all parameters)

Same format as global sanitizer, under key `"parameters_sanitizer"`.

### Per-Root Sanitizers

```json
"sanitizers": [
  {
    "port": "formal(x, position=0)",
    "sources": "All",
    "sinks": [ "TestSink" ]
  }
]
```

## Issue Structure

Issues are **separate top-level objects**, not nested inside models:

```json
{
  "kind": "issue",
  "data": {
    "callable": "module.function_name",
    "callable_line": 19,
    "code": 5002,
    "line": 21,
    "start": 15,
    "end": 16,
    "filename": "module.py",
    "message": "Data from [Test] source(s) may reach [Test] sink(s)",
    "traces": [
      {
        "name": "forward",
        "roots": [ <trace_elements> ]
      },
      {
        "name": "backward",
        "roots": [ <trace_elements> ]
      }
    ],
    "features": [ { "always-via": "special_source" } ],
    "sink_handle": {
      "kind": "Call",
      "callee": "module.sink_function",
      "index": 0,
      "parameter": "formal(arg)"
    },
    "master_handle": "module.function:5002:0:Call|module.sink|0|formal(arg):md5hash"
  }
}
```

Trace elements in `roots` use the same format as model trace elements (`kinds`/`call`/`origin`/`declaration`).

### Sink Handle Variants

| Variant | Fields |
|---------|--------|
| `Call` | `callee`, `index`, `parameter` |
| `Global` | `callee`, `index` |
| `Return` | (none) |
| `LiteralStringSink` | `sink` |
| `ConditionalTestSink` | `sink` |
| `StringFormat` | `callee`, `index`, `parameter_index` |

**`parameter` in sink handle uses the callee's parameter name** (e.g., `formal(arg)` for `_test_sink(arg)`), not the caller's argument variable name.

## Common Mistakes

| Mistake | Correct |
|---------|---------|
| `"formal(x)"` | `"formal(x, position=0)"` — always include position |
| `"declaration": {}` or `"decl": null` | `"declaration": null` — exact key and value matter |
| `"tito": null` (call info) | `"tito": {}` — empty object, not null |
| TITO port = `"result"` | TITO port = `"formal(...)"` — port is the *input* side |
| Issues inside model `"errors"` | Issues are separate `{"kind": "issue"}` objects |
| Including `"sources": []` | Omit empty fields entirely |
| Leaves: `{"kind": "Test", "name": "..."}` | Leaves: `{"name": "pysa._test_source", "port": "leaf:return"}` |
| sink_handle parameter = caller's variable | sink_handle `parameter` = **callee's** parameter name |

## Complete Example

Given:
```python
# example.py
from pysa import _test_source, _test_sink

def get_source():
    return _test_source()

def flow():
    x = get_source()
    _test_sink(x)
```

### Model for `_test_source` (user-declared source):
```json
{
  "kind": "model",
  "data": {
    "callable": "pysa._test_source",
    "filename": "pysa/__init__.pyi",
    "callable_line": 5,
    "sources": [
      {
        "port": "result",
        "taint": [
          {
            "kinds": [
              {
                "features": [ { "always-via": "special_source" } ],
                "kind": "Test"
              }
            ],
            "declaration": null
          }
        ]
      }
    ],
    "modes": [ "Obscure" ]
  }
}
```

### Model for `get_source` (inferred source via call):
```json
{
  "kind": "model",
  "data": {
    "callable": "example.get_source",
    "filename": "example.py",
    "callable_line": 4,
    "sources": [
      {
        "port": "result",
        "taint": [
          {
            "kinds": [
              {
                "local_features": [ { "always-via": "special_source" } ],
                "leaves": [ { "name": "pysa._test_source", "port": "leaf:return" } ],
                "kind": "Test"
              }
            ],
            "origin": { "line": 5, "start": 11, "end": 25 }
          }
        ]
      }
    ]
  }
}
```

### Issue for `flow`:
```json
{
  "kind": "issue",
  "data": {
    "callable": "example.flow",
    "callable_line": 7,
    "code": 5002,
    "line": 9,
    "start": 15,
    "end": 16,
    "filename": "example.py",
    "message": "Data from [Test] source(s) may reach [Test] sink(s)",
    "traces": [
      {
        "name": "forward",
        "roots": [
          {
            "kinds": [
              {
                "features": [ { "always-via": "special_source" } ],
                "leaves": [ { "name": "pysa._test_source", "port": "leaf:return" } ],
                "length": 1,
                "kind": "Test"
              }
            ],
            "call": {
              "position": { "line": 8, "start": 8, "end": 20 },
              "resolves_to": [ "example.get_source" ],
              "port": "result"
            }
          }
        ]
      },
      {
        "name": "backward",
        "roots": [
          {
            "kinds": [
              {
                "local_features": [ { "always-via": "special_sink" } ],
                "leaves": [ { "name": "pysa._test_sink", "port": "leaf:arg" } ],
                "kind": "Test"
              }
            ],
            "origin": { "line": 9, "start": 15, "end": 16 }
          }
        ]
      }
    ],
    "features": [
      { "always-via": "special_source" },
      { "always-via": "special_sink" }
    ],
    "sink_handle": {
      "kind": "Call",
      "callee": "pysa._test_sink",
      "index": 0,
      "parameter": "formal(arg)"
    },
    "master_handle": "example.flow:5002:0:Call|pysa._test_sink|0|formal(arg):abcdef1234567890"
  }
}
```

## Reference

- Model types defined in: `source/interprocedural_analyses/taint/model.ml`
- Call info, frames, taint serialization: `source/interprocedural_analyses/taint/domains.ml`
- Feature/breadcrumb format: `source/interprocedural_analyses/taint/features.ml`
- Issue format: `source/interprocedural_analyses/taint/issue.ml`
- Sanitizer format: `source/interprocedural_analyses/taint/sanitize.ml`
