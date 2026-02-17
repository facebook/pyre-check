# Guidance for Project Agents

## Project Overview

Pyre is Meta's performant type checker for Python (PEP 484 compliant).
It includes **Pysa**, a security-focused static analysis tool for taint analysis.
The core engine is written in OCaml (`source/`), with a Python CLI client (`client/`).
Note: [Pyrefly](https://github.com/facebook/pyrefly) is the next-generation replacement.

## Build and Test Commands

### Configure

If the file `source/dune` is missing, this must be a fresh repository clone.
In that case, run the following command:

```bash
# For open source users
./scripts/setup.sh --local
# For Meta internal users
./facebook/scripts/setup.sh --local
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

For Meta internal users, simply run:
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
pyre check  # Use one-off check mode, not the server, when recompiling frequently
```

## Architecture

### Type Checking Pipeline

- **Python CLI** (`client/`): Reads `.pyre_configuration`, determines flags, shells out to `pyre.bin`
- **Command dispatch** (`source/main.ml`): Routes to `Check`, `Analyze` (Pysa), `Infer`, `Server`, `CodeNavigation`, or `NoDaemonQuery`
- **Parsing** (`source/menhir_parser/`, `source/cpython_parser/`): Python source -> AST (`source/ast/`)
- **Environment building** (`source/analysis/`): Populates global type environment in parallel. All sources added at once (not recursively following imports). Builds a layered environment chain: `UnannotatedGlobalEnvironment` -> `ClassHierarchyEnvironment` -> `TypeAliasEnvironment` -> `AnnotatedGlobalEnvironment` -> `FunctionDefinitionEnvironment` -> `ErrorsEnvironment` -> `TypeEnvironment`
- **Type checking** (`source/analysis/typeCheck.ml`): Each function checked in parallel via control flow graph. Propagates parameter types through function body, checks compatibility at each operation. Never goes beyond function call boundaries.
- **Error reporting**: Results collected and returned to CLI

### Pysa (Taint Analysis) Pipeline

- **Analysis orchestrator** (`source/interprocedural_analyses/taint/taintAnalysis.ml`):  Main entrypoint of the analysis, runs the different analysis steps
- **PyrePysaApi** (`source/interprocedural/pyrePysaApi.ml`): Provides an API to query a type checker (pyre or pyrefly)
- **Class Hierarchy Graph** (`source/interprocedural/classHierarchyGraph.ml`): Builds a class hierarchy graph
- **Callables Shared Memory** (`source/interprocedural/callablesSharedMemory.ml`): Builds a mapping from callables (functions and methods) to their AST and signature
- **Call Graph** (`source/interprocedural/callGraph.ml`): Defines call graph data structures
- **Call Graph Builder** (`source/interprocedural/callGraphBuilder.ml`): Builds call graphs for all callables
- **Model parsing** (`source/interprocedural_analyses/taint/modelParser.ml`): Parses `.pysa` model files
- **Forward analysis** (`source/interprocedural_analyses/taint/forwardAnalysis.ml`): Tracks taint from sources
- **Backward analysis** (`source/interprocedural_analyses/taint/backwardAnalysis.ml`): Tracks taint to sinks
- **Configuration** (`source/interprocedural_analyses/taint/taintConfiguration.ml`): Defines rules connecting sources to sinks
- **Global Fixpoint** (`source/interprocedural/fixpointAnalysis.ml`): Implements a global fixpoint over callables
- **Model** (`source/interprocedural_analyses/taint/model.ml`): Defines a summary of the taint behavior of a function, inferred during analysis

### Key Source Directories

- `source/ast/`: Python AST representation (Expression, Statement, Source, Location, Reference)
- `source/analysis/`: Core type checker: type representation (`type.ml`), CFG, fixpoint, preprocessing, environments, type order, class hierarchy
- `source/interprocedural/`: Interprocedural framework: call graph, dependency graph, override graph, fixpoint
- `source/interprocedural_analyses/taint/`: Pysa taint analysis: sources, sinks, domains, models, rules, reporting
- `source/interprocedural_analyses/type_inference/`: Interprocedural type inference
- `source/server/`: Pyre daemon for incremental analysis
- `source/code_navigation_server/`: IDE code navigation server
- `source/command/`: OCaml CLI command implementations
- `source/buck_command/`: Buck-specific commands
- `source/buck_integration/`: Buck build system integration
- `source/service/`: Shared memory management, scheduling
- `source/domains/`: Abstract domain library (lattices for interprocedural analyses)
- `source/hack_parallel/`: Shared memory and multi-worker infrastructure (forked from Hack)
- `source/saved_state/`: Serialization for incremental analysis
- `client/`: Python CLI (`pyre` command)
- `client/commands/`: All user-facing commands (check, analyze, start, stop, infer, query, etc.)
- `client/configuration/`: `.pyre_configuration` file parsing
- `client/language_server/`: LSP protocol implementation
- `api/`: Programmatic Python API for Pyre server
- `pyre_extensions/`: Runtime Python helpers (`none_throws`, `safe_cast`, etc.)
- `tools/generate_taint_models/`: Auto-generation of Pysa taint models
- `tools/typeshed_patcher/`: Applies patches to bundled typeshed stubs
- `stubs/`: Bundled typeshed with patching mechanism
- `facebook/`: Meta-internal code (not open-sourced)

### OCaml Conventions

- Most types derive `show`, `eq`, `compare`, `to_yojson` via PPX derivers. For a type `Module.t`, `Module.show` gives a debug string.
- Debug with `Log.dump "format %s" value` and run with `pyre --noninteractive check`
- In Python test files, use `pyre_dump()`, `pyre_dump_cfg()`, `pyre_dump_locations()`, and `reveal_type(x)` to inspect Pyre's state

## Coding Style

- Avoid abbreviations
- Prefer `snake_case` for variables/functions, `CamelCase` for modules/classes
