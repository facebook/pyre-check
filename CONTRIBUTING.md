# Contributing to Pyre
We want to make contributing to this project as easy and transparent as
possible.

*Read this in other languages: [Español](CONTRIBUTING.es.md)*

## Pull Requests
We actively welcome your pull requests.

1. Fork the repo and create your branch from `master`.
2. If you've added code that should be tested, add tests.
3. If you've changed APIs, update the documentation.
4. Ensure the test suite passes (please see [the following section](#running-tests)).
5. Make sure your code lints (please see [the following section](#coding-style)).
6. If you haven't already, complete the Contributor License Agreement ("CLA").

## Running tests

Pyre runs two different test suites:

* unit tests in OCaml cover the main binary (`pyre.bin`); these tests
  are run via `make test` after the sources have been
  configured. Please refer to the [Getting
  Started](https://pyre-check.org/docs/installation.html) page for
  more information on bootstrapping the process.

* Python tests for the wrappers are run via `make python_tests`.

## Contributor License Agreement ("CLA")
In order to accept your pull request, we need you to submit a CLA. You only need
to do this once to work on any of Facebook's open source projects.

Complete your CLA here: <https://code.facebook.com/cla>. If you have any questions,
please drop us a line at cla@fb.com.

You are also expected to follow the [Code of Conduct](CODE_OF_CONDUCT.md),
so please read that if you are a new contributor.

## Issues
We use GitHub issues to track public bugs. Please ensure your description is
clear and has sufficient instructions to be able to reproduce the issue.

## Coding Style
We value consistent code. Please follow the style of the surrounding code. Useful rules of thumb for all languages are
* avoid abbreviations.
* avoid variable indentations (e.g. lining up parameters with the opening parenthesis of a function). It tends to mess up the git blame and quickly results with conflicts with the line length limit.
* add a trailing colon/semi-colon for multi-line lists and records, and have the closing bracket on the line after the semicolon.
* prefer snake_case over camelCase for variables and function names, and prefer CamelCase over Snake_case for modules and classes.

### Python
<p>
  <a href="https://github.com/ambv/black"><img alt="Code style: black" src="https://img.shields.io/badge/code%20style-black-000000.svg"></a>
</p>

We use the `Black` code formatter for all Python files.
You can install the latest release via `pip install black`, and you run it over the client files as `black pyre-check/client`.
More information are available at: https://github.com/ambv/black

### OCaml
- we don't use type annotations outside of interface files unless necessary
- use ocp-indent for formatting

## Architecture
On a high level, Pyre goes through the following steps to when "pyre" is called from the command line:

1. Read a .pyre_configuration to determine which source roots to analyze, as well as which python packages to analyze annotations for, and which pyre command to run. This information is used to determine which flags to pass into `pyre.bin`, and shell out to the OCaml binary. The implementation of this step can be found under `scripts/`.

2. Determine which pyre command to run. The commands include some which handle the lifetime and state of a persistent pyre server for a project, as well as a standalone run command called `pyre check`. The implementation of these commands are found under `commands/`. `main.ml` aggregates these commands, and handles the parsing of the command-line arguments. Most steps will eventually call the TypeCheckService.

3. The TypeCheckService module will call the ParseService, which will locate all sources in the given source root and all dependencies, and parse, preprocess & add the sources into shared memory. ParseService and TypeCheckService both live under `service/`, whereas the parser itself is located in `parser/`, and the preprocessor is under `analysis/analysisPreprocessing.ml`. The AST (abstract syntax tree) that the source code is parsed into is specified in files under the `ast/` directory.

4. Next, pyre will populate the global type environment with the project's sources as well as that of the dependencies. Note that we don't recursively analyze the imports of the project's files, but instead add all sources to the environment at once. This choice makes it easier to parallelize the building of the type environment, but comes at the cost of not being able to depend on a file's dependencies being analyzed when adding it to the type environment. The type environment can be thought of as a collection of hash tables, mapping function, class, global, etc. names into their implementations that can be accessed from shared memory. The type order, which is explained in more detail in a section below, is also built here.

The modules that do the heavy lifting here can be found under `analysis/analysisEnvironment.ml` and `analysis/analysisTypeOrder.ml`.

5. Once the environment is built, pyre will start type checking all files under the source root in parallel. The key property here is that building the environment in the above step allows pyre to type check each function in parallel. The type checker will not go beyond function call boundaries, which is possible because we will have accumulated the parameter and return annotations of all functions before this step.

During analysis, each function will be processed into a control flow graph to represent the flow of typing information (https://en.wikipedia.org/wiki/Control_flow_graph is a nice introduction to CFG's). The bird's eye view of the algorithm is that we initialize the analysis with the type information from the function's parameter, and follow the control flow of the function to annotate local variables that are encountered. When encountering an attribute access, call, etc., the propagated type information is checked against the already present signature, and an error is generated if the two aren't compatible. The `Abstract Interpretation` section provides a theoretical background for the analysis.

6. TypeCheckService will collect all the errors and return them to the caller. In the case of `pyre check`, all errors will be reported to stdout.

## License
By contributing to Pyre, you agree that your contributions will be licensed
under the LICENSE file in the root directory of this source tree.
