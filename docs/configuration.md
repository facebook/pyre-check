---
id: configuration
title: Configuring Pyre
sidebar_label: Configuring Pyre
---

# Persistent Configuration
The recommended way to use Pyre is with a configuration file set.
You can generate an initial configuration in the root of your project with
```bash
$ pyre init
```

This will create a basic configuration file at `.pyre_configuration` containing,
for example:
```bash
{
  "binary": "/Library/Frameworks/Python.framework/Versions/3.6/bin/pyre.bin",
  "source_directories": [
    "."
  ]
}
```

You can extend this configuration to configure Pyre for your project's specific
setup and needs. The following configuration options are supported:

`source_directories`: List of paths to type check. Defaults to current directory.

`search_path`: List of paths to Python modules to include in the typing
environment. For example, typeshed third-party modules. Pyre will use those
paths to build up the typing environment. Note that if the same Python module is
found both in `source_directories` and `search_path`, the `search_path` version
takes precendence. If the same Python module is found in two different
`search_path`s, the version that belongs to the path that comes earlier takes
precedence.

`exclude`: List of regular expressions for files and directories that should be
completely ignored by Pyre. The regular expression will be matched against the
*full* path of files as opposed to their relative path.

`ignore_all_errors`: A list of paths to omit from type-checking. This may be
useful for generated files, virtualenv directories, etc.  These should be paths
relative to the location of the configuration file (or the local configuration
if applicable).  These can also include basic globs using *. **Note**: files
matching these paths will still be processed (i.e. type and module names in those files are still visible to Pyre). Please refer to the `exclude`
configuration item if you have files that are intended to be hidden from Pyre.

`binary`: Location of pyre binary. This can be specified to gradually upgrade a Pyre
binary in a CI setting.

`logger`: If set, Pyre will invoke the binary passing it statistics in JSON format.
The statistics contain information about Pyre's performance as well as information about
the project's type coverage.

`typeshed`: Path to the [Typeshed](https://github.com/python/typeshed) standard library, which
provides typed stubs for library functions.

`workers`: Number of workers to spawn for multiprocessing.

`extensions`: Consider extensions in this list equivalent to `.py` for type checking.
Empty string indicates extensionless files.


# Local Configuration
If you have sub-projects within your project root that you would like to run Pyre on, you
can create a `.pyre_configuration.local` at the root of your subproject and override any
of the fields set in `.pyre_configuration` above.

When calling Pyre, the nearest local configuration at or above the current directory will be used.
You can use the `--local-configuration` (or `-l`) flag to invoke Pyre on a project that includes a
local configuration, while outside its source directory. It works like `make -C`:
```
$ ls project
  .pyre_configuration.local   project_file.py   ...
$ pyre -l project
  Checking...
```


# Command Line Arguments and Flags

The Pyre command line flags can be summarized by running `pyre --help` or `pyre -h`:

```bash
$ pyre --help
usage: pyre [-h] [-l LOCAL_CONFIGURATION] [--version] [--binary-version]
            [--show-error-traces] [--output {text,json}] [--verbose] [-n]
            [--hide-parse-errors] [--show-parse-errors] [--target TARGETS]
            [--build] [--source-directory SOURCE_DIRECTORIES]
            [--search-path SEARCH_PATH] [--preserve-pythonpath]
            [--binary BINARY] [--exclude EXCLUDE] [--typeshed TYPESHED]
            [--save-initial-state-to SAVE_INITIAL_STATE_TO]
            {analyze, check, kill, incremental, initialize init, query,
            rage, restart, start, stop} ...

positional arguments:
  {analyze, check, kill, incremental, initialize (init), query, rage, restart, start, stop}

optional arguments:
  -h, --help            show this help message and exit
  -l LOCAL_CONFIGURATION, --local-configuration LOCAL_CONFIGURATION
                        Use a local configuration
  --version             show program's version number and exit
  --binary-version      Print the pyre.bin version to be used
  --show-error-traces   Display errors trace information
  --output {text,json}  How to format output
  --verbose             Enable verbose logging
  -n, --noninteractive  Disable interactive logging
  --show-parse-errors   Display detailed information about parse errors
  --search-path SEARCH_PATH
                        Add an additional directory of modules and stubs to include in the type environment
  --preserve-pythonpath
                        Preserve the value of the PYTHONPATH environment variable and inherit the current python environment's search path
  --binary BINARY       Location of the pyre binary
  --exclude EXCLUDE     Exclude files and directories matching this regexp from parsing
  --typeshed TYPESHED   Location of the typeshed stubs
  --save-initial-state-to SAVE_INITIAL_STATE_TO
                        Path to serialize pyre's initial state to.

buck:
  --target TARGETS      The buck target to check
  --build               Freshly build all the necessary artifacts.

source-directories:
  --source-directory SOURCE_DIRECTORIES
                        The source directory to check
```

## Positional Arguments
If [Watchman](https://facebook.github.io/watchman/docs/install.html) is
installed, running `pyre` with no positional arguments defaults to incremental,
otherwise defaults to check.

`check`: Run Pyre.

`kill`: Kill the Pyre server.

`incremental`: Run Pyre and spin up a Pyre server, which listens to watchman changes in
your repository, and to changes reported by VSCode or Nuclide editors via LSP Protocol.
Subsequent runs of Pyre will be much faster than running Pyre from scratch with `check`.
Running `pyre` with no positional arguments defaults to `pyre incremental`.

`initialize`, `init`: Generate a basic `.pyre_configuration` file in the current directory.
Searches for a path to your Pyre binary and typeshed stubs.

`rage`: Print server logs for debugging or for context when reporting server errors.

`restart`: Restart the Pyre server.

`start`: Start the Pyre server.

`stop`: Stop the Pyre server.


## Flags
These flags can be passed in before any of the positional arguments above. For example:
```bash
pyre --source-directory "." --noninteractive check
pyre --source-directory "." restart
```

`--local-configuration LOCAL_CONFIGURATION`: Call Pyre specifying the path to a local
configuration outside of the current working directory.

`--noninteractive`: Disable interactive logging, which by default overwrites intermediate
logging output and adds colors for a more streamlined user experience.
Non-interactive mode ensures all terminal output remains visible.

`--output {text, json}`: Formatting for error return values. Defaults to text.

`--preserve-pythonpath`: Use the `$PYTHONPATH` environment variable to search for external
sources, along with the current environment's search path. This environment variable is
ignored otherwise.

`--search-path SEARCH_PATH`: Provide additional stubs or modules external to the project
being type-checked. Can also be set in `.pyre_configuration` or often lives in the
`$PYTHONPATH` environment variable (see `--preserve-pythonpath`).

`--show-error-traces`: Display more verbose error messages which include a more detailed
reason and relevant source code locations (for example, what line a type conflicting
  type was specified on).

`--source-directory SOURCE_DIRECTORY`: Provide a path to the source root to check. Can also
be specified in `.pyre_configuration`.

`--typeshed TYPESHED`: Path to the [Typeshed](https://github.com/python/typeshed) standard library,
which provides typed stubs for library functions. This can also be set in `.pyre_configuration`.

`--verbose`: Enable verbose logging. Most useful when used in conjunction with `--noninteractive`.

`--version`: Print the current version of Pyre.
