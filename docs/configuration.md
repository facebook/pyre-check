---
id: configuration
title: Configuration
sidebar_label: Configuration
---

## Configuration Files
We recommend that you always run Pyre with a configuration that you commit to your source control. This ensures everyone working on your project is working with the same settings. Pyre has two types of configurations: a global configuration covering the full project, and local configurations that apply to subdirectories of the project. In most cases you will only need a global configuration but local configurations can be useful if you are dealing with a big repository containing heterogenous projects.

### Global
You can generate an initial configuration in the root of your project with
```bash
$ pyre init
```

This will create a basic configuration file at `.pyre_configuration` containing,
for example:
```json
{
  "binary": "/Library/Frameworks/Python.framework/Versions/3.6/bin/pyre.bin",
  "source_directories": [
    "."
  ]
}
```

You can extend this configuration to configure Pyre for your project's specific
setup and needs. The following configuration options are supported:

- `source_directories`: List of paths to type check. Defaults to current directory.

- `search_path`: List of paths to Python modules to include in the typing
environment. For example, typeshed third-party modules. Pyre will use those
paths to build up the typing environment. Note that if the same Python module is
found both in `source_directories` and `search_path`, the `search_path` version
takes precendence. If the same Python module is found in two different
`search_path`s, the version that belongs to the path that comes earlier takes
precedence.

- `exclude`: List of regular expressions for files and directories that should be
completely ignored by Pyre. The regular expression will be matched against the
*full* path of files as opposed to their relative path.

- `ignore_all_errors`: A list of paths to omit from type-checking. This may be
useful for generated files, virtualenv directories, etc.  These should be paths
relative to the location of the configuration file (or the local configuration
if applicable).  These can also include basic globs using *. **Note**: files
matching these paths will still be processed (i.e. type and module names in those files are still visible to Pyre). Please refer to the `exclude`
configuration item if you have files that are intended to be hidden from Pyre.

- `binary`: Location of pyre binary. This can be specified to gradually upgrade a Pyre
binary in a CI setting.

- `logger`: If set, Pyre will invoke the binary passing it statistics in JSON format.
The statistics contain information about Pyre's performance as well as information about
the project's type coverage.

- `typeshed`: Path to the [Typeshed](https://github.com/python/typeshed) standard library, which
provides typed stubs for library functions.

- `workers`: Number of workers to spawn for multiprocessing.

- `extensions`: Consider extensions in this list equivalent to `.py` for type checking.
Empty string indicates extensionless files.


### Local
If you have sub-projects within your project root that you would like to run Pyre on, you
can create a `.pyre_configuration.local` at the root of your subproject and override any
of the fields set in `.pyre_configuration` above.

When calling Pyre, the nearest local configuration at or above the current directory will be used.
You can use the `--local-configuration` (or `-l`) flag to invoke Pyre on a project that includes a
local configuration, while outside its source directory. It works like `make -C`:
```bash
$ ls project
  .pyre_configuration.local   project_file.py   ...
$ pyre -l project
  Checking...
```

#### Nested Local Configurations
Nesting local configurations is not recommended. The configuration should live at the root of your
project unit and inclusion/exclusion of files from type checking can be done by specifying sources, using
`ignore_all_errors`, or by adding [local suppression](error_suppression.md).

If in rare cases the nested configuration cannot be combined upward and the parent cannot be split apart, the
parent configuration must list the directory containing the nested configuration in its `ignore_all_errors` field.
Pyre will warn if this is not the case, which prevents the possibility of introducing conflicting type errors.


## Command Line Arguments
You can get a full and current list of options to run Pyre by running `pyre --help`. The following is a list of commonly used commands and options.

### Commands
Pyre comes with a couple commands that can be invoked with `pyre <COMMAND>`.

The first command you might come in contact with is
- `initialize`, `init`: Initial setup of a configuration for a project.

If [Watchman](https://facebook.github.io/watchman/docs/install/) is
installed, running Pyre with no positional arguments defaults to `incremental`,
otherwise defaults to check.
- `check`: Run Pyre end-to-end, i.e. *not* incrementally.
- `incremental`: Run Pyre incrementally. When invoked for the first time, the command will automatically start a server listening to changes to the filesystem. Subsequent invocations will be faster.

When Pyre is run incrementally, you can control the Pyre's *server* working in the background with the following commands.
- `start`: Start the Pyre server.
- `stop`: Stop the Pyre server.
- `restart`: Restart the Pyre server.
- `kill`: In case somethign goes wrong and the server becomes unresponsivbe `kill` will attempt to terminate any processes.
- `rage`: Print server logs for debugging or for context when reporting server errors.

### Commonly used Flags
These flags can be passed in before any of the positional arguments above. For example:
```bash
$ pyre --source-directory "." --noninteractive check
$ pyre --source-directory "." restart
```

- `--local-configuration LOCAL_CONFIGURATION`: Call Pyre specifying the path to a local
configuration outside of the current working directory.

- `--noninteractive`: Disable interactive logging, which by default overwrites intermediate
logging output and adds colors for a more streamlined user experience.
Non-interactive mode ensures all terminal output remains visible.

- `--output {text, json}`: Formatting for error return values. Defaults to text.

- `--preserve-pythonpath`: Use the `$PYTHONPATH` environment variable to search for external
sources, along with the current environment's search path. This environment variable is
ignored otherwise.

- `--search-path SEARCH_PATH`: Provide additional stubs or modules external to the project
being type-checked. Can also be set in `.pyre_configuration` or often lives in the
`$PYTHONPATH` environment variable (see `--preserve-pythonpath`).

- `--source-directory SOURCE_DIRECTORY`: Provide a path to the source root to check. Can also
be specified in `.pyre_configuration`.

- `--typeshed TYPESHED`: Path to the [Typeshed](https://github.com/python/typeshed) standard library,
which provides typed stubs for library functions. This can also be set in `.pyre_configuration`.

- `--verbose`: Enable verbose logging. Most useful when used in conjunction with `--noninteractive`.

- `--version`: Print the current version of Pyre.
