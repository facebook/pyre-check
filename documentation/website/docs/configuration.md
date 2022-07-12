---
id: configuration
title: Configuration
sidebar_label: Configuration
---
<!-- There is a separate page for internal config at ./fb/configuration.md -->

Pyre can be run without a configuration (see [Command Line Arguments](configuration.md#command-line-arguments)) but we do recommend that you create a configuration (see [Getting Started](getting_started.md)) and commit that to your version control system to make sure everyone working on your project is using the same settings.

## Configuration Files
Pyre has two types of configurations: a *global* configuration covering the full project, and *local* configurations that apply to subdirectories of the project. In most cases you will only need a global configuration but local configurations can be useful if you are dealing with a big repository containing heterogeneous projects.

### The Global Configuration
The global configuration is a `.pyre_configuration` file sitting at the root of your project. Running Pyre anywhere inside your project directory will use the settings in this global configuration. You can generate an initial configuration in your project directory with
```bash
$ pyre init
```

The configuration is a `JSON` file. For example,
```json
{
  "source_directories": [
    "."
  ],
  "search_path": [
    "/external/library",
    {"site-package": "foo"}
  ]
}
```
specifies that the code Pyre checks is in the directory of the configuration and that Pyre should look in an additional directory as well as the `foo` package installed in your environment for library code.


You specify additional information to configure Pyre. The following fields are supported:

- `source_directories`: List of path to type check. Path can be a glob, for example, `"./foo*"`.

 Note: Pyre assumes that all imports are relative to the given source directory. For example, if your source directory is `root/directory`, then an import statement `import module` will be looking to import `root.directory.module`. If you wish to set a different import root for your source directory, you can provide an object `{"import_root": "root", "source": "directory"}` instead of `"root/directory"`. In this case, `import module` will be looking to import `root.module`.

- `site_package_search_strategy`: Configure how Pyre looks for type checking dependencies installed (e.g. by `pip`) on the local Python environment. Dependent libraries will not be type-checked, but they are consulted to determine the existence of globals/functions/classes. The value of this option can take one of the following forms:
  + `"none"`. This indicates that Pyre should not attempt to search for any additional dependencies. Use this option if you know exactly what packages you depend on, and want to manually specify them with the `search_path` option.
  + `"all"`. Pyre will pull in the entire site package roots (as specified in the `site_roots` option) as dependencies. Any libraries installed as site packages, regardless of whether they are typed or not, will be examined. Use this option if you do not know exactly which packages your code depend on, but want to make sure that no dependencies are missing.
  + `"pep561"`. Similar to `"all"` but instead of pull in everything, Pyre will only pull in typed packages as dependencies according to rules specified in [PEP 561](https://peps.python.org/pep-0561/). This is usually the recommended option, as the behavior is closer to what other type checkers would do by default.

  Note: If incremental check is used, and the search strategy is set to `"pep561"`, then a `pyre restart` is needed when new dependencies are installed -- Pyre will not automatically discover the new package by default. This is a limitation of the current implementation of Pyre and it may be lifted in the future.

- `site_roots`: List of path to where packages are installed.

  If not specified, Pyre will consult the current Python interpreter using `site.getusersitepackages()` and `site.getsitepackages()`, which should work in most cases. But if your codebase uses a different Python interpreter, you may want to specify this option manually so Pyre knows the correct location to look for site packages.

- `search_path`: List of additional paths to Python modules to include as dependencies. `search_path` takes precendence over `source_directories` and the order within the search path indicates precedence. Individual items in the list can take one of the following forms:
  + A plain string, representing a path to the directories from which Pyre will search for modules. The paths can be globs, for example, `"./foo*"`.
  + An object `{"import_root": "root", "source": "directory"}`, which can be used to control import root of the search path. See explaination for `source_directories`.
  + An object `{"site-package": "package_name"}`. It is equivalent to `{"import_root": "site_root", "source": "package_name"}`, where `site_root` is the first element in `site_roots` that has the site package named `package_name` installed. This can be useful when you want to manually specify which `pip` package you want the type checker to see as a dependency to your project (in which case it is recommended to set `site_package_search_strategy` to `"none"` to disable site package auto discovery).
  + An object `{"site-package": "package_name", "is_toplevel_module": true}`, to specify the name as a single-file module found in the site-root rather than as a package.

- `exclude`: List of regular expressions such as `".*\/node_modules\/.*"` which
specify files and directories that should be completely ignored by Pyre. The
regular expression will be matched against the *full* path of files as opposed
to their relative path.

- `ignore_all_errors`: A list of paths to omit from type-checking. This may be
useful for generated files, virtualenv directories, etc.  These should be paths
relative to the location of the configuration file (or the local configuration
if applicable) and support globs. **Note**: Files
matching these paths will still be processed (i.e. type and module names in those files are still visible to Pyre). Please refer to the `exclude`
configuration item if you have files that are intended to be hidden from Pyre.

- `binary`: Location of Pyre's native binary.

- `logger`: Pyre will invoke this exectuable on every run, passing it statistics in JSON format.

- `typeshed`: Path to the [Typeshed](https://github.com/python/typeshed) standard library, which
provides typed stubs for library functions.

- `workers`: Number of workers to spawn for multiprocessing.

- `extensions`: Consider extensions in this list equivalent to `.py` for type checking.
Empty string indicates extensionless files.

- `strict`: Setting this to `true` will make [strict mode](gradual_typing.md#strict-mode) the default in your project.

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
- `servers`: List all currently running Pyre servers.
- `kill`: In case somethign goes wrong and the server becomes unresponsivbe `kill` will attempt to terminate any processes.
- `rage`: Print server logs for debugging or for context when reporting server errors.

### Commonly Used Flags
These flags can be passed in before any of the positional arguments above. For example:
```bash
$ pyre --source-directory "." --noninteractive check
$ pyre --source-directory "." restart
```

- `--local-configuration LOCAL_CONFIGURATION`: Call Pyre specifying the path to a local
configuration.

- `--noninteractive`: Disable interactive logging, which by default overwrites intermediate
logging output and adds colors for a more streamlined user experience.
Non-interactive mode ensures all terminal output remains visible.

- `--output {text, json, sarif}`: Formatting for error return values. Defaults to text.

- `--search-path SEARCH_PATH`: Provide additional stubs or modules external to the project
being type-checked. Can also be set in `.pyre_configuration`.

- `--source-directory SOURCE_DIRECTORY`: Provide a path to the source root to check. This can also
be specified in `.pyre_configuration`.

- `--typeshed TYPESHED`: Path to the [Typeshed](https://github.com/python/typeshed) standard library,
which provides typed stubs for library functions. This can also be set in `.pyre_configuration`.

- `--version`: Print the current version of Pyre.

## Working with Multi-Project Repositories

If you have a single repository with multiple independent Python projects, we recommend you
use a separate `.pyre_configuration` for each one. This allows each project to be
type checked independently.

If you use virtual environments to manage separate dependencies for each project, you can install
`pyre` as a development dependency in each one; by default Pyre will detect system packages
from the environment it is installed in, so this will cause each project to detect the
correct dependencies (assuming you activate the virtual environment before running Pyre).
