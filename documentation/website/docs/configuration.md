---
id: configuration
title: Configuration
sidebar_label: Configuration
---
import {OssOnly, FbInternalOnly} from 'internaldocs-fb-helpers';
import FbConfiguration from './fb/configuration.md';

<FbInternalOnly>

<FbConfiguration />

</FbInternalOnly>

<OssOnly>

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

- `source_directories`: List of paths to type check.

 Note: Pyre assumes that all imports are relative to the given source directory. For example, if your source directory is `root/directory`, then an import statement `import module` will be looking to import `root.directory.module`. If you wish to set a different import root for your source directory, you can provide an object `{"import_root": "root", "source": "directory"}` instead of `"root/directory"`. In this case, `import module` will be looking to import `root.module`.

- `search_path`: List of paths to Python modules to include in the typing
environment. `search_path` takes precendence over `source_directories` and the order within the search path indicates precedence. Individual items in the list can take one of the following forms:
  + A plain string, representing the path to the directory from which Pyre will search for modules.
  + An object `{"import_root": "root", "source": "directory"}`, which can be used to control import root of the search path. See explaination for `source_directories`.
  + An object `{"site-package": "package_name"}`. It is equivalent to `{"import_root": "site_root", "source": "package_name"}`, where `site_root` is the return value of [`site.getsitepackages()`](https://docs.python.org/3/library/site.html#site.getsitepackages). This can be useful when you want to add installed `pip` packages as a dependency to your project.

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


### Local Configurations
If you have sub-projects within your project root that you would like to run Pyre on, you
can create a `.pyre_configuration.local` at the root of your subproject and override any
of the fields set in `.pyre_configuration` above. If you are using local configurations, your
`.pyre_configuration` should not specify any sources itself.

When calling Pyre, the nearest local configuration at- or above the current directory will be used.
You can use the `--local-configuration` (or `-l`) flag to invoke Pyre on a project that includes a
local configuration, while outside its source directory. It works like `make -C`:
```bash
$ ls project
  .pyre_configuration.local   project_file.py   ...
$ pyre -l project
  Checking...
```

#### Nested Local Configurations
Nesting local configurations is not supported. The configuration should live at the root of your
project unit and inclusion/exclusion of files from type checking can be done by specifying sources, using
`ignore_all_errors`, or by adding [local suppression](errors.md#suppressing-individual-errors).


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

- `--output {text, json}`: Formatting for error return values. Defaults to text.

- `--search-path SEARCH_PATH`: Provide additional stubs or modules external to the project
being type-checked. Can also be set in `.pyre_configuration`.

- `--source-directory SOURCE_DIRECTORY`: Provide a path to the source root to check. This can also
be specified in `.pyre_configuration`.

- `--typeshed TYPESHED`: Path to the [Typeshed](https://github.com/python/typeshed) standard library,
which provides typed stubs for library functions. This can also be set in `.pyre_configuration`.

- `--verbose`: Enable verbose logging. Most useful when used in conjunction with `--noninteractive`.

- `--version`: Print the current version of Pyre.

</OssOnly>
