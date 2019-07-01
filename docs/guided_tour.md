---
id: guided-tour
title: Guided Tour
sidebar_label: Guided Tour
---

# First Steps
Assuming you have completed your setup (see [Installation](installation.md)), you're now ready to
use Pyre. The following will give a short overview of the basic workflows that we recommend.


In the most basic case you have a `test` directory containing a `test.py` file.
```bash
$ cat test/test.py
def foo() -> int:
  return 'string'  # whoops
```

You can run pyre as follows
```bash
$ pyre --source-directory test check
 ƛ Found 1 type error!
 test/test.py:2:4 Incompatible return type [7]: Expected `int` but got `str`.
```

The default for `--source-directory` is the current directory. In most cases you will simply
```bash
$ cd test
$ pyre check
 ƛ Found 1 type error!
 test.py:2:4 Incompatible return type [7]: Expected `int` but got `str`.
```

Invoked with `check`, Pyre will do a complete pass over all the files in the provided directory.
As your codebase grows, this will get slower and slower. Pyre supports incremental type checking
which we'll cover in the next section.


# Running a Server
Pyre's incremental mode depends on [Watchman](https://facebook.github.io/watchman/docs/install.html)
to get notifications about changes on the filesystem. Without watchman, or an editor supporting LSP,
you cannot use Pyre's incremental mode.

If you have *Watchman* installed and assuming the scenario from the previous section, you can start
a server with
```bash
$ cd test
$ pyre
 ƛ Found 1 type error!
 test.py:2:4 Incompatible return type [7]: Expected `int` but got `str`.
```

The initial run will spawn the server. Subsequent calls to `pyre` will simply query the server for
changes and will be much faster than the initial request. The server can be stopped with
`pyre stop`.


# Configuring your Setup
Pyre's configuration typically sits at the root of the repository in `.pyre_configuration`.
You can override that configuration locally to set up your own default target to check.

Running
```bash
$ pyre init
```
from your project root will set up your `.pyre_configuration`, but you may want to override
the source-directories field in projects within that root, so you can easily run pyre on
different sets of default paths. Once you have a configuration file, you can run pyre with
no extra parameters:
```bash
$ pyre check
```

See [Configuring Pyre](configuration.md) for examples and
additional configuration settings.


# Using pyre-upgrade

`pyre-upgrade` is a tool to help you convert files to use `# pyre-strict`. You can use it
to automatically add `fixmes` to errors in the code, and help you improve strict coverage as you go.

Steps
1. Add `# pyre-strict` to the toplevel of your files
2. Run `pyre --output=json | pyre-upgrade fixme` to automatically add fixmes to all new errors


# Using pyre infer
Pyre infer helps you generate stubs and add annotations to your code. To generate stubs, simply run

`pyre infer`

to automatically add these annotations to your code, run

`pyre infer -i`
