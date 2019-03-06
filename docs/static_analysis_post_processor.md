---
id: static-analysis-post-processor
title: Static Analysis Post Processor
sidebar_label: Static Analysis Processor
---

The `pyre analyze` command runs static analysis and outputs the result as JSON.
The Static Analysis Post Processor (SAPP) tool can process these results and
allows the user to explore the results.

## Installation

In the `pyre-check/sapp` directory run

```shell
$ python3 setup.py install
```

(with `sudo` if not in a virtualenv).

## Parsing the JSON

The JSON output from `pyre analyze` can be difficult to read.

```shell
$ sapp_cli.py analyze path/to/json/output
```

will parse the JSON file and make it easier to work with. It will save the
results to a local sqlite file `sapp.db`.

## Interacting with the database

```shell
$ sapp_cli.py explore --database-name sapp.db
```

will launch a custom IPython interface that's connected to the sqlite file.

### Commands

`commands()`: show the available commands

`help(COMMAND)`: show more info about a specific command

`runs()`: list all completed static analysis runs

`set_run(ID)`: select a specific run for browsing issues

`issues()`: list all issues for the selected run

`set_issue(ID)`: select a specific issue for browsing a trace

`show()`: show info about selected issue

`trace()`: show a trace of the selected issue

`prev()`/`p()`: move backward within the trace

`next()`/`n()`: move forward within the trace

`expand()`: show alternative trace branches
