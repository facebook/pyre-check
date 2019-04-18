---
id: static-analysis-post-processor
title: Static Analysis Post Processor
sidebar_label: Static Analysis Processor
---

The `pyre analyze` command runs static analysis and outputs the result as JSON.
The Static Analysis Post Processor (SAPP) tool can process these results and
allows the user to explore the results.

See [commands](#commands) for the available commands in explore mode.

## Installation

If you have installed pyre version `>= 0.0.22` via `pip install pyre-check`,
then you already have the `sapp` binary installed.

## Example

This assumes you have followed the
[static analysis example](./static-analysis.html#example).

### Parsing the JSON

The JSON output from `pyre analyze` can be difficult to read.

```shell
[~/static_analysis_example] $ sapp --database-name sapp.db analyze ./taint-output.json
```

will parse the JSON file and make it easier to work with. It will save the
results to a local sqlite file `sapp.db`.

### Issue Exploration

```shell
[~/static_analysis_example] $ sapp --database-name sapp.db explore
```

This will launch a custom IPython interface that's connected to the sqlite file.
In this mode, you can dig into the issues that Pyre surfaces. Following is an
example of how to use the various commands.

Start out by listing all known issues:
```text
==========================================================
Interactive issue exploration. Type 'help' for help.
==========================================================

[ run 1 ]
>>> issues
Issue 1
    Code: 5001
 Message: Possible shell injection Data from [UserSpecified] source(s) may reach [RemoteCodeExecution] sink(s)
Callable: source.convert
 Sources: input
   Sinks: os.system
Location: source.py:9|22|32
Found 1 issues with run_id 1.
```
As expected, we have 1 issue. To select it:
```text
[ run 1 ]
>>> issue 1
Set issue to 1.

Issue 1
    Code: 5001
 Message: Possible shell injection Data from [UserSpecified] source(s) may reach [RemoteCodeExecution] sink(s)
Callable: source.convert
 Sources: input
   Sinks: os.system
Location: source.py:9|22|32
```
View how the data flows from source to sink:
```text
[ run 1 > issue 1 > source.convert ]
>>> trace
     # ⎇  [callable]       [port]      [location]
     1    leaf             source      source.py:8|17|22
 --> 2    source.convert   root        source.py:9|22|32
     3    source.get_image formal(url) source.py:9|22|32
     4    leaf             sink        source.py:5|21|28
```
Move to the next callable:
```text
[ run 1 > issue 1 > source.convert ]
>>> n
     # ⎇  [callable]       [port]      [location]
     1    leaf             source      source.py:8|17|22
     2    source.convert   root        source.py:9|22|32
 --> 3    source.get_image formal(url) source.py:9|22|32
     4    leaf             sink        source.py:5|21|28
```
Show the source code at that callable:
```text
[ run 1 > issue 1 > source.get_image ]
>>> list
In source.convert [source.py:9|22|32]
     4      command = "wget -q https:{}".format(url)
     5      return os.system(command)
     6
     7  def convert() -> None:
     8      image_link = input("image link: ")
 --> 9      image = get_image(image_link)
                              ^^^^^^^^^^
```
Move to the next callable and show source code:
```text
[ run 1 > issue 1 > source.get_image ]
>>> n
     # ⎇  [callable]       [port]      [location]
     1    leaf             source      source.py:8|17|22
     2    source.convert   root        source.py:9|22|32
     3    source.get_image formal(url) source.py:9|22|32
 --> 4    leaf             sink        source.py:5|21|28

[ run 1 > issue 1 > leaf ]
>>> list
In source.get_image [source.py:5|21|28]
     1  import os
     2
     3  def get_image(url: str) -> int:
     4      command = "wget -q https:{}".format(url)
 --> 5      return os.system(command)
                             ^^^^^^^
     6
     7  def convert() -> None:
     8      image_link = input("image link: ")
     9      image = get_image(image_link)
```
Jump to the first callable and show source code:
```text
[ run 1 > issue 1 > leaf ]
>>> jump 1
     # ⎇  [callable]       [port]      [location]
 --> 1    leaf             source      source.py:8|17|22
     2    source.convert   root        source.py:9|22|32
     3    source.get_image formal(url) source.py:9|22|32
     4    leaf             sink        source.py:5|21|28

[ run 1 > issue 1 > leaf ]
>>> list
In source.convert [source.py:8|17|22]
     3  def get_image(url: str) -> int:
     4      command = "wget -q https:{}".format(url)
     5      return os.system(command)
     6
     7  def convert() -> None:
 --> 8      image_link = input("image link: ")
                         ^^^^^
     9      image = get_image(image_link)
```

## Commands

### Information commands

`help`: show the available commands

`help COMMAND`: show more info about a specific command

`state`: show state and debugging information

### Display commands

`runs`: list all completed static analysis runs

`issues`: list all issues for the selected run

`show`: show info about selected issue

`frames`: show trace frames independently of an issue

### Selection commands

`analysis_output DIR`: sets the location of the analysis output

`run ID`: select a specific run for browsing issues

`latest_run KIND`: select the latest run of the given kind

`issue ID`: select a specific issue for browsing a trace

`frame ID`: select a trace frame

### Trace commands

`trace`: show a trace of the selected issue

`prev`/`p`: move backward within the trace

`next`/`n`: move forward within the trace

`jump NUM`: jump within a trace

`branch`: show and select alternative trace branches

`list`: show source code

### Debugging commands

`parents`: show the callers of the current trace frame

`details`: additional details about the current trace frame
