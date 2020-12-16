# SAPP
SAPP stands for Static Analysis Post Processor. SAPP takes the raw results of Pysa and makes them explorable both through a command line interface and a web UI.

## Installation
SAPP can be installed through [PyPI](https://pypi.org/) with `pip install fb-sapp`.

## Getting Started
This guide assumes that you have results from a Pysa run saved in a `~/example` directory. If you are new to Pysa, you can follow [this tutorial](https://pyre-check.org/docs/pysa-running#example) to get started.

### Processing the Results
The postprocessing will translate the raw output containing models for every analyzed function into a format that is more suitable for exploration.

```shell
[~/example]$ sapp --database-name sapp.db analyze taint-output.json
```

After the results have been processed we can now explore them through the UI and a command line interface. We will briefly look at both of those methods here.

### Web Interface
Start the web interface with

```shell
[~/example]$ sapp --database-name sapp.db server --source-directory=<WHERE YOUR CODE LIVES>
```

and visit http://localhost:5000 in your browser (note: the URL displayd in the code output currently will not work). You will be presented with a list of issues that provide access to example traces.

### Command Line Interface
The same information can be accessed through the command line interface:

```shell
[~/example]$ sapp --database-name sapp.db explore
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
 Message: Possible shell injection Data from [UserControlled] source(s) may reach [RemoteCodeExecution] sink(s)
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
 Message: Possible shell injection Data from [UserControlled] source(s) may reach [RemoteCodeExecution] sink(s)
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

You can refer to the `help` command to get more information about available commands in the command line interface.

## Terminology
A single SAPP database can keep track of more than just a single run. This opens up the possibility of reasoning about *newly introduced issues* in a codebase.

Every invocation of
```shell
[~/example]$ sapp --database-name sapp.db analyze taint-output.json
```
will add a single *run* to the database. An *issue* can exist over multiple runs (we typicall call the issue in a single run an *instance*). You can select a run from the web UI and look at all the instances of that run. You can also chose to only show the instances of issues that are newly introduced in this run in the filter menu.

Each instance consists of a *data flow* from a particular source kind (e.g. user controlled input) into a *callable* (i.e. a function or method), and a data flow from that callable into a particular sink kind (e.g. RCE).

*Note: the data can come from different sources of the same kind and flow into different sinks of the same kind. The traces view of a single instance represents a multitude of traces, not just a single trace.*


## FAQ
### Why is SAPP it's own project and not just part of Pysa?
Stay tuned for future annoucements.

## License

SAPP is licensed under the MIT license.
