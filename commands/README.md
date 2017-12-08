# Command

### Commands:

`main.ml` cases out on three commands:

- `full` : Analyze a directory (default `.`) without a server.
- `start` : Starts up a server if it does not exist. Initializes the server by running `full`.
- `stop` : Stops the server if it exists
- `check` :
  - Runs `start` if a server does not exist.
  - Then asks the server for current errors.
  - `check` optionally takes a list of comma-separated files which it can ask the server to force recheck

### Internal structure

You will find for each command an associated module containing:

- a `command : Core.Command.t` command specifying parameters of the command and default values.
- a `run_command` function that accepts the parsed parameters of `command`. `run_command` always returns `unit`.
- a `run` function that assigns labels to the parameters in `run_command`. `run` is intended to be used internally to share functionality, and returns a result.

Example:

- `full` will run a full analysis with default arguments by calling `Command.run Check.command`
- `Command.run Check.command` invokes `Check.run_command`.
- `Check.run_command` invokes `Check.run` with the same arguments, using labelled arguments.
- `Check.run` returns a list of errors. `Fullcheck.run_command` prints the results and returns unit.

The structure is set up so that we (1) have a way of invoking some command with
sensible defaults using `Command.run Check.command` and (2) we expose a
`run` function for internal use that can be used by other commands to get
results. E.g., the server runs an initial analysis by calling `Check.run`.
