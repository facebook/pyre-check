# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
TODO(T132414938) Add a module-level docstring
"""


import json
import logging

from .. import (
    command_arguments,
    configuration as configuration_module,
    identifiers,
    log,
)

from . import (
    commands,
    connections,
    daemon_query,
    daemon_socket,
    frontend_configuration,
    no_daemon_query,
)


LOG: logging.Logger = logging.getLogger(__name__)


HELP_MESSAGE: str = """
Possible queries:
  - attributes(class_name)
    Returns a list of attributes, including functions, for a class.
  - batch(query1(arg), query2(arg))
    Runs a batch of queries and returns a map of responses. List of given queries
    may include any combination of other valid queries except for `batch` itself.
  - callees(function)
    Calls from a given function.
  - callees_with_location(function)
    Calls from a given function, including the locations at which they are called.
  - defines(module_or_class_name)
    Returns a JSON with the signature of all defines for given module or class.
  - dump_call_graph()
    Returns a comprehensive JSON of caller -> list of callees.
  - inline_decorators(qualified_function_name, decorators_to_skip=[decorator1, ...])
    Returns the function definition after inlining decorators.
    Allows skipping certain decorators when inlining.
  - less_or_equal(T1, T2)
    Returns whether T1 is a subtype of T2.
  - model_query(path, 'model_query_name')
    Returns in JSON a list of all models generated from the query with the name
    `model_query_name` in the directory `path`.
  - path_of_module(module)
    Gives an absolute path for `module`.
  - save_server_state('path')
    Saves Pyre's serialized state into `path`.
  - superclasses(class_name1, class_name2, ...)
    Returns a mapping of class_name to the list of superclasses for `class_name`.
    If no class name is provided, return the mapping for all classes Pyre knows about.
  - type(expression)
    Evaluates the type of `expression`.
  - types(path='path') or types('path1', 'path2', ...)
    Returns a map from each given path to a list of all types for that path.
  - validate_taint_models('optional path')
    Validates models and returns errors.
    Defaults to model path in configuration if no parameter is passed in.
"""


def _print_help_message() -> None:
    log.stdout.write(HELP_MESSAGE)


def run_query(
    configuration: frontend_configuration.Base, query_text: str
) -> commands.ExitCode:
    socket_path = daemon_socket.get_socket_path(
        configuration.get_project_identifier(),
        flavor=identifiers.PyreFlavor.CLASSIC,
    )
    try:
        if query_text == "help":
            _print_help_message()
            return commands.ExitCode.SUCCESS

        response = daemon_query.execute_query(socket_path, query_text)
        log.stdout.write(json.dumps(response.payload))
        return commands.ExitCode.SUCCESS
    except connections.ConnectionFailure:
        LOG.warning(
            "A running Pyre server is required for queries to be responded. "
            "Please run `pyre` first to set up a server."
        )
        return commands.ExitCode.SERVER_NOT_FOUND


def run(
    configuration: configuration_module.Configuration,
    query_arguments: command_arguments.QueryArguments,
) -> commands.ExitCode:
    if query_arguments.no_daemon:
        response = no_daemon_query.execute_query(
            frontend_configuration.OpenSource(configuration),
            query_arguments,
        )
        if response is not None:
            log.stdout.write(json.dumps(response.payload))
            return commands.ExitCode.SUCCESS
        else:
            return commands.ExitCode.FAILURE
    else:
        return run_query(
            frontend_configuration.OpenSource(configuration), query_arguments.query
        )
