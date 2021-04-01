# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging
import os
import re
from logging import Logger
from typing import Dict, List, Optional

from .. import command_arguments, json_rpc, log
from ..analysis_directory import AnalysisDirectory
from ..configuration import Configuration
from .command import Command, ExitCode, Result, State


LOG: Logger = logging.getLogger(__name__)

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
  - less_or_equal(T1, T2)
    Returns whether T1 is a subtype of T2.
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


class Query(Command):
    NAME = "query"

    _result: Optional[Result] = None

    def result(self) -> Optional[Result]:
        return self._result

    def _rewrite_paths(self, query: str) -> str:
        paths = re.findall(r"'[a-zA-Z_\-\.\/0-9]+\.py'", query)
        symbolic_link_mapping: Optional[Dict[str, str]] = None
        for path in paths:
            # Lazily compute the symbolic link mapping, as it can add over a second of
            # latency for large projects.
            if symbolic_link_mapping is None:
                symbolic_link_mapping = (
                    self._analysis_directory.compute_symbolic_links()
                )
            path = path[1:-1]
            absolute_path = os.path.abspath(path)
            if absolute_path in symbolic_link_mapping:
                query = query.replace(path, symbolic_link_mapping[absolute_path])
        return query

    def __init__(
        self,
        command_arguments: command_arguments.CommandArguments,
        original_directory: str,
        *,
        configuration: Configuration,
        analysis_directory: Optional[AnalysisDirectory] = None,
        query: str,
    ) -> None:
        super(Query, self).__init__(
            command_arguments, original_directory, configuration, analysis_directory
        )
        self.query: str = self._rewrite_paths(query)

    def _flags(self) -> List[str]:
        flags = [self.query]
        flags.extend(["-log-directory", self._configuration.log_directory])
        return flags

    def _run(self) -> None:
        if self.query == "help":
            log.stdout.write(HELP_MESSAGE)
            return

        if self._state() == State.DEAD:
            LOG.error("No server running to query.")
            self._exit_code = ExitCode.SERVER_NOT_FOUND
            return
        LOG.info("Waiting for server...")
        with self._analysis_directory.acquire_shared_reader_lock():
            request = json_rpc.Request(
                method="typeQuery",
                parameters=json_rpc.ByNameParameters({"query": self.query}),
            )
            self._send_and_handle_socket_request(request, self._version_hash)

    def _socket_result_handler(self, result: Result) -> None:
        self._result = result
        LOG.log(log.SUCCESS, "Received response from server")
        log.stdout.write(result.output)
