# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import json
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
        if self._state() == State.DEAD:
            LOG.error("No server running to query.")
            self._exit_code = ExitCode.SERVER_NOT_FOUND
            return
        LOG.info("Waiting for server...")
        with self._analysis_directory.acquire_shared_reader_lock():
            request = json_rpc.Request(
                method="typeQuery", parameters={"query": self.query}
            )
            self._send_and_handle_socket_request(request, self._version_hash)

    def _socket_result_handler(self, result: Result) -> None:
        self._result = result
        LOG.log(log.SUCCESS, "Received response from server")
        if self.query == "help":
            response = json.loads(result.output).get("response")
            log.stdout.write(response.get("help"))
            return
        log.stdout.write(result.output)
