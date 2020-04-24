# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from pathlib import Path
from unittest.mock import MagicMock, patch

from ... import commands
from ...analysis_directory import AnalysisDirectory
from ...json_rpc import read_response
from ...socket_connection import SocketConnection
from .command_test import mock_arguments, mock_configuration


class QueryTest(unittest.TestCase):
    def test_query(self) -> None:
        original_directory = "/original/directory"
        arguments = mock_arguments(dot_pyre_directory=Path("/tmp/foo"))
        configuration = mock_configuration()

        with patch.object(SocketConnection, "connect") as connect:
            result = MagicMock()
            result.output = "{}"
            # pyre-fixme[16]: Callable `read_response` has no attribute `return_value`.
            read_response.return_value = result

            commands.Query(
                arguments,
                original_directory,
                configuration=configuration,
                analysis_directory=AnalysisDirectory("."),
                query="",
            ).run()
            connect.assert_called_once()

        self.assertEqual(
            commands.Query(
                arguments,
                original_directory,
                configuration=configuration,
                analysis_directory=AnalysisDirectory("."),
                query="query",
            )._flags(),
            ["query", "-log-directory", "/tmp/foo"],
        )

    def test_rewrite_paths(self) -> None:
        original_directory = "/original/directory"
        arguments = mock_arguments()
        configuration = mock_configuration()
        analysis_directory = MagicMock()
        analysis_directory.compute_symbolic_links.return_value = {
            "/root/a.py": "/shared/a.py",
            "/root/b.py": "/shared/b.py",
        }
        query = commands.Query(
            arguments,
            original_directory,
            configuration=configuration,
            analysis_directory=analysis_directory,
            query="",
        )
        self.assertEqual(
            query._rewrite_paths("run_check('awaitable', '/root/a.py')"),
            "run_check('awaitable', '/shared/a.py')",
        )
        self.assertEqual(
            query._rewrite_paths("run_check('awaitable', '/root/b.py')"),
            "run_check('awaitable', '/shared/b.py')",
        )
        self.assertEqual(
            query._rewrite_paths("run_check('awaitable', 'other/root/b.py')"),
            "run_check('awaitable', 'other/root/b.py')",
        )
        self.assertEqual(
            query._rewrite_paths("run_check('awaitable', '/root/b.py/suffix')"),
            "run_check('awaitable', '/root/b.py/suffix')",
        )
        # We don't parse anything when rewriting paths.
        self.assertEqual(
            query._rewrite_paths("'/root/b.py'run_check('awaitable', '/root/a.py')"),
            "'/shared/b.py'run_check('awaitable', '/shared/a.py')",
        )
