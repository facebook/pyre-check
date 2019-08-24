# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from unittest.mock import MagicMock, patch

from ... import commands  # noqa
from ...filesystem import AnalysisDirectory
from .command_test import mock_arguments, mock_configuration


class QueryTest(unittest.TestCase):
    def test_query(self) -> None:
        arguments = mock_arguments()
        arguments.query = ""
        configuration = mock_configuration()

        with patch.object(commands.Command, "_call_client") as call_client:
            result = MagicMock()
            result.output = "{}"
            call_client.return_value = result

            commands.Query(arguments, configuration, AnalysisDirectory(".")).run()
            call_client.assert_called_once_with(command=commands.Query.NAME)

    def test_rewrite_paths(self) -> None:
        arguments = mock_arguments()
        configuration = mock_configuration()
        analysis_directory = MagicMock()
        arguments.query = ""
        analysis_directory.compute_symbolic_links.return_value = {
            "/root/a.py": "/shared/a.py",
            "/root/b.py": "/shared/b.py",
        }
        query = commands.Query(arguments, configuration, analysis_directory)
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
