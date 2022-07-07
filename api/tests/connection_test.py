# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import subprocess
import unittest
from pathlib import Path
from unittest.mock import call, MagicMock, patch

from ..connection import PyreConnection, PyreQueryError, PyreQueryUnexpectedError


class ConnectionApiTest(unittest.TestCase):
    # pyre-ignore[56]
    @patch.object(
        PyreConnection,
        "_validate_query_response",
        side_effect=lambda response: response,
    )
    @patch("subprocess.run")
    def test_query_server(
        self, run: MagicMock, _validate_query_response: MagicMock
    ) -> None:
        run_result = MagicMock()
        run_result.returncode = 0
        run.return_value = run_result
        # We always start a server once when querying.
        pyre_connection = PyreConnection(Path("/tmp"))
        pyre_connection.server_initialized = False
        pyre_connection.query_server("hi")
        self.assertEqual(
            run.call_args_list,
            [
                call(
                    ["pyre", "--noninteractive", "incremental"],
                    cwd="/tmp",
                    stdout=subprocess.PIPE,
                ),
                call(
                    ["pyre", "--noninteractive", "query", "hi"],
                    cwd="/tmp",
                    stdout=subprocess.PIPE,
                ),
            ],
        )
        run.reset_mock()

        pyre_connection = PyreConnection(
            Path("/tmp"),
            skip_initial_type_check=True,
        )
        pyre_connection.query_server("hi")
        self.assertEqual(
            run.call_args_list,
            [
                call(
                    ["pyre", "--noninteractive", "start", "--skip-initial-type-check"],
                    cwd="/tmp",
                    stdout=subprocess.PIPE,
                ),
                call(
                    ["pyre", "--noninteractive", "query", "hi"],
                    cwd="/tmp",
                    stdout=subprocess.PIPE,
                ),
            ],
        )
        run.reset_mock()

        pyre_connection = PyreConnection(Path("/tmp"))
        pyre_connection.query_server("hi")
        pyre_connection.query_server("bye")
        self.assertEqual(
            run.call_args_list,
            [
                call(
                    ["pyre", "--noninteractive", "incremental"],
                    cwd="/tmp",
                    stdout=subprocess.PIPE,
                ),
                call(
                    ["pyre", "--noninteractive", "query", "hi"],
                    cwd="/tmp",
                    stdout=subprocess.PIPE,
                ),
                call(
                    ["pyre", "--noninteractive", "query", "bye"],
                    cwd="/tmp",
                    stdout=subprocess.PIPE,
                ),
            ],
        )
        run.reset_mock()

        with PyreConnection(Path("/tmp")) as pyre_connection:
            pyre_connection.query_server("hi")
        self.assertEqual(
            run.call_args_list,
            [
                call(
                    ["pyre", "--noninteractive", "incremental"],
                    cwd="/tmp",
                    stdout=subprocess.PIPE,
                ),
                call(
                    ["pyre", "--noninteractive", "query", "hi"],
                    cwd="/tmp",
                    stdout=subprocess.PIPE,
                ),
                call(["pyre", "--noninteractive", "stop"], check=True, cwd="/tmp"),
            ],
        )

    def test_validate_query_response(self) -> None:
        with self.assertRaisesRegex(PyreQueryError, "Foo"):
            PyreConnection._validate_query_response('{"error": "Foo"}')
        with self.assertRaisesRegex(PyreQueryUnexpectedError, "is not valid JSON."):
            PyreConnection._validate_query_response("asdf")
        with self.assertRaisesRegex(
            PyreQueryUnexpectedError, "The server response is invalid."
        ):
            PyreConnection._validate_query_response("{}")
        self.assertEqual(
            PyreConnection._validate_query_response('{"response": "Foo"}'),
            {"response": "Foo"},
        )

    def test_context_manager(self) -> None:
        with patch.object(PyreConnection, "start_server") as start_server, patch.object(
            PyreConnection, "stop_server"
        ) as stop_server:
            with PyreConnection():
                pass
            start_server.assert_called_once_with()
            stop_server.assert_called_once_with()
