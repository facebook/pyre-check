import subprocess
import unittest
from pathlib import Path
from unittest.mock import _patch, call, patch

from ..connection_api import PyreConnection


class ConnectionApiTest(unittest.TestCase):
    @patch("subprocess.run")
    def test_query_server(self, run: _patch) -> None:
        # We always start a server once when querying.
        pyre_connection = PyreConnection(Path("/tmp"))
        pyre_connection.start_server()
        pyre_connection.query_server("hi")
        self.assertEqual(
            # pyre-ignore: _patch types are deficient.
            run.call_args_list,
            [
                call(["pyre", "start"], cwd="/tmp"),
                call(["pyre", "incremental"], cwd="/tmp", stdout=subprocess.PIPE),
                call(["pyre", "query", "hi"], cwd="/tmp", stdout=subprocess.PIPE),
            ],
        )
        # pyre-ignore: _patch types are deficient.
        run.reset_mock()

        pyre_connection = PyreConnection(Path("/tmp"))
        pyre_connection.query_server("hi")
        self.assertEqual(
            run.call_args_list,
            [
                call(["pyre", "start"], cwd="/tmp"),
                call(["pyre", "incremental"], cwd="/tmp", stdout=subprocess.PIPE),
                call(["pyre", "query", "hi"], cwd="/tmp", stdout=subprocess.PIPE),
            ],
        )
        run.reset_mock()

        pyre_connection = PyreConnection(Path("/tmp"))
        pyre_connection.query_server("hi")
        pyre_connection.query_server("bye")
        self.assertEqual(
            run.call_args_list,
            [
                call(["pyre", "start"], cwd="/tmp"),
                call(["pyre", "incremental"], cwd="/tmp", stdout=subprocess.PIPE),
                call(["pyre", "query", "hi"], cwd="/tmp", stdout=subprocess.PIPE),
                call(["pyre", "query", "bye"], cwd="/tmp", stdout=subprocess.PIPE),
            ],
        )
        run.reset_mock()

        with PyreConnection(Path("/tmp")) as pyre_connection:
            pyre_connection.query_server("hi")
        self.assertEqual(
            run.call_args_list,
            [
                call(["pyre", "start"], cwd="/tmp"),
                call(["pyre", "incremental"], cwd="/tmp", stdout=subprocess.PIPE),
                call(["pyre", "query", "hi"], cwd="/tmp", stdout=subprocess.PIPE),
                call(["pyre", "stop"], check=True, cwd="/tmp"),
            ],
        )

    def test_context_manager(self) -> None:
        with patch.object(PyreConnection, "start_server") as start_server, patch.object(
            PyreConnection, "stop_server"
        ) as stop_server:
            with PyreConnection():
                pass
            start_server.assert_called_once_with()
            stop_server.assert_called_once_with()
