# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import asyncio
import sys
import unittest
from unittest import mock
from unittest.mock import MagicMock, patch

from .. import main as lsp_main
from ..main import NullServerAdapterProtocol, _parse_json_rpc, main


class AdapterProtocolTest(unittest.TestCase):
    @patch.object(sys.stdout.buffer, "write")
    def test_run_null_server(self, stdout_write: MagicMock) -> None:
        example_request = b'Content-Length: 2756\r\n\r\n{"jsonrpc":"2.0","id":0,"method":"initialize","params":{"processId":null}}'  # noqa
        adapter = NullServerAdapterProtocol()
        adapter.data_received(example_request)
        stdout_write.assert_called_once_with(
            b'Content-Length: 59\r\n\r\n{"jsonrpc": "2.0", "id": 0, "result": {"capabilities": {}}}'  # noqa
        )

    @patch.object(sys.stdout.buffer, "write")
    @patch.object(lsp_main, "run_null_server")
    # pyre-fixme[56]: Pyre was not able to infer the type of argument `asyncio` to
    #  decorator factory `unittest.mock.patch.object`.
    @patch.object(asyncio, "get_event_loop")
    def test_run_null_server_pyre_error(
        self, stdout_write: MagicMock, run_null_server: MagicMock, event_loop: MagicMock
    ) -> None:
        event_loop.run_forever = MagicMock()
        with mock.patch("subprocess.run") as subprocess_mock:
            subprocess_mock.side_effect = Exception
            main(root="test/project_root", null_server=False)
            run_null_server.assert_called_once
            event_loop.run_forever.assert_called_once

    def test_parse_json(self) -> None:
        """
        Sample_data in this test looks like two jsonrpc requests in one.
        {
            "jsonrpc": "2.0",
            "method": "initialized",
            "params": {}
        }Content-Length: 7273\r\n\r\n{
            "jsonrpc": "2.0",
            "method": "textDocument/didOpen",
            "params": {
                "textDocument": {
                    "uri": "file:///example/main.py",
                    "languageId": "python",
                    "version": 1,
                    "text": "# Example file text."
                }
            }
        }
        """
        sample_data = b'{"jsonrpc":"2.0","method":"initialized","params":{}}Content-Length: 7273\r\n\r\n{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"file:///example/main.py","languageId":"python","version":1,"text":"# Example file text."}}}'  # noqa
        parsed_data = _parse_json_rpc(sample_data)
        self.assertEqual(2, len(parsed_data))
        self.assertEqual(
            {"jsonrpc": "2.0", "method": "initialized", "params": {}}, parsed_data[0]
        )
