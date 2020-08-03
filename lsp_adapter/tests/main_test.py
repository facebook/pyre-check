# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import sys
import unittest
from unittest.mock import MagicMock, patch

from ..main import NullServerAdapterProtocol, _parse_json_rpc


class AdapterProtocolTest(unittest.TestCase):
    @patch.object(sys.stdout.buffer, "write")
    def test_run_null_server(self, stdout_write: MagicMock) -> None:
        example_request = b'Content-Length: 2756\r\n\r\n{"jsonrpc":"2.0","id":0,"method":"initialize","params":{"processId":null}}'  # noqa
        adapter = NullServerAdapterProtocol()
        adapter.data_received(example_request)
        stdout_write.assert_called_once_with(
            b'Content-Length: 59\r\n\r\n{"jsonrpc": "2.0", "id": 0, "result": {"capabilities": {}}}'  # noqa
        )

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
