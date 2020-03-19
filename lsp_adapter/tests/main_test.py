# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import sys
import unittest
from unittest.mock import MagicMock, patch

from ..main import AdapterProtocol


class AdapterProtocolTest(unittest.TestCase):
    @patch.object(sys.stdout.buffer, "write")
    def test_run_null_server(self, stdout_write: MagicMock) -> None:
        example_request = b'Content-Length: 2756\r\n\r\n{"jsonrpc":"2.0","id":0,"method":"initialize","params":{"processId":null}}'  # noqa
        adapter = AdapterProtocol()
        adapter.data_received(example_request)
        stdout_write.assert_called_once_with(
            b'Content-Length: 59\r\n\r\n{"jsonrpc": "2.0", "id": 0, "result": {"capabilities": {}}}'  # noqa
        )
