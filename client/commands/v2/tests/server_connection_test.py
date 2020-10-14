# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import hashlib
import tempfile
from pathlib import Path

import testslide

from ....tests import setup
from ..server_connection import get_socket_path


class SocketTest(testslide.TestCase):
    def test_get_socket_path(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root)
            setup.ensure_directories_exists(root_path, ["log"])

            md5_hash = hashlib.md5(str(root_path / "log").encode("utf-8")).hexdigest()
            self.assertEqual(
                get_socket_path(root=root_path, log_directory=root_path / "log"),
                root_path / f"pyre_server_{md5_hash}.sock",
            )
