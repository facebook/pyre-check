# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import hashlib
import tempfile
from pathlib import Path

import testslide

from ..daemon import get_socket_path


class SocketTest(testslide.TestCase):
    def test_get_socket_path(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root)
            project_root = Path("project_root")
            relative_local_root = "my/project"
            md5_hash = hashlib.md5(
                (str(project_root) + "//" + str(relative_local_root)).encode("utf-8")
            ).hexdigest()
            self.assertEqual(
                get_socket_path(root_path, project_root, relative_local_root),
                root_path / f"pyre_server_{md5_hash}.sock",
            )

            # No local directory
            root_path = Path(root)
            project_root = Path("project_root")
            relative_local_root = None
            md5_hash = hashlib.md5(
                str(project_root).encode("utf-8"),
            ).hexdigest()
            self.assertEqual(
                get_socket_path(root_path, project_root, relative_local_root),
                root_path / f"pyre_server_{md5_hash}.sock",
            )

            # Test different servers are differentiable
            project_root = Path("project_root")
            relative_local_root_a = Path("my/project")
            relative_local_root_b = Path("my/otherproject")
            md5_hash_a = hashlib.md5(
                (str(project_root) + "//" + str(relative_local_root_a)).encode("utf-8")
            ).hexdigest()
            md5_hash_a_recomputed = hashlib.md5(
                (str(project_root) + "//" + str(relative_local_root_a)).encode("utf-8")
            ).hexdigest()
            md5_hash_b = hashlib.md5(
                (str(project_root) + "//" + str(relative_local_root_b)).encode("utf-8")
            ).hexdigest()
            self.assertTrue(md5_hash_a == md5_hash_a_recomputed)
            self.assertFalse(md5_hash_a == md5_hash_b)

            # Test socket name length
            project_root = Path("project_root" * 100)
            relative_local_root = Path("my/project")
            md5_hash = hashlib.md5(
                (str(project_root) + "//" + str(relative_local_root)).encode("utf-8")
            ).hexdigest()
            self.assertTrue(len(md5_hash) < 100)
