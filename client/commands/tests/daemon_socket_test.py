# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import tempfile
from pathlib import Path

import testslide

from ..daemon_socket import get_md5, get_socket_path, MD5_LENGTH


class SocketTest(testslide.TestCase):
    def test_get_md5(self) -> None:
        # Test different servers are differentiable
        project_root = Path("project_root")
        relative_local_root_a = Path("my/project")
        relative_local_root_b = Path("my/otherproject")
        md5_hash_a = get_md5((str(project_root) + "//" + str(relative_local_root_a)))
        md5_hash_a_recomputed = get_md5(
            (str(project_root) + "//" + str(relative_local_root_a))
        )
        md5_hash_b = get_md5((str(project_root) + "//" + str(relative_local_root_b)))
        self.assertTrue(md5_hash_a == md5_hash_a_recomputed)
        self.assertFalse(md5_hash_a == md5_hash_b)

        # Test socket name length
        project_root = Path("project_root" * 100)
        relative_local_root = Path("my/project")
        md5_hash = get_md5((str(project_root) + "//" + str(relative_local_root)))
        self.assertTrue(len(md5_hash) == MD5_LENGTH)

    def test_get_socket_path(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            # With local directory
            root_path = Path(root)
            project_root = Path("project_root")
            relative_local_root = "my/project"
            md5_hash = get_md5((str(project_root) + "//" + str(relative_local_root)))
            self.assertEqual(
                get_socket_path(
                    root_path,
                    project_root,
                    relative_local_root,
                ),
                root_path / f"pyre_server_{md5_hash}.sock",
            )
            # No local directory
            root_path = Path(root)
            project_root = Path("project_root")
            relative_local_root = None
            md5_hash = get_md5(str(project_root))
            self.assertEqual(
                get_socket_path(
                    root_path,
                    project_root,
                    relative_local_root,
                ),
                root_path / f"pyre_server_{md5_hash}.sock",
            )
