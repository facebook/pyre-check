# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import tempfile
from pathlib import Path
from typing import Optional

import testslide

from ...identifiers import get_project_identifier, PyreFlavor

from ..daemon_socket import (
    _get_socket_path_in_root,
    find_socket_files,
    get_md5,
    MD5_LENGTH,
)


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

    def test_get_project_identifier(self) -> None:
        project_root = Path("project_root")
        self.assertEqual(
            get_project_identifier(project_root, relative_local_root=None),
            str(project_root),
        )
        self.assertEqual(
            get_project_identifier(project_root, relative_local_root="relative"),
            str(project_root) + "//relative",
        )

    def _assert_socket_path(
        self,
        socket_root: Path,
        project_root: Path,
        relative_local_root: Optional[str],
        flavor: PyreFlavor = PyreFlavor.CLASSIC,
        suffix: str = "",
    ) -> None:
        md5_hash = get_md5(get_project_identifier(project_root, relative_local_root))
        self.assertEqual(
            _get_socket_path_in_root(
                socket_root,
                get_project_identifier(project_root, relative_local_root),
                flavor,
            ),
            socket_root / f"pyre_server_{md5_hash}{suffix}.sock",
        )

    def test_get_socket_path(self) -> None:
        socket_root = Path("socket_root")
        # With local directory
        self._assert_socket_path(
            socket_root=socket_root,
            project_root=Path("project_root"),
            relative_local_root="my/project",
        )
        # No local directory
        self._assert_socket_path(
            socket_root=socket_root,
            project_root=Path("project_root"),
            relative_local_root=None,
        )
        # No local directory
        self._assert_socket_path(
            socket_root=socket_root,
            project_root=Path("project_root"),
            relative_local_root=None,
            flavor=PyreFlavor.SHADOW,
            suffix="__shadow",
        )

    def test_find_socket_files(self) -> None:
        with tempfile.TemporaryDirectory(dir="/tmp") as socket_root:
            socket_root_path = Path(socket_root)
            socket_a = _get_socket_path_in_root(
                socket_root_path,
                project_identifier="a",
                flavor=PyreFlavor.CLASSIC,
            )
            socket_a.touch()
            self.assertEqual(
                set(find_socket_files(socket_root_path)),
                {socket_a},
            )
            socket_b = _get_socket_path_in_root(
                socket_root_path,
                project_identifier="b//relative_to_b",
                flavor=PyreFlavor.CLASSIC,
            )
            socket_b.touch()
            self.assertEqual(
                set(find_socket_files(socket_root_path)),
                {socket_a, socket_b},
            )
            socket_c = _get_socket_path_in_root(
                socket_root_path,
                project_identifier="c",
                flavor=PyreFlavor.SHADOW,
            )
            socket_c.touch()
            self.assertEqual(
                set(find_socket_files(socket_root_path)),
                {socket_a, socket_b, socket_c},
            )

    def test_no_flavor_leads_to_too_long_name(self) -> None:
        # This isn't really a unit test of functionality per se; it is a
        # sanity check to make sure that PyreFlavor never leads to
        # socket name too long to be instantiated.
        for flavor in PyreFlavor:
            path = _get_socket_path_in_root(
                socket_root=Path("/dummy/socket/root"),
                project_identifier="dummy_project_identifier",
                flavor=flavor,
            )
            self.assertTrue(
                len(str(path)) < 100,
                msg=f"Path {path} is too long for a socket path",
            )
