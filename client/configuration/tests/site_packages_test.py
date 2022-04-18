# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import builtins
import tempfile
from pathlib import Path

import testslide

from ...tests.setup import (
    ensure_files_exist,
)
from ..search_path import SimpleElement
from ..site_packages import (
    SearchStrategy,
    search_for_paths,
    is_valid_package_name,
    PackageStatus,
    get_package_status,
    MARKER_FILE,
    create_package_from_path,
    NonStubPackage,
    StubPackage,
)


class SitePackagesTest(testslide.TestCase):
    def test_search_strategy_from_string(self) -> None:
        self.assertEqual(SearchStrategy.from_string("all"), SearchStrategy.ALL)
        self.assertEqual(SearchStrategy.from_string("none"), SearchStrategy.NONE)
        self.assertEqual(SearchStrategy.from_string("pep561"), SearchStrategy.PEP561)
        self.assertIsNone(SearchStrategy.from_string("derp"))

    def test_search_for_path_disabled(self) -> None:
        self.assertListEqual(
            search_for_paths(SearchStrategy.NONE, site_roots=["derp"]), []
        )

    def test_search_for_path_all(self) -> None:
        self.assertListEqual(
            search_for_paths(SearchStrategy.ALL, site_roots=["/foo", "/bar"]),
            [SimpleElement("/foo"), SimpleElement("/bar")],
        )

    def test_valid_package_name(self) -> None:
        self.assertTrue(is_valid_package_name("foo"))
        self.assertTrue(is_valid_package_name("foo_bar"))
        self.assertTrue(is_valid_package_name("_baz"))

        self.assertFalse(is_valid_package_name(""))
        self.assertFalse(is_valid_package_name("__pycache__"))
        self.assertFalse(is_valid_package_name(".pyre"))
        self.assertFalse(is_valid_package_name("foo-1.2.3.dist-info"))
        self.assertFalse(is_valid_package_name("bar-1.2.3-py3.10.egg-info"))

    def test_get_package_status_untyped(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            ensure_files_exist(root_path, ["derp.py"])
            self.assertEqual(get_package_status(root_path), PackageStatus.UNTYPED)

    def test_get_package_status_typed(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            ensure_files_exist(root_path, ["derp.py", MARKER_FILE])
            self.assertEqual(get_package_status(root_path), PackageStatus.TYPED)

    def test_get_package_status_partial(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            ensure_files_exist(root_path, ["derp.py"])
            (root_path / MARKER_FILE).write_text("partial\n")
            self.assertEqual(
                get_package_status(root_path), PackageStatus.PARTIALLY_TYPED
            )

    def test_get_package_status_permission_error(self) -> None:
        # A mock is needed here since there is no cross-platform API to set
        # file permissions in pure Python
        self.mock_callable(builtins, "open").to_raise(PermissionError)
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            ensure_files_exist(root_path, ["derp.py", MARKER_FILE])
            (root_path / MARKER_FILE).write_text("partial\n")
            self.assertEqual(get_package_status(root_path), PackageStatus.UNTYPED)

    def test_create_package_from_path(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            ensure_files_exist(
                root_path,
                [
                    "foo/foo.py",
                    "bar/bar.py",
                    "bar/py.typed",
                    "baz-stubs/baz.pyi",
                    "qux-stubs/qux.pyi",
                ],
            )

            foo_path = root_path / "foo"
            bar_path = root_path / "bar"
            baz_path = root_path / "baz-stubs"
            qux_path = root_path / "qux-stubs"
            (qux_path / MARKER_FILE).write_text("partial\n")

            self.assertEqual(
                create_package_from_path(foo_path),
                NonStubPackage(name="foo", path=foo_path, is_typed=False),
            )
            self.assertEqual(
                create_package_from_path(bar_path),
                NonStubPackage(name="bar", path=bar_path, is_typed=True),
            )
            self.assertEqual(
                create_package_from_path(baz_path),
                StubPackage(name="baz", path=baz_path, is_partial=False),
            )
            self.assertEqual(
                create_package_from_path(qux_path),
                StubPackage(name="qux", path=qux_path, is_partial=True),
            )
