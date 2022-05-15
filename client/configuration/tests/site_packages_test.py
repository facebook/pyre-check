# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import builtins
import tempfile
from pathlib import Path

import testslide

from ...tests.setup import ensure_files_exist
from ..search_path import SimpleElement, SitePackageElement
from ..site_packages import (
    create_package_from_path,
    find_packages,
    get_package_status,
    is_valid_package_name,
    MARKER_FILE,
    NonStubPackage,
    PackageInfo,
    PackageStatus,
    search_for_paths,
    SearchStrategy,
    StubPackage,
)


class SitePackagesTest(testslide.TestCase):
    def test_search_strategy_from_string(self) -> None:
        self.assertEqual(SearchStrategy.from_string("all"), SearchStrategy.ALL)
        self.assertEqual(SearchStrategy.from_string("none"), SearchStrategy.NONE)
        self.assertEqual(SearchStrategy.from_string("pep561"), SearchStrategy.PEP561)
        self.assertIsNone(SearchStrategy.from_string("derp"))

    def test_search_for_path_disabled(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            foo = str(root_path / Path("foo"))
            ensure_files_exist(root_path, ["foo/foo.py"])
            self.assertListEqual(
                search_for_paths(SearchStrategy.NONE, site_roots=[foo]), []
            )

    def test_search_for_path_all(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            foo = str(root_path / Path("foo"))
            bar = str(root_path / Path("bar"))
            ensure_files_exist(root_path, ["foo/foo.py", "bar/bar.py"])
            does_not_exist = str(root_path / Path("does_not_exist"))
            self.assertListEqual(
                search_for_paths(
                    SearchStrategy.ALL,
                    site_roots=[foo, bar, does_not_exist],
                ),
                [SimpleElement(foo), SimpleElement(bar)],
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

    def test_find_packages_basic(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            ensure_files_exist(
                root_path,
                [
                    "foo/foo.py",
                    "bar/bar.py",
                    "bar/py.typed",
                    "baz/baz.py",
                    "baz-stubs/baz.pyi",
                    "qux-stubs/qux.pyi",
                    "standalone.py",
                    "random.txt",
                ],
            )
            (root_path / "baz-stubs" / MARKER_FILE).write_text("partial\n")

            self.assertCountEqual(
                find_packages([str(root_path)]),
                [
                    PackageInfo(
                        nonstub_package=NonStubPackage(
                            name="foo", path=Path(root_path / "foo"), is_typed=False
                        )
                    ),
                    PackageInfo(
                        nonstub_package=NonStubPackage(
                            name="bar", path=Path(root_path / "bar"), is_typed=True
                        )
                    ),
                    PackageInfo(
                        nonstub_package=NonStubPackage(
                            name="baz", path=Path(root_path / "baz"), is_typed=False
                        ),
                        stub_package=StubPackage(
                            name="baz",
                            path=Path(root_path / "baz-stubs"),
                            is_partial=True,
                        ),
                    ),
                    PackageInfo(
                        stub_package=StubPackage(
                            name="qux",
                            path=Path(root_path / "qux-stubs"),
                            is_partial=False,
                        )
                    ),
                ],
            )

    def test_find_packages_priority(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            ensure_files_exist(
                root_path,
                [
                    "root0/foo/foo.py",
                    "root0/bar-stubs/bar.pyi",
                    "root1/foo/foo.py",
                    "root1/foo-stubs/foo.pyi",
                    "root1/bar-stubs/bar.pyi",
                ],
            )

            self.assertCountEqual(
                find_packages([str(root_path / "root0"), str(root_path / "root1")]),
                [
                    PackageInfo(
                        nonstub_package=NonStubPackage(
                            name="foo",
                            path=Path(root_path / "root0" / "foo"),
                            is_typed=False,
                        ),
                        stub_package=StubPackage(
                            name="foo",
                            path=Path(root_path / "root1" / "foo-stubs"),
                            is_partial=False,
                        ),
                    ),
                    PackageInfo(
                        stub_package=StubPackage(
                            name="bar",
                            path=Path(root_path / "root0" / "bar-stubs"),
                            is_partial=False,
                        )
                    ),
                ],
            )

    def test_to_search_path_element(self) -> None:
        self.assertEqual(
            NonStubPackage(
                name="foo", path=Path("/site_root/foo")
            ).to_search_path_element(),
            SitePackageElement(site_root="/site_root", package_name="foo"),
        )
        self.assertEqual(
            StubPackage(
                name="foo",
                path=Path("/site_root/foo-stubs"),
                is_partial=True,
            ).to_search_path_element(),
            SitePackageElement(site_root="/site_root", package_name="foo-stubs"),
        )

    def test_to_search_path_elements(self) -> None:
        self.assertListEqual(
            PackageInfo().to_search_path_elements(),
            [],
        )
        self.assertListEqual(
            PackageInfo(
                nonstub_package=NonStubPackage(
                    name="foo", path=Path("/site_root/foo"), is_typed=False
                )
            ).to_search_path_elements(),
            [],
        )
        self.assertListEqual(
            PackageInfo(
                nonstub_package=NonStubPackage(
                    name="foo", path=Path("/site_root/foo"), is_typed=True
                )
            ).to_search_path_elements(),
            [SitePackageElement(site_root="/site_root", package_name="foo")],
        )
        self.assertListEqual(
            PackageInfo(
                stub_package=StubPackage(
                    name="foo", path=Path("/site_root/foo-stubs"), is_partial=False
                )
            ).to_search_path_elements(),
            [SitePackageElement(site_root="/site_root", package_name="foo-stubs")],
        )
        self.assertListEqual(
            PackageInfo(
                nonstub_package=NonStubPackage(
                    name="foo", path=Path("/site_root/foo"), is_typed=False
                ),
                stub_package=StubPackage(
                    name="foo", path=Path("/site_root/foo-stubs"), is_partial=False
                ),
            ).to_search_path_elements(),
            [SitePackageElement(site_root="/site_root", package_name="foo-stubs")],
        )
        self.assertListEqual(
            PackageInfo(
                nonstub_package=NonStubPackage(
                    name="foo", path=Path("/site_root/foo"), is_typed=False
                ),
                stub_package=StubPackage(
                    name="foo", path=Path("/site_root/foo-stubs"), is_partial=True
                ),
            ).to_search_path_elements(),
            [
                SitePackageElement(site_root="/site_root", package_name="foo-stubs"),
                SitePackageElement(site_root="/site_root", package_name="foo"),
            ],
        )

    def test_search_for_path_pep561(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            ensure_files_exist(
                root_path,
                [
                    "foo/foo.py",
                    "bar/bar.py",
                    "bar/py.typed",
                    "baz/baz.py",
                    "baz-stubs/baz.pyi",
                    "qux-stubs/qux.pyi",
                    "standalone.py",
                    "random.txt",
                ],
            )

            self.assertCountEqual(
                search_for_paths(SearchStrategy.PEP561, site_roots=[str(root_path)]),
                [
                    SitePackageElement(site_root=str(root_path), package_name="bar"),
                    SitePackageElement(
                        site_root=str(root_path), package_name="baz-stubs"
                    ),
                    SitePackageElement(
                        site_root=str(root_path), package_name="qux-stubs"
                    ),
                ],
            )
