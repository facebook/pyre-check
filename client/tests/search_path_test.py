# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import tempfile
from pathlib import Path

import testslide

from ..configuration import create_search_paths, InvalidConfiguration
from ..search_path import SimpleElement, SubdirectoryElement, SitePackageElement
from .setup import (
    ensure_directories_exists,
)


class SearchPathTest(testslide.TestCase):
    def test_create(self) -> None:
        self.assertListEqual(
            create_search_paths("foo", site_roots=[]), [SimpleElement("foo")]
        )
        self.assertListEqual(
            create_search_paths({"root": "foo", "subdirectory": "bar"}, site_roots=[]),
            [SubdirectoryElement("foo", "bar")],
        )
        self.assertListEqual(
            create_search_paths({"import_root": "foo", "source": "bar"}, site_roots=[]),
            [SubdirectoryElement("foo", "bar")],
        )
        self.assertListEqual(
            create_search_paths({"site-package": "foo"}, site_roots=[]), []
        )
        self.assertListEqual(
            create_search_paths({"site-package": "foo"}, site_roots=["site0"]),
            [SitePackageElement("site0", "foo")],
        )
        self.assertListEqual(
            create_search_paths({"site-package": "foo"}, site_roots=["site1"]),
            [SitePackageElement("site1", "foo")],
        )
        self.assertListEqual(
            create_search_paths(
                {"site-package": "foo", "is_toplevel_module": "true"},
                site_roots=["site1"],
            ),
            [SitePackageElement("site1", "foo", True)],
        )

        with self.assertRaises(InvalidConfiguration):
            create_search_paths({}, site_roots=[])
        with self.assertRaises(InvalidConfiguration):
            create_search_paths({"foo": "bar"}, site_roots=[])
        with self.assertRaises(InvalidConfiguration):
            create_search_paths({"root": "foo"}, site_roots=[])

    def test_path(self) -> None:
        self.assertEqual(SimpleElement("foo").path(), "foo")
        self.assertEqual(SubdirectoryElement("foo", "bar").path(), "foo/bar")
        self.assertEqual(SitePackageElement("foo", "bar").path(), "foo/bar")

    def test_get_root(self) -> None:
        self.assertEqual(SimpleElement("foo").get_root(), "foo")
        self.assertEqual(SubdirectoryElement("foo", "bar").get_root(), "foo")
        self.assertEqual(SitePackageElement("foo", "bar").get_root(), "foo")

    def test_command_line_argument(self) -> None:
        self.assertEqual(SimpleElement("foo").command_line_argument(), "foo")
        self.assertEqual(
            SubdirectoryElement("foo", "bar").command_line_argument(),
            "foo$bar",
        )
        self.assertEqual(
            SitePackageElement("foo", "bar").command_line_argument(),
            "foo$bar",
        )
        self.assertEqual(
            SitePackageElement("foo", "bar", True).command_line_argument(),
            "foo$bar.py",
        )

    def test_expand_global_root(self) -> None:
        self.assertEqual(
            SimpleElement("//simple/path").expand_global_root("root"),
            SimpleElement("root/simple/path"),
        )
        self.assertEqual(
            SubdirectoryElement("//path", "sub").expand_global_root("root"),
            SubdirectoryElement("root/path", "sub"),
        )
        self.assertEqual(
            SitePackageElement("//site_root", "package").expand_global_root("root"),
            SitePackageElement("//site_root", "package"),
        )

    def test_expand_relative_root(self) -> None:
        self.assertEqual(
            SimpleElement("simple/path").expand_relative_root("root/local_project"),
            SimpleElement("root/local_project/simple/path"),
        )
        self.assertEqual(
            SubdirectoryElement("path", "sub").expand_relative_root(
                "root/local_project"
            ),
            SubdirectoryElement("root/local_project/path", "sub"),
        )
        self.assertEqual(
            SitePackageElement("site_root", "package").expand_relative_root(
                "root/local_project"
            ),
            SitePackageElement("site_root", "package"),
        )

    def test_expand_glob(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root)
            ensure_directories_exists(root_path, ["a1", "a2", "b"])

            search_path = SimpleElement(str(root_path / "a*"))

            self.assertListEqual(
                search_path.expand_glob(),
                [
                    SimpleElement(str(root_path / "a1")),
                    SimpleElement(str(root_path / "a2")),
                ],
            )
