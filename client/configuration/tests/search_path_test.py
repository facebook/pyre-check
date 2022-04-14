# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import tempfile
from pathlib import Path

import testslide

from ...tests.setup import (
    ensure_directories_exists,
)
from ..exceptions import InvalidConfiguration
from ..search_path import create_raw_elements
from ..search_path import (
    SimpleElement,
    SubdirectoryElement,
    SitePackageElement,
    SimpleRawElement,
    SubdirectoryRawElement,
    SitePackageRawElement,
    process_raw_elements,
)


class SearchPathTest(testslide.TestCase):
    def test_create_raw_elements(self) -> None:
        self.assertListEqual(
            create_raw_elements("foo", site_roots=[]), [SimpleRawElement("foo")]
        )
        self.assertListEqual(
            create_raw_elements({"root": "foo", "subdirectory": "bar"}, site_roots=[]),
            [SubdirectoryRawElement("foo", "bar")],
        )
        self.assertListEqual(
            create_raw_elements({"import_root": "foo", "source": "bar"}, site_roots=[]),
            [SubdirectoryRawElement("foo", "bar")],
        )
        self.assertListEqual(
            create_raw_elements({"site-package": "foo"}, site_roots=[]), []
        )
        self.assertListEqual(
            create_raw_elements({"site-package": "foo"}, site_roots=["site0"]),
            [SitePackageRawElement("site0", "foo")],
        )
        self.assertListEqual(
            create_raw_elements({"site-package": "foo"}, site_roots=["site1"]),
            [SitePackageRawElement("site1", "foo")],
        )
        self.assertListEqual(
            create_raw_elements(
                {"site-package": "foo", "is_toplevel_module": True},
                site_roots=["site1"],
            ),
            [SitePackageRawElement("site1", "foo", True)],
        )

        with self.assertRaises(InvalidConfiguration):
            create_raw_elements({}, site_roots=[])
        with self.assertRaises(InvalidConfiguration):
            create_raw_elements({"foo": "bar"}, site_roots=[])
        with self.assertRaises(InvalidConfiguration):
            create_raw_elements({"root": "foo"}, site_roots=[])
        with self.assertRaises(InvalidConfiguration):
            create_raw_elements({"root": 42, "subdirectory": "bar"}, site_roots=[])
        with self.assertRaises(InvalidConfiguration):
            create_raw_elements({"root": "foo", "subdirectory": []}, site_roots=[])
        with self.assertRaises(InvalidConfiguration):
            create_raw_elements({"import_root": 4.2, "source": "bar"}, site_roots=[])
        with self.assertRaises(InvalidConfiguration):
            create_raw_elements({"import_root": "foo", "source": False}, site_roots=[])
        with self.assertRaises(InvalidConfiguration):
            create_raw_elements({"site-package": {}}, site_roots=["site0"])
        with self.assertRaises(InvalidConfiguration):
            create_raw_elements(
                {"site-package": "foo", "is_toplevel_module": "derp"},
                site_roots=["site0"],
            )

    def test_path(self) -> None:
        self.assertEqual(SimpleElement("foo").path(), "foo")
        self.assertEqual(SubdirectoryElement("foo", "bar").path(), "foo/bar")
        self.assertEqual(SitePackageElement("foo", "bar").path(), "foo/bar")

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
            SimpleRawElement("//simple/path").expand_global_root("root"),
            SimpleRawElement("root/simple/path"),
        )
        self.assertEqual(
            SubdirectoryRawElement("//path", "sub").expand_global_root("root"),
            SubdirectoryRawElement("root/path", "sub"),
        )
        self.assertEqual(
            SitePackageRawElement("//site_root", "package").expand_global_root("root"),
            SitePackageRawElement("//site_root", "package"),
        )

    def test_expand_relative_root(self) -> None:
        self.assertEqual(
            SimpleRawElement("simple/path").expand_relative_root("root/local_project"),
            SimpleRawElement("root/local_project/simple/path"),
        )
        self.assertEqual(
            SubdirectoryRawElement("path", "sub").expand_relative_root(
                "root/local_project"
            ),
            SubdirectoryRawElement("root/local_project/path", "sub"),
        )
        self.assertEqual(
            SitePackageRawElement("site_root", "package").expand_relative_root(
                "root/local_project"
            ),
            SitePackageRawElement("site_root", "package"),
        )

    def test_expand_glob(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root)
            ensure_directories_exists(root_path, ["a1", "a2", "b"])

            search_path = SimpleRawElement(str(root_path / "a*"))

            self.assertListEqual(
                search_path.expand_glob(),
                [
                    SimpleRawElement(str(root_path / "a1")),
                    SimpleRawElement(str(root_path / "a2")),
                ],
            )

    def test_process_raw_elements_glob(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            ensure_directories_exists(root_path, ["a1", "a2", "b"])
            self.assertListEqual(
                process_raw_elements([SimpleRawElement(str(root_path / "a?"))]),
                [
                    SimpleElement(str(root_path / "a1")),
                    SimpleElement(str(root_path / "a2")),
                ],
            )

    def test_process_raw_elements_existence(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            ensure_directories_exists(
                root_path, ["a", "b/c", "d/e/f", "venv/lib/pythonX/site-packages"]
            )

            self.assertListEqual(
                process_raw_elements(
                    [
                        SimpleRawElement(str(root_path / "a")),
                        SimpleRawElement(str(root_path / "x")),
                        SubdirectoryRawElement(
                            root=str(root_path / "b"), subdirectory="c"
                        ),
                        SubdirectoryRawElement(
                            root=str(root_path / "y"), subdirectory="z"
                        ),
                        SitePackageRawElement(
                            site_root=str(root_path / "d/e"), package_name="f"
                        ),
                        SitePackageRawElement(
                            site_root=str(root_path / "u/v"), package_name="w"
                        ),
                    ]
                ),
                [
                    SimpleElement(str(root_path / "a")),
                    SubdirectoryElement(root=str(root_path / "b"), subdirectory="c"),
                    SitePackageElement(
                        site_root=str(root_path / "d/e"), package_name="f"
                    ),
                ],
            )
