# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import tempfile
from pathlib import Path

import testslide

from ...tests.setup import ensure_directories_exists
from ..exceptions import InvalidConfiguration
from ..search_path import (
    create_raw_element,
    process_raw_elements,
    SimpleElement,
    SimpleRawElement,
    SitePackageElement,
    SitePackageRawElement,
    SubdirectoryElement,
    SubdirectoryRawElement,
)


class SearchPathTest(testslide.TestCase):
    def test_create_raw_element(self) -> None:
        self.assertEqual(create_raw_element("foo"), SimpleRawElement("foo"))
        self.assertEqual(
            create_raw_element({"root": "foo", "subdirectory": "bar"}),
            SubdirectoryRawElement("foo", "bar"),
        )
        self.assertEqual(
            create_raw_element({"import_root": "foo", "source": "bar"}),
            SubdirectoryRawElement("foo", "bar"),
        )
        self.assertEqual(
            create_raw_element({"site-package": "foo"}),
            SitePackageRawElement("foo"),
        )
        self.assertEqual(
            create_raw_element({"site-package": "foo"}),
            SitePackageRawElement("foo"),
        )
        self.assertEqual(
            create_raw_element(
                {"site-package": "foo", "is_toplevel_module": True},
            ),
            SitePackageRawElement("foo", True),
        )

        with self.assertRaises(InvalidConfiguration):
            create_raw_element({})
        with self.assertRaises(InvalidConfiguration):
            create_raw_element({"foo": "bar"})
        with self.assertRaises(InvalidConfiguration):
            create_raw_element({"root": "foo"})
        with self.assertRaises(InvalidConfiguration):
            create_raw_element({"root": 42, "subdirectory": "bar"})
        with self.assertRaises(InvalidConfiguration):
            create_raw_element({"root": "foo", "subdirectory": []})
        with self.assertRaises(InvalidConfiguration):
            create_raw_element({"import_root": 4.2, "source": "bar"})
        with self.assertRaises(InvalidConfiguration):
            create_raw_element({"import_root": "foo", "source": False})
        with self.assertRaises(InvalidConfiguration):
            create_raw_element({"site-package": {}})
        with self.assertRaises(InvalidConfiguration):
            create_raw_element(
                {"site-package": "foo", "is_toplevel_module": "derp"},
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
            SitePackageRawElement("package").expand_global_root("root"),
            SitePackageRawElement("package"),
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
            SitePackageRawElement("package").expand_relative_root("root/local_project"),
            SitePackageRawElement("package"),
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
                process_raw_elements(
                    [SimpleRawElement(str(root_path / "a?"))], site_roots=[]
                ),
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
                        SitePackageRawElement(package_name="f"),
                        SitePackageRawElement(package_name="w"),
                    ],
                    site_roots=[str(root_path / "d/e"), str(root_path / "u/v")],
                ),
                [
                    SimpleElement(str(root_path / "a")),
                    SubdirectoryElement(root=str(root_path / "b"), subdirectory="c"),
                    SitePackageElement(
                        site_root=str(root_path / "d/e"), package_name="f"
                    ),
                ],
            )

    def test_process_raw_elements_site_root_priority(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            ensure_directories_exists(root_path, ["system/foo", "user/foo", "derp"])

            self.assertListEqual(
                process_raw_elements(
                    [
                        SitePackageRawElement(package_name="foo"),
                    ],
                    site_roots=[
                        str(root_path / "derp"),
                        str(root_path / "user"),
                        str(root_path / "system"),
                    ],
                ),
                [
                    SitePackageElement(
                        site_root=str(root_path / "user"), package_name="foo"
                    ),
                ],
            )
