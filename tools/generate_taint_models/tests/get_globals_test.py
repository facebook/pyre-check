# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import os  # noqa
import textwrap
import unittest
from typing import IO, Any, Callable, Dict
from unittest.mock import call, mock_open, patch

from ..get_globals import GlobalModelGenerator, __name__ as get_globals_name
from ..model_generator import Configuration


def _open_implementation(path_to_content: Dict[str, str]) -> Callable[[str, str], Any]:
    def _open_implementation(path: str, mode: str) -> IO[Any]:
        if path in path_to_content:
            return mock_open(read_data=path_to_content[path]).return_value
        else:
            raise FileNotFoundError(path)

    return _open_implementation


class GetGlobalsTest(unittest.TestCase):
    @patch(
        "os.path.exists",
        side_effect=lambda path: path in {"/root/a.py", "/root/a.pyi", "/root/b.py"}
        or "/stub_root" in path,
    )
    @patch("os.path.abspath", side_effect=lambda path: path)
    @patch("os.getcwd", return_value="/root")
    def test_get_globals(
        self,
        current_working_directory: unittest.mock._patch,
        absolute_path: unittest.mock._patch,
        exists: unittest.mock._patch,
    ) -> None:
        Configuration.root = "/root"
        with patch(
            f"{get_globals_name}.GlobalModelGenerator._globals"
        ) as globals, patch("glob.glob", return_value=["/root/a.py", "/root/b.py"]):

            GlobalModelGenerator().compute_models([])
            globals.assert_has_calls(
                [call("/root", "/root/a.pyi"), call("/root", "/root/b.py")],
                any_order=True,
            )
        directory_mapping = {
            "/root/**/*.py": ["/root/a.py", "/root/b.py"],
            "/stub_root/**/*.pyi": ["/stub_root/a.pyi", "/stub_root/b.pyi"],
        }
        Configuration.stub_root = "/stub_root"
        with patch(
            f"{get_globals_name}.GlobalModelGenerator._globals"
        ) as globals, patch(
            "glob.glob", side_effect=lambda root, recursive: directory_mapping[root]
        ):
            GlobalModelGenerator().compute_models([])
            globals.assert_has_calls(
                [
                    call("/root", "/root/a.pyi"),
                    call("/root", "/root/b.py"),
                    call("/stub_root", "/stub_root/a.pyi"),
                    call("/stub_root", "/stub_root/b.pyi"),
                ],
                any_order=True,
            )

    @patch("builtins.open")
    def test_globals(self, open: unittest.mock._patch) -> None:
        # pyre-fixme[16]: `_patch` has no attribute `side_effect`.
        open.side_effect = _open_implementation(
            {
                "/root/module.py": textwrap.dedent(
                    """
                    A = 1
                    def function():
                      B = 2
                    if "version" is None:
                      C = 2
                    D, E = 1, 2
                    __all__ = {}
                    """
                )
            }
        )
        generator = GlobalModelGenerator()
        self.assertSetEqual(
            set(generator._globals("/root", "/root/module.py")),
            {
                "module.A: TaintSink[Global] = ...",
                "module.D: TaintSink[Global] = ...",
                "module.E: TaintSink[Global] = ...",
            },
        )
        open.side_effect = _open_implementation(
            {
                "/root/class.py": textwrap.dedent(
                    """
                    class Class:
                      F: typing.ClassVar[int] = ...
                      G: int = ...
                      class Nested:
                        H: typing.ClassVar[int] = ...
                    """
                )
            }
        )
        self.assertSetEqual(
            set(generator._globals("/root", "/root/class.py")),
            {
                "class.Class.F: TaintSink[Global] = ...",
                "class.Class.G: TaintSink[Global] = ...",
            }
        )
        open.side_effect = _open_implementation(
            {
                "/root/attributes.py": textwrap.dedent(
                    """
                    Z.X = 1
                    A, B.C, D = 1, 2, 3
                    [Y, Q.W] = [1, 2]
                    """
                )
            }
        )
        self.assertSetEqual(
            set(generator._globals("/root", "/root/attributes.py")),
            {
                "attributes.A: TaintSink[Global] = ...",
                "attributes.D: TaintSink[Global] = ...",
                "attributes.Y: TaintSink[Global] = ...",
            },
        )
        open.side_effect = _open_implementation(
            {
                "/root/namedtuples.py": textwrap.dedent(
                    """
                    from collections import namedtuple
                    x = collections.namedtuple()
                    y = namedtuple()
                    """
                )
            }
        )
        self.assertSetEqual(
            set(generator._globals("/root", "/root/namedtuples.py")), set()
        )
        open.side_effect = _open_implementation(
            {
                "/root/alias_assignments.py": textwrap.dedent(
                    """
                    x = a
                    y = b.c
                    """
                )
            }
        )
        self.assertSetEqual(
            set(generator._globals("/root", "/root/alias_assignments.py")), set()
        )
        open.side_effect = _open_implementation(
            {
                "/root/assignment_to_fields.py": textwrap.dedent(
                    """
                    x[1] = 123
                    y.field = 456
                    """
                )
            }
        )
        self.assertSetEqual(
            set(generator._globals("/root", "/root/assignment_to_fields.py")), set()
        )
        open.side_effect = _open_implementation(
            {
                "/root/annotated_assignments.py": textwrap.dedent(
                    """
                    x: int = 1
                    y: str  # this is ignored, as it might not exist in the runtime
                    z: Any = alias_that_we_skip
                    """
                )
            }
        )
        self.assertSetEqual(
            set(generator._globals("/root", "/root/annotated_assignments.py")),
            {"annotated_assignments.x: TaintSink[Global] = ..."},
        )
