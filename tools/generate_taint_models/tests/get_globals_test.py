# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import os  # noqa
import textwrap
import unittest
from typing import IO, Any, Callable, Dict, Iterable, Optional, Set
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

    def assert_module_has_global_models(
        self, source: str, expected: Iterable[str], blacklist: Optional[Set[str]] = None
    ) -> None:
        blacklist = blacklist or set()
        Configuration.blacklisted_globals = blacklist
        with patch("builtins.open") as open:
            open.side_effect = _open_implementation(
                {"/root/module.py": textwrap.dedent(source)}
            )
            generator = GlobalModelGenerator()
            self.assertSetEqual(
                set(generator._globals("/root", "/root/module.py")), set(expected)
            )

    @patch("builtins.open")
    def test_globals(self, open: unittest.mock._patch) -> None:
        self.assert_module_has_global_models(
            """
            A = 1
            def function():
              B = 2
            if "version" is None:
              C = 2
            D, E = 1, 2
            __all__ = {}
            """,
            {
                "module.A: TaintSink[Global] = ...",
                "module.D: TaintSink[Global] = ...",
                "module.E: TaintSink[Global] = ...",
            },
        )
        self.assert_module_has_global_models(
            """
            class Class:
              F: typing.ClassVar[int] = ...
              G: int = ...
              class Nested:
                H: typing.ClassVar[int] = ...
            """,
            {
                "module.Class.F: TaintSink[Global] = ...",
                "module.Class.G: TaintSink[Global] = ...",
            },
        )
        self.assert_module_has_global_models(
            """
            Z.X = 1
            A, B.C, D = 1, 2, 3
            [Y, Q.W] = [1, 2]
            """,
            {
                "module.A: TaintSink[Global] = ...",
                "module.D: TaintSink[Global] = ...",
                "module.Y: TaintSink[Global] = ...",
            },
        )
        self.assert_module_has_global_models(
            """
            from collections import namedtuple
            x = collections.namedtuple()
            y = namedtuple()
            """,
            set(),
        )
        self.assert_module_has_global_models(
            """
            x = a
            y = b.c
            """,
            set(),
        )
        self.assert_module_has_global_models(
            """
            x[1] = 123
            y.field = 456
            """,
            set(),
        )
        self.assert_module_has_global_models(
            """
            x: int = 1
            y: str  # this is ignored, as it might not exist in the runtime
            z: Any = alias_that_we_skip
            """,
            {"module.x: TaintSink[Global] = ..."},
        )
        self.assert_module_has_global_models(
            """
            A, B = 1
            class Class:
              C: typing.ClassVar[int] = ...
              D: int = ...
            """,
            expected={
                "module.B: TaintSink[Global] = ...",
                "module.Class.D: TaintSink[Global] = ...",
            },
            blacklist={"module.A", "module.Class.C"},
        )
        self.assert_module_has_global_models(
            """
            from dataclasses import dataclass
            @dataclass
            class Class:
              C: int = ...
              D: int = ...
            @dataclass(frozen=True)
            class Frozen:
              C: int = ...
              D: int = ...
            """,
            set(),
        )
        self.assert_module_has_global_models(
            """
            import dataclasses
            @dataclasses.dataclass
            class Class:
              C: int = ...
              D: int = ...
            @dataclasses.dataclass(frozen=True)
            class Frozen:
              C: int = ...
              D: int = ...
            """,
            set(),
        )
        self.assert_module_has_global_models(
            """
            class MyClass:
              C: int = ...
              D: int = ...
              def __init__(self):
                self.C = 1
            """,
            {"module.MyClass.D: TaintSink[Global] = ..."},
        )
        # We ignore ClassVar for now.
        self.assert_module_has_global_models(
            """
            class MyClass:
              C: ClassVar[int] = ...
              def __init__(self):
                self.C = 1
            """,
            set(),
        )
        # Any attribute accessed in a method is considered to be an instance variable.
        self.assert_module_has_global_models(
            """
            class MyClass:
              C: ClassVar[int] = ...
              def foo(self):
                self.C = 1
            """,
            set(),
        )
        self.assert_module_has_global_models(
            """
            class MyClass:
              C: int = ...
              def __init__(self):
                self.C = 1
            class SubClass(MyClass):
              C: int = ...
            """,
            {"module.SubClass.C: TaintSink[Global] = ..."},
        )
