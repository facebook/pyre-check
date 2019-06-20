# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import textwrap
import unittest
from typing import IO, Any, Callable, Dict
from unittest.mock import MagicMock, call, mock_open, patch

from .. import generate_taint_models


# pyre-ignore: typing stubs is hard.
def _open_implementation(path_to_content: Dict[str, str]) -> Callable[[str, str], Any]:
    # pyre-ignore: that's just how open works
    def _open_implementation(path: str, mode: str) -> IO[Any]:
        if path in path_to_content:
            return mock_open(read_data=path_to_content[path]).return_value
        else:
            raise FileNotFoundError(path)

    return _open_implementation


class GenerateTaintModelsTest(unittest.TestCase):
    @patch("builtins.open")
    @patch("os.path.exists", return_value=True)
    def test_visit_views(
        self, os_path_exists: unittest.mock._patch, open: unittest.mock._patch
    ) -> None:
        arguments = MagicMock()
        arguments.as_view_base = []

        # Simple `url()` call.
        with patch(
            "tools.pyre.tools.generate_taint_models.generate_taint_models."
            "_load_function_definition"
        ) as load_function_definition:
            open.side_effect = _open_implementation(
                {
                    "urls.py": textwrap.dedent(
                        """
                        async def local(request): pass

                        url(r"^p-ng/?$", "module.views.function")
                        url(r"^p-ng/?$", module.views.imported)
                        url(r"^p-ng/?$", local)
                        """
                    )
                }
            )
            generate_taint_models._visit_views(arguments, "urls.py", lambda *_: None)
            load_function_definition.assert_has_calls(
                [
                    call(arguments, "module.views.function"),
                    call(arguments, "module.views.imported"),
                ],
                any_order=True,
            )

        # Simple `url()` call to method.
        with patch(
            "tools.pyre.tools.generate_taint_models.generate_taint_models."
            "_load_function_definition"
        ) as load_function_definition:
            open.side_effect = _open_implementation(
                {
                    "urls.py": textwrap.dedent(
                        """
                        from module.view import Class
                        url(r"^p-ng/?$", Class.method)
                        """
                    )
                }
            )
            generate_taint_models._visit_views(arguments, "urls.py", lambda *_: None)
            load_function_definition.assert_called_once_with(
                arguments, "module.view.Class.method"
            )

        # Multiple `url()` calls.
        with patch(
            "tools.pyre.tools.generate_taint_models.generate_taint_models."
            "_load_function_definition"
        ) as load_function_definition:
            open.side_effect = _open_implementation(
                {
                    "urls.py": textwrap.dedent(
                        """
                        url(r"^p-ng/?$", "some.view")
                        url(r"^p-ng/?$", "some.other.view")
                        """
                    )
                }
            )
            generate_taint_models._visit_views(arguments, "urls.py", lambda *_: None)
            load_function_definition.assert_has_calls(
                [call(arguments, "some.view"), call(arguments, "some.other.view")],
                any_order=True,
            )

        # Simple `include()` call.
        with patch(
            "tools.pyre.tools.generate_taint_models.generate_taint_models."
            "_load_function_definition"
        ) as load_function_definition:
            open.side_effect = _open_implementation(
                {
                    "urls.py": """url(r"derp", include("indirect.urls"))""",
                    "indirect/urls.py": textwrap.dedent(
                        """
                        url(r"^p-ng/?$", "indirect.view")
                        """
                    ),
                }
            )
            generate_taint_models._visit_views(arguments, "urls.py", lambda *_: None)
            load_function_definition.assert_called_once_with(arguments, "indirect.view")

        # Patterns.
        with patch(
            "tools.pyre.tools.generate_taint_models.generate_taint_models."
            "_load_function_definition"
        ) as load_function_definition:
            open.side_effect = _open_implementation(
                {
                    "urls.py": textwrap.dedent(
                        """
                        from module import Class
                        patterns(
                            "base",
                            (r"derp", "first_view"),
                            (r"derp", "second_view", { "extra": "information"}),
                            (r"derp", Class.method),
                            (r"derp", include("indirect.urls")),
                            url(r"derp", "from_url")
                        )
                        patterns("", (r"derp", "absolute.module.path"))
                        """
                    ),
                    "indirect/urls.py": textwrap.dedent(
                        """
                        url(r"derp", "indirect.view.function")
                        """
                    ),
                }
            )
            generate_taint_models._visit_views(arguments, "urls.py", lambda *_: None)
            load_function_definition.assert_has_calls(
                [
                    call(arguments, "base.first_view"),
                    call(arguments, "base.second_view"),
                    call(arguments, "absolute.module.path"),
                    call(arguments, "indirect.view.function"),
                    call(arguments, "base.from_url"),
                ],
                any_order=True,
            )

    @patch("builtins.open")
    @patch("os.path.exists", return_value=True)
    def test_load_function_definition(
        self, os_path_exists: unittest.mock._patch, open: unittest.mock._patch
    ) -> None:
        arguments = MagicMock()
        arguments.as_view_base = ["BaseView"]

        def _os_path_exists_implementation(path: str) -> bool:
            return path in ["urls.py", "module.py"]

        os_path_exists.side_effect = _os_path_exists_implementation
        open.side_effect = _open_implementation(
            {
                "module.py": textwrap.dedent(
                    """
                    A = 1
                    def function():
                        pass
                    async def async_function():
                        pass
                    def nesting():
                        def nested():
                            pass
                    class Class:
                        def method(self):
                            pass
                    class View(BaseView):
                        pass
                    """
                )
            }
        )
        definition = generate_taint_models._load_function_definition(
            arguments, "module.unknown"
        )
        self.assertIsNone(definition)

        definition = generate_taint_models._load_function_definition(
            arguments, "module.function"
        )
        self.assertIsNotNone(definition)
        self.assertEqual(definition.name, "function")

        definition = generate_taint_models._load_function_definition(
            arguments, "module.async_function"
        )
        self.assertIsNotNone(definition)
        self.assertEqual(definition.name, "async_function")

        definition = generate_taint_models._load_function_definition(
            arguments, "module.nesting.nested"
        )
        self.assertIsNone(definition)

        definition = generate_taint_models._load_function_definition(
            arguments, "module.Class.method"
        )
        self.assertIsNotNone(definition)
        self.assertEqual(definition.name, "method")

        definition = generate_taint_models._load_function_definition(
            arguments, "module.View.as_view"
        )
        self.assertIsNotNone(definition)
        self.assertEqual(definition.name, "as_view")

        definition = generate_taint_models._load_function_definition(
            arguments, "module.View.other"
        )
        self.assertIsNone(definition)

    @patch("builtins.open")
    @patch("os.path.exists", return_value=True)
    def test_get_exit_nodes(
        self, os_path_exists: unittest.mock._patch, open: unittest.mock._patch
    ) -> None:
        open.side_effect = _open_implementation(
            {
                "urls.py": """url(r"derp", "module.view.function")""",
                "module/view.py": textwrap.dedent(
                    """
                    def function(request: HttpRequest, **kwargs) -> HttpResponse:
                        pass
                    def unrelated() -> None: pass
                """
                ),
            }
        )
        arguments = MagicMock()
        arguments.urls_path = "urls.py"
        models = generate_taint_models._get_exit_nodes(arguments)
        self.assertSetEqual(
            models,
            {
                "def module.view.function(request, **kwargs)"
                " -> TaintSink[ReturnedToUser]: ..."
            },
        )

    @patch("builtins.open")
    @patch("os.path.exists", return_value=True)
    def test_get_REST_api_sources(
        self, os_path_exists: unittest.mock._patch, open: unittest.mock._patch
    ) -> None:
        open.side_effect = _open_implementation(
            {
                "urls.py": textwrap.dedent(
                    """
                    url(r"derp", "module.view.function")
                    url(r"derp", "module.view.unannotated")
                    """
                ),
                "module/view.py": textwrap.dedent(
                    """
                    def function(request: derp.HttpRequest, other: int) -> HttpResponse:
                        pass
                    def unannotated(request, other, *starred):
                        pass
                    def unrelated() -> None: pass
                """
                ),
            }
        )
        arguments = MagicMock()
        arguments.urls_path = "urls.py"
        arguments.whitelisted_class = ["HttpRequest"]
        models = generate_taint_models._get_REST_api_sources(arguments)
        self.assertSetEqual(
            models,
            {
                "def module.view.function(request, other: "
                "TaintSource[UserControlled]): ...",
                "def module.view.unannotated("
                "request: TaintSource[UserControlled], "
                "other: TaintSource[UserControlled], *starred): ...",
            },
        )

    def test_qualifier(self) -> None:
        self.assertEqual(generate_taint_models._qualifier("/root", "/root"), ".")
        self.assertEqual(generate_taint_models._qualifier("/root", "/root/a.py"), "a")
        self.assertEqual(
            generate_taint_models._qualifier("/root", "/root/dir/a.py"), "dir.a"
        )
        self.assertEqual(
            generate_taint_models._qualifier("/root", "/root/dir/__init__.py"), "dir"
        )
        self.assertEqual(generate_taint_models._qualifier("/root", "/root/a.pyi"), "a")
        self.assertEqual(
            generate_taint_models._qualifier("/root", "/root/dir/a.pyi"), "dir.a"
        )
        self.assertEqual(
            generate_taint_models._qualifier("/root", "/root/dir/__init__.pyi"), "dir"
        )

    @patch("builtins.open")
    def test_globals(self, open: unittest.mock._patch) -> None:
        open.side_effect = _open_implementation(
            {
                "/root/module.py": textwrap.dedent(
                    """
                    A = 1
                    def function():
                      B = 2
                    if "version" is None:
                      C = 2
                    __all__ = {}
                    D, E = 1, 2
                    class Class:
                      F: typing.ClassVar[int] = ...
                    """
                )
            }
        )
        self.assertSetEqual(
            generate_taint_models._globals("/root", "/root/module.py"),
            {
                "module.A: TaintSink[Global] = ...",
                "module.D: TaintSink[Global] = ...",
                "module.E: TaintSink[Global] = ...",
            },
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
            generate_taint_models._globals("/root", "/root/attributes.py"),
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
            generate_taint_models._globals("/root", "/root/namedtuples.py"), set()
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
            generate_taint_models._globals("/root", "/root/alias_assignments.py"), set()
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
            generate_taint_models._globals("/root", "/root/assignment_to_fields.py"),
            set(),
        )

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
        with patch(f"{generate_taint_models.__name__}._globals") as globals, patch(
            "glob.glob", return_value=["/root/a.py", "/root/b.py"]
        ):
            generate_taint_models._get_globals(MagicMock())
            globals.assert_has_calls(
                [call("/root", "/root/a.pyi"), call("/root", "/root/b.py")],
                any_order=True,
            )
        directory_mapping = {
            "/root/**/*.py": ["/root/a.py", "/root/b.py"],
            "/stub_root/**/*.pyi": ["/stub_root/a.pyi", "/stub_root/b.pyi"],
        }
        with patch(f"{generate_taint_models.__name__}._globals") as globals, patch(
            "glob.glob", side_effect=lambda root, recursive: directory_mapping[root]
        ):
            mock = MagicMock()
            mock.stub_root = "/stub_root"
            generate_taint_models._get_globals(mock)
            globals.assert_has_calls(
                [
                    call("/root", "/root/a.pyi"),
                    call("/root", "/root/b.py"),
                    call("/stub_root", "/stub_root/a.pyi"),
                    call("/stub_root", "/stub_root/b.pyi"),
                ],
                any_order=True,
            )
