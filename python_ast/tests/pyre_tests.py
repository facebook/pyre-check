#!/usr/bin/env python
# Copyright 2004-present Facebook. All Rights Reserved.

import ast
import os
import unittest
from tempfile import NamedTemporaryFile
from typing import Any, List, Mapping, Optional, Tuple  # noqa
from unittest.mock import Mock, patch

from tools.pyre.python_ast.pyre import (
    Annotation,
    Location,
    PyreAst,
    PyreServerException,
)


class AddTypeAttribute(ast.NodeTransformer):
    # pyre-fixme: Overridden generic_visit has return type of `None`
    def generic_visit(self, node: ast.AST) -> ast.AST:
        if hasattr(node, "lineno") and hasattr(node, "col_offset"):
            setattr(node, "_attributes", node._attributes + ("type",))
        super(AddTypeAttribute, self).generic_visit(node)
        return node


class TypedAstTestCase(unittest.TestCase):
    """Test tools.pyre.python_ast.pyre.PyreAST functionality"""

    @patch.object(PyreAst, "_initialize_server")
    @patch("os.path.abspath", side_effect=lambda x: x)
    @patch("os.path.isfile")
    def test_init(self, isfile, _abspath, _server) -> None:
        def assert_init(
            valid_configurations: List[str],
            valid_local_configurations: List[str],
            path: str,
            expected_project_root: Optional[str],
            expected_local_root: Optional[str],
        ) -> None:
            valid_files = [
                os.path.join(file, ".pyre_configuration")
                for file in valid_configurations
            ]
            valid_local_files = [
                os.path.join(file, ".pyre_configuration.local")
                for file in valid_local_configurations
            ]
            isfile.side_effect = lambda x: x in (valid_files + valid_local_files)
            if not expected_local_root or not expected_project_root:
                with self.assertRaises(PyreServerException):
                    pyre_ast = PyreAst(path)
            else:
                pyre_ast = PyreAst(path)
                self.assertEqual(pyre_ast._project_configuration, expected_project_root)
                self.assertEqual(pyre_ast._local_configuration, expected_local_root)

        assert_init(["/root"], [], "/root", "/root", "/root")
        assert_init(["/root"], ["/root/a"], "/root/a", "/root", "/root/a")
        assert_init([], [], "/root", None, None)

    def create_file_types(
        self, annotation_list: List[Tuple[int, int, int, int, str]]
    ) -> Mapping[Location, Annotation]:
        def add_file_type(
            file_types: Mapping[Location, Annotation],
            start_line: int,
            start_column: int,
            end_line: int,
            end_column: int,
            annotation: str,
        ) -> Mapping[Location, Annotation]:
            start_location = Location(start_line, start_column)
            end_location = Location(end_line, end_column)
            annotation_object = Annotation(start_location, end_location, annotation)
            file_types[start_location] = annotation_object
            return file_types

        file_types: Mapping[Location, Annotation] = {}
        for annotation in annotation_list:
            # pyre-ignore: Unpacking tuple issue
            file_types = add_file_type(file_types, *annotation)
        return file_types

    def print_file_types(self, file_types: Mapping[Location, Annotation]) -> str:
        file_types_string = ""
        for location in file_types.keys():
            file_types_string = (
                file_types_string
                + str(location)
                + ": "
                + str(file_types[location])
                + "; "
            )
        return file_types_string

    @patch.object(PyreAst, "__init__", return_value=None)
    @patch("subprocess.Popen")
    def test_query_file_types(self, subprocess_popen, _init) -> None:
        def assert_file_types(
            json_response: str, expected_file_types: Mapping[Location, Annotation]
        ) -> None:
            pyre_ast = PyreAst("/config_root")
            pyre_ast._local_configuration = "/config_root"
            # Configure process stdout file
            process_mock = Mock()
            mock_stdout = json_response.encode("utf-8")
            process_mock.communicate.return_value = (mock_stdout, "")
            subprocess_popen.return_value = process_mock
            # Check query output
            file_types = pyre_ast._query_file_types("/config_root/file.py")
            message = "Expected: %s\nGot: %s" % (
                self.print_file_types(expected_file_types),
                self.print_file_types(file_types),
            )
            self.assertEqual(expected_file_types, file_types, message)

        assert_file_types('{"response": {"types": {}}}', self.create_file_types([]))
        assert_file_types(
            '{"response":{"types":[{'
            '"location":{"path":"file.py",'
            '"start":{"line":90,"column":27},'
            '"stop":{"line":90,"column":29}},'
            '"annotation":"typing.List[]"},'
            '{"location":{"path":"file.py",'
            '"start":{"line":31,"column":30},'
            '"stop":{"line":31,"column":39}},'
            '"annotation":"typing.Any"}]}}',
            self.create_file_types(
                [(90, 27, 90, 29, "typing.List[]"), (31, 30, 31, 39, "typing.Any")]
            ),
        )
        assert_file_types(
            '{"response":{"types":[{'
            '"location":{"path":"file.py",'
            '"start":{"line":1,"column":1},'
            '"stop":{"line":1,"column":1}},'
            '"annotation":"typing.List[]"},'
            '{"location":{"path":"file.py",'
            '"start":{"line":1,"column":1},'
            '"stop":{"line":1,"column":1}},'
            '"annotation":"typing.Any"}]}}',
            self.create_file_types([(1, 1, 1, 1, "typing.Any")]),
        )

    @patch.object(PyreAst, "__init__", return_value=None)
    @patch("os.path.abspath", side_effect=lambda path: path)
    @patch("os.path.relpath", side_effect=lambda dir, path: path)
    def test_typed_ast(self, _relpath, _abspath, _init) -> None:
        def assert_typed_ast(
            file_types: Mapping[Location, Annotation],
            existing_ast: ast.AST,
            expected_ast: ast.AST,
        ) -> None:
            pyre_ast = PyreAst("/config_root")
            pyre_ast._local_configuration = "/config_root"
            pyre_ast._project_configuration = "/config_root"
            expected_ast: ast.AST = AddTypeAttribute().visit(expected_ast)
            with patch.object(PyreAst, "_query_file_types", return_value=file_types):
                typed_ast = pyre_ast.typed_ast(existing_ast, "/config_root/file.py")
                self.maxDiff = None
                self.assertEqual(
                    ast.dump(expected_ast, include_attributes=True),
                    ast.dump(typed_ast, include_attributes=True),
                )

        file_types = self.create_file_types([])
        existing_ast = ast.parse("")
        expected_ast = ast.Module(body=[])
        assert_typed_ast(file_types, existing_ast, expected_ast)

        file_types = self.create_file_types([(1, 0, 1, 1, "str")])
        existing_ast = ast.parse("x = 5")
        expected_ast = ast.Module(
            body=[
                ast.Assign(
                    targets=[
                        ast.Name(
                            id=str("x"),
                            ctx=ast.Store(),
                            lineno=1,
                            col_offset=0,
                            type="str",
                        )
                    ],
                    value=ast.Num(n=5, lineno=1, col_offset=4, type=None),
                    lineno=1,
                    col_offset=0,
                    type="str",
                )
            ]
        )
        assert_typed_ast(file_types, existing_ast, expected_ast)
