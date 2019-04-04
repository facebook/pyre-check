# Copyright (c) 2019-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import ast
import os
from typing import Dict, NamedTuple

from .build_target import SUPPORTED_RULES, BuildTarget


BuildFile = NamedTuple(
    "BuildFile", [("path", str), ("targets", Dict[str, BuildTarget])]
)


class ParserException(Exception):
    pass


class Parser(object):
    def __init__(self, buck_root: str, build_file_name: str = "TARGETS") -> None:
        self.buck_root = buck_root
        self.build_file_name = build_file_name
        self.build_files = {}  # type: Dict[str, BuildFile]

    def parse_file(self, build_file_directory: str) -> BuildFile:
        if build_file_directory in self.build_files:
            return self.build_files[build_file_directory]

        build_file_path = self._build_file_path(build_file_directory)
        try:
            with open(build_file_path, "r") as build_file:
                file_contents = build_file.read()
                tree = ast.parse(file_contents)
        except FileNotFoundError:
            raise ParserException("No build file found at {}.".format(build_file_path))
        except SyntaxError as error:
            raise ParserException(
                "Could not parse build file at {}. Reason: {}".format(
                    build_file_path, error
                )
            )

        build_targets = self._parse_targets(tree, build_file_directory)
        build_file = BuildFile(build_file_directory, build_targets)
        self.build_files[build_file_directory] = build_file
        return build_file

    def _build_file_path(self, build_file_directory: str) -> str:
        return os.path.join(self.buck_root, build_file_directory, self.build_file_name)

    def _parse_targets(
        self, tree: ast.AST, build_file_directory: str
    ) -> Dict[str, BuildTarget]:
        assert isinstance(tree, ast.Module)
        expressions = tree.body

        targets = {}
        for expression in expressions:
            try:
                assert isinstance(expression, ast.Expr)
                call = expression.value
                assert isinstance(call, ast.Call)
                named = call.func
                assert isinstance(named, ast.Name)
                rule = named.id
                if rule in SUPPORTED_RULES:
                    target = SUPPORTED_RULES[rule].parse(call, build_file_directory)
                    targets[target.name] = target
            except (AssertionError, ValueError) as error:
                raise ParserException(
                    "Error occurred parsing targets in file {}, "
                    "at line {}, column {}: {}".format(
                        self._build_file_path(build_file_directory),
                        expression.lineno,
                        expression.col_offset,
                        error,
                    )
                )

        return targets
