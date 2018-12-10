#!/usr/bin/env python3

import ast
from pathlib import Path
from typing import Iterator, List, NamedTuple, Type

from tools.pyre.python_ast.pyre import PyreAst


class Error(NamedTuple):
    line: int
    column: int
    message: str
    type: Type


class AstChecker:
    name = "flake8-pyre-test-linter"
    version = "0.0.1"

    def __init__(
        self, tree: ast.Module, lines: List[str], repository: str, filename: str
    ) -> None:
        self.tree = PyreAst(repository).typed_ast(tree, filename)
        self.lines = lines
        self.filename = Path(filename).resolve()

    def run(self) -> Iterator[Error]:
        visitor = AstVisitor()
        visitor.visit(self.tree)
        for error in visitor.errors:
            yield error


class AstVisitor(ast.NodeVisitor):
    def __init__(self) -> None:
        self.errors = []  # type: List[Error]

    def _create_error(self, node, message) -> None:
        self.errors.append(Error(node.lineno, node.col_offset, message, AstChecker))

    def visit_Assign(self, node: ast.Assign) -> None:
        # pyre-fixme: ast.AST doesn't have attribute 'type'
        if node.targets[0].type == "int":
            # TODO(T37004997): Type should be fully qualified
            self._create_error(node, "Assigning to expression of type `int`.")
        self.generic_visit(node)
