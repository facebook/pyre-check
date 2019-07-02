# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict


import ast
import glob
import os
from typing import Callable, Iterable, Optional, Set

from .model_generator import ModelGenerator, load_module, qualifier


class GlobalModelGenerator(ModelGenerator):
    def __init__(self, root: str, stub_root: Optional[str]) -> None:
        self.root = root
        self.stub_root = stub_root

    def _globals(self, root: str, path: str) -> Iterable[str]:
        globals: Set[str] = set()

        module = load_module(path)

        if not module:
            return globals

        class NameVisitor(ast.NodeVisitor):
            def __init__(self, globals: Set[str]) -> None:
                self.globals = globals

            def visit_Name(self, name: ast.Name) -> None:
                self.globals.add(name.id)

            # Ensure that we stop recursing when we're in a complex assign, such as
            # a.b = ... or a[b] = ... .
            def visit_Attribute(self, attribute: ast.Attribute) -> None:
                return

            def visit_Subscript(self, subscript: ast.Subscript) -> None:
                return

        visitor = NameVisitor(globals)

        def visit_assignment(target: ast.expr, value: Optional[ast.expr]) -> None:
            if value is not None:
                # namedtuples get preprocessed out by Pyre, and shouldn't be added
                # as globals.
                if isinstance(value, ast.Call):
                    callee = value.func
                    if (
                        isinstance(callee, ast.Attribute)
                        and callee.attr == "namedtuple"
                    ):
                        return
                    if isinstance(callee, ast.Name) and callee.id == "namedtuple":
                        return
                # Omit pure aliases of the form `x = alias`.
                if isinstance(value, ast.Name) or isinstance(value, ast.Attribute):
                    return
            visitor.visit(target)

        for statement in module.body:
            if isinstance(statement, ast.Assign):
                # Omit pure aliases of the form `x = alias`.
                for target in statement.targets:
                    visit_assignment(target, statement.value)
            elif isinstance(statement, ast.AugAssign):
                visitor.visit(statement.target)
            elif isinstance(statement, ast.AnnAssign):
                visit_assignment(statement.target, statement.value)

        return {
            f"{qualifier(root, path)}.{target}: TaintSink[Global] = ..."
            for target in globals
            if target != "__all__"
        }

    def compute_models(
        self, functions_to_model: Iterable[Callable[..., None]]
    ) -> Iterable[str]:
        sinks: Set[str] = set()

        paths = [path for path in glob.glob(self.root + "/**/*.py", recursive=True)]
        for path in paths:
            # Stubs take precedence if both module.py and module.pyi exist.
            stub_path = f"{path}i"
            if os.path.exists(stub_path):
                path = stub_path
            sinks = sinks.union(self._globals(self.root, path))
        stub_root = self.stub_root
        if stub_root:
            stub_root = os.path.abspath(stub_root)
            paths = [
                path for path in glob.glob(stub_root + "/**/*.pyi", recursive=True)
            ]
            for path in paths:
                sinks = sinks.union(self._globals(stub_root, path))
        return sinks
