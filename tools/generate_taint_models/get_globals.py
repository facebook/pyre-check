# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict


import ast
import functools
import glob
import logging
import os
from typing import Callable, Iterable, Optional, Set

from .model_generator import Configuration, ModelGenerator, Registry, qualifier


LOG: logging.Logger = logging.getLogger(__name__)


@functools.lru_cache(maxsize=1024)
def _load_module(module_path: str) -> Optional[ast.Module]:
    try:
        with open(module_path, "r") as file:
            parsed = ast.parse(file.read())
            if not isinstance(parsed, ast.Module):
                return None
            return parsed
    except (FileNotFoundError, SyntaxError) as error:
        LOG.warning(f"Could not load `{module_path}`: {str(error)}")
    return None


class GlobalModelGenerator(ModelGenerator):
    def _globals(self, root: str, path: str) -> Iterable[str]:
        globals: Set[str] = set()

        module = _load_module(path)

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

    def gather_functions_to_model(self) -> Iterable[Callable[..., object]]:
        return []

    def compute_models(
        self, functions_to_model: Iterable[Callable[..., None]]
    ) -> Iterable[str]:
        sinks: Set[str] = set()

        paths = [
            path for path in glob.glob(Configuration.root + "/**/*.py", recursive=True)
        ]
        for path in paths:
            # Stubs take precedence if both module.py and module.pyi exist.
            stub_path = f"{path}i"
            if os.path.exists(stub_path):
                path = stub_path
            sinks = sinks.union(self._globals(Configuration.root, path))
        stub_root = Configuration.stub_root
        if stub_root:
            stub_root = os.path.abspath(stub_root)
            paths = [
                path for path in glob.glob(stub_root + "/**/*.pyi", recursive=True)
            ]
            for path in paths:
                sinks = sinks.union(self._globals(stub_root, path))
        return sinks


Registry.register("get_globals", GlobalModelGenerator, include_by_default=False)
