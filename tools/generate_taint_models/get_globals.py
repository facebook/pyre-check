# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict


import ast
import glob
import logging
import os
from typing import Callable, Iterable, Optional, Set

from .model import AssignmentModel
from .model_generator import Configuration, ModelGenerator, Registry, qualifier


LOG: logging.Logger = logging.getLogger(__name__)


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


def _get_self_attribute(target: ast.expr) -> Optional[str]:
    if isinstance(target, ast.Attribute):
        value = target.value
        if isinstance(value, ast.Name) and value.id == "self":
            return target.attr
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
                self.blacklist: Optional[Set[str]] = None
                self.parent: Optional[str] = None

            def visit_Name(self, name: ast.Name) -> None:
                blacklist = self.blacklist
                if blacklist is not None and name.id in blacklist:
                    return
                parent = self.parent
                if parent is not None:
                    name_to_register = f"{parent}.{name.id}"
                else:
                    name_to_register = name.id
                self.globals.add(name_to_register)

            # Ensure that we stop recursing when we're in a complex assign, such as
            # a.b = ... or a[b] = ... .
            def visit_Attribute(self, attribute: ast.Attribute) -> None:
                return

            def visit_Subscript(self, subscript: ast.Subscript) -> None:
                return

        visitor = NameVisitor(globals)

        def visit_assignment(target: ast.expr, value: ast.expr) -> None:
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

        def should_visit_class(class_definition: ast.ClassDef) -> bool:
            # Ensure that we don't visit nested classes for now.
            if visitor.parent is not None:
                return False

            def is_dataclass_decorator(expression: ast.expr) -> bool:
                if isinstance(expression, ast.Call):
                    # pyre-ignore[18]: Not handling nested recursive functions.
                    return is_dataclass_decorator(expression.func)
                if isinstance(expression, ast.Name):
                    return expression.id == "dataclass"
                if isinstance(expression, ast.Attribute):
                    base = expression.value
                    if isinstance(base, ast.Name) and base.id == "dataclasses":
                        return expression.attr == "dataclass"
                return False

            for decorator in class_definition.decorator_list:
                # Skip visiting dataclasses, as they use class variables to generate
                # instance variables. They can have one of the following forms:
                # @dataclass(args), @dataclass, or `@dataclasses.dataclass(args)`.
                if is_dataclass_decorator(decorator):
                    return False
            return True

        def all_attributes(class_definition: ast.ClassDef) -> Set[str]:
            attributes = set()
            for statement in class_definition.body:
                if not isinstance(statement, ast.FunctionDef):
                    continue
                for assignment in statement.body:
                    if isinstance(assignment, ast.Assign):
                        for target in assignment.targets:
                            attribute = _get_self_attribute(target)
                            if attribute is not None:
                                attributes.add(attribute)
                    elif isinstance(assignment, ast.AnnAssign):
                        attribute = _get_self_attribute(assignment.target)
                        if attribute is not None:
                            attributes.add(attribute)
            return attributes

        def visit_statement(statement: ast.stmt) -> None:
            if isinstance(statement, ast.Assign):
                # Omit pure aliases of the form `x = alias`.
                for target in statement.targets:
                    visit_assignment(target, statement.value)
            elif isinstance(statement, ast.AugAssign):
                visitor.visit(statement.target)
            # Don't attempt to register statements of the form `x: int`.
            elif isinstance(statement, ast.AnnAssign) and statement.value is not None:
                visit_assignment(statement.target, statement.value)
            elif isinstance(statement, ast.ClassDef) and should_visit_class(statement):
                visitor.parent = statement.name
                visitor.blacklist = all_attributes(statement)

                for toplevel_statement in statement.body:
                    # pyre-ignore: T46622677
                    visit_statement(toplevel_statement)
                visitor.parent = None
                visitor.blacklist = None

        for statement in module.body:
            visit_statement(statement)

        module_qualifier = qualifier(root, path)

        models = set()
        for target in globals:
            if target == "__all__":
                continue
            qualified_target = f"{module_qualifier}.{target}"
            if qualified_target in Configuration.blacklisted_globals:
                continue
            generated = AssignmentModel(
                annotation="TaintSink[Global]", target=qualified_target
            ).generate()
            if generated is not None:
                models.add(generated)
        return models

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
