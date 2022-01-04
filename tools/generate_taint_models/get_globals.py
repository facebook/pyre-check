# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict


import ast
import glob
import logging
import os
from typing import Callable, Iterable, Optional, Set, Tuple, Union

from typing_extensions import Final

from .model import AssignmentModel, FunctionDefinitionModel, Model
from .model_generator import ModelGenerator, qualifier
from .module_loader import find_all_paths, load_module


LOG: logging.Logger = logging.getLogger(__name__)
FunctionDefinition = Union[ast.FunctionDef, ast.AsyncFunctionDef]


class GlobalModelGenerator(ModelGenerator[Model]):
    def __init__(
        self,
        root: str,
        stub_root: Optional[str] = None,
        blacklisted_globals: Optional[Set[str]] = None,
        blacklisted_global_directories: Optional[Set[str]] = None,
    ) -> None:
        self.root: str = root
        self.stub_root: Final[Optional[str]] = stub_root
        self.blacklisted_globals: Set[str] = blacklisted_globals or set()
        self.blacklisted_global_directories: Set[str] = (
            blacklisted_global_directories or set()
        )

    # flake8 suggests to reduce the complexity of the function, hence the noqa line
    def _globals(self, root: str, path: str) -> Iterable[Model]:  # noqa: C901
        globals = set()
        # The parent of the property needs to be stored as well, as we only store the
        # module qualifier.
        cached_properties: Set[Tuple[Optional[str], FunctionDefinition]] = set()

        module = load_module(path)

        if not module:
            return globals

        class NameVisitor(ast.NodeVisitor):
            def __init__(self, globals: Set) -> None:
                self.globals = globals
                self.blacklist: Optional[Set[str]] = None
                self.parent: Optional[str] = None

            def visit_Name(self, name: ast.Name) -> None:
                blacklist = self.blacklist
                if blacklist is not None and name.id in blacklist:
                    return
                parent = self.parent
                if parent is not None:
                    name_to_register = f"{parent}.__class__.{name.id}"
                else:
                    name_to_register = name.id
                self.globals.add(name_to_register)

            # Ensure that we stop recursing when we're in a complex assign, such as
            # a.b = ... or a[b] = ... .
            def visit_Attribute(self, attribute: ast.Attribute) -> None:
                return

            def visit_Subscript(self, subscript: ast.Subscript) -> None:
                return

        visitor: NameVisitor = NameVisitor(globals)

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
                # x = lambda: _ can safely be avoided, as the models confuse our taint
                # analysis.
                if isinstance(value, ast.Lambda):
                    return
            visitor.visit(target)

        def should_visit_class(class_definition: ast.ClassDef) -> bool:
            # Ensure that we don't visit nested classes for now.
            if visitor.parent is not None:
                return False

            # TypedDicts use top-level attribute declarations to declare attributes.
            for base in class_definition.bases:
                base_name = None
                if isinstance(base, ast.Name):
                    base_name = base.id
                if isinstance(base, ast.Attribute):
                    base_name = base.attr
                if base_name == "TypedDict":
                    return False

            def is_dataclass_decorator(expression: ast.expr) -> bool:
                if isinstance(expression, ast.Call):
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
            elif isinstance(statement, ast.AnnAssign):
                value = statement.value
                if value is not None:
                    visit_assignment(statement.target, value)

            elif isinstance(statement, ast.FunctionDef) or isinstance(
                statement, ast.AsyncFunctionDef
            ):
                for decorator in statement.decorator_list:
                    if _is_cached_property_decorator(decorator):
                        cached_properties.add((visitor.parent, statement))

            elif isinstance(statement, ast.ClassDef) and should_visit_class(statement):
                visitor.parent = statement.name
                visitor.blacklist = all_attributes(statement)

                for toplevel_statement in statement.body:
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
            if qualified_target in self.blacklisted_globals:
                continue
            try:
                generated = AssignmentModel(
                    annotation="TaintSink[Global]", target=qualified_target
                )
                models.add(generated)
            except ValueError:
                pass

        for (parent, function_definition) in cached_properties:
            is_class_property = any(
                (
                    _is_class_property_decorator(decorator)
                    for decorator in function_definition.decorator_list
                )
            )
            if is_class_property:
                returns = "TaintSink[Global, Via[cached_class_property]]"
            else:
                returns = "TaintSink[Global, Via[cached_property]]"
            if parent is not None:
                function_qualifier = f"{module_qualifier}.{parent}"
            else:
                function_qualifier = module_qualifier
            try:
                function_definition_model = FunctionDefinitionModel(
                    qualifier=function_qualifier,
                    definition=function_definition,
                    returns=returns,
                )
                models.add(function_definition_model)
            except ValueError:
                pass

        return models

    def gather_functions_to_model(self) -> Iterable[Callable[..., object]]:
        return []

    def compute_models(
        self, functions_to_model: Iterable[Callable[..., object]]
    ) -> Iterable[Model]:
        sinks: Set[Model] = set()

        for path in find_all_paths(self.root):
            relative_path = os.path.relpath(path, self.root)
            should_skip = any(
                (
                    relative_path.startswith(blacklisted)
                    for blacklisted in self.blacklisted_global_directories
                )
            )
            if should_skip:
                LOG.info("Skipping %s", os.path.relpath(path, self.root))
            else:
                sinks = sinks.union(self._globals(self.root, path))

        stub_root = self.stub_root
        if stub_root is not None:
            stub_root = os.path.abspath(stub_root)
            paths = glob.glob(stub_root + "/**/*.pyi", recursive=True)
            for path in paths:
                sinks = sinks.union(self._globals(stub_root, path))
        return sinks


def _get_self_attribute(target: ast.expr) -> Optional[str]:
    if isinstance(target, ast.Attribute):
        value = target.value
        if isinstance(value, ast.Name) and value.id == "self":
            return target.attr
    return None


def _is_cached_property_decorator(decorator: ast.expr) -> bool:
    if isinstance(decorator, ast.Name):
        name = decorator.id
    elif isinstance(decorator, ast.Attribute):
        name = decorator.attr
    else:
        name = None
    if name is None:
        return False
    return "cached" in name and "property" in name


def _is_class_property_decorator(decorator: ast.expr) -> bool:
    if isinstance(decorator, ast.Name):
        name = decorator.id
    elif isinstance(decorator, ast.Attribute):
        name = decorator.attr
    else:
        name = None
    if name is None:
        return False
    return "class" in name and "property" in name
