# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import argparse
import ast
import functools
import glob
import logging
import os
from pathlib import Path
from typing import Callable, Dict, Mapping, Optional, Set, Union

import _ast

from .taint_annotator import Model, annotate_function


LOG: logging.Logger = logging.getLogger(__name__)


def _find_module(module: str) -> Optional[str]:
    path = module.replace(".", "/") + ".py"
    if os.path.exists(path):
        return path
    path = os.path.dirname(path) + "/__init__.py"
    if os.path.exists(path):
        return path
    return None


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


FunctionDefinition = Union[_ast.FunctionDef, _ast.AsyncFunctionDef]


def _load_function_definition(
    arguments: argparse.Namespace, function: str
) -> Optional[FunctionDefinition]:
    split = function.split(".")
    if len(split) <= 1:
        return
    module = ".".join(split[:-1])
    parent = None
    base = None
    name = split[-1]

    module_path = _find_module(module)
    if not module_path:
        module = ".".join(split[:-2])
        parent = split[-2]
        module_path = _find_module(module)

        if not module_path:
            LOG.warning(f"Could not find module for `{function}`.")
            return None

    tree = _load_module(module_path)
    if not tree:
        return

    statements = tree.body
    if parent:
        found_class = False
        for statement in statements:
            if not isinstance(statement, ast.ClassDef):
                continue

            if statement.name != parent:
                continue

            name = function.split(".")[-1]
            if name in ["as_view", "async_as_view"]:
                for base in statement.bases:
                    if not isinstance(base, ast.Name):
                        continue
                    if base.id in arguments.as_view_base:
                        # pyre-ignore
                        return ast.parse(
                            f"def {name}(cls, request: HttpRequest): pass"
                        ).body[0]

            found_class = True
            statements = statement.body
            break
        if not found_class:
            LOG.warning(f"Could not find class `{parent}` in `{module}`.")
            return

    for statement in statements:
        if not isinstance(statement, ast.FunctionDef) and not isinstance(
            statement, ast.AsyncFunctionDef
        ):
            continue
        if statement.name == name:
            return statement

    return None


def _qualifier(root: str, path: str) -> str:
    # Normalize out symlinks, etc
    root = os.path.realpath(root)
    path = os.path.realpath(path)

    path = os.path.relpath(path, root)
    if path.endswith(".pyi"):
        path = path[:-4]
    elif path.endswith(".py"):
        path = path[:-3]
    qualifier = path.replace("/", ".")
    if qualifier.endswith(".__init__"):
        qualifier = qualifier[:-9]
    return qualifier


def _globals(root: str, path: str) -> Set[str]:
    module = _load_module(path)

    globals = set()
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
                if isinstance(callee, ast.Attribute) and callee.attr == "namedtuple":
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

    def formatted_target(target: str) -> str:
        return f"{_qualifier(root, path)}.{target}: TaintSink[Global] = ..."

    return {formatted_target(target) for target in globals if target != "__all__"}


def _visit_views(
    arguments: argparse.Namespace,
    path: Optional[str],
    callback: Callable[[str, FunctionDefinition], None],
) -> None:
    functions: Set[str] = set()

    if not path:
        return

    class UrlVisitor(ast.NodeVisitor):
        def __init__(self) -> None:
            self._aliases: Dict[str, str] = {}

        def _resolve_name(self, name: Union[ast.Name, ast.Attribute]) -> str:
            if isinstance(name, ast.Name):
                name = name.id
                if name in self._aliases:
                    return self._aliases[name]
                else:
                    return name
            elif isinstance(name, ast.Attribute):
                value = name.value
                if isinstance(value, ast.Name) or isinstance(value, ast.Attribute):
                    return f"{self._resolve_name(value)}.{name.attr}"
                else:
                    raise ValueError("Trying to resolve expression.")

        def _handle_view(self, argument: ast.AST, base: str = "") -> None:
            if isinstance(argument, ast.Str):
                function = argument.s if base == "" else base + "." + argument.s
                functions.add(function)

            elif isinstance(argument, ast.Call):
                name = argument.func
                if not isinstance(name, ast.Name) or name.id != "include":
                    return
                call_arguments = argument.args
                if len(call_arguments) < 1:
                    return
                argument = call_arguments[0]
                if not isinstance(argument, ast.Str):
                    return
                include = argument.s
                _visit_views(arguments, _find_module(include), callback)

            elif isinstance(argument, ast.Attribute) or isinstance(argument, ast.Name):
                functions.add(self._resolve_name(argument))

        def _handle_url(self, call: _ast.Call, base: str = "") -> None:
            call_arguments = call.args
            if len(call_arguments) < 2:
                return
            self._handle_view(call_arguments[1], base)

        def _handle_patterns(self, call: _ast.Call) -> None:
            call_arguments = call.args
            if len(call_arguments) < 1:
                return

            base = call_arguments[0]
            if not isinstance(base, ast.Str):
                return
            base = base.s

            for argument in call_arguments[1:]:
                if isinstance(argument, ast.Tuple):
                    elements = argument.elts
                    if len(elements) < 2:
                        continue
                    self._handle_view(elements[1], base)
                elif isinstance(argument, ast.Call):
                    name = argument.func
                    if isinstance(name, ast.Name) and name.id == "url":
                        self._handle_url(argument, base)

        def _handle_graphql_field(self, call: _ast.Call, base: str = "") -> None:
            kwargs = call.keywords

            # Note: resolver could be a lambda, which _handle_view will politely
            # ignore for now
            for kwarg in kwargs:
                if kwarg.arg == "resolver":
                    self._handle_view(kwarg.value, base)

        def visit_ImportFrom(self, import_from: _ast.ImportFrom) -> None:
            for name in import_from.names:
                if not isinstance(name, ast.alias):
                    continue
                if name.asname is not None:
                    # Not yet supported.
                    continue
                module = import_from.module
                if not module:
                    continue
                self._aliases[name.name] = f"{module}.{name.name}"

        def visit_FunctionDef(self, definition: _ast.FunctionDef) -> None:
            name = definition.name
            module = _qualifier(os.getcwd(), path)
            self._aliases[name] = f"{module}.{name}"

        def visit_AsyncFunctionDef(self, definition: _ast.AsyncFunctionDef) -> None:
            name = definition.name
            module = _qualifier(os.getcwd(), path)
            self._aliases[name] = f"{module}.{name}"

        def handle_call(self, call: _ast.Call) -> None:
            name = call.func
            if not isinstance(name, ast.Name):
                return
            if name.id == "url":
                self._handle_url(call)
            elif name.id == "patterns":
                self._handle_patterns(call)
            elif name.id == "GraphQLField":
                self._handle_graphql_field(call)
            elif name.id == "add_connection":
                resolver_arg = next(
                    (
                        keyword
                        for keyword in call.keywords
                        if keyword.arg == "connection_resolver"
                    ),
                    None,
                )
                if resolver_arg:
                    self._handle_view(resolver_arg.value)

    LOG.info(f"Reading file `{path}`...")
    with open(path, "r") as file:
        visitor = UrlVisitor()
        root = ast.parse(file.read())

        # First pass to build up our knowledge of what functions are defined
        # where
        visitor.visit(root)

        # Second pass to find all places where the functions (which we now have
        # fully qualified names for) are incorporated into django, graphql, etc.
        for node in ast.walk(root):
            if isinstance(node, _ast.Call):
                visitor.handle_call(node)

    for function in functions:
        definition = _load_function_definition(arguments, function)
        if not definition:
            LOG.warning(f"Unable to find definition for {function}.")
        else:
            callback(function, definition)


def _file_exists(path: str) -> str:
    if not os.path.exists(path):
        raise ValueError(f"No file at `{path}`")
    return path


def _get_exit_nodes(arguments: argparse.Namespace) -> Set[str]:
    exit_nodes: Set[str] = set()

    if not arguments.urls_path:
        LOG.warn("Ran in get_exit_nodes mode, but didn't supply urls_path")
        return exit_nodes

    def callback(function: str, definition: FunctionDefinition) -> None:
        model = Model(returns=" -> TaintSink[ReturnedToUser]")
        exit_nodes.add(annotate_function(function, definition, model))

    _visit_views(arguments, arguments.urls_path, callback)
    return exit_nodes


def _get_REST_api_sources(arguments: argparse.Namespace) -> Set[str]:
    sources = set()

    if not arguments.urls_path:
        LOG.warn("Ran in get_REST_api_sources mode, but didn't supply urls_path")
        return sources

    whitelist = arguments.whitelisted_class

    def callback(function: str, definition: FunctionDefinition) -> None:
        model = Model(
            arg=": TaintSource[UserControlled]",
            # These are commented out to preserve existing behaviour, but
            # I actually think it's more correct to uncomment them:
            # vararg=": TaintSource[UserControlled]",
            # kwarg=": TaintSource[UserControlled]",
        )
        sources.add(annotate_function(function, definition, model, whitelist))

    _visit_views(arguments, arguments.urls_path, callback)

    return sources


def _get_graphql_sources(arguments: argparse.Namespace) -> Set[str]:
    sources = set()

    if not arguments.graphql_path:
        LOG.warn("Ran in get_graphql_sources mode, but didn't supply graphql_path")
        return sources

    def callback(function: str, definition: FunctionDefinition) -> None:
        model = Model(
            vararg=": TaintSource[UserControlled]",
            kwarg=": TaintSource[UserControlled]",
        )
        sources.add(annotate_function(function, definition, model))

    for module in Path(arguments.graphql_path).iterdir():
        if module.is_file():
            _visit_views(arguments, str(module), callback)

    return sources


def _get_globals(arguments: argparse.Namespace) -> Set[str]:
    sinks: Set[str] = set()
    root = os.path.abspath(os.getcwd())
    paths = [path for path in glob.glob(root + "/**/*.py", recursive=True)]
    for path in paths:
        # Stubs take precedence if both module.py and module.pyi exist.
        stub_path = f"{path}i"
        if os.path.exists(stub_path):
            path = stub_path
        sinks = sinks.union(_globals(root, path))
    stub_root = arguments.stub_root
    if stub_root:
        stub_root = os.path.abspath(stub_root)
        paths = [path for path in glob.glob(stub_root + "/**/*.pyi", recursive=True)]
        for path in paths:
            sinks = sinks.union(_globals(stub_root, path))
    return sinks


MODES: Mapping[str, Callable[[argparse.Namespace], Set[str]]] = {
    "get_globals": _get_globals,
    "get_exit_nodes": _get_exit_nodes,
    "get_REST_api_sources": _get_REST_api_sources,
    "get_graphql_sources": _get_graphql_sources,
}

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("-v", "--verbose", action="store_true", help="Verbose logging")
    parser.add_argument(
        "urls_path", type=_file_exists, help="Path to django `urls.py` file"
    )
    parser.add_argument(
        "--graphql-path",
        type=_file_exists,
        help="Path to directory containing GraphQL definitions "
        "for which to generate taint models",
    )
    parser.add_argument("--whitelisted-class", action="append")
    parser.add_argument("--as-view-base", action="append")
    parser.add_argument(
        "--stub-root", type=_file_exists, help="Root of the stubs directory"
    )

    parser.add_argument("--mode", action="append", choices=MODES.keys())
    arguments: argparse.Namespace = parser.parse_args()

    if not arguments.mode:
        arguments.mode = [
            "get_exit_nodes",
            "get_REST_api_sources",
            "get_graphql_sources",
        ]

    if not arguments.whitelisted_class:
        arguments.whitelisted_class = ["HttpRequest"]

    logging.basicConfig(
        format="%(asctime)s %(levelname)s %(message)s",
        datefmt="%Y-%m-%d %H:%M:%S",
        level=logging.DEBUG if arguments.verbose else logging.INFO,
    )

    os.chdir(os.path.dirname(arguments.urls_path))
    arguments.urls_path = os.path.basename(arguments.urls_path)

    models: Set[str] = set()
    for mode in arguments.mode:
        models = models.union(MODES[mode](arguments))

    print("\n".join(sorted(models)))
