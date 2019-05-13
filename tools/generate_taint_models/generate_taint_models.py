# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import argparse
import ast
import functools
import logging
import os
from typing import Callable, Mapping, Optional, Set, Union

import _ast


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
def _load_module_ast(module_path: str) -> Optional[ast.AST]:
    try:
        with open(module_path, "r") as file:
            return ast.parse(file.read())
    except (FileNotFoundError, SyntaxError) as error:
        LOG.warning(f"Could not load `{module_path}`: {str(error)}")
    return None


FunctionDefinition = Union[_ast.FunctionDef, _ast.AsyncFunctionDef]


def _load_function_definition(function: str) -> Optional[FunctionDefinition]:
    split = function.split(".")
    assert len(split) > 1, "Got unqualified or builtin function"
    module = ".".join(split[:-1])
    name = split[-1]

    module_path = _find_module(module)
    if not module_path:
        LOG.warning(f"Could not find module for `{function}`.")
        return None

    tree = _load_module_ast(module_path)
    if not tree:
        return

    if not isinstance(tree, ast.Module):
        return

    for statement in tree.body:
        if not isinstance(statement, ast.FunctionDef) and not isinstance(
            statement, ast.AsyncFunctionDef
        ):
            continue
        if statement.name == name:
            return statement

    return None


def _visit_views(
    urls_path: Optional[str], callback: Callable[[str, FunctionDefinition], None]
) -> None:
    functions: Set[str] = set()

    class UrlVisitor(ast.NodeVisitor):
        def _handle_url(self, call: _ast.Call) -> None:
            arguments = call.args
            if len(arguments) < 2:
                return
            argument = arguments[1]

            if isinstance(argument, ast.Str):
                function = argument.s
                functions.add(function)

            elif isinstance(argument, ast.Call):
                name = argument.func
                if not isinstance(name, ast.Name) or name.id != "include":
                    return
                arguments = argument.args
                if len(arguments) < 1:
                    return
                argument = arguments[0]
                if not isinstance(argument, ast.Str):
                    return
                include = argument.s
                _visit_views(_find_module(include), callback)

        def _handle_patterns(self, call: _ast.Call) -> None:
            arguments = call.args
            if len(arguments) < 1:
                return

            base = arguments[0]
            if not isinstance(base, ast.Str):
                return
            base = base.s

            for argument in arguments[1:]:
                if not isinstance(argument, ast.Tuple):
                    continue
                elements = argument.elts
                if len(elements) != 2:
                    continue
                target = elements[1]
                if not isinstance(target, ast.Str):
                    continue

                function = target.s if base == "" else base + "." + target.s
                functions.add(function)

        def visit_Call(self, call: _ast.Call) -> None:
            name = call.func
            if not isinstance(name, ast.Name):
                return
            if name.id == "url":
                self._handle_url(call)
            elif name.id == "patterns":
                self._handle_patterns(call)

    if not urls_path:
        return

    LOG.info(f"Reading urls from `{urls_path}`...")
    with open(urls_path, "r") as file:
        UrlVisitor().visit(ast.parse(file.read()))

    for function in functions:
        definition = _load_function_definition(function)
        if not definition:
            LOG.warning(f"Unable to find definition for {function}.")
        else:
            callback(function, definition)


def _file_exists(path: str) -> str:
    if not os.path.exists(path):
        raise ValueError(f"No file at `{path}`")
    return path


def _get_exit_nodes(urls_path: str) -> Set[str]:
    exit_nodes: Set[str] = set()
    # TODO(T40359712): add implementation

    def callback(function: str, definition: FunctionDefinition) -> None:
        LOG.info(f"Exit node: {function}:\n{ast.dump(definition)}")

    _visit_views(urls_path, callback)
    return exit_nodes


def _get_REST_api_sources(urls_path: str) -> Set[str]:
    sources: Set[str] = set()
    # TODO(T40359712): add implementation
    return sources


MODES: Mapping[str, Callable[[str], Set[str]]] = {
    "get_exit_nodes": _get_exit_nodes,
    "get_REST_api_sources": _get_REST_api_sources,
}

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("-v", "--verbose", action="store_true", help="Verbose logging")
    parser.add_argument(
        "urls_path", type=_file_exists, help="Path to django `urls.py` file"
    )
    parser.add_argument("--mode", action="append", choices=MODES.keys())
    arguments: argparse.Namespace = parser.parse_args()

    if not arguments.mode:
        arguments.mode = MODES.keys()

    logging.basicConfig(
        format="%(asctime)s %(levelname)s %(message)s",
        datefmt="%Y-%m-%d %H:%M:%S",
        level=logging.DEBUG if arguments.verbose else logging.INFO,
    )

    os.chdir(os.path.dirname(arguments.urls_path))

    models: Set[str] = set()
    for mode in arguments.mode:
        models = models.union(MODES[mode](arguments.urls_path))

    print("\n".join(models))
