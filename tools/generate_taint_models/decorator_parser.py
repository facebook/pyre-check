# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import ast
import logging
from dataclasses import dataclass
from typing import List, Optional, Set, Tuple, Union, cast

from typing_extensions import Final


FunctionDefinition = Union[ast.FunctionDef, ast.AsyncFunctionDef]

LOG: logging.Logger = logging.getLogger(__name__)


@dataclass
class Decorator:
    name: str
    arguments: Final[Optional[Set[str]]] = None
    keywords: Final[Optional[Set[Tuple[Optional[str], str]]]] = None

    def has_attributes(self) -> bool:
        return bool(self.arguments or self.keywords)


class DecoratorParser:
    def __init__(self, unparsed_target_decorators: str) -> None:
        self._unparsed_target_decorators: str = unparsed_target_decorators
        self._target_decorators: Optional[List[Decorator]] = None

    @property
    def target_decorators(self) -> List[Decorator]:
        target_decorators = self._target_decorators
        if target_decorators is None:
            target_decorators = self._parse_target_decorators(
                self._unparsed_target_decorators
            )
            self._target_decorators = target_decorators
        return target_decorators

    def function_matches_target_decorators(self, node: FunctionDefinition) -> bool:
        ## TODO T58744796: In the future, change this to support
        ## filtering on multiple decorators.
        target_decorator: Decorator = self.target_decorators[0]
        for decorator in node.decorator_list:
            if (
                isinstance(decorator, ast.Name)
                and decorator.id == target_decorator.name
                and not target_decorator.has_attributes()
            ):
                return True
            elif isinstance(decorator, ast.Call):
                callable = decorator.func
                if (
                    isinstance(callable, ast.Name)
                    and callable.id == target_decorator.name
                    and self._are_attributes_matching(decorator, target_decorator)
                ):
                    return True
        return False

    def _parse_target_decorators(self, target_decorator: str) -> List[Decorator]:
        """
        Responsible for parsing the target decorator to extract the
        decorator name, named and unnamed attributes.
        """
        # We need to create a well formed decorator so we attach a bogus
        # function to the decorators.
        well_formed_decorator = target_decorator + """\ndef foo(): ..."""
        try:
            parsed_ast = ast.parse(well_formed_decorator)
        except SyntaxError as error:
            LOG.error(f"Can't parse `{well_formed_decorator}`.")
            raise error

        target_decorator_list = []
        function_definition = parsed_ast.body[0]
        if not isinstance(function_definition, ast.FunctionDef):
            return []
        decorator_list = function_definition.decorator_list
        if len(decorator_list) < 1:
            LOG.error("No target decorators were specified.")
            raise Exception("No target decorators were specified.")

        for decorator in decorator_list:
            if isinstance(decorator, ast.Call):
                target_decorator_name = cast(ast.Name, decorator.func).id
                target_decorator_arguments = {
                    argument.s
                    for argument in decorator.args
                    if isinstance(argument, ast.Str)
                }
                target_decorator_keywords = {
                    (keyword.arg, cast(ast.Str, keyword.value).s)
                    for keyword in decorator.keywords
                    if isinstance(keyword.value, ast.Str)
                }
                target_decorator_list.append(
                    Decorator(
                        target_decorator_name,
                        target_decorator_arguments,
                        target_decorator_keywords,
                    )
                )
            else:
                target_decorator_list.append(
                    Decorator(cast(ast.Name, decorator).id, set(), set())
                )
        return target_decorator_list

    def _are_attributes_matching(
        self, decorator: ast.Call, target_decorator: Decorator
    ) -> bool:
        # Handle unnamed attributes.
        decorator_arguments_set = {
            argument.s for argument in decorator.args if isinstance(argument, ast.Str)
        }
        arguments = target_decorator.arguments
        if arguments and not arguments.issubset(decorator_arguments_set):
            return False

        # Handle named attributes.
        decorator_keywords_set = {
            (keyword.arg, cast(ast.Str, keyword.value).s)
            for keyword in decorator.keywords
            if isinstance(keyword.value, ast.Str)
        }
        keywords = target_decorator.keywords
        if keywords and not keywords.issubset(decorator_keywords_set):
            return False
        return True
