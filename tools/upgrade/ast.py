# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import ast
import logging
from logging import Logger
from typing import Callable

# pyre-fixme[21]: Could not find name `ListVariadic` in `pyre_extensions`.
from pyre_extensions import ListVariadic
from pyre_extensions.type_variable_operators import Concatenate


Ts = ListVariadic("Ts")


LOG: Logger = logging.getLogger(__name__)


class UnstableAST(Exception):
    pass


def check_stable(input: str, transformed: str) -> None:
    parsed_original = ast.parse(input)
    try:
        parsed_transformed = ast.parse(transformed)
        if ast.dump(parsed_original) != ast.dump(parsed_transformed):
            raise UnstableAST("ASTs differ")
    except SyntaxError:
        raise UnstableAST("Could not parse transformed AST")


def check_stable_transformation(
    transform: "Callable[Concatenate[str, Ts], str]",
) -> "Callable[Concatenate[str, Ts], str]":
    def wrapper(input: str, *args: Ts) -> str:
        transformed = transform(input, *args)
        check_stable(input, transformed)
        return transformed

    return wrapper
