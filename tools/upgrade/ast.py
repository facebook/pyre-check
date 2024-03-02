# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
TODO(T132414938) Add a module-level docstring
"""


import ast
import logging
from logging import Logger
from typing import Callable

from pyre_extensions import TypeVarTuple
from pyre_extensions.type_variable_operators import Concatenate


Ts = TypeVarTuple("Ts")


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
    # pyre-fixme[31]: Expression `Concatenate[(str,
    #  $local_tools?pyre?tools?upgrade?ast$Ts)], str)]` is not a valid type.
    # pyre-fixme[31]: Expression `Concatenate[(str,
    #  $local_tools?pyre?tools?upgrade?ast$Ts)], str)]` is not a valid type.
    transform: "Callable[Concatenate[str, Ts], str]",
    # pyre-fixme[31]: Expression `Concatenate[(str,
    #  $local_tools?pyre?tools?upgrade?ast$Ts)], str)]` is not a valid type.
) -> "Callable[Concatenate[str, Ts], str]":
    # pyre-fixme[11]: Annotation `Ts` is not defined as a type.
    def wrapper(input: str, *args: Ts) -> str:
        transformed = transform(input, *args)
        check_stable(input, transformed)
        return transformed

    return wrapper
