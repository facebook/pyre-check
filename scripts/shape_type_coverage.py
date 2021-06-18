# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import List, Optional

from ..api.connection import PyreConnection
from ..client.find_directories import find_global_and_local_root, FoundRoot

LOG: logging.Logger = logging.getLogger(__name__)


@dataclass(frozen=True)
class ParametricType:
    name: str
    parameters: List[str]


def _create_pyre_connection(
    roots: FoundRoot,
) -> PyreConnection:
    if roots.local_root is not None:
        return PyreConnection(roots.local_root)
    return PyreConnection(roots.global_root)


def _parametric_type(string: str) -> Optional[ParametricType]:
    left_bracket_index = string.find("[")
    right_bracket_index = string.rfind("]")
    if left_bracket_index == -1 or right_bracket_index == -1:
        return None
    return ParametricType(
        name=string[:left_bracket_index],
        parameters=[
            parameter.strip()
            for parameter in string[left_bracket_index + 1 : right_bracket_index].split(
                ","
            )
        ],
    )


def _is_tensor(parametric: ParametricType) -> bool:
    return parametric.name == "torch.Tensor"


def _is_literal_integer(type_name: str) -> bool:
    parametric = _parametric_type(type_name)
    if parametric is None:
        return False
    return (
        parametric.name == "typing_extensions.Literal"
        and len(parametric.parameters) == 1
        and parametric.parameters[0].isnumeric()
    )


def _is_precise_tensor(parametric: ParametricType) -> bool:
    """Assumes it is given a torch tensor, and that everything from the
    first parameter on is a dimension."""
    return all(
        _is_literal_integer(dimension) for dimension in parametric.parameters[1:]
    )


def main() -> None:
    logging.basicConfig(
        format="[%(asctime)s][%(levelname)s]: %(message)s", level=logging.INFO
    )

    roots = find_global_and_local_root(Path("."))
    if roots is None:
        LOG.error(f"Failed to find global Pyre configuration for {Path().absolute()}.")
        sys.exit(1)

    try:
        with _create_pyre_connection(roots) as pyre_connection:
            LOG.info(
                f"Server is up: {pyre_connection.server_initialized} at "
                f"{pyre_connection.pyre_directory}"
            )
    except Exception as exception:
        LOG.error(f"Pyre server raised an exception: {exception}")


if __name__ == "__main__":
    main()
