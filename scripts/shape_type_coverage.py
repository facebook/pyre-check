# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import List, Optional, Dict, Iterable

from ..api.connection import PyreConnection
from ..api.query import Annotation, get_types
from ..client.find_directories import find_global_and_local_root, FoundRoot

LOG: logging.Logger = logging.getLogger(__name__)


@dataclass(frozen=True)
class ParametricType:
    name: str
    parameters: List[str]


@dataclass(frozen=True)
class ShapeAnnotations:
    precise_annotations: List[Annotation]
    imprecise_annotations: List[Annotation]


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


def _collect_shape_types(
    mapping: Dict[str, List[Annotation]]
) -> Dict[str, ShapeAnnotations]:
    final_dictionary = {}
    for filename, annotations in mapping.items():
        precise_annotations = []
        imprecise_annotations = []
        for annotation in annotations:
            parametric = _parametric_type(annotation.type_name)
            if parametric is None or not _is_tensor(parametric):
                continue
            if _is_precise_tensor(parametric):
                precise_annotations.append(annotation)
            else:
                imprecise_annotations.append(annotation)
        final_dictionary[filename] = ShapeAnnotations(
            precise_annotations, imprecise_annotations
        )

    return final_dictionary


def _report_percentages(mapping: Dict[str, ShapeAnnotations]) -> None:
    for filename, shape_annotations in mapping.items():
        precise_count = len(shape_annotations.precise_annotations)
        imprecise_count = len(shape_annotations.imprecise_annotations)
        if (precise_count + imprecise_count) == 0:
            LOG.info(f"File {filename} has no expressions with tensor shape type.")
            continue
        percentage = round(precise_count / (imprecise_count + precise_count) * 100.0, 2)
        LOG.info(
            f"File {filename} has {percentage}% precise tensor "
            f"shape types ({precise_count} out of "
            f"{imprecise_count + precise_count} expressions)"
        )


def main(filenames: Iterable[str]) -> None:
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
                f"Server is up: {pyre_connection.server_initialized} "
                f"at {pyre_connection.pyre_directory}"
            )
            typing_summary = get_types(pyre_connection, *filenames)
            shape_mapping = _collect_shape_types(typing_summary)
            _report_percentages(shape_mapping)
    except Exception as exception:
        LOG.error(f"Pyre server raised an exception: {exception}")


if __name__ == "__main__":
    main(sys.argv[1:])
