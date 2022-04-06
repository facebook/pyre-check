# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Optional

from ..api.connection import PyreConnection
from ..api.query import Annotation, get_types, Position
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


def _split_list(string: str) -> List[str]:
    """Assumes an input of the form `[s1, s2, s3, ..., sn]`,
    where each si may itself contain lists."""
    assert string[0] == "[" and string[-1] == "]"
    nesting_depth = 0
    all_strings = []
    current_string = ""
    for character in string[1:-1]:
        if character == "," and nesting_depth == 0:
            all_strings.append(current_string)
            current_string = ""
            continue

        if character == "[":
            nesting_depth += 1
        elif character == "]":
            nesting_depth -= 1
        current_string += character
    if current_string != "":
        all_strings.append(current_string)
    return [string.strip() for string in all_strings]


def _parametric_type(string: str) -> Optional[ParametricType]:
    left_bracket_index = string.find("[")
    right_bracket_index = string.rfind("]")
    if left_bracket_index == -1 or right_bracket_index == -1:
        return None
    return ParametricType(
        name=string[:left_bracket_index],
        parameters=_split_list(string[left_bracket_index : right_bracket_index + 1]),
    )


def _is_tensor(parametric: ParametricType) -> bool:
    return parametric.name == "torch.Tensor"


def _is_int_variable(type_name: str) -> bool:
    parametric = _parametric_type(type_name)
    return (
        parametric is not None
        and parametric.name == "Variable"
        and len(parametric.parameters) == 1
        and parametric.parameters[0].endswith("(bound to int)")
    )


def _is_literal_integer(type_name: str) -> bool:
    parametric = _parametric_type(type_name)
    return (
        parametric is not None
        and parametric.name == "typing_extensions.Literal"
        and len(parametric.parameters) == 1
        and parametric.parameters[0].isnumeric()
    )


def _is_precise_unpacked(type_name: str) -> bool:
    """A precise unpacked type can be either an unpacked
    simple identifier (presumably a TypeVarTuple), or an unpacked
    single precise tuple.
    There's not enough information here to tell whether the name
     is _really_ a TypeVarTuple, but anything that's not should be
    a type error, and thus not given to us by Pyre."""
    if len(type_name) == 0 or type_name[0] != "*":
        return False

    parametric = _parametric_type(type_name[1:])

    return parametric is None or _is_precise_tuple(type_name[1:])


def _is_int_expression(type_name: str) -> bool:
    parametric = _parametric_type(type_name)

    return (
        parametric is not None
        and parametric.name == "pyre_extensions.IntExpression"
        and len(parametric.parameters) == 1
    )


def _is_precise_tuple(type_name: str) -> bool:
    """A legal precise tuple will be a `Tuple` of precise dimensions,
    or a `Broadcast` of precise tuples."""
    parametric = _parametric_type(type_name)

    return parametric is not None and (
        (parametric.name == "Tuple" or parametric.name == "typing.Tuple")
        and all(
            _is_precise_tensor_dimension(dimension)
            for dimension in parametric.parameters
        )
        or (
            parametric.name == "Broadcast"
            and len(parametric.parameters) == 2
            and all(_is_precise_tuple(parameter) for parameter in parametric.parameters)
        )
    )


def _is_precise_tensor_dimension(dimension: str) -> bool:
    return (
        _is_literal_integer(dimension)
        or _is_int_variable(dimension)
        or _is_precise_unpacked(dimension)
        or _is_int_expression(dimension)
    )


def _is_precise_tensor(parametric: ParametricType) -> bool:
    """Assumes it is given a torch tensor, and that everything from the
    second parameter on is a dimension."""
    return all(
        _is_precise_tensor_dimension(dimension)
        for dimension in parametric.parameters[1:]
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


def _extract_substring(
    line: str, line_number: int, start_position: Position, stop_position: Position
) -> str:
    assert line_number >= start_position.line and line_number <= stop_position.line
    if start_position.line == stop_position.line and line_number == start_position.line:
        return line[start_position.column : stop_position.column]
    elif line_number == start_position.line:
        return line[start_position.column :]
    elif line_number == stop_position.line:
        return line[: stop_position.column]
    else:
        return line


def _extract_multiline_text(corpus: List[str], start: Position, stop: Position) -> str:
    return " ".join(
        [
            _extract_substring(
                line,
                relative_row_number + start.line,
                start,
                stop,
            )
            for relative_row_number, line in enumerate(
                # Lines are 1-indexed
                corpus[start.line - 1 : stop.line]
            )
        ]
    )


def _report_imprecise_warnings(mapping: Dict[str, ShapeAnnotations]) -> None:
    for filename, shape_annotations in mapping.items():
        try:
            lines = Path(filename).read_text().split("\n")
            for annotation in shape_annotations.imprecise_annotations:
                expression = " ".join(
                    _extract_multiline_text(
                        lines, annotation.start, annotation.stop
                    ).split()
                )
                LOG.error(
                    f"{filename}:{annotation.start.line}:{annotation.start.column} "
                    f"Expression `{expression}` has imprecise tensor shape type "
                    f"`{annotation.type_name}`"
                )
        except Exception as exception:
            LOG.error(f"Unable to read from file {filename}, got exception {exception}")


def _report_percentages(mapping: Dict[str, ShapeAnnotations]) -> None:
    global_precise_count = 0
    global_imprecise_count = 0
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
        global_precise_count += precise_count
        global_imprecise_count += imprecise_count
    if (global_precise_count + global_imprecise_count) == 0:
        LOG.info("Overall, files contain no expressions with tensor shape type.")
        return
    percentage = round(
        global_precise_count / (global_imprecise_count + global_precise_count) * 100.0,
        2,
    )
    LOG.info(
        f"Overall, files have {percentage}% precise tensor "
        f"shape types ({global_precise_count} out of "
        f"{global_imprecise_count + global_precise_count} expressions)"
    )


def main(filenames: Iterable[str]) -> None:
    """Prints out coverage statistics and errors related to expressions with
    imprecise shape types. Note that currently due to the `pyre query` API, lhs
    and rhs expressions are both counted as imprecise. This means that we will get
    double-counting for assignments, and duplicated errors."""
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
            _report_imprecise_warnings(shape_mapping)
            _report_percentages(shape_mapping)
    except Exception as exception:
        LOG.error(f"Pyre server raised an exception: {exception}")


if __name__ == "__main__":
    main(sys.argv[1:])
