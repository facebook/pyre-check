# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import dataclasses
import enum
import pathlib

from typing import List, Optional, Union

from typing_extensions import TypeAlias


class ReadPatchException(Exception):
    pass


def _read_string(input_object: object, field_name: Optional[str] = None) -> str:
    if not isinstance(input_object, str):
        field_message = f" for field `{field_name}`" if field_name is not None else ""
        raise ReadPatchException(
            f"Expect a string{field_message} but got {input_object}"
        )
    return input_object


@dataclasses.dataclass(frozen=True)
class QualifiedName:
    names: List[str] = dataclasses.field(default_factory=list)

    def to_string(self) -> str:
        return ".".join(self.names)

    @staticmethod
    def from_string(qualified_name: str) -> "QualifiedName":
        if len(qualified_name) == 0:
            return QualifiedName([])
        else:
            return QualifiedName(qualified_name.split("."))

    @staticmethod
    def from_json(input_object: object) -> "QualifiedName":
        input_string = _read_string(input_object, field_name="parent")
        return QualifiedName.from_string(input_string)

    def is_empty(self) -> bool:
        return len(self.names) == 0


class AddPosition(str, enum.Enum):
    TOP_OF_SCOPE: str = "top"
    BOTTOM_OF_SCOPE: str = "bottom"

    @staticmethod
    def from_json(input_object: object) -> "AddPosition":
        for element in AddPosition:
            if element.value == input_object:
                return element
        raise ReadPatchException(f"Unrecognized position: {input_object}")


@dataclasses.dataclass(frozen=True)
class DeleteAction:
    name: str


@dataclasses.dataclass(frozen=True)
class DeleteImportAction:
    name: str


@dataclasses.dataclass(frozen=True)
class AddAction:
    content: str
    position: AddPosition = AddPosition.BOTTOM_OF_SCOPE


@dataclasses.dataclass(frozen=True)
class ReplaceAction:
    name: str
    content: str


Action: TypeAlias = Union[AddAction, DeleteAction, DeleteImportAction, ReplaceAction]


@dataclasses.dataclass(frozen=True)
class Patch:
    action: Action
    parent: QualifiedName  # Can be empty, which implies global scope.


@dataclasses.dataclass(frozen=True)
class FilePatch:
    path: pathlib.Path
    patches: List[Patch] = dataclasses.field(default_factory=list)
