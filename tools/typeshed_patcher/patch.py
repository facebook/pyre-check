# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import dataclasses
import enum
import pathlib

from typing import List, Union

from typing_extensions import TypeAlias


@dataclasses.dataclass(frozen=True)
class QualifiedName:
    names: List[str] = dataclasses.field(default_factory=list)


class AddPosition(str, enum.Enum):
    TOP_OF_SCOPE: str = "top"
    BOTTOM_OF_SCOPE: str = "bottom"


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
