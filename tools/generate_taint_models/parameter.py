# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

from enum import auto, Enum
from typing import NamedTuple, Optional


# pyre-fixme[4]: Attribute must be annotated.
class Parameter(NamedTuple):
    class Kind(Enum):
        ARG = auto()
        VARARG = auto()
        KWARG = auto()

    name: str
    annotation: Optional[str]
    # pyre-fixme[11]: Annotation `Kind` is not defined as a type.
    kind: Kind

    def __eq__(self, other: "Parameter") -> bool:
        if not isinstance(other, self.__class__):
            return False
        return self.name == other.name
