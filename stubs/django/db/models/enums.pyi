# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from enum import Enum
from typing import Sequence, Tuple

class Choices(Enum):
    def __str__(self) -> str: ...
    @property
    def label(self) -> str: ...

class IntegerChoices(int, Choices):
    def choices(self) -> Sequence[Tuple[int, str]]: ...

class TextChoices(str, Choices):
    @property
    def choices(self) -> Sequence[Tuple[str, str]]: ...
