from enum import Enum
from typing import Sequence, Tuple

class Choices(Enum):
    def __str__(self) -> str: ...

class IntegerChoices(int, Choices):
    def choices(self) -> Sequence[Tuple[int, str]]: ...

class TextChoices(str, Choices):
    @property
    def choices(self) -> Sequence[Tuple[str, str]]: ...
