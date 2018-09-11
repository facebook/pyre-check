from typing import Any, ClassVar

from django.db.aggregates import (
    Avg as Avg,
    Count as Count,
    Max as Max,
    Min as Min,
    StdDev as StdDev,
    Sum as Sum,
    Variance as Variance,
)
from django.db.models.fields import DateTimeField as DateTimeField


class Model:
    id: int = ...
    objects: ClassVar[Any] = ...
    DoesNotExist: Any

    def save(self) -> None:
        ...
