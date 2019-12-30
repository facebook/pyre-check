# pyre-unsafe

from typing import Any, ClassVar, Type

from django.core.exceptions import (
    MultipleObjectsReturned as MultipleObjectsReturnedException,
)
from django.db.aggregates import (
    Avg as Avg,
    Count as Count,
    Max as Max,
    Min as Min,
    StdDev as StdDev,
    Sum as Sum,
    Variance as Variance,
)
from django.db.models.fields import (
    AutoField as AutoField,
    BigIntegerField as BigIntegerField,
    BooleanField as BooleanField,
    CharField as CharField,
    DateTimeField as DateTimeField,
    IntegerField as IntegerField,
    NullBooleanField as NullBooleanField,
    PositiveIntegerField as PositiveIntegerField,
    PositiveSmallIntegerField as PositiveSmallIntegerField,
    TextField as TextField,
)
from django.db.models.fields.related import ForeignKey
from django.db.models.fields.subclassing import SubfieldBase
from django.db.models.manager import Manager
from django.db.models.options import Options

class Model:
    id: int = ...
    objects: Manager = ...
    _meta: Options = ...
    DoesNotExist: Any
    MultipleObjectsReturned: ClassVar[Type[MultipleObjectsReturnedException]] = ...
    def __init__(self, *args, **kwargs) -> None: ...
    def save(self) -> None: ...
