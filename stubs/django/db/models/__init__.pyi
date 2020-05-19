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
from django.db.models.deletion import (
    CASCADE,
    DO_NOTHING,
    PROTECT,
    RESTRICT,
    SET_DEFAULT,
    SET_NULL,
)
from django.db.models.enums import IntegerChoices, TextChoices
from django.db.models.fields import (
    AutoField,
    BigIntegerField,
    BooleanField,
    CharField,
    DateField,
    DateTimeField,
    DecimalField,
    EmailField,
    FloatField,
    IntegerField,
    NullBooleanField,
    PositiveIntegerField,
    PositiveSmallIntegerField,
    SmallAutoField,
    TextField,
    TimeField,
    URLField,
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
