# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

from typing import Any, ClassVar, Type

from django.db.models.base import Model as Model

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
    CASCADE as CASCADE,
    DO_NOTHING as DO_NOTHING,
    PROTECT as PROTECT,
    RESTRICT as RESTRICT,
    SET_DEFAULT as SET_DEFAULT,
    SET_NULL as SET_NULL,
)
from django.db.models.enums import (
    IntegerChoices as IntegerChoices,
    TextChoices as TextChoices,
)
from django.db.models.fields import (
    AutoField as AutoField,
    BigIntegerField as BigIntegerField,
    BooleanField as BooleanField,
    CharField as CharField,
    DateField as DateField,
    DateTimeField as DateTimeField,
    DecimalField as DecimalField,
    EmailField as EmailField,
    FloatField as FloatField,
    IntegerField as IntegerField,
    NullBooleanField as NullBooleanField,
    PositiveIntegerField as PositiveIntegerField,
    PositiveSmallIntegerField as PositiveSmallIntegerField,
    SmallAutoField as SmallAutoField,
    TextField as TextField,
    TimeField as TimeField,
    URLField as URLField,
)

from django.db.models.fields.related import ForeignKey as ForeignKey
from django.db.models.fields.subclassing import SubfieldBase as SubfieldBase

from django.db.models.fields.files import (
    ImageField as ImageField,
    FileField as FileField,
    FieldFile as FieldFile,
    FileDescriptor as FileDescriptor,
)

from django.db.models.manager import BaseManager as BaseManager, Manager as Manager
from django.db.models.options import Options as Options

from django.db.models.query import (
    Prefetch as Prefetch,
    QuerySet as QuerySet,
    RawQuerySet as RawQuerySet,
    prefetch_related_objects as prefetch_related_objects,
)
