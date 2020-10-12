# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import datetime
from decimal import Decimal
from typing import Any, Callable, Mapping, Optional, Sequence, Tuple, Union

from django.db.models.enums import TextChoices

# This stub is a lie. These are all actually classes that inherit from a common
# base Field class. But their usage is to be attached to Model classes like this:

#   class MyModel(Model):
#       name = CharField(...)

# Due to Django metaclass magic, the effect of this assignment is that the
# `name` attribute on `MyModel` instances will actually end up being a normal
# string, not an instance of the `CharField` class. But this of course confuses
# Pyre no end, because it doesn't know about the metaclass magic.

# As a workaround, we just model these as functions returning the eventual
# primitive type (e.g. `CharField` returns `str`, etc), which makes Pyre happy
# in the above usage. Since we don't do any advanced introspection of the Field
# objects in our codebase (and going forward, our use of the Django ORM should
# only decrease), this lie is adequate for our needs.

def CharField(
    verbose_name: str = ...,
    name: str = ...,
    primary_key: bool = ...,
    max_length: int = ...,
    unique: bool = ...,
    blank: bool = ...,
    null: bool = ...,
    db_index: bool = ...,
    rel: object = ...,
    default: Optional[str] = ...,
    editable: bool = ...,
    serialize: bool = ...,
    unique_for_date: datetime.date = ...,
    unique_for_month: int = ...,
    unique_for_year: int = ...,
    choices: Union[
        Sequence[Tuple[str, str]], Callable[[TextChoices], Sequence[Tuple[str, str]]]
    ] = ...,
    help_text: str = ...,
    db_column: str = ...,
    db_tablespace: str = ...,
    auto_created: bool = ...,
    validators: Sequence[Callable[[str], None]] = ...,
    error_messages: Mapping[str, str] = ...,
) -> str: ...

EmailField = CharField
TextField = CharField
URLField = CharField

def DateField(
    verbose_name: str = ...,
    name: str = ...,
    auto_now: bool = ...,
    auto_now_add: bool = ...,
    primary_key: bool = ...,
    unique: bool = ...,
    blank: bool = ...,
    null: bool = ...,
    db_index: bool = ...,
    rel: object = ...,
    default: Optional[Union[Callable[[], datetime.date], datetime.date]] = ...,
    editable: bool = ...,
    serialize: bool = ...,
    unique_for_date: datetime.date = ...,
    unique_for_month: int = ...,
    unique_for_year: int = ...,
    choices: Sequence[Tuple[datetime.datetime, str]] = ...,
    help_text: str = ...,
    db_column: str = ...,
    db_tablespace: str = ...,
    auto_created: bool = ...,
    validators: Sequence[Callable[[datetime.datetime], None]] = ...,
    error_messages: Mapping[str, str] = ...,
) -> datetime.date: ...
def DateTimeField(
    verbose_name: str = ...,
    name: str = ...,
    auto_now: bool = ...,
    auto_now_add: bool = ...,
    primary_key: bool = ...,
    unique: bool = ...,
    blank: bool = ...,
    null: bool = ...,
    db_index: bool = ...,
    rel: object = ...,
    default: Optional[Union[Callable[[], datetime.datetime], datetime.datetime]] = ...,
    editable: bool = ...,
    serialize: bool = ...,
    unique_for_date: datetime.date = ...,
    unique_for_month: int = ...,
    unique_for_year: int = ...,
    choices: Sequence[Tuple[datetime.datetime, str]] = ...,
    help_text: str = ...,
    db_column: str = ...,
    db_tablespace: str = ...,
    auto_created: bool = ...,
    validators: Sequence[Callable[[datetime.datetime], None]] = ...,
    error_messages: Mapping[str, str] = ...,
) -> datetime.datetime: ...
def TimeField(
    verbose_name: str = ...,
    name: str = ...,
    auto_now: bool = ...,
    auto_now_add: bool = ...,
    primary_key: bool = ...,
    unique: bool = ...,
    blank: bool = ...,
    null: bool = ...,
    db_index: bool = ...,
    rel: object = ...,
    default: Optional[Union[Callable[[], datetime.time], datetime.time]] = ...,
    editable: bool = ...,
    serialize: bool = ...,
    unique_for_date: datetime.date = ...,
    unique_for_month: int = ...,
    unique_for_year: int = ...,
    choices: Sequence[Tuple[datetime.datetime, str]] = ...,
    help_text: str = ...,
    db_column: str = ...,
    db_tablespace: str = ...,
    auto_created: bool = ...,
    validators: Sequence[Callable[[datetime.datetime], None]] = ...,
    error_messages: Mapping[str, str] = ...,
) -> datetime.time: ...
def DecimalField(
    verbose_name: str = ...,
    name: str = ...,
    max_digits: Optional[int] = ...,
    decimal_places: Optional[int] = ...,
    primary_key: bool = ...,
    unique: bool = ...,
    blank: bool = ...,
    null: bool = ...,
    db_index: bool = ...,
    rel: object = ...,
    default: Optional[int] = ...,
    editable: bool = ...,
    serialize: bool = ...,
    unique_for_date: datetime.date = ...,
    unique_for_month: int = ...,
    unique_for_year: int = ...,
    choices: Sequence[Tuple[int, str]] = ...,
    help_text: str = ...,
    db_column: str = ...,
    db_tablespace: str = ...,
    auto_created: bool = ...,
    validators: Sequence[Callable[[int], None]] = ...,
    error_messages: Mapping[str, str] = ...,
) -> Decimal: ...
def FloatField(
    verbose_name: str = ...,
    name: str = ...,
    primary_key: bool = ...,
    unique: bool = ...,
    blank: bool = ...,
    null: bool = ...,
    db_index: bool = ...,
    rel: object = ...,
    default: Optional[int] = ...,
    editable: bool = ...,
    serialize: bool = ...,
    unique_for_date: datetime.date = ...,
    unique_for_month: int = ...,
    unique_for_year: int = ...,
    choices: Sequence[Tuple[int, str]] = ...,
    help_text: str = ...,
    db_column: str = ...,
    db_tablespace: str = ...,
    auto_created: bool = ...,
    validators: Sequence[Callable[[int], None]] = ...,
    error_messages: Mapping[str, str] = ...,
) -> float: ...
def IntegerField(
    verbose_name: str = ...,
    name: str = ...,
    primary_key: bool = ...,
    unique: bool = ...,
    blank: bool = ...,
    null: bool = ...,
    db_index: bool = ...,
    rel: object = ...,
    default: Optional[int] = ...,
    editable: bool = ...,
    serialize: bool = ...,
    unique_for_date: datetime.date = ...,
    unique_for_month: int = ...,
    unique_for_year: int = ...,
    choices: Sequence[Tuple[int, str]] = ...,
    help_text: str = ...,
    db_column: str = ...,
    db_tablespace: str = ...,
    auto_created: bool = ...,
    validators: Sequence[Callable[[int], None]] = ...,
    error_messages: Mapping[str, str] = ...,
) -> int: ...

PositiveIntegerField = IntegerField
PositiveSmallIntegerField = IntegerField
BigIntegerField = IntegerField

AutoField = IntegerField
SmallAutoField = IntegerField

def BooleanField(
    verbose_name: str = ...,
    name: str = ...,
    primary_key: bool = ...,
    unique: bool = ...,
    blank: bool = ...,
    null: bool = ...,
    db_index: bool = ...,
    rel: object = ...,
    default: Optional[bool] = ...,
    editable: bool = ...,
    serialize: bool = ...,
    unique_for_date: datetime.date = ...,
    unique_for_month: int = ...,
    unique_for_year: int = ...,
    choices: Sequence[Tuple[bool, str]] = ...,
    help_text: str = ...,
    db_column: str = ...,
    db_tablespace: str = ...,
    auto_created: bool = ...,
    validators: Sequence[Callable[[bool], None]] = ...,
    error_messages: Mapping[str, str] = ...,
) -> bool: ...
def NullBooleanField(
    verbose_name: str = ...,
    name: str = ...,
    primary_key: bool = ...,
    unique: bool = ...,
    blank: bool = ...,
    null: bool = ...,
    db_index: bool = ...,
    rel: object = ...,
    default: Optional[bool] = ...,
    editable: bool = ...,
    serialize: bool = ...,
    unique_for_date: datetime.date = ...,
    unique_for_month: int = ...,
    unique_for_year: int = ...,
    choices: Sequence[Tuple[Optional[bool], str]] = ...,
    help_text: str = ...,
    db_column: str = ...,
    db_tablespace: str = ...,
    auto_created: bool = ...,
    validators: Sequence[Callable[[Optional[bool]], None]] = ...,
    error_messages: Mapping[str, str] = ...,
) -> Optional[bool]: ...

# This is a workaround for the field types being functions and not
# actual types.
Field = Any
