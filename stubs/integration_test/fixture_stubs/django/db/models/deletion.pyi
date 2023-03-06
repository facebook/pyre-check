# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import Collection

from django.db.models import Model
from django.db.models.fields import Field

class Collector: ...

def CASCADE(
    collector: Collector, field: Field, sub_objs: Collection[Model], using: str
) -> None: ...
def PROTECT(
    collector: Collector, field: Field, sub_objs: Collection[Model], using: str
) -> None: ...
def RESTRICT(
    collector: Collector, field: Field, sub_objs: Collection[Model], using: str
) -> None: ...
def SET_NULL(
    collector: Collector, field: Field, sub_objs: Collection[Model], using: str
) -> None: ...
def SET_DEFAULT(
    collector: Collector, field: Field, sub_objs: Collection[Model], using: str
) -> None: ...
def DO_NOTHING(
    collector: Collector, field: Field, sub_objs: Collection[Model], using: str
) -> None: ...
