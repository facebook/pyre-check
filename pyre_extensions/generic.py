# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-ignore-all-errors
from typing import Any


class GenericMeta(type):
    def __getitem__(cls, *args) -> Any:
        return cls.__class__(cls.__name__, cls.__bases__, dict(cls.__dict__))


class Generic(metaclass=GenericMeta):
    pass
