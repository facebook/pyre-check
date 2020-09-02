# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

from typing import Any, TypeVar

_T = TypeVar("_T")

def curry(_curried_func, *args, **kwargs) -> Any: ...
def memoize(func: _T, cache, num_args) -> _T: ...

class cached_property:
    func: Any
    name: str
    def __get__(self, instance, type=...) -> Any: ...

# In the future, we'll properly type promises as parameteric types.
Promise = Any

def lazy(func: _T, *resultclasses) -> _T: ...

class LazyObject: ...

def partition(predicate: Any, values: Any) -> Any: ...
