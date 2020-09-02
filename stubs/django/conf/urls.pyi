# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

from typing import Any, Callable

from django.core.urlresolvers import RegexURLPattern, RegexURLResolver

def include(
    arg: Any, namespace: str = ..., app_name: str = ...
) -> RegexURLResolver: ...
def url(
    regex: str, view: Callable, kwargs: Any = ..., name: str = ..., prefix: str = ...
) -> RegexURLPattern: ...
