# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

from typing import Any, Mapping, Optional, Tuple, Union

from django.urls import URLPattern, URLResolver

def include(arg: Any, namespace: Optional[str] = ...) -> Tuple[Any, str, str]: ...
def path(
    route: str,
    view: Any,
    kwargs: Optional[Mapping[str, Any]] = ...,
    name: Optional[str] = ...,
) -> Union[URLPattern, URLResolver]: ...
def re_path(
    route: str,
    view: Any,
    kwargs: Optional[Mapping[str, Any]] = ...,
    name: Optional[str] = ...,
) -> Union[URLPattern, URLResolver]: ...
