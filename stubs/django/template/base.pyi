# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import Union
from .engine import Engine
from .loaders import Loader

class Origin:
    def __init__(
        self, name: str, template_name: Optional[Union[bytes, str]] = ..., loader: Optional[Loader] = ...
    ) -> None: ...

class Template:
    def __init__(
        self,
        template_string: Union[Template, str],
        origin: Optional[Origin] = ...,
        name: Optional[str] = ...,
        engine: Optional[Engine] = ...,
    ) -> None: ...
