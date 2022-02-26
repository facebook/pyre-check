# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from types import TracebackType
from typing import NoReturn, Optional, Type

def reraise(
    exc_type: Optional[Type[BaseException]],
    exc_value: Optional[BaseException],
    exc_traceback: Optional[TracebackType],
) -> NoReturn: ...
def text_type(
    object: object = b"", encoding: str = "utf-8", errors: str = "strict"
) -> str: ...
