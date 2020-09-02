# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from types import TracebackType
from typing import Iterable, Type, TypeVar

from django.core.mail.message import EmailMessage

_ExceptionT = TypeVar("_ExceptionT", bound="BaseException")

class BaseEmailBackend:
    def open(self) -> None: ...
    def close(self) -> None: ...
    def __enter__(self) -> None: ...
    def __exit__(
        self,
        exc_type: Type[_ExceptionT],
        exc_value: _ExceptionT,
        traceback: TracebackType,
    ) -> None: ...
    def send_messages(self, email_messages: Iterable[EmailMessage]) -> int: ...
