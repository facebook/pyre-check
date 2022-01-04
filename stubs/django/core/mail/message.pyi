# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

from email.mime.base import MIMEBase
from typing import Any, Iterable, Mapping, Optional, Sequence, Tuple, Union

from django.core.mail.backends.base import BaseEmailBackend

class SafeMIMEText: ...
class SafeMIMEMultipart: ...

class EmailMessage:
    def __init__(
        self,
        subject: str,
        body: str,
        from_email: Optional[str],
        to: Optional[Iterable[str]],
        bcc: Optional[Iterable[str]],
        connection: Any,
        attachments: Union[MIMEBase, Tuple[str, str, str]],
        headers: Mapping[str, str],
        cc: Optional[Iterable[str]],
        reply_to: Optional[Iterable[str]],
    ) -> None: ...
    def get_connection(self, fail_silently: bool = False) -> BaseEmailBackend: ...
    def message(self) -> Union[SafeMIMEText, SafeMIMEMultipart]: ...
    def recipients(self) -> Sequence[str]: ...
    def send(self, fail_silently: bool = False) -> int: ...
    # def attach(self, filename=None, content=None, mimetype=None): ...
    def attach_file(self, path: str, mimetype: Optional[str] = None) -> None: ...
