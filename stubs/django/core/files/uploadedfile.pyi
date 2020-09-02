# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

from typing import Any

from django.core.files.base import File

class UploadedFile(File):
    size: Any
    content_type: Any
    charset: Any
    content_type_extra: Any
    name: str
    def __init__(
        self,
        file=None,
        name=None,
        content_type=None,
        size=None,
        charset=None,
        content_type_extra=None,
    ) -> None: ...
    def read(self) -> bytes: ...

class InMemoryUploadedFile(UploadedFile):
    field_name: Any
    def __init__(
        self,
        file,
        field_name,
        name,
        content_type,
        size,
        charset,
        content_type_extra=None,
    ) -> None: ...

class SimpleUploadedFile(File):
    def __init__(self, name, content, content_type="text/plain") -> None: ...
