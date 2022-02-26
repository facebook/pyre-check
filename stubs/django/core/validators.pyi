# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

from typing import Any, Optional

from django.core.exceptions import ValidationError as ValidationError

class EmailValidator:
    def __call__(self, value: Optional[str]) -> None: ...

class URLValidator:
    def __call__(self, value: Optional[str]) -> None: ...

class RegexValidator(object):
    def __init__(
        self, regex=..., message=..., code=..., inverse_match=..., flags=...
    ) -> None: ...
    def __call__(self, value: Optional[str]) -> None: ...

validate_email: EmailValidator = ...
