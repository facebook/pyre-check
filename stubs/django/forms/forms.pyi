# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

from typing import Any, Dict, Optional, Sequence, Type

from django.forms.utils import ErrorDict

class Form:
    cleaned_data: Dict[str, Any] = ...
    def __init__(
        self,
        data: Optional[Dict[str, Any]] = ...,
        files: Optional[Any] = ...,
        auto_id: str = ...,
        prefix: Optional[str] = ...,
        initial: Optional[Dict[str, Any]] = ...,
        error_class: Type = ...,
        label_suffix: Optional[str] = ...,
        empty_permitted: bool = ...,
        field_order: Optional[Sequence[str]] = ...,
        use_required_attribute: Optional[bool] = ...,
        renderer: Optional[Any] = ...,
    ) -> None: ...
    def clean(self) -> Dict[str, Any]: ...
    def is_valid(self) -> bool: ...
    @property
    def errors(self) -> ErrorDict: ...
