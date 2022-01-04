# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

from typing import Any, Callable, Dict, Optional, Sequence, Type

from django.forms.widgets import Widget

class Field:
    widget: Type[Widget] = ...
    hidden_widget: Type[Widget] = ...
    default_validators: Sequence[Callable[[Any], None]] = ...
    default_error_messages: Dict[str, str] = ...
    def __init__(
        self,
        *,
        required: bool = ...,
        widget: Optional[Widget] = ...,
        label: Optional[str] = ...,
        initial: Optional[Any] = ...,
        help_text: str = ...,
        error_messages: Optional[Dict[str, str]] = ...,
        show_hidden_initial: bool = ...,
        validators: Sequence[Callable[[Any], None]] = ...,
        localize: bool = ...,
        disabled: bool = ...,
        label_suffix: Optional[str] = ...,
    ) -> None: ...

BooleanField = Field
EmailField = Field
