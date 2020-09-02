# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

from typing import Any, Dict, Optional, Type

from django.db.models import Model
from django.forms.forms import Form

class ModelForm(Form):
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
        instance: Optional[Model] = ...,
        use_required_attribute: Optional[bool] = ...,
        renderer: Optional[Any] = ...,
    ) -> None: ...
    def save(self, commit: bool = ...) -> Model: ...
