# pyre-unsafe

from typing import Any, Dict, Optional, Sequence, Type

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
