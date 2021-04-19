from django.db.models.manager import Manager as Manager
from django.db.models.options import Options as Options

class Model:
    objects: Manager = ...
    _meta: Options = ...
    def __init__(self, *args, **kwargs) -> None: ...
    def save(
        self,
        force_insert: bool = ...,
        force_update: bool = ...,
        using: Optional[str] = ...,
        update_fields: Optional[Iterable[str]] = ...,
    ) -> None: ...
    def save_base(
        self,
        raw: bool = ...,
        force_insert: bool = ...,
        force_update: bool = ...,
        using: Optional[str] = ...,
        update_fields: Optional[Iterable[str]] = ...,
    ): ...
