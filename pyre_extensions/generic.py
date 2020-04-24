# pyre-ignore-all-errors
from typing import Any


class GenericMeta(type):
    def __getitem__(cls, *args) -> Any:
        return cls.__class__(cls.__name__, cls.__bases__, dict(cls.__dict__))


class Generic(metaclass=GenericMeta):
    pass
