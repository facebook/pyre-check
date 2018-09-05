from typing import Any


class LazySettings:
    def __getattr__(self, name: str) -> Any:
        ...


class BaseSettings:
    def __setattr__(self, name: str, value: Any):
        ...


class Settings(BaseSettings):
    ...


settings: LazySettings = ...
