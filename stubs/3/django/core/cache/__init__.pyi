from typing import Any


class CacheHandler:
    def __getitem__(self, alias: Any) -> Any:
        ...


caches: CacheHandler
