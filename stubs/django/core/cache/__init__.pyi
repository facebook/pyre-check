# pyre-unsafe

from typing import Any

from caching.backends.facebookcache import FacebookMemcacheCache

class CacheHandler:
    def __getitem__(self, alias: Any) -> FacebookMemcacheCache: ...

caches: CacheHandler
