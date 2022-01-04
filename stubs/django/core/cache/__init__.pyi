# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

from typing import Any

from caching.backends.facebookcache import FacebookMemcacheCache

class CacheHandler:
    def __getitem__(self, alias: Any) -> FacebookMemcacheCache: ...

caches: CacheHandler
