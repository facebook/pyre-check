# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
TODO(T132414938) Add a module-level docstring
"""


import dataclasses
from typing import Dict, Optional

from .. import dataclasses_merge


@dataclasses_merge.dataclass_merge
@dataclasses.dataclass(frozen=True)
class SharedMemory:
    heap_size: Optional[int] = None
    dependency_table_power: Optional[int] = None
    hash_table_power: Optional[int] = None

    def to_json(self) -> Dict[str, int]:
        heap_size = self.heap_size
        dependency_table_power = self.dependency_table_power
        hash_table_power = self.hash_table_power
        return {
            **({"heap_size": heap_size} if heap_size is not None else {}),
            **(
                {"dependency_table_power": dependency_table_power}
                if dependency_table_power is not None
                else {}
            ),
            **(
                {"hash_table_power": hash_table_power}
                if hash_table_power is not None
                else {}
            ),
        }
