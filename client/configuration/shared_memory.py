# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
This module specifies how one can tune the backend's shared memory settings
in a .pyre_configuration file. This is needed because for large projects we
might want a larger heap and fatter hash table than the defaults.
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
