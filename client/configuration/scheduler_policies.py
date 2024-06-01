# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
This module specifies how one can tune the backend's map-reduce operations using
a "scheduler policies" configuration file. This can be used to improve
performance on specific machines.
"""

import dataclasses
import json
from pathlib import Path
from typing import Dict, Optional, Union

from .exceptions import InvalidConfiguration


@dataclasses.dataclass(frozen=True)
class FixedChunkSize:
    minimum_chunks_per_worker: int
    preferred_chunk_size: int
    minimum_chunk_size: Optional[int] = None


@dataclasses.dataclass(frozen=True)
class FixedChunkCount:
    minimum_chunk_size: int
    preferred_chunks_per_worker: int
    minimum_chunks_per_worker: Optional[int] = None


def optional_positive_int_member(
    object: Dict[str, object], key: str, identifier: str
) -> Optional[int]:
    if key not in object:
        return None

    value = object[key]
    if not isinstance(value, int):
        raise InvalidConfiguration(
            f"Invalid scheduler policy for `{identifier}`: expected integer for `{key}`, got `{value}`"
        )

    if value < 1:
        raise InvalidConfiguration(
            f"Invalid scheduler policy for `{identifier}`: expected positive integer for `{key}`, got `{value}`"
        )

    return value


def positive_int_member(object: Dict[str, object], key: str, identifier: str) -> int:
    value = optional_positive_int_member(object, key, identifier)

    if value is None:
        raise InvalidConfiguration(
            f"Invalid scheduler policy for `{identifier}`: missing `{key}` key in `{object}`"
        )

    return value


@dataclasses.dataclass(frozen=True)
class SchedulerPolicy:
    value: Union[FixedChunkSize, FixedChunkCount]

    @staticmethod
    def from_json(value: object, identifier: str) -> "SchedulerPolicy":
        if not isinstance(value, dict):
            raise InvalidConfiguration(
                f"Invalid scheduler policy for `{identifier}`: expected object, but got `{type(value).__name__}`"
            )

        if "kind" not in value:
            raise InvalidConfiguration(
                f"Invalid scheduler policy for `{identifier}`: missing `kind` key in `{value}`"
            )

        kind = value["kind"]
        if kind == "fixed_chunk_size":
            minimum_chunk_size = optional_positive_int_member(
                value, "minimum_chunk_size", identifier
            )
            minimum_chunks_per_worker = positive_int_member(
                value, "minimum_chunks_per_worker", identifier
            )
            preferred_chunk_size = positive_int_member(
                value, "preferred_chunk_size", identifier
            )
            return SchedulerPolicy(
                value=FixedChunkSize(
                    minimum_chunk_size=minimum_chunk_size,
                    minimum_chunks_per_worker=minimum_chunks_per_worker,
                    preferred_chunk_size=preferred_chunk_size,
                )
            )
        elif kind == "fixed_chunk_count":
            minimum_chunks_per_worker = optional_positive_int_member(
                value, "minimum_chunks_per_worker", identifier
            )
            minimum_chunk_size = positive_int_member(
                value, "minimum_chunk_size", identifier
            )
            preferred_chunks_per_worker = positive_int_member(
                value, "preferred_chunks_per_worker", identifier
            )
            return SchedulerPolicy(
                value=FixedChunkCount(
                    minimum_chunks_per_worker=minimum_chunks_per_worker,
                    minimum_chunk_size=minimum_chunk_size,
                    preferred_chunks_per_worker=preferred_chunks_per_worker,
                )
            )
        else:
            raise InvalidConfiguration(
                f"Invalid scheduler policy kind: got `{kind}`, expected `fixed_chunk_size` or `fixed_chunk_count`"
            )

    def to_json(self) -> Dict[str, Union[int, str]]:
        value = self.value
        if isinstance(value, FixedChunkSize):
            minimum_chunk_size = value.minimum_chunk_size
            return {
                "kind": "fixed_chunk_size",
                **(
                    {"minimum_chunk_size": minimum_chunk_size}
                    if minimum_chunk_size is not None
                    else {}
                ),
                "minimum_chunks_per_worker": value.minimum_chunks_per_worker,
                "preferred_chunk_size": value.preferred_chunk_size,
            }
        elif isinstance(value, FixedChunkCount):
            minimum_chunks_per_worker = value.minimum_chunks_per_worker
            return {
                "kind": "fixed_chunk_count",
                **(
                    {"minimum_chunks_per_worker": minimum_chunks_per_worker}
                    if minimum_chunks_per_worker is not None
                    else {}
                ),
                "minimum_chunk_size": value.minimum_chunk_size,
                "preferred_chunks_per_worker": value.preferred_chunks_per_worker,
            }
        else:
            raise AssertionError("unexpected policy")


@dataclasses.dataclass(frozen=True)
class SchedulerPolicies:
    policies: Dict[str, SchedulerPolicy] = dataclasses.field(default_factory=dict)

    @staticmethod
    def from_json(value: object) -> "SchedulerPolicies":
        if not isinstance(value, dict):
            raise InvalidConfiguration(
                f"Expected object for scheduler policies, but got `{type(value).__name__}`"
            )

        return SchedulerPolicies(
            policies={
                name: SchedulerPolicy.from_json(policy, name)
                for name, policy in value.items()
            }
        )

    @staticmethod
    def from_path(path: Path) -> "SchedulerPolicies":
        with open(path, "r") as f:
            try:
                return SchedulerPolicies.from_json(json.load(f))
            except json.JSONDecodeError as error:
                raise InvalidConfiguration(
                    f"Error while parsing `{str(path)}`: {error.lineno}:{error.colno}: {error.msg}"
                ) from error

    def to_json(self) -> Dict[str, Dict[str, Union[int, str]]]:
        return {name: policy.to_json() for name, policy in self.policies.items()}
