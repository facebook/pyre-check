# Copyright (c) 2019-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import List, Mapping, Optional, Tuple

from ..build_target import BuildTarget
from ..filesystem import Sources


def base(
    name: str,
    dependencies: Optional[List[str]] = None,
    sources: Optional[Sources] = None,
    base_module: Optional[str] = None,
    external_dependencies: Optional[List[Tuple[str, str]]] = None,
) -> BuildTarget.BaseInformation:
    return BuildTarget.BaseInformation(
        keywords={},
        name=name,
        dependencies=dependencies or [],
        sources=sources or Sources(),
        base_module=base_module,
        external_dependencies=external_dependencies or [],
    )


def identity_mapping(files: List[str]) -> Mapping[str, str]:
    return dict(zip(files, files))
