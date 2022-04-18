# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import dataclasses
import enum
import logging
from pathlib import Path
from typing import Dict, List, Iterable, Optional, Union

from . import search_path

LOG: logging.Logger = logging.getLogger(__name__)
MARKER_FILE: str = "py.typed"
STUBS_SUFFIX: str = "-stubs"


class SearchStrategy(str, enum.Enum):
    NONE: str = "none"
    ALL: str = "all"
    PEP561: str = "pep561"

    def __str__(self) -> str:
        return self.value

    @staticmethod
    def from_string(input: str) -> "Optional[SearchStrategy]":
        try:
            return next(item for item in SearchStrategy if item.value == input)
        except StopIteration:
            return None


class PackageStatus(enum.IntEnum):
    UNTYPED: int = 0
    PARTIALLY_TYPED: int = 1
    TYPED: int = 2


@dataclasses.dataclass(frozen=True)
class NonStubPackage:
    name: str
    path: Path  # NOTE: parent of this path would be the site root
    is_typed: bool = False


@dataclasses.dataclass(frozen=True)
class StubPackage:
    name: str
    path: Path  # NOTE: parent of this path would be the site root
    is_partial: bool = False


# For the same name, it is possible for both a stub package and a non-stub package
# to co-exist. This class is used to group the two.
@dataclasses.dataclass(frozen=True)
class PackageInfo:
    nonstub_package: Optional[NonStubPackage] = None
    stub_package: Optional[StubPackage] = None


def is_valid_package_name(name: str) -> bool:
    if len(name) == 0:
        return False
    if name == "__pycache__":
        return False
    if "." in name:
        return False
    return True


def get_package_status(path: Path) -> PackageStatus:
    marker_path = path / MARKER_FILE
    try:
        # lint-ignore: NoUnsafeFilesystemRule
        with open(marker_path, "r") as marker_file:
            first_line = marker_file.readline().strip()
        if first_line == "partial":
            return PackageStatus.PARTIALLY_TYPED
        else:
            return PackageStatus.TYPED
    except FileNotFoundError:
        return PackageStatus.UNTYPED
    except Exception as error:
        LOG.debug(
            f"Exception raised while getting package status for `{path}``: {error}"
        )
        return PackageStatus.UNTYPED


def create_package_from_path(path: Path) -> Union[StubPackage, NonStubPackage, None]:
    if not path.is_dir():
        return None
    name = path.name
    if not is_valid_package_name(name):
        return None
    status = get_package_status(path)
    if name.endswith(STUBS_SUFFIX):
        return StubPackage(
            name=name[: -len(STUBS_SUFFIX)],
            path=path,
            is_partial=(status == PackageStatus.PARTIALLY_TYPED),
        )
    else:
        return NonStubPackage(
            name=name,
            path=path,
            is_typed=(status != PackageStatus.UNTYPED),
        )


def find_packages(site_roots: Iterable[str]) -> List[PackageInfo]:
    nonstub_packages: Dict[str, NonStubPackage] = {}
    stub_packages: Dict[str, StubPackage] = {}
    for site_root in site_roots:
        site_root_path = Path(site_root)
        if not site_root_path.is_dir():
            continue
        for site_path in site_root_path.iterdir():
            package = create_package_from_path(site_path)
            if isinstance(package, NonStubPackage):
                nonstub_packages.setdefault(package.name, package)
            elif isinstance(package, StubPackage):
                stub_packages.setdefault(package.name, package)

    return [
        PackageInfo(
            nonstub_package=nonstub_packages.get(name, None),
            stub_package=stub_packages.get(name, None),
        )
        for name in nonstub_packages.keys() | stub_packages.keys()
    ]


def search_for_paths(
    strategy: SearchStrategy, site_roots: Iterable[str]
) -> List[search_path.Element]:
    if strategy == SearchStrategy.NONE:
        return []
    elif strategy == SearchStrategy.ALL:
        return [search_path.SimpleElement(root) for root in site_roots]
    elif strategy == SearchStrategy.PEP561:
        raise NotImplementedError
    else:
        raise RuntimeError(f"Unhandled site package search strategy: {strategy}")
