# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import abc
import dataclasses
import glob
import logging
import os
from typing import (
    List,
)

from . import filesystem

LOG: logging.Logger = logging.getLogger(__name__)


def _expand_relative_root(path: str, relative_root: str) -> str:
    if not path.startswith("//"):
        return filesystem.expand_relative_path(relative_root, path)
    return path


class Element(abc.ABC):
    @abc.abstractmethod
    def path(self) -> str:
        raise NotImplementedError

    @abc.abstractmethod
    def get_root(self) -> str:
        raise NotImplementedError

    @abc.abstractmethod
    def command_line_argument(self) -> str:
        raise NotImplementedError

    @abc.abstractmethod
    def expand_global_root(self, global_root: str) -> "Element":
        raise NotImplementedError

    @abc.abstractmethod
    def expand_relative_root(self, relative_root: str) -> "Element":
        raise NotImplementedError

    @abc.abstractmethod
    def expand_glob(self) -> List["Element"]:
        raise NotImplementedError


@dataclasses.dataclass(frozen=True)
class SimpleElement(Element):
    root: str

    def path(self) -> str:
        return self.root

    def get_root(self) -> str:
        return self.root

    def command_line_argument(self) -> str:
        return self.root

    def expand_global_root(self, global_root: str) -> "SimpleElement":
        return SimpleElement(
            filesystem.expand_global_root(self.root, global_root=global_root)
        )

    def expand_relative_root(self, relative_root: str) -> "SimpleElement":
        return SimpleElement(
            _expand_relative_root(self.root, relative_root=relative_root)
        )

    def expand_glob(self) -> List[Element]:
        expanded = sorted(glob.glob(self.get_root()))
        if expanded:
            return [SimpleElement(path) for path in expanded]
        else:
            LOG.warning(f"'{self.path()}' does not match any paths.")
            return []


@dataclasses.dataclass(frozen=True)
class SubdirectoryElement(Element):
    root: str
    subdirectory: str

    def path(self) -> str:
        return os.path.join(self.root, self.subdirectory)

    def get_root(self) -> str:
        return self.root

    def command_line_argument(self) -> str:
        return self.root + "$" + self.subdirectory

    def expand_global_root(self, global_root: str) -> "SubdirectoryElement":
        return SubdirectoryElement(
            root=filesystem.expand_global_root(self.root, global_root=global_root),
            subdirectory=self.subdirectory,
        )

    def expand_relative_root(self, relative_root: str) -> "SubdirectoryElement":
        return SubdirectoryElement(
            root=_expand_relative_root(self.root, relative_root=relative_root),
            subdirectory=self.subdirectory,
        )

    def expand_glob(self) -> List[Element]:
        return [self]


@dataclasses.dataclass(frozen=True)
class SitePackageElement(Element):
    site_root: str
    package_name: str
    is_toplevel_module: bool = False

    def package_path(self) -> str:
        module_suffix = ".py" if self.is_toplevel_module else ""
        return self.package_name + module_suffix

    def path(self) -> str:
        return os.path.join(self.site_root, self.package_path())

    def get_root(self) -> str:
        return self.site_root

    def command_line_argument(self) -> str:
        return self.site_root + "$" + self.package_path()

    def expand_global_root(self, global_root: str) -> "SitePackageElement":
        # Site package does not participate in root expansion.
        return self

    def expand_relative_root(self, relative_root: str) -> "SitePackageElement":
        # Site package does not participate in root expansion.
        return self

    def expand_glob(self) -> List["Element"]:
        return [self]
