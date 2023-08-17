# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
This module specifies the format for search path specifications
in the Pyre configuration.

There are several options that modify how Pyre handles a search
path, see
https://pyre-check.org/docs/configuration/
for details.
"""


import abc
import dataclasses
import glob
import logging
import os
import re
from typing import Dict, Iterable, List, Sequence, Tuple, Union

from .. import filesystem
from . import exceptions

LOG: logging.Logger = logging.getLogger(__name__)
site_packages_in_root: Dict[str : Tuple[str]] = {}


def _expand_relative_root(path: str, relative_root: str) -> str:
    if not path.startswith("//"):
        return filesystem.expand_relative_path(relative_root, path)
    return path


class Element(abc.ABC):
    @abc.abstractmethod
    def path(self) -> str:
        raise NotImplementedError()

    @abc.abstractmethod
    def command_line_argument(self) -> str:
        raise NotImplementedError()


@dataclasses.dataclass(frozen=True)
class SimpleElement(Element):
    root: str

    def path(self) -> str:
        return self.root

    def command_line_argument(self) -> str:
        return self.root


@dataclasses.dataclass(frozen=True)
class SubdirectoryElement(Element):
    root: str
    subdirectory: str

    def path(self) -> str:
        return os.path.join(self.root, self.subdirectory)

    def command_line_argument(self) -> str:
        return self.root + "$" + self.subdirectory


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

    def command_line_argument(self) -> str:
        return self.site_root + "$" + self.package_path()


class RawElement(abc.ABC):
    @abc.abstractmethod
    def expand_global_root(self, global_root: str) -> "RawElement":
        raise NotImplementedError()

    @abc.abstractmethod
    def expand_relative_root(self, relative_root: str) -> "RawElement":
        raise NotImplementedError()

    @abc.abstractmethod
    def expand_glob(self) -> List["RawElement"]:
        raise NotImplementedError()


@dataclasses.dataclass(frozen=True)
class SimpleRawElement(RawElement):
    root: str

    def expand_global_root(self, global_root: str) -> "SimpleRawElement":
        return SimpleRawElement(
            filesystem.expand_global_root(self.root, global_root=global_root)
        )

    def expand_relative_root(self, relative_root: str) -> "SimpleRawElement":
        return SimpleRawElement(
            _expand_relative_root(self.root, relative_root=relative_root)
        )

    def expand_glob(self) -> List[RawElement]:
        expanded = sorted(glob.glob(self.root))
        if expanded:
            return [SimpleRawElement(path) for path in expanded]
        else:
            LOG.warning(f"'{self.root}' does not match any paths.")
            return []

    def to_element(self) -> SimpleElement:
        return SimpleElement(self.root)


@dataclasses.dataclass(frozen=True)
class SubdirectoryRawElement(RawElement):
    root: str
    subdirectory: str

    def expand_global_root(self, global_root: str) -> "SubdirectoryRawElement":
        return SubdirectoryRawElement(
            root=filesystem.expand_global_root(self.root, global_root=global_root),
            subdirectory=self.subdirectory,
        )

    def expand_relative_root(self, relative_root: str) -> "SubdirectoryRawElement":
        return SubdirectoryRawElement(
            root=_expand_relative_root(self.root, relative_root=relative_root),
            subdirectory=self.subdirectory,
        )

    def expand_glob(self) -> List[RawElement]:
        return [self]

    def to_element(self) -> SubdirectoryElement:
        return SubdirectoryElement(self.root, self.subdirectory)


@dataclasses.dataclass(frozen=True)
class SitePackageRawElement(RawElement):
    package_name: str
    is_toplevel_module: bool = False

    def package_path(self) -> str:
        module_suffix = ".py" if self.is_toplevel_module else ""
        return self.package_name + module_suffix

    def expand_global_root(self, global_root: str) -> "SitePackageRawElement":
        # Site package does not participate in root expansion.
        return self

    def expand_relative_root(self, relative_root: str) -> "SitePackageRawElement":
        # Site package does not participate in root expansion.
        return self

    def expand_glob(self) -> List["RawElement"]:
        return [self]

    def to_element(self, site_root: str) -> SitePackageElement:
        return SitePackageElement(site_root, self.package_name, self.is_toplevel_module)


def create_raw_element(json: Union[str, Dict[str, object]]) -> RawElement:
    if isinstance(json, str):
        return SimpleRawElement(json)
    elif isinstance(json, dict):

        def assert_string_item(input: Dict[str, object], name: str) -> str:
            value = input.get(name, None)
            if not isinstance(value, str):
                raise exceptions.InvalidConfiguration(
                    "Invalid search path element. "
                    f"Expected item `{name}` to be a string but got {value}"
                )
            return value

        if "root" in json and "subdirectory" in json:
            return SubdirectoryRawElement(
                root=assert_string_item(json, "root"),
                subdirectory=assert_string_item(json, "subdirectory"),
            )
        elif "import_root" in json and "source" in json:
            return SubdirectoryRawElement(
                root=assert_string_item(json, "import_root"),
                subdirectory=assert_string_item(json, "source"),
            )
        elif "site-package" in json:
            is_toplevel_module = (
                "is_toplevel_module" in json and json["is_toplevel_module"]
            )
            if not isinstance(is_toplevel_module, bool):
                raise exceptions.InvalidConfiguration(
                    "Invalid search path element. "
                    "Expected `is_toplevel_module` to be a boolean but "
                    f"got {is_toplevel_module}"
                )
            return SitePackageRawElement(
                package_name=assert_string_item(json, "site-package"),
                is_toplevel_module=bool(is_toplevel_module),
            )

    raise exceptions.InvalidConfiguration(
        f"Invalid JSON format for search path element: {json}"
    )


def process_raw_elements(
    raw_elements: Iterable[RawElement],
    site_roots: Sequence[str],
    required: bool = False,
) -> List[Element]:
    elements: List[Element] = []

    def verify_valid(element: Union[SimpleElement, SitePackageElement]) -> bool:
        # verify if the package_path or path(for SimpleElement) exist.(see facebook/pyre-check#773)
        if os.path.exists(element.path()):
            elements.append(element)
            return True
        if isinstance(element, SitePackageElement):
            if not os.path.exists(element.site_root):
                return False
            if element.site_root not in site_packages_in_root:
                site_packages_in_root[element.site_root] = tuple(
                    re.split(r"-([0-99]\.)*dist-info", package_path)[0]
                    for package_path in os.listdir(element.site_root)
                    if re.fullmatch(
                        rf"{element.package_name}-([0-99]\.)*dist-info", package_path
                    )
                    is not None
                )
            return element.package_name in site_packages_in_root[element.site_root]
        return False

    def add_if_exists(element: Union[SimpleElement, SitePackageElement]) -> None:
        if os.path.exists(element.path()):
            elements.append(element)

    for raw_element in raw_elements:
        expanded_raw_elements = raw_element.expand_glob()
        if len(expanded_raw_elements) == 0 and required:
            raise exceptions.InvalidConfiguration(
                f"Invalid path {raw_element}: does not exist."
            )
        for expanded_raw_element in expanded_raw_elements:
            if isinstance(expanded_raw_element, SitePackageRawElement):
                valid = False
                for site_root in site_roots:
                    element = expanded_raw_element.to_element(site_root)
                    valid = verify_valid(element)
                    if not valid:
                        break
                    add_if_exists(element)
                if not valid:
                    if required:
                        raise exceptions.InvalidConfiguration(
                            f"Invalid path {expanded_raw_element.package_name}: does not exist."
                        )
                    else:
                        LOG.warning(
                            "Site package does not exist: "
                            f"`{expanded_raw_element.package_name}`"
                        )
            elif isinstance(
                expanded_raw_element, (SimpleRawElement, SubdirectoryRawElement)
            ):
                element = expanded_raw_element.to_element()
                valid = verify_valid(element)
                if not valid:
                    if required:
                        raise exceptions.InvalidConfiguration(
                            f"Path does not exist for search path: {element}"
                        )
                    else:
                        LOG.warning(f"Path does not exist for search path: {element}")
                else:
                    add_if_exists(element)
            else:
                raise RuntimeError(
                    f"Unhandled raw search path element type: {expanded_raw_element}"
                )

    return elements
