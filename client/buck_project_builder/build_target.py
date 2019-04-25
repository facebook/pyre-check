# Copyright (c) 2019-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import ast
import distutils.dir_util
import os
import shutil
import tempfile
from typing import (  # noqa
    Dict,
    Iterable,
    List,
    Mapping,
    NamedTuple,
    Optional,
    Tuple,
    Type,
)

from . import filesystem, platform
from ..filesystem import add_symbolic_link, find_python_paths


class BuildTarget:
    # A minimal set of information that can be parsed from (almost) all targets.
    BaseInformation = NamedTuple(
        "BaseInformation",
        [
            ("keywords", Dict[str, ast.expr]),
            ("name", str),
            ("dependencies", List[str]),
            ("sources", filesystem.Sources),
            ("base_module", Optional[str]),
            ("external_dependencies", List[Tuple[str, str]]),
            ("version_subdirectory", Optional[str]),
        ],
    )

    def __init__(
        self,
        buck_root: str,
        build_file_directory: str,
        base_information: BaseInformation,
    ) -> None:
        self.buck_root = buck_root
        self.build_file_directory = build_file_directory
        self.name = base_information.name  # type: str
        self.dependencies = base_information.dependencies  # type: List[str]
        self.sources = base_information.sources  # type: filesystem.Sources
        self.base_module = base_information.base_module  # type: Optional[str]
        self.external_dependencies = (
            base_information.external_dependencies
        )  # type: List[Tuple[str, str]]
        self.version_subdirectory = (
            base_information.version_subdirectory
        )  # type: Optional[str]

    def __str__(self) -> str:
        return "{}(name={})".format(self.rule_name(), self.name)

    def __repr__(self) -> str:
        return str(self)

    def __hash__(self) -> int:
        return hash((type(self), self.buck_root, self.target))

    def __eq__(self, other: object) -> bool:
        return (
            isinstance(other, type(self))
            and self.buck_root == other.buck_root
            and self.target == other.target
        )

    @property
    def target(self) -> str:
        return "//{}:{}".format(self.build_file_directory, self.name)

    def rule_name(self) -> str:
        raise NotImplementedError

    def build(self, output_directory: str) -> None:
        source_directory = os.path.join(self.buck_root, self.build_file_directory)
        if self.version_subdirectory:
            source_directory = os.path.join(source_directory, self.version_subdirectory)

        if self.base_module is not None:
            base_path = os.path.join(*self.base_module.split("."))
        else:
            base_path = self.build_file_directory
        output_directory = os.path.join(output_directory, base_path)
        source_mapping = filesystem.resolve_source_mapping(
            source_directory, output_directory, self.sources
        )
        for source_path, output_path in source_mapping.items():
            add_symbolic_link(output_path, source_path)


class NonPythonTarget(BuildTarget):
    def __init__(
        self,
        buck_root: str,
        build_file_directory: str,
        base_information: BuildTarget.BaseInformation,
        rule_name: str,
    ) -> None:
        self._rule_name = rule_name
        # We don't want to include dependencies of these targets.
        base_information = base_information._replace(dependencies=[])
        super().__init__(buck_root, build_file_directory, base_information)

    def rule_name(self) -> str:
        return self._rule_name

    def build(self, output_directory: str) -> None:
        pass


class PythonBinary(BuildTarget):
    def rule_name(self) -> str:
        return "python_binary"


class PythonLibrary(BuildTarget):
    def rule_name(self) -> str:
        return "python_library"


class PythonUnitTest(BuildTarget):
    def rule_name(self) -> str:
        return "python_unittest"


class ThriftLibrary(BuildTarget):
    def __init__(
        self,
        buck_root: str,
        build_file_directory: str,
        base_information: BuildTarget.BaseInformation,
        thrift_sources: Iterable[str],
        include_json_converters: bool,
    ) -> None:
        super().__init__(buck_root, build_file_directory, base_information)
        self._thrift_sources = thrift_sources
        self._include_json_converters = include_json_converters

    def rule_name(self) -> str:
        return "thrift_library"

    def build(self, output_directory: str) -> None:
        thrift_sources_relative_to_buck_root = [
            os.path.join(self.build_file_directory, source)
            for source in self._thrift_sources
        ]
        with tempfile.TemporaryDirectory() as temporary_directory:
            thrift_output_directory = filesystem.build_thrift_stubs(
                self.buck_root,
                thrift_sources_relative_to_buck_root,
                temporary_directory,
                include_json_converters=self._include_json_converters,
            )

            for output_file in find_python_paths(thrift_output_directory):
                # Thrift generates __init__.pyi files which Pyre doesn't need.
                if os.path.basename(output_file) == "__init__.pyi":
                    continue
                relative_path = os.path.relpath(output_file, thrift_output_directory)
                destination_path = os.path.join(output_directory, relative_path)
                os.makedirs(os.path.dirname(destination_path), exist_ok=True)
                shutil.copy2(output_file, destination_path)


class PythonWheel(BuildTarget):
    VersionedWheel = NamedTuple(
        "VersionedWheel",
        [
            ("version", str),
            ("url_mapping", Mapping[str, str]),  # From platform to wheel url.
            ("dependencies", Iterable[str]),
            ("external_dependencies", Iterable[Tuple[str, str]]),
        ],
    )
    PlatformInformation = NamedTuple(
        "PlatformInformation",
        [
            ("platform", str),
            ("version", str),
            ("url", str),
            ("dependencies", Iterable[str]),
            ("external_dependencies", Iterable[Tuple[str, str]]),
        ],
    )

    def __init__(
        self,
        buck_root: str,
        build_file_directory: str,
        base_information: BuildTarget.BaseInformation,
        platforms_to_wheel_version: Mapping[str, str],
        wheel_versions_mapping: Mapping[str, VersionedWheel],
    ) -> None:
        super(PythonWheel, self).__init__(
            buck_root, build_file_directory, base_information
        )

        build_platform = platform.get_platform()
        python_major_version, _ = platform.get_python_version()

        platform_information = PythonWheel._get_platform_information(
            platforms_to_wheel_version,
            wheel_versions_mapping,
            build_platform,
            python_major_version,
        )
        if not platform_information:
            raise ValueError(
                "No suitable wheel could be found "
                "for {} on Python {}, platform {}.".format(
                    self.target, python_major_version, build_platform
                )
            )
        self._platform = platform_information.platform  # type: str
        self._version = platform_information.version  # type: str
        self._url = platform_information.url  # type: str

        self.dependencies.extend(platform_information.dependencies)
        self.external_dependencies.extend(platform_information.external_dependencies)

    def rule_name(self) -> str:
        return "python_wheel"

    @staticmethod
    def _get_platform_information(
        platforms_to_wheel_version: Mapping[str, str],
        wheel_versions_mapping: Mapping[str, VersionedWheel],
        build_platform: str,
        python_major_version: int,
    ) -> Optional[PlatformInformation]:
        try:
            python_platform = "py{}-{}".format(python_major_version, build_platform)
            wheel_version = platforms_to_wheel_version[python_platform]
            versioned_wheel = wheel_versions_mapping[wheel_version]
            return PythonWheel.PlatformInformation(
                platform=python_platform,
                version=wheel_version,
                url=versioned_wheel.url_mapping[python_platform],
                dependencies=versioned_wheel.dependencies,
                external_dependencies=versioned_wheel.external_dependencies,
            )
        except KeyError:
            pass

    def build(self, output_directory: str) -> None:
        with tempfile.TemporaryDirectory(prefix="pyre_tmp_") as temporary_directory:
            filesystem.download_and_extract_zip_file(self._url, temporary_directory)
            distutils.dir_util.copy_tree(temporary_directory, output_directory)
