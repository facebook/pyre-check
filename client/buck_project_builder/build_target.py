# Copyright (c) 2019-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import ast
import os
from typing import Dict, List, Mapping, NamedTuple, Optional, Type  # noqa

from . import filesystem


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

    def __str__(self) -> str:
        return "{}(name={})".format(self.rule_name(), self.name)

    def __repr__(self) -> str:
        return str(self)

    @property
    def target(self) -> str:
        return "//{}:{}".format(self.build_file_directory, self.name)

    def rule_name(self) -> str:
        raise NotImplementedError

    def build(self, output_directory: str) -> None:
        source_directory = os.path.join(self.buck_root, self.build_file_directory)
        sources = filesystem.resolve_sources(source_directory, self.sources)
        if self.base_module is not None:
            base_path = os.path.join(*self.base_module.split("."))
        else:
            base_path = self.build_file_directory
        filesystem.link_paths(
            sources, source_directory, os.path.join(output_directory, base_path)
        )


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
        super(NonPythonTarget, self).__init__(
            buck_root, build_file_directory, base_information
        )

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
