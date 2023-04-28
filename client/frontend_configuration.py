# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
This module contains Python classes representing the information found
in a Pyre configuration file (.pyre_configuration).

The implementation is split into Base and OpenSource so that it is
possible to customize Pyre by implementing a new command-line tool
with additional configuration, using open-source Pyre as a library.
"""


import abc
import json
import subprocess
from pathlib import Path
from typing import List, Optional

from . import configuration as configuration_module, find_directories

# TODO(T120824066): Break this class down into smaller pieces. Ideally, one
# class per command.
class Base(abc.ABC):
    @abc.abstractmethod
    def get_dot_pyre_directory(self) -> Path:
        raise NotImplementedError()

    @abc.abstractmethod
    def get_binary_location(self, download_if_needed: bool = False) -> Optional[Path]:
        raise NotImplementedError()

    @abc.abstractmethod
    def get_typeshed_location(self, download_if_needed: bool = False) -> Optional[Path]:
        raise NotImplementedError()

    @abc.abstractmethod
    def get_binary_version(self) -> Optional[str]:
        raise NotImplementedError()

    @abc.abstractmethod
    def get_content_for_display(self) -> str:
        raise NotImplementedError()

    @abc.abstractmethod
    def get_global_root(self) -> Path:
        raise NotImplementedError()

    @abc.abstractmethod
    def get_relative_local_root(self) -> Optional[str]:
        raise NotImplementedError()

    @abc.abstractmethod
    def get_excludes(self) -> List[str]:
        raise NotImplementedError()

    @abc.abstractmethod
    def is_strict(self) -> bool:
        raise NotImplementedError()

    @abc.abstractmethod
    def get_remote_logger(self) -> Optional[str]:
        raise NotImplementedError()

    @abc.abstractmethod
    def get_number_of_workers(self) -> int:
        raise NotImplementedError()

    @abc.abstractmethod
    def get_python_version(self) -> configuration_module.PythonVersion:
        raise NotImplementedError()

    @abc.abstractmethod
    def get_shared_memory(self) -> configuration_module.SharedMemory:
        raise NotImplementedError()

    @abc.abstractmethod
    def get_valid_extension_suffixes(self) -> List[str]:
        raise NotImplementedError()

    @abc.abstractmethod
    def get_ignore_all_errors(self) -> List[str]:
        raise NotImplementedError()

    @abc.abstractmethod
    def get_only_check_paths(self) -> List[str]:
        raise NotImplementedError()

    @abc.abstractmethod
    def get_existent_user_specified_search_paths(
        self,
    ) -> List[configuration_module.search_path.Element]:
        raise NotImplementedError()

    @abc.abstractmethod
    def get_existent_source_directories(
        self,
    ) -> List[configuration_module.search_path.Element]:
        raise NotImplementedError()

    @abc.abstractmethod
    def get_existent_unwatched_dependency(
        self,
    ) -> Optional[configuration_module.unwatched.UnwatchedDependency]:
        raise NotImplementedError()

    @abc.abstractmethod
    def is_source_directories_defined(
        self,
    ) -> bool:
        raise NotImplementedError()

    @abc.abstractmethod
    def get_buck_targets(
        self,
    ) -> Optional[List[str]]:
        raise NotImplementedError()

    @abc.abstractmethod
    def uses_buck2(self) -> bool:
        raise NotImplementedError()

    @abc.abstractmethod
    def get_buck_mode(self) -> Optional[str]:
        raise NotImplementedError()

    @abc.abstractmethod
    def get_buck_isolation_prefix(self) -> Optional[str]:
        raise NotImplementedError()

    @abc.abstractmethod
    def get_buck_bxl_builder(self) -> Optional[str]:
        raise NotImplementedError()

    @abc.abstractmethod
    def get_other_critical_files(self) -> List[str]:
        raise NotImplementedError()

    @abc.abstractmethod
    def get_taint_models_path(self) -> List[str]:
        raise NotImplementedError()

    @abc.abstractmethod
    def get_project_identifier(self) -> str:
        raise NotImplementedError()

    @abc.abstractmethod
    def get_enable_readonly_analysis(self) -> Optional[bool]:
        raise NotImplementedError()

    @abc.abstractmethod
    def get_enable_unawaited_awaitable_analysis(self) -> Optional[bool]:
        raise NotImplementedError()

    @abc.abstractmethod
    def get_saved_state_project(self) -> Optional[str]:
        raise NotImplementedError()

    @abc.abstractmethod
    def get_include_suppressed_errors(self) -> Optional[bool]:
        raise NotImplementedError()

    def get_use_errpy_parser(self) -> bool:
        raise NotImplementedError()

    def get_local_root(self) -> Optional[Path]:
        relative_local_root = self.get_relative_local_root()
        if relative_local_root is None:
            return None
        return self.get_global_root() / relative_local_root

    def get_log_directory(self) -> Path:
        dot_pyre_directory = self.get_dot_pyre_directory()
        relative_local_root = self.get_relative_local_root()
        return (
            dot_pyre_directory
            if relative_local_root is None
            else dot_pyre_directory / relative_local_root
        )

    def get_existent_typeshed_search_paths(
        self,
    ) -> List[configuration_module.search_path.Element]:
        typeshed_root = self.get_typeshed_location(download_if_needed=True)
        if typeshed_root is None:
            return []
        return [
            configuration_module.search_path.SimpleElement(str(element))
            for element in find_directories.find_typeshed_search_paths(typeshed_root)
        ]

    def get_existent_search_paths(
        self,
    ) -> List[configuration_module.search_path.Element]:
        return [
            *self.get_existent_user_specified_search_paths(),
            *self.get_existent_typeshed_search_paths(),
        ]


class OpenSource(Base):
    def __init__(
        self,
        configuration: configuration_module.Configuration,
    ) -> None:
        self.configuration = configuration

    def get_dot_pyre_directory(self) -> Path:
        return (
            self.configuration.dot_pyre_directory
            or self.get_global_root() / find_directories.LOG_DIRECTORY
        )

    def get_binary_location(self, download_if_needed: bool = False) -> Optional[Path]:
        location = self.configuration.get_binary_respecting_override()
        # Auto-download is not supported in OSS
        return Path(location) if location is not None else None

    def get_typeshed_location(self, download_if_needed: bool = False) -> Optional[Path]:
        location = self.configuration.get_typeshed_respecting_override()
        # Auto-download is not supported in OSS
        return Path(location) if location is not None else None

    def get_binary_version(self) -> Optional[str]:
        binary = self.configuration.get_binary_respecting_override()
        if binary is None:
            return None
        # lint-ignore: NoUnsafeExecRule
        status = subprocess.run(
            [binary, "-version"], stdout=subprocess.PIPE, universal_newlines=True
        )
        return status.stdout.strip() if status.returncode == 0 else None

    def get_content_for_display(self) -> str:
        return json.dumps(self.configuration.to_json(), indent=2)

    def get_global_root(self) -> Path:
        return self.configuration.global_root

    def get_relative_local_root(self) -> Optional[str]:
        return self.configuration.relative_local_root

    def get_excludes(self) -> List[str]:
        return list(self.configuration.excludes)

    def is_strict(self) -> bool:
        return self.configuration.strict

    def get_remote_logger(self) -> Optional[str]:
        return self.configuration.logger

    def get_number_of_workers(self) -> int:
        return self.configuration.get_number_of_workers()

    def get_python_version(self) -> configuration_module.PythonVersion:
        return self.configuration.get_python_version()

    def get_shared_memory(self) -> configuration_module.SharedMemory:
        return self.configuration.shared_memory

    def get_valid_extension_suffixes(self) -> List[str]:
        return self.configuration.get_valid_extension_suffixes()

    def get_ignore_all_errors(self) -> List[str]:
        return list(self.configuration.ignore_all_errors)

    def get_only_check_paths(self) -> List[str]:
        return list(self.configuration.only_check_paths)

    def get_existent_user_specified_search_paths(
        self,
    ) -> List[configuration_module.search_path.Element]:
        return self.configuration.expand_and_get_existent_search_paths()

    def get_existent_source_directories(
        self,
    ) -> List[configuration_module.search_path.Element]:
        return self.configuration.expand_and_get_existent_source_directories()

    def get_existent_unwatched_dependency(
        self,
    ) -> Optional[configuration_module.unwatched.UnwatchedDependency]:
        return self.configuration.get_existent_unwatched_dependency()

    def is_source_directories_defined(
        self,
    ) -> bool:
        return self.configuration.source_directories is not None

    def get_buck_targets(
        self,
    ) -> Optional[List[str]]:
        targets = self.configuration.targets
        return list(targets) if targets is not None else None

    def uses_buck2(self) -> bool:
        return self.configuration.use_buck2

    def get_buck_mode(self) -> Optional[str]:
        mode = self.configuration.buck_mode
        return mode.get() if mode is not None else None

    def get_buck_isolation_prefix(self) -> Optional[str]:
        return self.configuration.isolation_prefix

    def get_buck_bxl_builder(self) -> Optional[str]:
        return self.configuration.bxl_builder

    def get_other_critical_files(self) -> List[str]:
        return list(self.configuration.other_critical_files)

    def get_taint_models_path(self) -> List[str]:
        return list(self.configuration.taint_models_path)

    def get_enable_readonly_analysis(self) -> Optional[bool]:
        return self.configuration.enable_readonly_analysis

    def get_enable_unawaited_awaitable_analysis(self) -> Optional[bool]:
        return self.configuration.enable_unawaited_awaitable_analysis

    def get_project_identifier(self) -> str:
        return self.configuration.project_identifier

    def get_saved_state_project(self) -> Optional[str]:
        return None

    def get_include_suppressed_errors(self) -> Optional[bool]:
        return self.configuration.include_suppressed_errors

    def get_use_errpy_parser(self) -> bool:
        return self.configuration.use_errpy_parser
