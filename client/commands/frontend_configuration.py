# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
TODO(T132414938) Add a module-level docstring
"""


import abc
import json
from pathlib import Path
from typing import List, Optional

from .. import configuration as configuration_module

# TODO(T120824066): Break this class down into smaller pieces. Ideally, one
# class per command.
class Base(abc.ABC):
    @abc.abstractmethod
    def get_dot_pyre_directory(self) -> Path:
        raise NotImplementedError()

    @abc.abstractmethod
    def get_log_directory(self) -> Path:
        raise NotImplementedError()

    @abc.abstractmethod
    def get_binary_location(self, download_if_needed: bool = False) -> Optional[Path]:
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
    def get_existent_search_paths(
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
    def get_other_critical_files(self) -> List[str]:
        raise NotImplementedError()

    @abc.abstractmethod
    def get_taint_models_path(self) -> List[str]:
        raise NotImplementedError()

    @abc.abstractmethod
    def get_project_identifier(self) -> str:
        raise NotImplementedError()

    def get_local_root(self) -> Optional[Path]:
        relative_local_root = self.get_relative_local_root()
        if relative_local_root is None:
            return None
        return self.get_global_root() / relative_local_root


class OpenSource(Base):
    def __init__(
        self,
        configuration: configuration_module.Configuration,
    ) -> None:
        self.configuration = configuration

    def get_dot_pyre_directory(self) -> Path:
        return self.configuration.dot_pyre_directory

    def get_log_directory(self) -> Path:
        return Path(self.configuration.log_directory)

    def get_binary_location(self, download_if_needed: bool = False) -> Optional[Path]:
        location = self.configuration.get_binary_respecting_override()
        # Auto-download is not supported in OSS
        return Path(location) if location is not None else None

    def get_binary_version(self) -> Optional[str]:
        return self.configuration.get_binary_version()

    def get_content_for_display(self) -> str:
        return json.dumps(self.configuration.to_json(), indent=2)

    def get_global_root(self) -> Path:
        return Path(self.configuration.project_root)

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

    def get_existent_search_paths(
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

    def get_other_critical_files(self) -> List[str]:
        return list(self.configuration.other_critical_files)

    def get_taint_models_path(self) -> List[str]:
        return list(self.configuration.taint_models_path)

    def get_project_identifier(self) -> str:
        return self.configuration.project_identifier
