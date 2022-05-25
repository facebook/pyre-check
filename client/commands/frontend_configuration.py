# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import abc
import json
from pathlib import Path
from typing import List, Optional

from ..configuration import configuration as configuration_module

# TODO(T120824066): Break this class down into smaller pieces. Ideally, one
# class per command.
class Base(abc.ABC):
    @abc.abstractmethod
    def get_dot_pyre_directory(self) -> Path:
        raise NotImplementedError

    @abc.abstractmethod
    def get_log_directory(self) -> Path:
        raise NotImplementedError

    @abc.abstractmethod
    def get_binary_location(self, download_if_needed: bool = False) -> Optional[Path]:
        raise NotImplementedError

    @abc.abstractmethod
    def get_binary_version(self) -> Optional[str]:
        raise NotImplementedError

    @abc.abstractmethod
    def get_content_for_display(self) -> str:
        raise NotImplementedError

    @abc.abstractmethod
    def get_global_root(self) -> Path:
        raise NotImplementedError

    @abc.abstractmethod
    def get_relative_local_root(self) -> Optional[str]:
        raise NotImplementedError

    @abc.abstractmethod
    def get_excludes(self) -> List[str]:
        raise NotImplementedError

    @abc.abstractmethod
    def is_strict(self) -> bool:
        raise NotImplementedError

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
