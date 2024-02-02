# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from pathlib import Path
from typing import Any, List, Optional

from ...configuration import Configuration

from ...repository import Repository
from ..configurationless import Configurationless


class TestConfigurationless(unittest.TestCase):
    def __init__(self, *args: Any, **kwargs: Any) -> None:
        super().__init__(*args, **kwargs)

        self.configurationless = Configurationless(
            repository=Repository(),
            path=Path("."),
            includes=["**.py"],
            commit=False,
        )

    @classmethod
    def get_configuration(
        cls,
        path: Path,
        *,
        strict: Optional[bool] = True,
        targets: Optional[List[str]] = None,
        source_directories: Optional[List[str]] = None,
        ignore_all_errors: Optional[List[str]] = None,
        exclude: Optional[List[str]] = None,
    ) -> Configuration:
        if targets is not None:
            sources = {"targets": targets}
        elif source_directories is not None:
            sources = {"source_directories": source_directories}
        else:
            print(f"{targets} {source_directories}\n: {locals()}")
            raise ValueError("source_directories and targets can't both be None")

        configuration_json = {
            "strict": strict,
            **sources,
            "version": "abcdef12345",
            "ignore_all_errors": ignore_all_errors or [],
            "exclude": exclude or [],
        }

        return Configuration(path, configuration_json)
