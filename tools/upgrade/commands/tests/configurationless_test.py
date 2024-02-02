# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from pathlib import Path
from typing import Any, List, Optional

from ...configuration import Configuration

from ...filesystem import LocalMode
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

    def test_get_default_global_mode_no_configuration(self) -> None:
        global_configuration = self.get_configuration(
            Path("../.pyre_configuration.json"), source_directories=["."], strict=None
        )

        self.assertEqual(
            self.configurationless.get_default_global_mode(global_configuration),
            LocalMode.STRICT,
        )

    def test_get_default_global_mode_strict_configuration(self) -> None:
        global_configuration = self.get_configuration(
            Path("../.pyre_configuration.json"), source_directories=["."]
        )

        self.assertEqual(
            self.configurationless.get_default_global_mode(global_configuration),
            LocalMode.STRICT,
        )

    def test_get_default_global_mode_unsafe_configuration(self) -> None:
        global_configuration = self.get_configuration(
            Path("../.pyre_configuration.json"), source_directories=["."], strict=False
        )

        self.assertEqual(
            self.configurationless.get_default_global_mode(global_configuration),
            LocalMode.UNSAFE,
        )

    def test_get_default_mode_local_strict(self) -> None:
        local_configuration = self.get_configuration(
            Path("./local_configuration.json"), targets=["..."]
        )

        self.assertEqual(
            self.configurationless.get_default_local_mode(
                local_configuration, LocalMode.UNSAFE
            ),
            LocalMode.STRICT,
        )

    def test_get_default_mode_local_empty_global_strict(self) -> None:
        local_configuration = self.get_configuration(
            Path("./local_configuration.json"), targets=["..."], strict=None
        )

        self.assertEqual(
            self.configurationless.get_default_local_mode(
                local_configuration, LocalMode.STRICT
            ),
            LocalMode.STRICT,
        )

    def test_get_default_mode_local_empty_global_unsafe(self) -> None:
        local_configuration = self.get_configuration(
            Path("./local_configuration.json"), targets=["..."], strict=None
        )

        self.assertEqual(
            self.configurationless.get_default_local_mode(
                local_configuration, LocalMode.UNSAFE
            ),
            LocalMode.UNSAFE,
        )

    def test_get_default_mode_local_unsafe_global_strict(self) -> None:
        local_configuration = self.get_configuration(
            Path("./local_configuration.json"), targets=["..."], strict=False
        )

        self.assertEqual(
            self.configurationless.get_default_local_mode(
                local_configuration, LocalMode.STRICT
            ),
            LocalMode.UNSAFE,
        )
