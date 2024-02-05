# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from pathlib import Path
from typing import Any, Iterable, List, Optional

from ...configuration import Configuration

from ...filesystem import LocalMode
from ...repository import Repository
from ..configurationless import Configurationless, ConfigurationlessOptions


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

    @classmethod
    def get_options(
        cls,
        *,
        global_configuration: Optional[Configuration] = None,
        local_configuration: Optional[Configuration] = None,
        default_project_mode: LocalMode = LocalMode.STRICT,
        default_global_mode: LocalMode = LocalMode.STRICT,
        ignore_all_errors_prefixes: Optional[Iterable[str]] = None,
        exclude_patterns: Optional[Iterable[str]] = None,
    ) -> ConfigurationlessOptions:
        if global_configuration is None:
            global_configuration = cls.get_configuration(
                Path("../.pyre_configuration"),
                source_directories=["."],
                ignore_all_errors=None,
                exclude=None,
            )
        if local_configuration is None:
            local_configuration = cls.get_configuration(
                Path(".pyre_configuration.local"),
                targets=["//path/to/my:target"],
                ignore_all_errors=list(ignore_all_errors_prefixes)
                if ignore_all_errors_prefixes is not None
                else None,
                exclude=list(exclude_patterns)
                if exclude_patterns is not None
                else None,
            )

        return ConfigurationlessOptions(
            global_configuration=global_configuration,
            local_configuration=local_configuration,
            default_project_mode=default_project_mode,
            default_global_mode=default_global_mode,
        )

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

    def test_get_mode_to_apply_file_in_exclude(self) -> None:
        options = self.get_options(
            ignore_all_errors_prefixes=["path/to/ignore"],
            exclude_patterns=[r".*/exclude/.*"],
        )
        self.assertIsNone(
            self.configurationless.get_file_mode_to_apply(
                Path("path/to/exclude/file.py"),
                options,
            )
        )

    def test_get_mode_to_apply_file_in_ignore(self) -> None:
        options = self.get_options(
            ignore_all_errors_prefixes=["path/to/ignore"],
            exclude_patterns=[r".*/exclude/.*"],
        )
        self.assertEqual(
            self.configurationless.get_file_mode_to_apply(
                Path("path/to/ignore/file.py"),
                options,
            ),
            LocalMode.IGNORE,
        )

    def test_get_mode_to_apply_file_ignore_exclude_precedence(self) -> None:
        options = self.get_options(
            ignore_all_errors_prefixes=["path/to/regex"],
            exclude_patterns=[r".*/regex/.*"],
        )
        self.assertIsNone(
            self.configurationless.get_file_mode_to_apply(
                Path("path/to/regex/file.py"), options
            ),
        )

    def test_get_mode_to_apply_file_project_mode_same_as_global(self) -> None:
        options = self.get_options()
        self.assertIsNone(
            self.configurationless.get_file_mode_to_apply(
                Path("path/to/file.py"),
                options,
            ),
        )

    def test_get_mode_to_apply_file_project_mode_local_unsafe(self) -> None:
        options = self.get_options(default_project_mode=LocalMode.UNSAFE)
        self.assertEqual(
            self.configurationless.get_file_mode_to_apply(
                Path("path/to/file.py"),
                options,
            ),
            LocalMode.UNSAFE,
        )

    def test_get_mode_to_apply_file_project_mode_local_strict(self) -> None:
        options = self.get_options(default_global_mode=LocalMode.UNSAFE)
        self.assertEqual(
            self.configurationless.get_file_mode_to_apply(
                Path("path/to/file.py"), options
            ),
            LocalMode.STRICT,
        )
