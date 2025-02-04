# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

from pathlib import Path
from typing import Any, Iterable, List, Optional
from unittest import mock, TestCase

from ...configuration import Configuration
from ...filesystem import LocalMode
from ...repository import Repository
from ..configurationless import Configurationless, ConfigurationlessOptions


class TestConfigurationless(TestCase):
    def __init__(self, *args: Any, **kwargs: Any) -> None:
        super().__init__(*args, **kwargs)

        self.configurationless = Configurationless(
            repository=Repository(),
            path=Path("."),
            includes=["**.py"],
            commit=False,
            force_remigrate=False,
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
                ignore_all_errors=(
                    list(ignore_all_errors_prefixes)
                    if ignore_all_errors_prefixes is not None
                    else None
                ),
                exclude=(
                    list(exclude_patterns) if exclude_patterns is not None else None
                ),
            )

        return ConfigurationlessOptions(
            global_configuration=global_configuration,
            local_configuration=local_configuration,
        )

    def test_get_default_global_mode_no_configuration(self) -> None:
        global_configuration = self.get_configuration(
            Path("../.pyre_configuration.json"),
            source_directories=["."],
            strict=None,
        )
        options = self.get_options(global_configuration=global_configuration)

        self.assertEqual(
            options.default_global_mode,
            LocalMode.UNSAFE,
        )

    def test_get_default_global_mode_strict_configuration(self) -> None:
        options = self.get_options()

        self.assertEqual(
            options.default_global_mode,
            LocalMode.STRICT,
        )

    def test_get_default_global_mode_unsafe_configuration(self) -> None:
        global_configuration = self.get_configuration(
            Path("../.pyre_configuration.json"), source_directories=["."], strict=False
        )
        options = self.get_options(global_configuration=global_configuration)

        self.assertEqual(
            options.default_global_mode,
            LocalMode.UNSAFE,
        )

    def test_get_default_mode_local_strict(self) -> None:
        global_configuration = self.get_configuration(
            Path("../.pyre_configuration.json"), source_directories=["."], strict=False
        )
        local_configuration = self.get_configuration(
            Path("./local_configuration.json"), targets=["..."]
        )
        options = self.get_options(
            local_configuration=local_configuration,
            global_configuration=global_configuration,
        )

        self.assertEqual(
            options.default_local_mode,
            LocalMode.STRICT,
        )

    def test_get_default_mode_local_empty_global_strict(self) -> None:
        local_configuration = self.get_configuration(
            Path("./local_configuration.json"), targets=["..."], strict=None
        )
        options = self.get_options(local_configuration=local_configuration)

        self.assertEqual(
            options.default_local_mode,
            LocalMode.STRICT,
        )

    def test_get_default_mode_local_empty_global_unsafe(self) -> None:
        global_configuration = self.get_configuration(
            Path("../.pyre_configuration.json"), source_directories=["."], strict=False
        )
        local_configuration = self.get_configuration(
            Path("./local_configuration.json"), targets=["..."], strict=None
        )
        options = self.get_options(
            global_configuration=global_configuration,
            local_configuration=local_configuration,
        )

        self.assertEqual(
            options.default_local_mode,
            LocalMode.UNSAFE,
        )

    def test_get_default_mode_local_unsafe_global_strict(self) -> None:
        local_configuration = self.get_configuration(
            Path("./local_configuration.json"), targets=["..."], strict=False
        )
        options = self.get_options(local_configuration=local_configuration)

        self.assertEqual(
            options.default_local_mode,
            LocalMode.UNSAFE,
        )

    def test_get_mode_to_apply_file_in_ignore(self) -> None:
        options = self.get_options(
            ignore_all_errors_prefixes=["path/to/ignore"],
            exclude_patterns=[r".*/exclude/.*"],
        )
        self.assertEqual(
            options.get_file_mode_to_apply(
                Path("path/to/ignore/file.py"),
            ),
            LocalMode.IGNORE,
        )

    def test_get_file_mode_to_apply_file_in_ignore_root_path(self) -> None:
        options = self.get_options(ignore_all_errors_prefixes=["//path/to/ignore"])
        with mock.patch.object(
            Configuration,
            "find_project_configuration",
            return_value=options.global_configuration.get_path(),
        ):
            self.assertEqual(
                options.get_file_mode_to_apply(
                    Path("path/to/ignore/file.py").resolve(),
                ),
                LocalMode.STRICT,
            )
            self.assertEqual(
                options.get_file_mode_to_apply(
                    Path("../path/to/ignore/file.py").resolve(),
                ),
                LocalMode.IGNORE,
            )

    def test_get_mode_to_apply_file_project_mode_same_as_global(self) -> None:
        options = self.get_options()
        self.assertEqual(
            options.get_file_mode_to_apply(
                Path("path/to/file.py"),
            ),
            LocalMode.STRICT,
        )

    def test_get_mode_to_apply_file_project_mode_local_unsafe(self) -> None:
        local_configuration = self.get_configuration(
            Path("../.pyre_configuration.json"), source_directories=["."], strict=False
        )
        options = self.get_options(local_configuration=local_configuration)

        self.assertEqual(
            options.get_file_mode_to_apply(
                Path("path/to/file.py"),
            ),
            LocalMode.UNSAFE,
        )

    def test_get_mode_to_apply_file_project_mode_local_strict(self) -> None:
        global_configuration = self.get_configuration(
            Path(".pyre_configuration.json"),
            source_directories=["."],
            strict=False,
        )
        local_configuration = self.get_configuration(
            Path("../.pyre_configuration.json"), source_directories=["."], strict=True
        )
        options = self.get_options(
            global_configuration=global_configuration,
            local_configuration=local_configuration,
        )
        self.assertEqual(
            options.get_file_mode_to_apply(Path("path/to/file.py")),
            LocalMode.STRICT,
        )
