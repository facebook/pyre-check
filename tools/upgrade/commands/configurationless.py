# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import argparse
import json
import logging
import re
import subprocess
import tempfile
from dataclasses import dataclass
from functools import cached_property
from pathlib import Path
from typing import Collection, List, Optional, Set

from .. import filesystem
from ..configuration import Configuration
from ..repository import Repository
from .command import Command

LOG: logging.Logger = logging.getLogger(__name__)


@dataclass(frozen=True)
class ConfigurationlessOptions:
    global_configuration: Configuration
    local_configuration: Configuration

    @cached_property
    def ignore_all_errors_prefixes(self) -> Collection[Path]:
        return (
            self.global_configuration.get_resolved_ignore_path_prefixes()
            | self.local_configuration.get_resolved_ignore_path_prefixes()
        )

    @cached_property
    def exclude_patterns(self) -> Collection[re.Pattern[str]]:
        return (
            self.global_configuration.get_exclude_as_patterns()
            | self.local_configuration.get_exclude_as_patterns()
        )

    @cached_property
    def default_global_mode(self) -> filesystem.LocalMode:
        global_is_strict = (
            self.global_configuration.strict
            if self.global_configuration.strict is not None
            else True  # set default configuration strictness to STRICT
        )
        return (
            filesystem.LocalMode.STRICT
            if global_is_strict
            else filesystem.LocalMode.UNSAFE
        )

    @cached_property
    def default_local_mode(self) -> filesystem.LocalMode:
        default_project_strictness_setting = self.local_configuration.strict

        if default_project_strictness_setting is None:
            return self.default_global_mode
        elif default_project_strictness_setting:
            return filesystem.LocalMode.STRICT
        else:
            return filesystem.LocalMode.UNSAFE

    def __str__(self) -> str:
        local_path = str(self.local_configuration.get_path())
        global_path = str(self.global_configuration.get_path())
        return f"ConfigurationlessOptions(local={local_path}, global={global_path})"

    def no_changes_to_make(self) -> bool:
        return (
            self.default_global_mode == self.default_local_mode
            and len(self.ignore_all_errors_prefixes) == 0
            and len(self.exclude_patterns) == 0
        )


class Configurationless(Command):
    def __init__(
        self, *, repository: Repository, path: Path, includes: List[str], commit: bool
    ) -> None:
        super().__init__(repository)
        self._path: Path = path
        self._includes: List[str] = includes
        self._commit: bool = commit

    @staticmethod
    def from_arguments(
        arguments: argparse.Namespace, repository: Repository
    ) -> "Configurationless":
        return Configurationless(
            repository=repository,
            path=arguments.path.resolve(),
            includes=arguments.include_file_suffixes,
            commit=(not arguments.no_commit),
        )

    @classmethod
    def add_arguments(cls, parser: argparse.ArgumentParser) -> None:
        super(Configurationless, cls).add_arguments(parser)
        parser.set_defaults(command=cls.from_arguments)
        parser.add_argument(
            "path",
            help="Path to project root with local configuration.",
            type=Path,
        )
        parser.add_argument(
            "--no-commit",
            help="Do not commit changes after completing codemod.",
            action="store_true",
        )
        parser.add_argument(
            "--include-file-suffixes",
            action="extend",
            nargs="+",
            type=str,
            default=["**.py"],
            help="The suffixes to search for and include in the codemod. Default is '**.py'.",
        )

    def get_file_mode_to_apply(
        self, file: Path, options: ConfigurationlessOptions
    ) -> Optional[filesystem.LocalMode]:
        file = file.resolve()
        default_local_mode = options.default_local_mode
        if any(
            exclude_pattern.search(str(file)) is not None
            for exclude_pattern in options.exclude_patterns
        ):
            return None
        elif any(
            file.is_relative_to(ignore_prefix)
            for ignore_prefix in options.ignore_all_errors_prefixes
        ):
            return filesystem.LocalMode.IGNORE
        else:
            return default_local_mode

    def _get_buck_root(self) -> Path:
        try:
            root = Path(
                subprocess.check_output(
                    ["buck2", "root", "--kind", "project"],
                    text=True,
                    cwd=self._path,
                ).strip()
            )
            LOG.info(f"buck2 root is {str(root)}")
        except FileNotFoundError as e:
            raise ValueError(
                "Could not find `buck2` executable when `targets` were specified in local configuration."
            ) from e
        return root

    @staticmethod
    def format_buck_targets_for_query(targets: Collection[str]) -> List[str]:
        targets = [
            target_expression
            for target in targets
            for target_expression in ["--target", target]
        ]
        return targets

    def _get_applicable_targets_from_wildcard_targets_buck_query(
        self, targets: Collection[str]
    ) -> Collection[str]:
        targets = self.format_buck_targets_for_query(targets)
        buck_command = [
            "buck2",
            "bxl",
            "prelude//python/sourcedb/filter.bxl:filter",
            "--",
            *targets,
        ]

        LOG.info(
            f"Finding targets from wildcard expression with buck2 command: `{buck_command}`"
        )

        raw_result = subprocess.check_output(
            buck_command,
            text=True,
            cwd=self._path,
        )
        LOG.debug(f"Found targets:\n{raw_result}")
        result = json.loads(raw_result)

        return set(result)

    def _get_files_to_process_from_applicable_targets(
        self, applicable_targets: Collection[str], buck_root: Path
    ) -> Set[Path]:
        formatted_targets = " ".join([f"{target!r}" for target in applicable_targets])
        arguments = f"inputs( set( {formatted_targets} ) )"

        with tempfile.NamedTemporaryFile("w+", prefix="pyre_configurationless") as file:
            file.write(arguments)
            file.flush()

            buck_command = [
                "buck2",
                "uquery",
                f"@{file.name}",
            ]

            LOG.info(
                f"Finding files from wildcard target expression with buck2 command: `{buck_command}`"
            )

            result = subprocess.check_output(
                buck_command,
                text=True,
                cwd=self._path,
            ).strip()

            LOG.debug(f"Found files:\n`{result}`")

            return {(buck_root / file.strip()).resolve() for file in result.split("\n")}

    def _get_files_from_wildcard_targets(
        self, wildcard_targets: Collection[str], buck_project_root: Path
    ) -> Set[Path]:
        if len(wildcard_targets) == 0:
            return set()

        applicable_targets = (
            self._get_applicable_targets_from_wildcard_targets_buck_query(
                wildcard_targets
            )
        )
        wildcard_target_files = self._get_files_to_process_from_applicable_targets(
            applicable_targets, buck_project_root
        )

        LOG.debug(
            f"Files found from wildcard target filter BXL query\n{wildcard_target_files}"
        )
        return wildcard_target_files

    def _get_sourcedb_from_buck_classic_query(
        self, targets: Collection[str]
    ) -> Optional[Path]:
        targets = self.format_buck_targets_for_query(targets)
        buck_command = [
            "buck2",
            "bxl",
            "prelude//python/sourcedb/classic.bxl:build",
            "--",
            *targets,
        ]

        LOG.info(f"Finding classic targets with buck2 command: `{buck_command}`")

        raw_result = subprocess.check_output(
            buck_command,
            text=True,
            cwd=self._path,
        )
        result = json.loads(raw_result)
        if "db" in result:
            return Path(result["db"])
        return None

    def _get_files_from_sourcedb(
        self, sourcedb_path: Path, buck_root: Path
    ) -> Set[Path]:
        LOG.debug(f"Loading files from sourcedb at {str(sourcedb_path)}")
        with sourcedb_path.open() as file:
            loaded_sourcedb = json.load(file)

        if not isinstance(loaded_sourcedb, dict) or "build_map" not in loaded_sourcedb:
            LOG.warn(f"Malformed sourcedb at {sourcedb_path}")
            return set()

        build_map = {buck_root / file for file in loaded_sourcedb["build_map"].values()}

        if "dropped_targets" in loaded_sourcedb:
            dropped_target_paths = {
                buck_root / dropped_target["dropped_source_path"]
                for dropped_target in loaded_sourcedb["dropped_targets"].values()
            }
            build_map |= dropped_target_paths

        return {
            file
            for file in build_map
            if file.exists() and file.is_relative_to(self._path)
        }

    def _get_files_from_classic_targets(
        self, classic_targets: Collection[str], buck_project_root: Path
    ) -> Set[Path]:
        if len(classic_targets) == 0:
            return set()

        sourcedb_path = self._get_sourcedb_from_buck_classic_query(classic_targets)
        if sourcedb_path is None:
            LOG.warn("No sourcedb path produced")
            return set()
        LOG.debug(f"Sourcedb path found: {sourcedb_path}")

        classic_target_files = self._get_files_from_sourcedb(
            sourcedb_path, buck_project_root
        )
        LOG.debug(
            f"Files found from classic target filter BXL query\n{classic_target_files}"
        )

        return classic_target_files

    def _get_files_to_migrate_from_targets(
        self, configuration_targets: List[str]
    ) -> Set[Path]:
        buck_project_root = self._get_buck_root()

        wildcard_targets: List[str] = [
            target for target in configuration_targets if target.endswith("...")
        ]
        classic_targets: List[str] = [
            target for target in configuration_targets if not target.endswith("...")
        ]

        wildcard_target_files = self._get_files_from_wildcard_targets(
            wildcard_targets, buck_project_root
        )
        classic_target_files = self._get_files_from_classic_targets(
            classic_targets, buck_project_root
        )

        return {
            file
            for file in wildcard_target_files | classic_target_files
            if any(file.match(pattern) for pattern in self._includes)
        }

    def _get_files_to_migrate_from_source_directories(
        self, source_directories: List[Path]
    ) -> Collection[Path]:
        LOG.debug(f"Finding files with filesystem under {source_directories}")
        file_system = filesystem.get_filesystem()

        return {
            (source_directory / file).resolve()
            for source_directory in source_directories
            for file in file_system.list(str(source_directory), patterns=self._includes)
        }

    def get_files_to_migrate(
        self, local_configuration: Configuration
    ) -> Collection[Path]:
        if local_configuration.targets is not None:
            files = self._get_files_to_migrate_from_targets(local_configuration.targets)
        elif local_configuration.source_directories is not None:
            source_directories = local_configuration.source_directories
            local_root = Path(local_configuration.root)
            files = self._get_files_to_migrate_from_source_directories(
                [local_root / directory for directory in source_directories]
            )
        else:
            raise ValueError(
                "Could not find `targets` or `source_directories` keys in local configuration"
            )
        LOG.info(f"Found {len(files)} files to migrate")
        return files

    def get_options(
        self,
    ) -> ConfigurationlessOptions:
        global_configuration = Configuration(
            Configuration.find_project_configuration().resolve()
        )
        configuration_path = (self._path / ".pyre_configuration.local").resolve()
        local_configuration = Configuration(configuration_path)

        if not global_configuration.get_path().exists():
            raise ValueError(
                f"Global configuration found at {str(global_configuration.get_path())}, but path doesn't exist"
            )
        if not local_configuration.get_path().exists():
            raise ValueError(
                f"Local configuration found at {str(local_configuration.get_path())}, but path doesn't exist"
            )

        options = ConfigurationlessOptions(
            global_configuration=global_configuration,
            local_configuration=local_configuration,
        )

        LOG.info(f"Configurationless options:\n{str(options)}")

        return options

    def run(self) -> None:
        options = self.get_options()
        if options.no_changes_to_make():
            return

        files_to_migrate = self.get_files_to_migrate(options.local_configuration)

        for file in files_to_migrate:
            file_mode = self.get_file_mode_to_apply(file, options)
            if file_mode is not None:
                filesystem.add_local_mode(str(file), file_mode, ignore_empty_files=True)

        options.local_configuration.original_contents["migration_status"] = "mode"
        options.local_configuration.write()

        self._repository.commit_changes(
            commit=self._commit,
            title=f"Configurationless migration for {str(options.local_configuration.get_path())}",
            summary="""
            We are migrating local configurations to work with
            upcoming changes we'll be making to how Pyre will do type checking
            on the CLI and in the IDE. Part of this migration requires pushing
            the type checker mode into each file that gets type checked if it's
            Pyre mode differs from the strict default in FBCode
            (i.e. `# pyre-unsafe` and `# pyre-ignore-all-errors`).
            """,
        )
