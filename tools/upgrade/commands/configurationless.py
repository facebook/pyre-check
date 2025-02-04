# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

from __future__ import annotations

import argparse
import dataclasses
import json
import logging
import re
import subprocess
import sys
import tempfile
from functools import cached_property
from pathlib import Path
from typing import Collection, List, Optional, Set

from .. import filesystem
from ..configuration import Configuration
from ..repository import Repository
from .command import Command

LOG: logging.Logger = logging.getLogger(__name__)
DEFAULT_INCLUDES: Collection[str] = ["**.py"]


def path_is_relative_to(file: Path, path: Path) -> bool:
    if sys.version_info >= (3, 10):
        return file.is_relative_to(path)
    else:
        # use a string hack on old Python versions; mostly needed to
        # prevent failures in github CI.
        return str(file.absolute()).startswith(str(path))


@dataclasses.dataclass(frozen=True)
class ConfigurationlessOptions:
    global_configuration: Configuration
    local_configuration: Configuration
    includes: Collection[str] = dataclasses.field(
        default_factory=lambda: DEFAULT_INCLUDES
    )
    isolation_dir: Optional[str] = dataclasses.field(default=None)

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
            else False  # set default configuration strictness to UNSAFE
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

    @cached_property
    def buck_root(self) -> Path:
        return self.get_buck_root()

    @cached_property
    def included_files(self) -> Set[Path]:
        files_to_migrate = self.get_files_from_local_configuration(
            self.local_configuration, self.buck_root
        )
        already_migrated_files = self.get_already_migrated_files(self.buck_root)

        LOG.info(f"Found {len(files_to_migrate)} files to migrate")
        LOG.debug(f"Files to migrate\n:{[str(file) for file in files_to_migrate]}")
        LOG.info(f"Found {len(already_migrated_files)} already migrated files")
        LOG.debug(
            f"Files already migrated\n:{[str(file) for file in already_migrated_files]}"
        )
        files_to_migrate -= already_migrated_files
        LOG.info(
            f"Found {len(files_to_migrate)} files to migrate, not including files already migrated"
        )

        return files_to_migrate

    def __str__(self) -> str:
        local_path = str(self.local_configuration.get_path())
        global_path = str(self.global_configuration.get_path())
        return f"ConfigurationlessOptions(local={local_path}, global={global_path})"

    def get_file_mode_to_apply(
        self,
        file: Path,
    ) -> filesystem.LocalMode:
        file = file.resolve()
        default_local_mode = self.default_local_mode
        if any(
            path_is_relative_to(file, ignore_prefix)
            for ignore_prefix in self.ignore_all_errors_prefixes
        ):
            return filesystem.LocalMode.IGNORE
        else:
            return default_local_mode

    def _get_isolation_dir(self) -> List[str]:
        if self.isolation_dir is not None:
            return ["--isolation-dir", self.isolation_dir]
        else:
            return []

    def get_buck_root(self) -> Path:
        try:
            root = Path(
                subprocess.check_output(
                    ["buck2", *self._get_isolation_dir(), "root", "--kind", "project"],
                    text=True,
                    cwd=self.local_configuration.root,
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
            *self._get_isolation_dir(),
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
            cwd=self.local_configuration.root,
        )
        LOG.debug(f"Found targets:\n{raw_result}")
        result = json.loads(raw_result)

        return set(result)

    def _get_files_to_process_from_applicable_targets(
        self, applicable_targets: Collection[str], buck_root: Path
    ) -> Set[Path]:
        formatted_targets = " ".join([f"{target!r}" for target in applicable_targets])
        arguments = f"inputs( set( {formatted_targets} ) )"

        with tempfile.NamedTemporaryFile(
            "w+", prefix="pyre_configurationless_"
        ) as file:
            file.write(arguments)
            file.flush()

            buck_command = [
                "buck2",
                *self._get_isolation_dir(),
                "uquery",
                f"@{file.name}",
            ]

            LOG.info(
                f"Finding files from wildcard target expression with buck2 command: `{buck_command}`"
            )

            result = subprocess.check_output(
                buck_command,
                text=True,
                cwd=self.local_configuration.root,
            ).strip()

            LOG.debug(f"Found files from applicable targets:\n`{result}`")

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
            *self._get_isolation_dir(),
            "bxl",
            "prelude//python/sourcedb/classic.bxl:build",
            "--",
            *targets,
        ]

        LOG.info(f"Finding classic targets with buck2 command: `{buck_command}`")

        raw_result = subprocess.check_output(
            buck_command,
            text=True,
            cwd=self.local_configuration.root,
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
            LOG.warning(f"Malformed sourcedb at {sourcedb_path}")
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
            if file.exists()
            and path_is_relative_to(file, self.local_configuration.get_path().parent)
        }

    def _get_files_from_classic_targets(
        self, classic_targets: Collection[str], buck_project_root: Path
    ) -> Set[Path]:
        if len(classic_targets) == 0:
            return set()

        sourcedb_path = self._get_sourcedb_from_buck_classic_query(classic_targets)
        if sourcedb_path is None:
            LOG.warning("No sourcedb path produced")
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
        self,
        configuration_targets: List[str],
        buck_project_root: Path,
    ) -> Set[Path]:
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
            if any(file.match(pattern) for pattern in self.includes)
        }

    def _get_files_to_migrate_from_source_directories(
        self,
        source_directories: List[Path],
    ) -> Set[Path]:
        LOG.debug(f"Finding files with filesystem under {source_directories}")
        file_system = filesystem.get_filesystem()

        return {
            (source_directory / file).resolve()
            for source_directory in source_directories
            for file in file_system.list(
                str(source_directory), patterns=list(self.includes)
            )
        }

    def get_files_from_local_configuration(
        self,
        local_configuration: Configuration,
        buck_root: Path,
    ) -> Set[Path]:
        if local_configuration.targets is not None:
            files = self._get_files_to_migrate_from_targets(
                local_configuration.targets,
                buck_root,
            )
        elif local_configuration.source_directories is not None:
            source_directories = local_configuration.source_directories
            local_root = Path(local_configuration.root)
            files = self._get_files_to_migrate_from_source_directories(
                [local_root / directory for directory in source_directories],
            )
        else:
            raise ValueError(
                "Could not find `targets` or `source_directories` keys in local configuration"
            )
        LOG.debug(
            f"Found {len(files)} files in local configuration {local_configuration.get_path()}"
        )
        non_excluded_files = {
            file
            for file in files
            if not any(
                exclude_pattern.search(str(file)) is not None
                for exclude_pattern in self.exclude_patterns
            )
        }
        LOG.debug(
            f"Found {len(non_excluded_files)} in local configuration {local_configuration.get_path()}"
        )
        return non_excluded_files

    def get_already_migrated_files(
        self,
        buck_root: Path,
    ) -> Set[Path]:
        already_migrated_files: Set[Path] = set()
        nested_configurations = (
            self.local_configuration.get_nested_configuration_paths()
        )
        for configuration_path in nested_configurations:
            nested_local_configuration = Configuration(Path(configuration_path))
            migrated_files = self.get_files_from_local_configuration(
                nested_local_configuration,
                buck_root,
            )
            already_migrated_files |= migrated_files
        return already_migrated_files


class Configurationless(Command):
    def __init__(
        self,
        *,
        repository: Repository,
        path: Path,
        includes: List[str],
        commit: bool,
        force_remigrate: bool,
        isolation_dir: Optional[str] = None,
    ) -> None:
        super().__init__(repository)
        self._path: Path = path
        self._includes: List[str] = includes
        self._commit: bool = commit
        self._force_remigrate: bool = force_remigrate
        self._isolation_dir = isolation_dir

    @staticmethod
    def from_arguments(
        arguments: argparse.Namespace, repository: Repository
    ) -> "Configurationless":
        return Configurationless(
            repository=repository,
            path=arguments.path.resolve(),
            includes=arguments.include_file_suffixes,
            commit=(not arguments.no_commit),
            force_remigrate=arguments.force_remigrate,
            isolation_dir=arguments.isolation_dir,
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
            default=DEFAULT_INCLUDES,
            help="The suffixes to search for and include in the codemod. Default is '**.py'.",
        )
        parser.add_argument(
            "--force-remigrate",
            help="Remigrate all files, even if they already have a mode header present",
            action="store_true",
        )
        parser.add_argument("--isolation-dir", help="Buck2 isolation dir", default=None)

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

    def migrate_files(
        self, options: ConfigurationlessOptions, files_to_migrate: Set[Path]
    ) -> None:
        for file in files_to_migrate:
            file_mode = options.get_file_mode_to_apply(file)
            existing_modes = set(
                filesystem.get_lines_with_modes(
                    file.read_text().split("\n"), list(filesystem.LocalMode)
                ).values()
            )
            if file_mode is not None and (
                len(existing_modes) != 1 or file_mode not in existing_modes
            ):
                if self._force_remigrate:
                    filesystem.remove_local_mode(file, list(filesystem.LocalMode))
                filesystem.add_local_mode(str(file), file_mode, ignore_empty_files=True)

    def run(self) -> None:
        options = self.get_options()

        files_to_migrate = options.included_files

        self.migrate_files(options, files_to_migrate)

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
