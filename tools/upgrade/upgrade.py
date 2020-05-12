# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import json
import logging
import subprocess
import sys
import traceback
from logging import Logger
from pathlib import Path
from typing import List

from ...client.commands import ExitCode
from . import UserError
from .ast import UnstableAST
from .codemods import MissingGlobalAnnotations, MissingOverrideReturnAnnotations
from .commands.command import Command, ErrorSuppressingCommand
from .commands.consolidate_nested_configurations import ConsolidateNestedConfigurations
from .commands.fixme import Fixme
from .commands.targets_to_configuration import TargetsToConfiguration
from .configuration import Configuration
from .errors import Errors, errors_from_targets
from .filesystem import (
    LocalMode,
    add_local_mode,
    find_files,
    find_targets,
    get_filesystem,
    path_exists,
    remove_non_pyre_ignores,
)
from .repository import Repository


LOG: Logger = logging.getLogger(__name__)


class StrictDefault(Command):
    def __init__(self, arguments: argparse.Namespace, repository: Repository) -> None:
        super().__init__(arguments, repository)
        self._local_configuration: Path = arguments.local_configuration
        self._lint: bool = arguments.lint

    def run(self) -> None:
        project_configuration = Configuration.find_project_configuration()
        if project_configuration is None:
            LOG.info("No project configuration found for the given directory.")
            return
        local_configuration = self._local_configuration
        if local_configuration:
            configuration_path = local_configuration / ".pyre_configuration.local"
        else:
            configuration_path = project_configuration
        with open(configuration_path) as configuration_file:
            configuration = Configuration(
                configuration_path, json.load(configuration_file)
            )
            LOG.info("Processing %s", configuration.get_directory())
            configuration.add_strict()
            configuration.write()
            errors = configuration.get_errors()

            if len(errors) == 0:
                return
            for filename, _ in errors:
                add_local_mode(filename, LocalMode.UNSAFE)

            if self._lint:
                self._repository.format()


class GlobalVersionUpdate(Command):
    def __init__(self, arguments: argparse.Namespace, repository: Repository) -> None:
        super().__init__(arguments, repository)
        self._hash: str = self._arguments.hash
        self._paths: List[Path] = self._arguments.paths
        self._submit: bool = self._arguments.submit

    def run(self) -> None:
        global_configuration = Configuration.find_project_configuration()
        if global_configuration is None:
            LOG.error("No global configuration file found.")
            return

        with open(global_configuration, "r") as global_configuration_file:
            configuration = json.load(global_configuration_file)
            if "version" not in configuration:
                LOG.error(
                    "Global configuration at %s has no version field.",
                    global_configuration,
                )
                return

            old_version = configuration["version"]

        # Rewrite.
        with open(global_configuration, "w") as global_configuration_file:
            configuration["version"] = self._hash

            # This will sort the keys in the configuration - we won't be clobbering
            # comments since Python's JSON parser disallows comments either way.
            json.dump(
                configuration, global_configuration_file, sort_keys=True, indent=2
            )
            global_configuration_file.write("\n")

        paths = self._paths
        configuration_paths = (
            [path / ".pyre_configuration.local" for path in paths]
            if paths
            else [
                configuration.get_path()
                for configuration in Configuration.gather_local_configurations()
                if configuration.is_local
            ]
        )
        for configuration_path in configuration_paths:
            if "mock_repository" in str(configuration_path):
                # Skip local configurations we have for testing.
                continue
            with open(configuration_path) as configuration_file:
                contents = json.load(configuration_file)
                if "version" in contents:
                    LOG.info(
                        "Skipping %s as it already has a custom version field.",
                        configuration_path,
                    )
                    continue
                if contents.get("differential"):
                    LOG.info(
                        "Skipping differential configuration at `%s`",
                        configuration_path,
                    )
                    continue
                contents["version"] = old_version

            with open(configuration_path, "w") as configuration_file:
                json.dump(contents, configuration_file, sort_keys=True, indent=2)
                configuration_file.write("\n")

        self._repository.submit_changes(
            commit=True,
            submit=self._submit,
            title="Update pyre global configuration version",
            summary=f"Automatic upgrade to hash `{self._hash}`",
            ignore_failures=True,
        )


class FixmeSingle(ErrorSuppressingCommand):
    def __init__(self, arguments: argparse.Namespace, repository: Repository) -> None:
        super().__init__(arguments, repository)
        self._path: Path = Path(arguments.path)

    def run(self) -> None:
        project_configuration = Configuration.find_project_configuration()
        if project_configuration is None:
            LOG.info("No project configuration found for the given directory.")
            return
        configuration_path = self._path / ".pyre_configuration.local"
        with open(configuration_path) as configuration_file:
            configuration = Configuration(
                configuration_path, json.load(configuration_file)
            )
            self._suppress_errors_in_project(
                configuration, project_configuration.parent
            )


class FixmeAll(ErrorSuppressingCommand):
    def run(self) -> None:
        project_configuration = Configuration.find_project_configuration()
        if project_configuration is None:
            LOG.info("No project configuration found for the current directory.")
            return

        configurations = Configuration.gather_local_configurations()
        for configuration in configurations:
            self._suppress_errors_in_project(
                configuration, project_configuration.parent
            )


class FixmeTargets(ErrorSuppressingCommand):
    def __init__(self, arguments: argparse.Namespace, repository: Repository) -> None:
        super().__init__(arguments, repository)
        self._subdirectory: str = arguments.subdirectory
        self._no_commit: bool = arguments.no_commit
        self._submit: bool = arguments.submit
        self._lint: bool = arguments.lint

    def run(self) -> None:
        subdirectory = self._subdirectory
        subdirectory = Path(subdirectory) if subdirectory else None
        project_configuration = Configuration.find_project_configuration(subdirectory)
        if project_configuration is None:
            LOG.error("No project configuration found for the given directory.")
            return
        project_directory = project_configuration.parent
        search_root = subdirectory if subdirectory else project_directory

        all_targets = find_targets(search_root)
        if not all_targets:
            return
        for path, target_names in all_targets.items():
            self._run_fixme_targets_file(project_directory, path, target_names)

        self._repository.submit_changes(
            commit=(not self._no_commit),
            submit=self._submit,
            title=f"Upgrade pyre version for {search_root} (TARGETS)",
        )

    def _run_fixme_targets_file(
        self, project_directory: Path, path: str, target_names: List[str]
    ) -> None:
        LOG.info("Processing %s/TARGETS...", path)
        targets = [path + ":" + name + "-pyre-typecheck" for name in target_names]
        errors = errors_from_targets(project_directory, path, targets)
        if not errors:
            return
        LOG.info("Found %d type errors in %s/TARGETS.", len(errors), path)

        if not errors:
            return

        self._suppress_errors(errors)

        if not self._lint:
            return

        if self._repository.format():
            errors = errors_from_targets(project_directory, path, targets)
            if not errors:
                LOG.info("Errors unchanged after linting.")
                return
            LOG.info("Found %d type errors after linting.", len(errors))
            self._suppress_errors(errors)


class MigrateTargets(Command):
    def __init__(self, arguments: argparse.Namespace, repository: Repository) -> None:
        super().__init__(arguments, repository)
        self._subdirectory: str = arguments.subdirectory

    def run(self) -> None:
        subdirectory = self._subdirectory
        subdirectory = Path(subdirectory) if subdirectory else Path.cwd()
        LOG.info("Migrating typecheck targets in {}".format(subdirectory))

        # Remove explicit check types options.
        targets_files = [
            str(subdirectory / path)
            for path in get_filesystem().list(
                str(subdirectory), patterns=[r"**/TARGETS"]
            )
        ]
        LOG.info("...found {} targets files".format(len(targets_files)))
        remove_check_types_command = [
            "sed",
            "-i",
            r'/check_types_options \?= \?"mypy",/d',
        ] + targets_files
        remove_options_command = [
            "sed",
            "-i",
            r's/typing_options \?= \?".*strict",/check_types_options = "strict",/g',
        ] + targets_files
        subprocess.check_output(remove_check_types_command)
        subprocess.check_output(remove_options_command)

        remove_non_pyre_ignores(subdirectory)
        FixmeTargets(self._arguments, self._repository).run()


class ExpandTargetCoverage(ErrorSuppressingCommand):
    def __init__(self, arguments: argparse.Namespace, repository: Repository) -> None:
        super().__init__(arguments, repository)
        self._subdirectory: str = arguments.subdirectory
        self._fixme_threshold: bool = arguments.fixme_threshold
        self._no_commit: bool = arguments.no_commit
        self._submit: bool = arguments.submit
        self._lint: bool = arguments.lint

    def run(self) -> None:
        subdirectory = self._subdirectory
        subdirectory = Path(subdirectory) if subdirectory else Path.cwd()

        # Do not change if configurations exist below given root
        existing_configurations = find_files(subdirectory, ".pyre_configuration.local")
        if existing_configurations and not existing_configurations == [
            str(subdirectory / ".pyre_configuration.local")
        ]:
            LOG.warning(
                "Cannot expand targets because nested configurations exist:\n%s",
                "\n".join(existing_configurations),
            )
            return

        # Expand coverage
        local_configuration = Configuration.find_local_configuration(subdirectory)
        if not local_configuration:
            LOG.warning("Could not find a local configuration to codemod.")
            return
        LOG.info("Expanding typecheck targets in `%s`", local_configuration)
        with open(local_configuration) as configuration_file:
            configuration = Configuration(
                local_configuration, json.load(configuration_file)
            )
            configuration.add_targets(["//" + str(subdirectory) + "/..."])
            configuration.deduplicate_targets()
            configuration.write()

        # Suppress errors
        all_errors = configuration.get_errors()
        error_threshold = self._fixme_threshold

        for path, errors in all_errors:
            errors = list(errors)
            error_count = len(errors)
            if error_threshold and error_count > error_threshold:
                LOG.info(
                    "%d errors found in `%s`. Adding file-level ignore.",
                    error_count,
                    path,
                )
                add_local_mode(path, LocalMode.IGNORE)
            else:
                self._suppress_errors(Errors(errors))

        # Lint and re-run pyre once to resolve most formatting issues
        if self._lint:
            if self._repository.format():
                errors = configuration.get_errors(should_clean=False)
                self._suppress_errors(errors)

        self._repository.submit_changes(
            commit=(not self._no_commit),
            submit=self._submit,
            title=f"Expand target type coverage in {local_configuration}",
            summary="Expanding type coverage of targets in configuration.",
            set_dependencies=False,
        )

def run(repository: Repository) -> None:
    parser = argparse.ArgumentParser(fromfile_prefix_chars="@")
    parser.add_argument("--verbose", action="store_true", help="Enable verbose logging")
    parser.add_argument(
        "--truncate",
        action="store_true",
        help="Truncate error messages to maximum line length.",
    )
    parser.add_argument(
        "--max-line-length",
        default=88,
        type=int,
        help="Enforce maximum line length on new comments "
        + "(default: %(default)s, use 0 to set no maximum line length)",
    )
    parser.add_argument(
        "--only-fix-error-code",
        type=int,
        help="Only add fixmes for errors with this specific error code.",
        default=None,
    )

    commands = parser.add_subparsers()

    # Subcommands: Codemods
    missing_overridden_return_annotations = commands.add_parser(
        "missing-overridden-return-annotations",
        help="Add annotations according to errors inputted through stdin.",
    )
    missing_overridden_return_annotations.set_defaults(
        command=MissingOverrideReturnAnnotations
    )

    missing_global_annotations = commands.add_parser(
        "missing-global-annotations",
        help="Add annotations according to errors inputted through stdin.",
    )
    missing_global_annotations.set_defaults(command=MissingGlobalAnnotations)

    # Subcommand: Change default pyre mode to strict and adjust module headers.
    strict_default = commands.add_parser("strict-default")
    strict_default.set_defaults(command=StrictDefault)
    strict_default.add_argument(
        "-l",
        "--local-configuration",
        type=path_exists,
        help="Path to project root with local configuration",
    )
    strict_default.add_argument(
        # TODO(T53195818): Not implemented
        "--remove-strict-headers",
        action="store_true",
        help="Delete unnecessary `# pyre-strict` headers.",
    )
    strict_default.add_argument("--lint", action="store_true", help=argparse.SUPPRESS)

    # Subcommand: Set global configuration to given hash, and add version override
    # to all local configurations to run previous version.
    update_global_version = commands.add_parser("update-global-version")
    update_global_version.set_defaults(command=GlobalVersionUpdate)
    update_global_version.add_argument("hash", help="Hash of new Pyre version")
    update_global_version.add_argument(
        "--paths",
        nargs="*",
        help="A list of paths to local Pyre projects.",
        default=[],
        type=path_exists,
    )
    update_global_version.add_argument(
        "--submit", action="store_true", help=argparse.SUPPRESS
    )

    # Subcommand: Fixme all errors inputted through stdin.
    fixme = commands.add_parser("fixme")
    fixme.set_defaults(command=Fixme)
    fixme.add_argument("--error-source", choices=["stdin", "generate"], default="stdin")
    fixme.add_argument("--comment", help="Custom comment after fixme comments")
    fixme.add_argument(
        "--unsafe", action="store_true", help="Don't check syntax when applying fixmes."
    )
    fixme.add_argument("--lint", action="store_true", help=argparse.SUPPRESS)

    # Subcommand: Fixme all errors for a single project.
    fixme_single = commands.add_parser("fixme-single")
    fixme_single.set_defaults(command=FixmeSingle)
    fixme_single.add_argument(
        "path", help="Path to project root with local configuration", type=path_exists
    )
    fixme_single.add_argument(
        "--upgrade-version",
        action="store_true",
        help="Upgrade and clean project if a version override set.",
    )
    fixme_single.add_argument(
        "--error-source", choices=["stdin", "generate"], default="generate"
    )
    fixme_single.add_argument("--submit", action="store_true", help=argparse.SUPPRESS)
    fixme_single.add_argument(
        "--no-commit", action="store_true", help=argparse.SUPPRESS
    )
    fixme_single.add_argument("--lint", action="store_true", help=argparse.SUPPRESS)
    fixme_single.add_argument(
        "--unsafe", action="store_true", help="Don't check syntax when applying fixmes."
    )

    # Subcommand: Fixme all errors in all projects with local configurations.
    fixme_all = commands.add_parser("fixme-all")
    fixme_all.set_defaults(command=FixmeAll)
    fixme_all.add_argument(
        "-c", "--comment", help="Custom comment after fixme comments"
    )
    fixme_all.add_argument(
        "--upgrade-version",
        action="store_true",
        help="Upgrade and clean projects with a version override set.",
    )
    fixme_all.add_argument("--submit", action="store_true", help=argparse.SUPPRESS)
    fixme_all.add_argument("--no-commit", action="store_true", help=argparse.SUPPRESS)
    fixme_all.add_argument("--lint", action="store_true", help=argparse.SUPPRESS)

    # Subcommand: Fixme all errors in targets running type checking
    fixme_targets = commands.add_parser("fixme-targets")
    fixme_targets.set_defaults(command=FixmeTargets)
    fixme_targets.add_argument(
        "-c", "--comment", help="Custom comment after fixme comments"
    )
    fixme_targets.add_argument("--submit", action="store_true", help=argparse.SUPPRESS)
    fixme_targets.add_argument("--lint", action="store_true", help=argparse.SUPPRESS)
    fixme_targets.add_argument(
        "--subdirectory", help="Only upgrade TARGETS files within this directory."
    )
    fixme_targets.add_argument(
        "--no-commit", action="store_true", help="Keep changes in working state."
    )

    # Subcommand: Migrate and fixme errors in targets running type checking
    migrate_targets = commands.add_parser("migrate-targets")
    migrate_targets.set_defaults(command=MigrateTargets)
    migrate_targets.add_argument(
        "-c", "--comment", help="Custom comment after fixme comments"
    )
    migrate_targets.add_argument(
        "--submit", action="store_true", help=argparse.SUPPRESS
    )
    migrate_targets.add_argument("--lint", action="store_true", help=argparse.SUPPRESS)
    migrate_targets.add_argument(
        "--subdirectory", help="Only upgrade TARGETS files within this directory."
    )
    migrate_targets.add_argument(
        "--no-commit", action="store_true", help="Keep changes in working state."
    )

    # Subcommand: Remove targets integration and replace with configuration
    targets_to_configuration = commands.add_parser("targets-to-configuration")
    targets_to_configuration.set_defaults(command=TargetsToConfiguration)
    targets_to_configuration.add_argument(
        "-c", "--comment", help="Custom comment after fixme comments"
    )
    targets_to_configuration.add_argument(
        "--submit", action="store_true", help=argparse.SUPPRESS
    )
    targets_to_configuration.add_argument(
        "--lint", action="store_true", help=argparse.SUPPRESS
    )
    targets_to_configuration.add_argument(
        "--subdirectory", help="Only upgrade TARGETS files within this directory."
    )
    targets_to_configuration.add_argument(
        "--glob",
        type=int,
        help="Use a toplevel glob target instead of listing individual targets. \
        Fall back to individual targets if errors per file ever hits given threshold.",
    )
    targets_to_configuration.add_argument(
        "--fixme-threshold",
        type=int,
        help="Ignore all errors in a file if fixme count exceeds threshold.",
    )
    targets_to_configuration.add_argument(
        "--no-commit", action="store_true", help="Keep changes in working state."
    )

    # Subcommand: Expand target coverage in configuration up to given error limit
    expand_target_coverage = commands.add_parser("expand-target-coverage")
    expand_target_coverage.set_defaults(command=ExpandTargetCoverage)
    expand_target_coverage.add_argument(
        "-c", "--comment", help="Custom comment after fixme comments"
    )
    expand_target_coverage.add_argument(
        "--submit", action="store_true", help=argparse.SUPPRESS
    )
    expand_target_coverage.add_argument(
        "--lint", action="store_true", help=argparse.SUPPRESS
    )
    expand_target_coverage.add_argument(
        "--subdirectory", help="Only upgrade TARGETS files within this directory."
    )
    expand_target_coverage.add_argument(
        "--fixme-threshold",
        type=int,
        help="Ignore all errors in a file if fixme count exceeds threshold.",
    )
    expand_target_coverage.add_argument(
        "--no-commit", action="store_true", help="Keep changes in working state."
    )

    # Subcommand: Consolidate nested local configurations
    consolidate_nested_configurations = commands.add_parser("consolidate-nested")
    consolidate_nested_configurations.set_defaults(
        command=ConsolidateNestedConfigurations
    )
    consolidate_nested_configurations.add_argument(
        "-c", "--comment", help="Custom comment after fixme comments"
    )
    consolidate_nested_configurations.add_argument(
        "--submit", action="store_true", help=argparse.SUPPRESS
    )
    consolidate_nested_configurations.add_argument(
        "--lint", action="store_true", help=argparse.SUPPRESS
    )
    consolidate_nested_configurations.add_argument("--subdirectory")
    consolidate_nested_configurations.add_argument(
        "--no-commit", action="store_true", help="Keep changes in working state."
    )

    # Initialize default values.
    arguments = parser.parse_args()
    if not hasattr(arguments, "command"):
        arguments.command = Fixme
        arguments.error_source = "stdin"

    # Initialize values that may be null-checked, but do not exist as a flag
    # for all subcommands
    if not hasattr(arguments, "paths"):
        arguments.paths = None
    if not hasattr(arguments, "error_source"):
        arguments.error_source = None
    if not hasattr(arguments, "comment"):
        arguments.comment = None

    logging.basicConfig(
        format="%(asctime)s %(levelname)s %(message)s",
        datefmt="%Y-%m-%d %H:%M:%S",
        level=logging.DEBUG if arguments.verbose else logging.INFO,
    )

    try:
        exit_code = ExitCode.SUCCESS
        arguments.command(arguments, repository).run()
    except UnstableAST as error:
        LOG.error(str(error))
        exit_code = ExitCode.FOUND_ERRORS
    except UserError as error:
        LOG.error(str(error))
        exit_code = ExitCode.FAILURE
    except Exception as error:
        LOG.error(str(error))
        LOG.debug(traceback.format_exc())
        exit_code = ExitCode.FAILURE

    sys.exit(exit_code)


def main() -> None:
    run(Repository())


if __name__ == "__main__":
    main()
