# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe


import argparse
import json
import logging
import re
import subprocess
import sys
import traceback
from collections import defaultdict
from enum import Enum
from logging import Logger
from pathlib import Path
from typing import Any, Dict, Iterator, List, Optional, Sequence, Tuple

from ...client.commands import ExitCode
from ...client.filesystem import get_filesystem
from .ast import verify_stable_ast
from .codemods import (
    run_missing_global_annotations,
    run_missing_overridden_return_annotations,
)
from .errors import (
    errors_from_stdin,
    filter_errors,
    fix_file,
    json_to_errors,
    sort_errors,
)
from .postprocess import apply_lint, get_lint_status


LOG: Logger = logging.getLogger(__name__)


class LocalMode(Enum):
    IGNORE = "pyre-ignore-all-errors"
    UNSAFE = "pyre-unsafe"
    STRICT = "pyre-strict"

    def get_regex(self) -> str:
        return "^[ \t]*# *" + self.value + " *$"

    def get_comment(self) -> str:
        return "# " + self.value


class VersionControl:
    LINTERS_TO_SKIP: List[str] = []

    @staticmethod
    def commit_message(directory: str, summary_override: Optional[str] = None) -> str:
        return ""

    @staticmethod
    def submit_changes(
        submit: bool, message: str, ignore_failures: bool = False
    ) -> None:
        pass


class Configuration:
    def __init__(self, path: Path, json_contents: Dict[str, Any]) -> None:
        self._path = path
        if path.name == ".pyre_configuration.local":
            self.is_local = True
        else:
            self.is_local = False
        self.root = str(path.parent)
        self.original_contents = json_contents

        # Configuration fields
        self.strict: bool = bool(json_contents.get("strict"))
        self.targets: Optional[List[str]] = json_contents.get("targets")
        self.source_directories: Optional[List[str]] = json_contents.get(
            "source_directories"
        )
        self.push_blocking: bool = bool(json_contents.get("push_blocking"))
        self.version: Optional[str] = json_contents.get("version")

    def get_contents(self) -> Dict[str, Any]:
        contents = self.original_contents

        def update_contents(key: str) -> None:
            attribute = getattr(self, key)
            if attribute:
                contents[key] = attribute
            elif key in contents:
                del contents[key]

        update_contents("targets")
        update_contents("source_directories")
        update_contents("push_blocking")
        update_contents("version")
        update_contents("strict")
        return contents

    @staticmethod
    def find_parent_file(
        filename: str, directory: Optional[Path] = None
    ) -> Optional[Path]:
        directory = directory or Path.cwd()
        root = directory.root
        while directory != root:
            configuration_path = directory / filename
            if configuration_path.is_file():
                return configuration_path
            parent = directory.parent
            if directory == parent:
                return None
            directory = parent
        return None

    @staticmethod
    def find_project_configuration(directory: Optional[Path] = None) -> Optional[Path]:
        return Configuration.find_parent_file(".pyre_configuration", directory)

    @staticmethod
    def find_local_configuration(directory: Optional[Path] = None) -> Optional[Path]:
        return Configuration.find_parent_file(".pyre_configuration.local", directory)

    @staticmethod
    def gather_local_configuration_paths(directory: str) -> Sequence[Path]:
        return [
            Path(path)
            for path in get_filesystem().list(
                directory, patterns=[r"**\.pyre_configuration.local"]
            )
        ]

    @staticmethod
    def gather_local_configurations(arguments) -> List["Configuration"]:
        LOG.info("Finding configurations...")
        configuration_paths = Configuration.gather_local_configuration_paths(".")
        if not configuration_paths:
            LOG.info("No projects with local configurations found.")
            project_configuration = Configuration.find_project_configuration()
            if project_configuration:
                configuration_paths = [project_configuration]
            else:
                LOG.error("No project configuration found.")
                return []
        configurations = []
        for configuration_path in configuration_paths:
            with open(configuration_path) as configuration_file:
                try:
                    configuration = Configuration(
                        configuration_path, json.load(configuration_file)
                    )
                    if configuration.push_blocking or (
                        not arguments.push_blocking_only
                    ):
                        configurations.append(configuration)
                except json.decoder.JSONDecodeError:
                    LOG.error(
                        "Configuration at `%s` is invalid, skipping.",
                        configuration_path,
                    )
        LOG.info(
            "Found %d %sconfiguration%s",
            len(configurations),
            "push-blocking " if arguments.push_blocking_only else "",
            "s" if len(configurations) != 1 else "",
        )
        return configurations

    def get_path(self) -> Path:
        return self._path

    def get_directory(self) -> Path:
        return self._path.parent

    def write(self) -> None:
        with open(self._path, "w") as configuration_file:
            json.dump(self.get_contents(), configuration_file, sort_keys=True, indent=2)
            configuration_file.write("\n")

    def remove_version(self) -> None:
        if not self.version:
            LOG.info("Version not found in configuration.")
            return
        self.version = None
        self.write()

    def add_strict(self) -> None:
        if self.strict:
            LOG.info("Configuration is already strict.")
            return
        self.strict = True
        self.write()

    def add_targets(self, targets: List[str]) -> None:
        existing_targets = self.targets
        if existing_targets:
            self.targets = sorted(set(existing_targets + targets))
        else:
            self.targets = targets
        self.write()

    def get_errors(self, should_clean: bool = True) -> List[Dict[str, Any]]:
        # TODO(T37074129): Better parallelization or truncation needed for fbcode
        if self.targets and should_clean:
            try:
                # If building targets, run clean or space may run out on device!
                LOG.info("Running `buck clean`...")
                subprocess.call(["buck", "clean"], timeout=200)
            except subprocess.TimeoutExpired:
                LOG.warning("Buck timed out. Try running `buck kill` before retrying.")
                return []
            except subprocess.CalledProcessError as error:
                LOG.warning("Error calling `buck clean`: %s", str(error))
                return []
        try:
            LOG.info("Checking `%s`...", self.root)
            if self.is_local:
                process = subprocess.run(
                    ["pyre", "-l", self.root, "--output=json", "check"],
                    stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE,
                )
            else:
                process = subprocess.run(
                    ["pyre", "--output=json", "check"],
                    stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE,
                )
            json_string = process.stdout.decode().strip()
            errors = json_to_errors(json_string)
            LOG.info("Found %d error%s.", len(errors), "s" if len(errors) != 1 else "")
            return errors
        except subprocess.CalledProcessError as error:
            LOG.warning("Error calling pyre: %s", str(error))
            return []


def errors_from_run(_arguments: argparse.Namespace) -> List[Dict[str, Any]]:
    configuration_path = Configuration.find_project_configuration()
    if not configuration_path:
        LOG.warning("Could not find pyre configuration.")
        return []
    with open(configuration_path) as configuration_file:
        configuration = Configuration(configuration_path, json.load(configuration_file))
        errors = configuration.get_errors()
        return filter_errors(_arguments, errors)


# Exposed for testing.
def _upgrade_project(
    arguments: argparse.Namespace,
    configuration: Configuration,
    root: Path,
    version_control: VersionControl,
) -> None:
    LOG.info("Processing %s", configuration.get_directory())
    if not configuration.is_local or not configuration.version:
        return
    configuration.remove_version()
    errors = (
        errors_from_stdin(arguments)
        if arguments.from_stdin
        else configuration.get_errors()
    )
    if len(errors) > 0:
        fix(arguments, sort_errors(errors))

        # Lint and re-run pyre once to resolve most formatting issues
        if arguments.lint:
            lint_status = get_lint_status(version_control.LINTERS_TO_SKIP)
            if lint_status:
                apply_lint(version_control.LINTERS_TO_SKIP)
                errors = configuration.get_errors(should_clean=False)
                fix(arguments, sort_errors(errors))
    try:
        project_root = root.resolve()
        local_root = configuration.get_directory().resolve()
        version_control.submit_changes(
            arguments.submit,
            version_control.commit_message(str(local_root.relative_to(project_root))),
        )
    except subprocess.CalledProcessError:
        LOG.info("Error while running hg.")


def fix(
    arguments: argparse.Namespace,
    result: Iterator[Tuple[str, Iterator[Dict[str, Any]]]],
) -> None:
    for path, errors in result:
        LOG.info("Processing `%s`", path)

        # Build map from line to error codes.
        error_map = defaultdict(lambda: [])
        for error in errors:
            if error["concise_description"]:
                description = error["concise_description"]
            else:
                description = error["description"]
            match = re.search(r"\[(\d+)\]: (.*)", description)
            if match:
                error_map[error["line"]].append(
                    {"code": match.group(1), "description": match.group(2)}
                )
        custom_comment = arguments.comment if hasattr(arguments, "comment") else ""
        max_line_length = (
            arguments.max_line_length if arguments.max_line_length > 0 else None
        )
        fix_file(path, error_map, custom_comment, max_line_length, arguments.truncate)


@verify_stable_ast
def add_local_mode(filename: str, mode: LocalMode) -> None:
    LOG.info("Processing `%s`", filename)
    path = Path(filename)
    text = path.read_text()
    if "@" "generated" in text:
        LOG.warning("Attempting to edit generated file %s, skipping.", filename)
        return

    lines = text.split("\n")  # type: List[str]

    # Check if a local mode is already set.
    for line in lines:
        for local_mode in LocalMode:
            if re.match(local_mode.get_regex(), line):
                return

    def is_header(line: str) -> bool:
        is_comment = line.lstrip().startswith("#")
        is_pyre_ignore = (
            re.match("^[ \t]*# *pyre-ignore *$", line)
            or re.match("^[ \t]*# *pyre-fixme *$", line)
            or re.match("^[ \t]*# *type: ignore *$", line)
        )
        return is_comment and not is_pyre_ignore

    # Add local mode.
    new_lines = []
    past_header = False
    for line in lines:
        if not past_header and not is_header(line):
            past_header = True
            if len(new_lines) != 0:
                new_lines.append("")
            new_lines.append(mode.get_comment())
        new_lines.append(line)
    new_text = "\n".join(new_lines)
    path.write_text(new_text)


def run_global_version_update(
    arguments: argparse.Namespace, version_control: VersionControl
) -> None:
    global_configuration = Configuration.find_project_configuration()
    if global_configuration is None:
        LOG.error("No global configuration file found.")
        return

    with open(global_configuration, "r") as global_configuration_file:
        configuration = json.load(global_configuration_file)
        if "version" not in configuration:
            LOG.error(
                "Global configuration at %s has no version field.", global_configuration
            )
            return

        old_version = configuration["version"]

    # Rewrite.
    with open(global_configuration, "w") as global_configuration_file:
        configuration["version"] = arguments.hash

        # This will sort the keys in the configuration - we won't be clobbering comments
        # since Python's JSON parser disallows comments either way.
        json.dump(configuration, global_configuration_file, sort_keys=True, indent=2)
        global_configuration_file.write("\n")

    paths = arguments.paths
    configuration_paths = (
        [path / ".pyre_configuration.local" for path in paths]
        if paths
        else [
            configuration.get_path()
            for configuration in Configuration.gather_local_configurations(arguments)
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
            contents["version"] = old_version

        with open(configuration_path, "w") as configuration_file:
            json.dump(contents, configuration_file, sort_keys=True, indent=2)
            configuration_file.write("\n")

    try:
        commit_summary = "Automatic upgrade to hash `{}`".format(arguments.hash)
        version_control.submit_changes(
            arguments.submit,
            version_control.commit_message(
                "global configuration", summary_override=commit_summary
            ),
            ignore_failures=True,
        )
    except subprocess.CalledProcessError:
        LOG.info("Error while running hg.")


def run_strict_default(
    arguments: argparse.Namespace, version_control: VersionControl
) -> None:
    project_configuration = Configuration.find_project_configuration()
    if project_configuration is None:
        LOG.info("No project configuration found for the given directory.")
        return
    local_configuration = arguments.local_configuration
    if local_configuration:
        configuration_path = local_configuration / ".pyre_configuration.local"
    else:
        configuration_path = project_configuration
    with open(configuration_path) as configuration_file:
        configuration = Configuration(configuration_path, json.load(configuration_file))
        LOG.info("Processing %s", configuration.get_directory())
        configuration.add_strict()
        errors = configuration.get_errors()

        if len(errors) > 0:
            result = sort_errors(errors)
            for filename, _ in result:
                add_local_mode(filename, LocalMode.UNSAFE)

            if arguments.lint:
                lint_status = get_lint_status(version_control.LINTERS_TO_SKIP)
                if lint_status:
                    apply_lint(version_control.LINTERS_TO_SKIP)


def run_fixme(arguments: argparse.Namespace, version_control: VersionControl) -> None:
    if arguments.run:
        errors = errors_from_run(arguments)
        fix(arguments, sort_errors(errors))

        if arguments.lint:
            lint_status = get_lint_status(version_control.LINTERS_TO_SKIP)
            if lint_status:
                apply_lint(version_control.LINTERS_TO_SKIP)
                errors = errors_from_run(arguments)
                fix(arguments, sort_errors(errors))
    else:
        errors = errors_from_stdin(arguments)
        fix(arguments, sort_errors(errors))


def run_fixme_single(
    arguments: argparse.Namespace, version_control: VersionControl
) -> None:
    project_configuration = Configuration.find_project_configuration()
    if project_configuration is None:
        LOG.info("No project configuration found for the given directory.")
        return
    configuration_path = arguments.path / ".pyre_configuration.local"
    with open(configuration_path) as configuration_file:
        configuration = Configuration(configuration_path, json.load(configuration_file))
        _upgrade_project(
            arguments, configuration, project_configuration.parent, version_control
        )


def run_fixme_all(
    arguments: argparse.Namespace, version_control: VersionControl
) -> None:
    # Create sandcastle command.
    if arguments.sandcastle:
        configurations = Configuration.gather_local_configurations(arguments)
        paths = [str(configuration.get_directory()) for configuration in configurations]
        with open(arguments.sandcastle) as sandcastle_file:
            sandcastle_command = json.load(sandcastle_file)
        if arguments.hash:
            sandcastle_command["args"]["hash"] = arguments.hash
        sandcastle_command["args"]["paths"] = paths
        sandcastle_command["args"]["push_blocking_only"] = arguments.push_blocking_only
        command = ["scutil", "create"]
        subprocess.run(command, input=json.dumps(sandcastle_command).encode())
        return

    # Run locally.
    if arguments.hash and isinstance(arguments.hash, str):
        run_global_version_update(arguments, version_control)

    project_configuration = Configuration.find_project_configuration()
    if project_configuration is None:
        LOG.info("No project configuration found for the current directory.")
        return

    configurations = Configuration.gather_local_configurations(arguments)
    for configuration in configurations:
        _upgrade_project(
            arguments, configuration, project_configuration.parent, version_control
        )


def run_fixme_targets_file(
    arguments: argparse.Namespace,
    project_directory: Path,
    path: str,
    target_names: List[str],
    version_control: VersionControl,
) -> None:
    LOG.info("Processing %s/TARGETS...", path)

    def get_errors(
        path: str, targets: List[str], check_alternate_names: bool = True
    ) -> Optional[List[Dict[str, Any]]]:
        buck_test_command = (
            ["buck", "test", "--show-full-json-output"]
            + targets
            + ["--", "--run-disabled"]
        )
        buck_test = subprocess.run(
            buck_test_command, stdout=subprocess.PIPE, stderr=subprocess.PIPE
        )
        errors = None
        if buck_test.returncode == 0:
            # Successful run with no type errors
            LOG.info("No errors in %s/TARGETS...", path)
        elif buck_test.returncode == 32:
            buck_test_output = buck_test.stdout.decode().split("\n")
            pyre_error_pattern = re.compile(
                r"\W*(.*\.pyi?):(\d*):(\d*) (.* \[(\d*)\]: .*)"
            )
            errors = {}
            for output_line in buck_test_output:
                matched = pyre_error_pattern.match(output_line)
                if matched:
                    path = matched.group(1)
                    line = int(matched.group(2))
                    column = int(matched.group(3))
                    description = matched.group(4)
                    code = matched.group(5)
                    error = {
                        "line": line,
                        "column": column,
                        "path": project_directory / path,
                        "code": code,
                        "description": description,
                        "concise_description": description,
                    }
                    errors[(line, column, path, code)] = error
            errors = list(errors.values())
        elif check_alternate_names and buck_test.returncode == 5:
            # Generated type check target was not named as expected.
            LOG.warning("Could not find buck test targets: %s", targets)
            LOG.info("Looking for similar targets...")
            targets_to_retry = []
            for target in targets:
                query_command = ["buck", "query", target]
                similar_targets = subprocess.run(
                    query_command, stdout=subprocess.PIPE, stderr=subprocess.PIPE
                )
                output = similar_targets.stdout.decode()
                error_output = similar_targets.stderr.decode()
                if output:
                    targets_to_retry.append(output)
                elif error_output:
                    typecheck_targets = [
                        target.strip()
                        for target in error_output.split("\n")
                        if target.strip().endswith("-pyre-typecheck")
                    ]
                    targets_to_retry += typecheck_targets
            if targets_to_retry:
                LOG.info("Retrying similar targets: %s", targets_to_retry)
                errors = get_errors(path, targets_to_retry, check_alternate_names=False)
            else:
                LOG.error("No similar targets to retry.")
        else:
            LOG.error(
                "Failed to run buck test command:\n\t%s\n\n%s",
                " ".join(buck_test_command),
                buck_test.stderr.decode(),
            )
        return errors

    targets = [path + ":" + name + "-pyre-typecheck" for name in target_names]
    errors = get_errors(path, targets)
    if not errors:
        return
    LOG.info("Found %d type errors in %s/TARGETS.", len(errors), path)
    if not errors:
        return
    fix(arguments, sort_errors(errors))
    if not arguments.lint:
        return
    lint_status = get_lint_status(version_control.LINTERS_TO_SKIP)
    if lint_status:
        apply_lint(version_control.LINTERS_TO_SKIP)
        errors = get_errors(path, targets)
        if not errors:
            LOG.info("Errors unchanged after linting.")
            return
        LOG.info("Found %d type errors after linting.", len(errors))
        fix(arguments, sort_errors(errors))


def find_targets(search_root: Path) -> Dict[str, List[str]]:
    LOG.info("Finding typecheck targets in %s", search_root)
    # TODO(T56778370): Clean up code by parsing the TARGETS file rather than using grep.
    typing_field = "check_types ?= ?True"
    targets_regex = r"(?s)name = ((?!\n\s*name).)*{}((?!\n\s*name).)*".format(
        typing_field
    )
    find_targets_command = [
        "grep",
        "-RPzo",
        "--include=*TARGETS",
        targets_regex,
        search_root,
    ]
    find_targets = subprocess.run(
        find_targets_command, stdout=subprocess.PIPE, stderr=subprocess.PIPE
    )
    if find_targets.returncode == 1:
        LOG.info("Did not find any targets.")
        return {}
    if find_targets.returncode != 0:
        LOG.error("Failed to search for targets: %s", find_targets.stderr.decode())
        return {}
    output = find_targets.stdout.decode()
    targets = re.split(typing_field, output)
    target_pattern = re.compile(r".*?([^\s]*)\/TARGETS:.*name = \"([^\"]*)\".*")
    target_names = {}
    total_targets = 0
    for target in targets:
        matched = target_pattern.match(target.replace("\n", " ").strip())
        if matched:
            total_targets += 1
            path = matched.group(1).strip()
            target_name = matched.group(2)
            if path in target_names:
                target_names[path].append(target_name)
            else:
                target_names[path] = [target_name]
    LOG.info(
        "Found {} typecheck targets in {} TARGETS files to analyze".format(
            total_targets, len(target_names)
        )
    )
    return target_names


def run_fixme_targets(
    arguments: argparse.Namespace, version_control: VersionControl
) -> None:
    # Currently does not support sandcastle integration, or setting the global hash
    # at the same time. As-is, run this locally after the global hash is updated.
    subdirectory = arguments.subdirectory
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
        run_fixme_targets_file(
            arguments, project_directory, path, target_names, version_control
        )
    try:
        if not arguments.no_commit:
            version_control.submit_changes(
                arguments.submit,
                version_control.commit_message("{} (TARGETS)".format(search_root)),
            )
    except subprocess.CalledProcessError:
        LOG.info("Error while running hg.")


def remove_non_pyre_ignores(
    subdirectory: Path, arguments: argparse.Namespace, version_control: VersionControl
) -> None:
    python_files = [
        str(subdirectory / path)
        for path in get_filesystem().list(str(subdirectory), patterns=[r"**/*.py"])
    ]
    if python_files:
        LOG.info("...cleaning %s python files", len(python_files))
        remove_type_ignore_command = [
            "sed",
            "-i",
            r"s/# \?type: \?ignore$//g",
        ] + python_files
        subprocess.check_output(remove_type_ignore_command)


def run_migrate_targets(
    arguments: argparse.Namespace, version_control: VersionControl
) -> None:
    subdirectory = arguments.subdirectory
    subdirectory = Path(subdirectory) if subdirectory else Path.cwd()
    LOG.info("Migrating typecheck targets in {}".format(subdirectory))

    # Remove explicit check types options.
    targets_files = [
        str(subdirectory / path)
        for path in get_filesystem().list(str(subdirectory), patterns=[r"**/TARGETS"])
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

    remove_non_pyre_ignores(subdirectory, arguments, version_control)
    run_fixme_targets(arguments, version_control)


def run_targets_to_configuration(
    arguments: argparse.Namespace, version_control: VersionControl
) -> None:
    # TODO(T62926437): Basic integration testing.
    # TODO(T62926437): Support glob target with file-level suppression of files
    # excluded from original targets.
    # TODO(T62926437): Dedup additional targets with existing glob targets.
    subdirectory = arguments.subdirectory
    subdirectory = Path(subdirectory) if subdirectory else Path.cwd()
    LOG.info("Converting typecheck targets to pyre configuration in `%s`", subdirectory)

    # Create or amend to existing pyre configuration
    all_targets = find_targets(subdirectory)
    new_targets = []
    if not all_targets:
        LOG.warning("No configuration created because no targets found.")
        return
    targets_files = [
        str(subdirectory / path)
        for path in get_filesystem().list(str(subdirectory), patterns=[r"**/TARGETS"])
    ]
    for path, target_names in all_targets.items():
        new_targets += [path + ":" + name for name in target_names]
    project_configuration = Configuration.find_project_configuration(subdirectory)
    local_configuration = Configuration.find_local_configuration(subdirectory)
    if local_configuration:
        LOG.warning(
            "Pyre project already exists at %s.\n\
            Amending targets to existing configuration.",
            local_configuration,
        )
        with open(local_configuration) as configuration_file:
            configuration = Configuration(
                local_configuration, json.load(configuration_file)
            )
            configuration.add_targets(new_targets)
    elif project_configuration:
        LOG.info("Found project configuration at %s.", project_configuration)
        with open(project_configuration) as configuration_file:
            configuration = Configuration(
                project_configuration, json.load(configuration_file)
            )
            if (
                configuration.targets
                or configuration.source_directories
                or configuration.get_path() == subdirectory / ".pyre_configuration"
            ):
                LOG.info("Amending targets to existing project configuration.")
                configuration.add_targets(new_targets)
            else:
                local_configuration_path = subdirectory / ".pyre_configuration.local"
                LOG.info(
                    "Creating local configuration at %s.", local_configuration_path
                )
                configuration_contents = {
                    "targets": new_targets,
                    "push_blocking": True,
                    "strict": True,
                }
                # Heuristic: if all targets with type checked targets are setting
                # a target to be strictly checked, let's turn on default strict.
                for targets_file in targets_files:
                    regex_patterns = [
                        r"check_types_options \?=.*strict.*",
                        r"typing_options \?=.*strict.*",
                    ]
                    result = subprocess.run(
                        ["grep", "-x", r"\|".join(regex_patterns), targets_file]
                    )
                    if result.returncode != 0:
                        configuration_contents["strict"] = False
                        break
                configuration = Configuration(
                    local_configuration_path, configuration_contents
                )
                configuration.write()
    else:
        LOG.warning(
            "Could not find a project configuration with binary and typeshed \
            locations.\nPlease run `pyre init` before attempting to migrate."
        )
        return

    # Remove all type-related target settings
    LOG.info("Removing typing options from %s targets files", len(targets_files))
    typing_options_regex = [
        r"typing \?=.*",
        r"check_types \?=.*",
        r"check_types_options \?=.*",
        r"typing_options \?=.*",
    ]
    remove_typing_fields_command = [
        "sed",
        "-i",
        "/" + r"\|".join(typing_options_regex) + "/d",
    ] + targets_files
    subprocess.run(remove_typing_fields_command)

    remove_non_pyre_ignores(subdirectory, arguments, version_control)
    arguments.path = subdirectory
    run_fixme_single(arguments, version_control)


def path_exists(filename: str) -> Path:
    path = Path(filename)
    if not path.exists():
        raise ValueError("No file at {}".format(filename))
    return path


def run(version_control: VersionControl) -> None:
    parser = argparse.ArgumentParser()
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
        function=run_missing_overridden_return_annotations
    )

    missing_global_annotations = commands.add_parser(
        "missing-global-annotations",
        help="Add annotations according to errors inputted through stdin.",
    )
    missing_global_annotations.set_defaults(function=run_missing_global_annotations)

    # Subcommand: Change default pyre mode to strict and adjust module headers.
    strict_default = commands.add_parser("strict-default")
    strict_default.set_defaults(function=run_strict_default)
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
    update_global_version.set_defaults(function=run_global_version_update)
    update_global_version.add_argument("hash", help="Hash of new Pyre version")
    update_global_version.add_argument(
        "-p", "--push-blocking-only", action="store_true"
    )
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
    fixme.set_defaults(function=run_fixme)
    fixme.add_argument("--comment", help="Custom comment after fixme comments")
    fixme.add_argument("--run", action="store_true")
    fixme.add_argument("--lint", action="store_true", help=argparse.SUPPRESS)

    # Subcommand: Fixme all errors for a single project.
    fixme_single = commands.add_parser("fixme-single")
    fixme_single.set_defaults(function=run_fixme_single)
    fixme_single.add_argument(
        "path", help="Path to project root with local configuration", type=path_exists
    )
    fixme_single.add_argument(
        "--from-stdin", action="store_true", help=argparse.SUPPRESS
    )
    fixme_single.add_argument("--submit", action="store_true", help=argparse.SUPPRESS)
    fixme_single.add_argument("--lint", action="store_true", help=argparse.SUPPRESS)

    # Subcommand: Fixme all errors in all projects with local configurations.
    fixme_all = commands.add_parser("fixme-all")
    fixme_all.set_defaults(function=run_fixme_all)
    fixme_all.add_argument(
        "-c", "--comment", help="Custom comment after fixme comments"
    )
    fixme_all.add_argument("-p", "--push-blocking-only", action="store_true")
    fixme_all.add_argument("--submit", action="store_true", help=argparse.SUPPRESS)
    fixme_all.add_argument("--lint", action="store_true", help=argparse.SUPPRESS)
    fixme_all.add_argument(
        "-s",
        "--sandcastle",
        help="Create upgrade stack on sandcastle.",
        type=path_exists,
    )
    fixme_all.add_argument(
        "hash", nargs="?", default=None, help="Hash of new Pyre version"
    )

    # Subcommand: Fixme all errors in targets running type checking
    fixme_targets = commands.add_parser("fixme-targets")
    fixme_targets.set_defaults(function=run_fixme_targets)
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
    migrate_targets.set_defaults(function=run_migrate_targets)
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
    targets_to_configuration.set_defaults(function=run_targets_to_configuration)
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
        action="store_true",
        help="Use a toplevel glob target and suppress unchecked files.",
    )

    # Initialize default values.
    arguments = parser.parse_args()
    if not hasattr(arguments, "function"):
        arguments.run = False
        arguments.function = run_fixme

    # Initialize values that may be null-checked, but do not exist as a flag
    # for all subcommands
    if not hasattr(arguments, "paths"):
        arguments.paths = None
    if not hasattr(arguments, "from_stdin"):
        arguments.from_stdin = None

    logging.basicConfig(
        format="%(asctime)s %(levelname)s %(message)s",
        datefmt="%Y-%m-%d %H:%M:%S",
        level=logging.DEBUG if arguments.verbose else logging.INFO,
    )

    try:
        exit_code = ExitCode.SUCCESS
        arguments.function(arguments, version_control)
    except Exception as error:
        LOG.error(str(error))
        LOG.info(traceback.format_exc())
        exit_code = ExitCode.FAILURE

    sys.exit(exit_code)
