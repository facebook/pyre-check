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
from logging import Logger
from pathlib import Path
from typing import Any, Dict, Iterator, List, Optional, Tuple

from ...client.commands import ExitCode
from ...client.filesystem import get_filesystem
from .ast import verify_stable_ast
from .codemods import (
    run_missing_global_annotations,
    run_missing_overridden_return_annotations,
)
from .errors import errors_from_stdin, filter_errors, json_to_errors, sort_errors
from .postprocess import apply_lint, get_lint_status


LOG: Logger = logging.getLogger(__name__)


class Configuration:
    def __init__(self, path: Path, json_contents: Dict[str, Any]) -> None:
        self._path = path
        if path.name == ".pyre_configuration.local":
            self.is_local = True
        else:
            self.is_local = False
        self.root = str(path.parent)
        self.targets = json_contents.get("targets")
        self.source_directories = json_contents.get("source_directories")
        self.push_blocking = bool(json_contents.get("push_blocking"))
        self.version = json_contents.get("version")

    @staticmethod
    def find_project_configuration(directory: Optional[Path] = None) -> Optional[Path]:
        directory = directory or Path.cwd()
        root = directory.root
        while directory != root:
            configuration_path = directory / ".pyre_configuration"
            if configuration_path.is_file():
                return configuration_path
            directory = directory.parent
        return None

    @staticmethod
    def gather_local_configurations(arguments) -> List["Configuration"]:
        LOG.info("Finding configurations...")
        configuration_paths = [
            Path(path)
            for path in get_filesystem().list(
                ".", patterns=[r"**\.pyre_configuration.local"]
            )
        ]
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

    def remove_version(self) -> None:
        with open(self._path) as configuration_file:
            contents = json.load(configuration_file)
        if "version" not in contents:
            LOG.info("Version not found in configuration.")
            return
        del contents["version"]
        with open(self._path, "w") as configuration_file:
            json.dump(contents, configuration_file, sort_keys=True, indent=2)
            configuration_file.write("\n")

    def add_strict(self) -> None:
        with open(self._path) as configuration_file:
            contents = json.load(configuration_file)
        if "strict" in contents:
            LOG.info("Configuration is already strict.")
            return
        contents["strict"] = True
        with open(self._path, "w") as configuration_file:
            json.dump(contents, configuration_file, sort_keys=True, indent=2)
            configuration_file.write("\n")

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


def errors_from_run(_arguments) -> List[Dict[str, Any]]:
    configuration_path = Configuration.find_project_configuration()
    if not configuration_path:
        LOG.warning("Could not find pyre configuration.")
        return []
    with open(configuration_path) as configuration_file:
        configuration = Configuration(configuration_path, json.load(configuration_file))
        errors = configuration.get_errors()
        return filter_errors(_arguments, errors)


def remove_comment_preamble(lines: List[str]) -> None:
    # Deprecated: leaving remove logic until live old-style comments are cleaned up.
    while lines:
        old_line = lines.pop()
        new_line = re.sub(r"# pyre: .*$", "", old_line).rstrip()
        if old_line == "" or new_line != "":
            # The preamble has ended.
            lines.append(new_line)
            return


def _split_across_lines(
    comment: str, indent: int, max_line_length: Optional[int]
) -> List[str]:
    if not max_line_length or len(comment) <= max_line_length:
        return [comment]

    comment = comment.lstrip()
    available_columns = max_line_length - indent - len("#  ")

    buffered_line = ""
    result = []
    prefix = " " * indent
    for token in comment.split():
        if buffered_line and (
            len(buffered_line) + len(token) + len(" ") > available_columns
        ):
            # This new token would make the line exceed the limit,
            # hence terminate what we have accumulated.
            result.append(("{}{}".format(prefix, buffered_line)).rstrip())
            # The first line already has a comment token on it, so don't prefix #. For
            # the rest, we need to add the comment symbol manually.
            prefix = "{}#  ".format(" " * indent)
            buffered_line = ""

        buffered_line = buffered_line + token + " "

    result.append(("{}{}".format(prefix, buffered_line)).rstrip())
    return result


@verify_stable_ast
def fix_file(
    arguments: argparse.Namespace,
    filename: str,
    errors: Dict[int, List[Dict[str, str]]],
) -> None:
    custom_comment = arguments.comment if hasattr(arguments, "comment") else ""
    max_line_length = (
        arguments.max_line_length if arguments.max_line_length > 0 else None
    )
    path = Path(filename)
    text = path.read_text()
    if "@" "generated" in text:
        LOG.warning("Attempting to upgrade generated file %s, skipping.", filename)
        return
    lines = text.split("\n")  # type: List[str]

    # Replace lines in file.
    new_lines = []
    removing_pyre_comments = False
    for index, line in enumerate(lines):
        if removing_pyre_comments:
            # Only delete continuation comments of the form
            # "# pyre-fixme[2]:\n#  expected type `T`."
            if line.lstrip().startswith("#  "):
                continue
            else:
                removing_pyre_comments = False
        number = index + 1
        if number not in errors:
            new_lines.append(line)
            continue
        if errors[number][0]["code"] == "0":
            # Handle unused ignores.
            removing_pyre_comments = True
            replacement = re.sub(r"# pyre-(ignore|fixme).*$", "", line).rstrip()
            if replacement == "":
                remove_comment_preamble(new_lines)
            else:
                new_lines.append(replacement)
            continue

        comments = []
        for error in errors[number]:
            indent = len(line) - len(line.lstrip(" "))
            description = custom_comment if custom_comment else error["description"]
            comment = "{}# pyre-fixme[{}]: {}".format(
                " " * indent, error["code"], description
            )

            if not max_line_length or len(comment) <= max_line_length:
                comments.append(comment)
            else:
                if arguments.truncate:
                    comments.append(comment[: (max_line_length - 3)] + "...")
                else:
                    comments.extend(
                        _split_across_lines(comment, indent, max_line_length)
                    )

        LOG.info(
            "Adding comment%s on line %d: %s",
            "s" if len(comments) > 1 else "",
            number,
            " \n".join(comments),
        )
        new_lines.extend(comments)
        new_lines.append(line)
    new_text = "\n".join(new_lines)
    path.write_text(new_text)


# Exposed for testing.
def _upgrade_project(
    arguments: argparse.Namespace,
    configuration: Configuration,
    root: Path,
    version_control,
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
            lint_status = get_lint_status()
            if lint_status:
                apply_lint()
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
        fix_file(arguments, path, error_map)


@verify_stable_ast
def add_local_unsafe(arguments: argparse.Namespace, filename: str) -> None:
    LOG.info("Processing `%s`", filename)
    path = Path(filename)
    text = path.read_text()
    if "@" "generated" in text:
        LOG.warning("Attempting to edit generated file %s, skipping.", filename)
        return

    lines = text.split("\n")  # type: List[str]

    # Check if already locally strict or ignore-all.
    for line in lines:
        if re.match("^[ \t]*# *pyre-strict *$", line) or re.match(
            "^[ \t]*# *pyre-ignore-all-errors *$", line
        ):
            return

    def is_header(line: str) -> bool:
        is_comment = line.lstrip().startswith("#")
        is_pyre_ignore = (
            re.match("^[ \t]*# *pyre-ignore *$", line)
            or re.match("^[ \t]*# *pyre-fixme *$", line)
            or re.match("^[ \t]*# *type: ignore *$", line)
        )
        return is_comment and not is_pyre_ignore

    # Add local unsafe.
    new_lines = []
    past_header = False
    for line in lines:
        if not past_header and not is_header(line):
            past_header = True
            if len(new_lines) != 0:
                new_lines.append("")
            new_lines.append("# pyre-unsafe")
        new_lines.append(line)
    new_text = "\n".join(new_lines)
    path.write_text(new_text)


def run_global_version_update(arguments: argparse.Namespace, version_control) -> None:
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
        )
    except subprocess.CalledProcessError:
        LOG.info("Error while running hg.")


def run_strict_default(arguments: argparse.Namespace, _version_control) -> None:
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
                add_local_unsafe(arguments, filename)

            if arguments.lint:
                lint_status = get_lint_status()
                if lint_status:
                    apply_lint()


def run_fixme(arguments: argparse.Namespace, version_control) -> None:
    if arguments.run:
        errors = errors_from_run(arguments)
        fix(arguments, sort_errors(errors))

        if arguments.lint:
            lint_status = get_lint_status()
            if lint_status:
                apply_lint()
                errors = errors_from_run(arguments)
                fix(arguments, sort_errors(errors))
    else:
        errors = errors_from_stdin(arguments)
        fix(arguments, sort_errors(errors))


def run_fixme_single(arguments: argparse.Namespace, version_control) -> None:
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


def run_fixme_all(arguments: argparse.Namespace, version_control) -> None:
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
) -> None:
    LOG.info("Processing %s/TARGETS...", path)
    targets = [path + ":" + name + "-typecheck" for name in target_names]
    buck_test_command = ["buck", "test", "--show-full-json-output"] + targets

    def get_errors(path: str) -> Optional[List[Dict[str, Any]]]:
        buck_test = subprocess.run(
            buck_test_command, stdout=subprocess.PIPE, stderr=subprocess.PIPE
        )
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
            return list(errors.values())
        else:
            LOG.error(
                "Failed to run buck test command:\n\t%s\n\n%s",
                " ".join(buck_test_command),
                buck_test.stderr.decode(),
            )

    errors = get_errors(path)
    if not errors:
        return
    LOG.info("Found %d type errors in %s/TARGETS.", len(errors), path)
    if not errors:
        return
    fix(arguments, sort_errors(errors))
    if not arguments.lint:
        return
    lint_status = get_lint_status()
    if lint_status:
        LOG.info("Linting...")
        apply_lint()
        errors = get_errors(path)
        if not errors:
            LOG.info("Errors unchanged after linting.")
            return
        LOG.info("Found %d type errors after linting.", len(errors))
        fix(arguments, sort_errors(errors))


def run_fixme_targets(arguments: argparse.Namespace, version_control) -> None:
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
    LOG.info("Finding typecheck targets in %s", search_root)
    # TODO(T56778370): Clean up code by parsing the TARGETS file rather than using grep.
    typing_field = "check_types ?= ?True"
    typing_options_field = 'check_types_options ?= ?"[^"]*pyre[^"]*",?'
    targets_regex = r"(?s)name = ((?!\n\s*name).)*{}((?!\n\s*name).)*{}".format(
        typing_field, typing_options_field
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
        LOG.info("Did not find any targets to upgrade.")
        return
    if find_targets.returncode != 0:
        LOG.error("Failed to search for targets: %s", find_targets.stderr.decode())
        return
    output = find_targets.stdout.decode()
    targets = re.split(typing_options_field, output)
    target_pattern = re.compile(r"(.*)\/TARGETS:.*name = \"([^\"]*)\".*")
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
        "Found %d typecheck targets in %d TARGETS files to analyze",
        total_targets,
        len(target_names),
    )
    for path, target_names in target_names.items():
        run_fixme_targets_file(arguments, project_directory, path, target_names)
    try:
        if not arguments.no_commit:
            version_control.submit_changes(
                arguments.submit,
                version_control.commit_message("{} (TARGETS)".format(search_root)),
            )
    except subprocess.CalledProcessError:
        LOG.info("Error while running hg.")


def path_exists(filename: str) -> Path:
    path = Path(filename)
    if not path.exists():
        raise ValueError("No file at {}".format(filename))
    return path


def run(version_control) -> None:
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
