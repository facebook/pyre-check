# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import argparse
import itertools
import json
import logging
import os
import pathlib
import re
import subprocess
import sys
import traceback
from collections import defaultdict
from typing import Any, Dict, List, Optional, Tuple

from ...client.commands import ExitCode
from ...client.filesystem import get_filesystem


LOG = logging.getLogger(__name__)


def json_to_errors(json_string: Optional[str]) -> List[Dict[str, Any]]:
    if json_string:
        try:
            return json.loads(json_string)
        except json.decoder.JSONDecodeError:
            LOG.error(
                "Recevied invalid JSON as input."
                "If piping from `pyre check` be sure to use `--output=json`."
            )
    else:
        LOG.error(
            "Recevied no input."
            "If piping from `pyre check` be sure to use `--output=json`."
        )
    return []


def sort_errors(errors: List[Dict[str, Any]]) -> List[Tuple[str, List[Any]]]:
    def error_path(error):
        return error["path"]

    return itertools.groupby(sorted(errors, key=error_path), error_path)


class Configuration:
    def __init__(self, path: str, json_contents: Dict[str, Any]):
        self._path = path
        if path.endswith("/.pyre_configuration.local"):
            self.is_local = True
        else:
            self.is_local = False
        self.root = os.path.dirname(path)
        self.targets = json_contents.get("targets")
        self.source_directories = json_contents.get("source_directories")
        self.push_blocking = bool(json_contents.get("push_blocking"))
        self.version = json_contents.get("version")

    @staticmethod
    def find_project_configuration() -> Optional[str]:
        directory = os.getcwd()
        while directory != "/":
            configuration_path = os.path.join(directory, ".pyre_configuration")
            if os.path.isfile(configuration_path):
                return configuration_path
            directory = os.path.dirname(directory)
        return None

    @staticmethod
    def gather_local_configurations(arguments) -> List["Configuration"]:
        LOG.info("Finding configurations...")
        configuration_paths = get_filesystem().list(
            ".", patterns=[r"**\.pyre_configuration.local"]
        )
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
                configuration = Configuration(
                    configuration_path, json.load(configuration_file)
                )
                if configuration.push_blocking or (not arguments.push_blocking_only):
                    configurations.append(configuration)
        LOG.info(
            "Found %d %sconfiguration%s",
            len(configurations),
            "push-blocking " if arguments.push_blocking_only else "",
            "s" if len(configurations) != 1 else "",
        )
        return configurations

    def get_path(self) -> str:
        return self._path

    def get_directory(self) -> str:
        return os.path.dirname(self._path)

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


def errors_from_stdin(_arguments) -> List[Dict[str, Any]]:
    input = sys.stdin.read()
    return json_to_errors(input)


def errors_from_run(_arguments) -> List[Dict[str, Any]]:
    configuration_path = Configuration.find_project_configuration()
    if not configuration_path:
        LOG.warning("Could not find pyre configuration.")
        return []
    with open(configuration_path) as configuration_file:
        configuration = Configuration(configuration_path, json.load(configuration_file))
        errors = configuration.get_errors()
        return errors


def _get_lint_status() -> int:
    lint_status = subprocess.call(
        [
            "arc",
            "lint",
            "--never-apply-patches",
            "--enforce-lint-clean",
            "--output",
            "none",
        ]
    )
    return lint_status


def _apply_lint() -> None:
    LOG.info("Lint was dirty after adding fixmes. Cleaning lint and re-checking.")
    subprocess.call(["arc", "lint", "--apply-patches", "--output", "none"])


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


def fix_file(
    arguments: argparse.Namespace,
    filename: str,
    errors: Dict[int, List[Dict[str, str]]],
) -> None:
    custom_comment = arguments.comment if hasattr(arguments, "comment") else ""
    max_line_length = (
        arguments.max_line_length if arguments.max_line_length > 0 else None
    )
    path = pathlib.Path(filename)
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
    path.write_text("\n".join(new_lines))


def _commit_message(directory, summary_override: Optional[str] = None):
    summary = (
        summary_override
        or "Automatic upgrade to remove `version` override and silence errors."
    )
    commit_message = """[typing] Update pyre version for {}

        Summary: {}
        #accept2ship

        Test Plan: sandcastle pyre run

        Reviewers: pyre

        Subscribers:

        Tasks:

        Tags: codemod

        Blame Revision:
        """.format(
        directory, summary
    ).replace(
        "        ", ""
    )
    return commit_message


def _submit_changes(arguments, message):
    LOG.info("Committing changes.")
    subprocess.call(["hg", "commit", "--message", message])
    submit_command = ["jf", "submit", "--update-fields", "--no-deps"]
    if arguments.submit is True:
        subprocess.call(submit_command)


# Exposed for testing.
def _upgrade_project(
    arguments: argparse.Namespace, configuration: Configuration, root: str
) -> None:
    LOG.info("Processing %s", configuration.get_directory())
    if not configuration.is_local or not configuration.version:
        return
    configuration.remove_version()
    errors = configuration.get_errors()
    if len(errors) > 0:
        fix(arguments, sort_errors(errors))

        # Lint and re-run pyre once to resolve most formatting issues
        if arguments.lint:
            lint_status = _get_lint_status()
            if lint_status:
                _apply_lint()
                errors = configuration.get_errors(should_clean=False)
                fix(arguments, sort_errors(errors))
    try:
        project_root = os.path.realpath(root)
        local_root = os.path.realpath(configuration.get_directory())
        _submit_changes(
            arguments, _commit_message(os.path.relpath(local_root, project_root))
        )
    except subprocess.CalledProcessError:
        LOG.info("Error while running hg.")


def fix(arguments: argparse.Namespace, result: List[Tuple[str, List[Any]]]) -> None:
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


def run_global_version_update(arguments: argparse.Namespace) -> None:
    global_configuration = Configuration.find_project_configuration()
    if global_configuration is None:
        LOG.error("No global configuration file found.")
        return

    local_configurations = [
        configuration
        for configuration in Configuration.gather_local_configurations(arguments)
        if configuration.is_local
    ]

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

    for configuration in local_configurations:
        path = configuration.get_path()
        if "mock_repository" in path:
            # Skip local configurations we have for testing.
            continue
        with open(path) as configuration_file:
            contents = json.load(configuration_file)
            if "version" in contents:
                LOG.info("Skipping %s as it already has a custom version field.", path)
                continue
            contents["version"] = old_version

        with open(path, "w") as configuration_file:
            json.dump(contents, configuration_file, sort_keys=True, indent=2)
            configuration_file.write("\n")

    try:
        commit_summary = "Automatic upgrade to hash `{}`".format(arguments.hash)
        _submit_changes(
            arguments,
            _commit_message("global configuration", summary_override=commit_summary),
        )
    except subprocess.CalledProcessError:
        LOG.info("Error while running hg.")


def run_missing_overridden_return_annotations(arguments: argparse.Namespace) -> None:
    errors = sort_errors(errors_from_stdin(arguments))
    for path, errors in errors:
        LOG.info("Patching errors in `%s`.", path)
        errors = reversed(sorted(errors, key=lambda error: error["line"]))

        path = pathlib.Path(path)
        lines = path.read_text().split("\n")

        for error in errors:
            if error["code"] != 15:
                continue
            line = error["line"] - 1

            match = re.match(r".*`(.*)`\.", error["description"])
            if not match:
                continue
            annotation = match.groups()[0]

            # Find last closing parenthesis in after line.
            LOG.info("Looking at %d: %s", line, lines[line])
            while True:
                if "):" in lines[line]:
                    lines[line] = lines[line].replace("):", ") -> %s:" % annotation)
                    LOG.info("%d: %s", line, lines[line])
                    break
                else:
                    line = line + 1

        LOG.warn("Writing patched %s", str(path))
        path.write_text("\n".join(lines))


def run_missing_global_annotations(arguments: argparse.Namespace) -> None:
    errors = sort_errors(errors_from_stdin(arguments))
    for path, errors in errors:
        LOG.info("Patching errors in `%s`", path)
        errors = reversed(sorted(errors, key=lambda error: error["line"]))

        path = pathlib.Path(path)
        lines = path.read_text().split("\n")

        for error in errors:
            if error["code"] != 5:
                continue
            line = error["line"] - 1

            match = re.match(r".*`.*`.*`(.*)`.*", error["description"])
            if not match:
                continue
            annotation = match.groups()[0]

            LOG.info("Looking at %d: %s", line, lines[line])
            if " =" in lines[line]:
                lines[line] = lines[line].replace(" =", ": %s =" % annotation)
                LOG.info("%d: %s", line, lines[line])

        path.write_text("\n".join(lines))


def run_fixme(arguments: argparse.Namespace) -> None:
    if arguments.run:
        errors = errors_from_run(arguments)
        fix(arguments, sort_errors(errors))

        if arguments.lint:
            lint_status = _get_lint_status()
            if lint_status:
                _apply_lint()
                errors = errors_from_run(arguments)
                fix(arguments, sort_errors(errors))
    else:
        errors = errors_from_stdin(arguments)
        fix(arguments, sort_errors(errors))


def run_fixme_single(arguments: argparse.Namespace) -> None:
    project_configuration = Configuration.find_project_configuration()
    if project_configuration is None:
        LOG.info("No project configuration found for the given directory.")
        return
    configuration_path = arguments.path + "/.pyre_configuration.local"
    with open(configuration_path) as configuration_file:
        configuration = Configuration(configuration_path, json.load(configuration_file))
        _upgrade_project(
            arguments, configuration, os.path.dirname(project_configuration)
        )


def run_fixme_all(arguments: argparse.Namespace) -> None:
    # Create sandcastle command.
    if arguments.sandcastle and isinstance(arguments.sandcastle, str):
        configurations = Configuration.gather_local_configurations(arguments)
        paths = [configuration.get_directory() for configuration in configurations]
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
        run_global_version_update(arguments)

    project_configuration = Configuration.find_project_configuration()
    if project_configuration is None:
        LOG.info("No project configuration found for the current directory.")
        return

    configurations = Configuration.gather_local_configurations(arguments)
    for configuration in configurations:
        _upgrade_project(
            arguments, configuration, os.path.dirname(project_configuration)
        )


def main():
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

    # Subcommand: Set global configuration to given hash, and add version override
    # to all local configurations to run previous version.
    update_global_version = commands.add_parser("update-global-version")
    update_global_version.set_defaults(function=run_global_version_update)
    update_global_version.add_argument("hash", help="Hash of new Pyre version")
    update_global_version.add_argument(
        "-p", "--push-blocking-only", action="store_true"
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
        "path", help="Path to project root with local configuration"
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
        "-s", "--sandcastle", help="Create upgrade stack on sandcastle."
    )
    fixme_all.add_argument(
        "hash", nargs="?", default=None, help="Hash of new Pyre version"
    )

    # Initialize default values.
    arguments = parser.parse_args()
    if not hasattr(arguments, "function"):
        arguments.run = False
        arguments.function = run_fixme

    logging.basicConfig(
        format="%(asctime)s %(levelname)s %(message)s",
        datefmt="%Y-%m-%d %H:%M:%S",
        level=logging.DEBUG if arguments.verbose else logging.INFO,
    )

    try:
        exit_code = ExitCode.SUCCESS
        arguments.function(arguments)
    except Exception as error:
        LOG.error(str(error))
        LOG.info(traceback.format_exc())
        exit_code = ExitCode.FAILURE

    sys.exit(exit_code)


if __name__ == "__main__":
    main()
