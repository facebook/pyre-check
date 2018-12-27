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

from .commands import ExitCode
from .filesystem import get_filesystem


LOG = logging.getLogger(__name__)


def json_to_errors(json) -> List[Dict[str, Any]]:
    try:
        return json.loads(input) if input else []
    except json.decoder.JSONDecodeError:
        if not input:
            LOG.error(
                "Recevied no input."
                "If piping from `pyre check` be sure to use `--output=json`."
            )
        else:
            LOG.error(
                "Recevied invalid JSON as input."
                "If piping from `pyre check` be sure to use `--output=json`."
            )
        return []


class Configuration:
    def __init__(self, path: str, json_contents: Dict[str, Any]):
        if path.endswith("/.pyre_configuration.local"):
            self.is_local = True
        else:
            self.is_local = False
        self.root = os.path.dirname(path)
        self.targets = json_contents.get("targets")
        self.source_directories = json_contents.get("source_directories")
        self.push_blocking = bool(json_contents.get("push_blocking"))

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
        configuration_paths = get_filesystem().list(".", ".pyre_configuration.local")
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
        return configurations

    def get_errors(self) -> List[Dict[str, Any]]:
        # TODO(T37074129): Better parallelization or truncation needed for fbcode
        if self.targets:
            try:
                # If building targets, run clean or space may run out on device!
                LOG.info("Running `buck clean`...")
                subprocess.call(["buck", "clean"], timeout=200)
            except subprocess.TimeoutExpired as error:
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
            json = process.stdout.decode().strip()
            errors = json_to_errors(json)
            LOG.info("Found %d error%s.", len(errors), "s" if len(errors) != 1 else "")
            return errors
        except subprocess.CalledProcessError as error:
            LOG.warning("Error calling pyre: %s", str(error))
            return []


def errors_from_configurations(arguments) -> List[Dict[str, Any]]:
    configurations = Configuration.gather_local_configurations(arguments)
    LOG.info(
        "Found %d %sconfiguration%s",
        len(configurations),
        "push-blocking " if arguments.push_blocking_only else "",
        "s" if len(configurations) != 1 else "",
    )
    total_errors = []
    for configuration in configurations:
        total_errors += configuration.get_errors()
    return total_errors


def errors_from_stdin(_arguments) -> List[Dict[str, Any]]:
    input = sys.stdin.read()
    return json_to_errors(input)


def run_fixme(arguments, result) -> None:
    for path, errors in result:
        LOG.info("Processing `%s`", path)

        # Build map from line to error codes.
        codes = defaultdict(lambda: set())
        descriptions = defaultdict(lambda: set())
        for error in errors:
            match = re.search(r"\[(\d+)\]: (.*)", error["description"])
            if match:
                codes[error["line"]].add(match.group(1))
                descriptions[error["line"]].add(match.group(2))

        # Replace lines in file.
        path = pathlib.Path(path)
        lines = path.read_text().split("\n")

        new_lines = []
        for index, line in enumerate(lines):
            number = index + 1
            if number in codes:
                if list(codes[number]) == ["0"]:
                    # Handle unused ignores.
                    replacement = re.sub(r"# pyre-(ignore|fixme).*$", "", line).rstrip()
                    if replacement != "":
                        new_lines.append(replacement)
                    continue

                sorted_codes = sorted(list(codes[number]))
                sorted_descriptions = sorted(list(descriptions[number]))

                description = ""
                if hasattr(arguments, "comment") and arguments.comment:
                    description = ": " + arguments.comment
                else:
                    description = ": " + ", ".join(sorted_descriptions)

                comment = "{}# pyre-fixme[{}]{}".format(
                    line[: (len(line) - len(line.lstrip(" ")))],  # indent
                    ", ".join([str(code) for code in sorted_codes]),
                    description,
                )
                LOG.info("Adding `%s` on line %d", comment, number)

                new_lines.extend([comment, line])
            else:
                new_lines.append(line)

        path.write_text("\n".join(new_lines))


def run_missing_overridden_return_annotations(
    _arugments, errors: List[Tuple[str, List[Any]]]
) -> None:
    for path, errors in result:
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


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--verbose", action="store_true", help="Enable verbose logging")

    commands = parser.add_subparsers()

    # Subcommand: Fixme all errors inputted through stdin.
    fixme = commands.add_parser("fixme")
    fixme.set_defaults(errors=errors_from_stdin, function=run_fixme)
    fixme.add_argument("--comment", help="Custom comment after fixme comments")

    # Subcommand: Add annotations according to errors inputted through stdin.
    missing_overridden_return_annotations = commands.add_parser(
        "missing-overridden-return-annotations"
    )
    missing_overridden_return_annotations.set_defaults(
        errors=errors_from_stdin, function=run_missing_overridden_return_annotations
    )

    # Subcommand: Find and run pyre against all configurations,
    # and fixme all errors in each project.
    fixme_all = commands.add_parser("fixme-all")
    fixme_all.set_defaults(errors=errors_from_configurations, function=run_fixme)
    fixme_all.add_argument(
        "-c", "--comment", help="Custom comment after fixme comments"
    )
    fixme_all.add_argument("-p", "--push-blocking-only", action="store_true")

    # Initialize default values.
    arguments = parser.parse_args()
    if not hasattr(arguments, "function"):
        arguments.function = run_fixme
    if not hasattr(arguments, "errors"):
        arguments.errors = errors_from_stdin

    logging.basicConfig(
        format="%(asctime)s %(levelname)s %(message)s",
        datefmt="%Y-%m-%d %H:%M:%S",
        level=logging.DEBUG if arguments.verbose else logging.INFO,
    )

    try:
        exit_code = ExitCode.SUCCESS

        def error_path(error):
            return error["path"]

        result = itertools.groupby(
            sorted(arguments.errors(arguments), key=error_path), error_path
        )
        arguments.function(arguments, result)
    except Exception as error:
        LOG.error(str(error))
        LOG.info(traceback.format_exc())
        exit_code = ExitCode.FAILURE

    sys.exit(exit_code)
