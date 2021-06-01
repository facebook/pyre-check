# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import functools
import glob
import json
import logging
import os
import subprocess
import sys
import threading
from json.decoder import JSONDecodeError
from logging import Logger
from pathlib import Path
from typing import Dict, Iterable, List, NamedTuple, Optional, Set, Tuple

from typing_extensions import Final

from . import source_database_buck_builder
from .find_directories import find_parent_directory_containing_file


LOG: Logger = logging.getLogger(__name__)


class BuckOut(NamedTuple):
    source_directories: Set[str]
    targets_not_found: Set[str]


class BuckBuildOutput(NamedTuple):
    output_directories: List[str]
    unsupported_files: List[str]


class BuckException(Exception):
    pass


class BuckUserError(BuckException):
    pass


class BuckBuilder:
    def build(self, targets: Iterable[str]) -> BuckBuildOutput:
        raise NotImplementedError

    def __str__(self) -> str:
        return type(self).__name__


class SourceDatabaseBuckBuilder(BuckBuilder):
    def __init__(
        self,
        buck_root: str,
        output_directory: str,
        isolation_prefix: Optional[str],
        buck_mode: Optional[str] = None,
    ) -> None:
        self._buck_root = buck_root
        self._output_directory = output_directory
        self._isolation_prefix = isolation_prefix
        self._buck_mode = buck_mode

    def build(self, targets: Iterable[str]) -> BuckBuildOutput:
        try:
            source_database_buck_builder.build(
                list(targets),
                Path(self._output_directory),
                Path(self._buck_root),
                self._buck_mode,
                self._isolation_prefix,
            )
            return BuckBuildOutput(
                output_directories=[self._output_directory], unsupported_files=[]
            )
        except subprocess.CalledProcessError as exception:
            returncode = exception.returncode
            stderr = exception.stderr
            reason = stderr.decode().strip() if stderr else exception
            message = (
                "Failed to build targets because of buck error "
                + f"[{returncode}]: {reason}"
            )
            if returncode < 10:
                raise BuckUserError(message)
            else:
                raise BuckException(message)
        except Exception as exception:
            raise BuckException(
                f"Failed to build targets because of exception: {exception}"
            )

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, SourceDatabaseBuckBuilder):
            return False
        return (
            self._buck_root == other._buck_root
            and self._output_directory == other._output_directory
            and self._buck_mode == other._buck_mode
            and self._isolation_prefix == other._isolation_prefix
        )


class FastBuckBuilder(BuckBuilder):
    def __init__(
        self,
        buck_root: str,
        output_directory: str,
        isolation_prefix: Optional[str],
        buck_builder_binary: Optional[str] = None,
        buck_mode: Optional[str] = None,
        project_name: Optional[str] = None,
    ) -> None:
        self._buck_root = buck_root
        self._output_directory = output_directory
        self._buck_builder_binary = buck_builder_binary
        self._buck_mode = buck_mode
        self._project_name = project_name
        self._isolation_prefix: Final[Optional[str]] = isolation_prefix
        self.conflicting_files: List[str] = []
        self.unsupported_files: List[str] = []

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, FastBuckBuilder):
            return False
        return (
            self._buck_root == other._buck_root
            and self._output_directory == other._output_directory
            and self._buck_builder_binary == other._buck_builder_binary
            and self._buck_mode == other._buck_mode
            and self._project_name == other._project_name
            and self.conflicting_files == other.conflicting_files
            and self.unsupported_files == other.unsupported_files
            and self._isolation_prefix == other._isolation_prefix
        )

    def _get_builder_executable(self) -> str:
        builder_binary = self._buck_builder_binary
        if builder_binary is None:
            raise BuckException(
                "--buck-builder-binary must be provided "
                + "if fast buck builder is used."
            )
        return builder_binary

    def build(self, targets: Iterable[str]) -> BuckBuildOutput:
        isolation_prefix_arguments = (
            ["--isolation_prefix", self._isolation_prefix]
            if self._isolation_prefix is not None
            else []
        )
        command = (
            [
                self._get_builder_executable(),
                "-J-Djava.net.preferIPv6Addresses=true",
                "-J-Djava.net.preferIPv6Stack=true",
                "--buck_root",
                self._buck_root,
                "--output_directory",
                self._output_directory,
            ]
            + isolation_prefix_arguments
            + list(targets)
        )
        command.append("--debug")
        buck_mode = self._buck_mode
        if buck_mode:
            command.extend(["--mode", buck_mode])
        project_name = self._project_name
        if project_name:
            command.extend(["--project_name", project_name])
        LOG.info("Building buck targets...")
        LOG.debug("Buck builder command: `{}`".format(" ".join(command)))
        with subprocess.Popen(
            command,
            stdin=subprocess.DEVNULL,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            universal_newlines=True,
        ) as buck_builder_process:
            # Java's logging conflicts with Python's logging, we capture the
            # logs and re-log them with python's logger.
            log_processor = threading.Thread(
                target=self._read_stderr, args=(buck_builder_process.stderr,)
            )
            log_processor.daemon = True
            log_processor.start()
            return_code = buck_builder_process.wait()
            # Wait until all stderr have been printed.
            log_processor.join()
            if return_code == 0:
                LOG.info("Finished building targets.")
                # pyre-fixme[6]: Expected `_Reader` for 1st param but got
                #  `Optional[typing.IO[typing.Any]]`.
                debug_output = json.load(buck_builder_process.stdout)
                self.conflicting_files += debug_output["conflictingFiles"]
                self.unsupported_files += debug_output["unsupportedFiles"]
                return BuckBuildOutput(
                    output_directories=[self._output_directory],
                    unsupported_files=self.unsupported_files,
                )
            else:
                raise BuckException(
                    f"Failed to build targets with:\n`{' '.join(command)}`"
                )

    def _read_stderr(
        self, stream: Iterable[str], default_logging_section: int = logging.INFO
    ) -> None:
        for line in stream:
            line = line.rstrip()
            if line.startswith("INFO: "):
                LOG.info(line[6:])
            elif line.startswith("WARNING: "):
                LOG.warning(line[9:])
            elif line.startswith("ERROR: "):
                LOG.error(line[7:])
            elif line.startswith("[WARNING:"):
                # Filter away thrift warnings.
                pass
            else:
                LOG.log(default_logging_section, line)


class SimpleBuckBuilder(BuckBuilder):
    def build(self, targets: Iterable[str]) -> BuckBuildOutput:
        """
        Shell out to buck to build the targets, then yield the paths to the
        link trees.
        """
        return BuckBuildOutput(
            output_directories=list(generate_source_directories(targets)),
            unsupported_files=[],
        )

    def __eq__(self, other: object) -> bool:
        if isinstance(other, SimpleBuckBuilder):
            return True
        return False


def presumed_target_root(target: str) -> str:
    root_index = target.find("//")
    if root_index != -1:
        target = target[root_index + 2 :]
    target = target.replace("/...", "")
    target = target.split(":")[0]
    return target


# Expects the targets to be already normalized.
def _find_built_source_directories(
    targets_to_destinations: Iterable[Tuple[str, str]]
) -> BuckOut:
    targets_not_found = []
    source_directories = []
    buck_root = find_buck_root(os.getcwd())
    if buck_root is None:
        raise Exception("No .buckconfig found in ancestors of the current directory.")

    directories = set()
    for target, destination in targets_to_destinations:
        directories.add((target, os.path.dirname(destination)))

    for target, directory in directories:
        target_name = target.split(":")[1]
        discovered_source_directories = glob.glob(
            os.path.join(buck_root, directory, "{}#*link-tree".format(target_name))
        )
        if len(discovered_source_directories) == 0:
            targets_not_found.append(target)
        source_directories.extend(
            [
                tree
                for tree in discovered_source_directories
                if not tree.endswith(
                    (
                        "-vs_debugger#link-tree",
                        "-interp#link-tree",
                        "-ipython#link-tree",
                    )
                )
            ]
        )
    return BuckOut(set(source_directories), set(targets_not_found))


def _normalize(targets: List[str]) -> List[Tuple[str, str]]:
    LOG.info(
        "Normalizing target%s `%s`",
        "s:" if len(targets) > 1 else "",
        "`, `".join(targets),
    )
    try:
        command = (
            ["buck", "targets", "--show-output"]
            + targets
            + ["--type", "python_binary", "python_test"]
        )
        targets_to_destinations: List[str] = (
            subprocess.check_output(command, stderr=subprocess.PIPE, timeout=600)
            .decode()
            .strip()
            .split("\n")
        )
        targets_to_destinations = list(filter(bool, targets_to_destinations))
        # The output is of the form //target //corresponding.par
        result = []
        for target in targets_to_destinations:
            pair = target.split(" ")
            if len(pair) != 2:
                pass
            else:
                result.append((pair[0], pair[1]))
        if not result:
            LOG.warning(
                "Provided targets do not contain any binary or unittest targets."
            )
            return []
        else:
            LOG.info(
                "Found %d buck target%s.", len(result), "s" if len(result) > 1 else ""
            )
        return result
    except subprocess.TimeoutExpired as error:
        LOG.error("Buck output so far: %s", error.stderr.decode().strip())
        raise BuckException(
            "Seems like `{}` is hanging.\n   "
            "Try running `buck clean` before trying again.".format(
                " ".join(command[:-1])
            )
        )
    except subprocess.CalledProcessError as error:
        LOG.error("Buck returned error: %s" % error.stderr.decode().strip())
        raise BuckException(
            "Could not normalize targets. Check the paths or run `buck clean`."
        )


def _build_targets(targets: List[str], original_targets: List[str]) -> None:
    LOG.info(
        "Building target%s `%s`",
        "s:" if len(original_targets) > 1 else "",
        "`, `".join(original_targets),
    )
    command = ["buck", "build"] + targets
    try:
        subprocess.check_output(command, stderr=subprocess.PIPE)
        LOG.warning("Finished building targets.")
    except subprocess.CalledProcessError as error:
        # The output can be overwhelming, hence print only the last 20 lines.
        lines = error.stderr.decode().splitlines()
        LOG.error("Buck returned error: %s" % "\n".join(lines[-20:]))
        raise BuckException(
            "Could not build targets. Check the paths or run `buck clean`."
        )


def _map_normalized_targets_to_original(
    unbuilt_targets: Iterable[str], original_targets: Iterable[str]
) -> List[str]:
    mapped_targets = set()
    for target in unbuilt_targets:
        # Each original target is either a `/...` glob or a proper target.
        # If it's a glob, we're looking for the glob to be a prefix of the unbuilt
        # target. Otherwise, we care about exact matches.
        name = None
        for original in original_targets:
            if original.endswith("/..."):
                if target.startswith(original[:-4]):
                    name = original
            else:
                if target == original:
                    name = original
        # No original target matched, fallback to normalized.
        if name is None:
            name = target
        mapped_targets.add(name)
    return list(mapped_targets)


@functools.lru_cache()
def find_buck_root(path: str) -> Optional[str]:
    return str(find_parent_directory_containing_file(Path(path), ".buckconfig"))


@functools.lru_cache()
def _buck_query(
    project_paths: Tuple[str],
    targets: Tuple[str],
    buck_mode: Optional[str],
    isolation_prefix: Optional[str],
) -> str:
    """We accept Tuples because `lru_cache` expects hashable arguments."""
    target_string = " ".join(targets)
    command = [
        "buck",
        *(
            ["--isolation_prefix", isolation_prefix]
            if isolation_prefix is not None
            else []
        ),
        "query",
        "--json",
        *(["@mode/" + buck_mode] if buck_mode is not None else []),
        "--config",
        "client.id=pyre",
        "--output-attribute",
        ".*",
        # This will get only those owner targets that are beneath our targets or
        # the dependencies of our targets.
        f"owner(%s) ^ deps(set({target_string}))",
        *project_paths,
    ]
    LOG.info(f"Running command: {command}")
    return (
        subprocess.check_output(command, timeout=30, stderr=subprocess.DEVNULL)
        .decode()
        .strip()
    )


def clear_buck_query_cache() -> None:
    _buck_query.cache_clear()


def query_buck_relative_paths(
    project_paths: Iterable[str],
    targets: Iterable[str],
    buck_mode: Optional[str] = None,
    isolation_prefix: Optional[str] = None,
) -> Dict[str, str]:
    """Return a mapping from each absolute project path to its relative location
    in the buck output directory.
    This queries buck and only returns paths that are covered by `targets`."""
    buck_root = find_buck_root(os.getcwd())
    if buck_root is None:
        LOG.error(
            "Buck root couldn't be found. Returning empty analysis directory mapping."
        )
        return {}

    project_paths = tuple(project_paths)
    targets = tuple(targets)
    try:
        owner_output = json.loads(
            _buck_query(project_paths, targets, buck_mode, isolation_prefix)
        )
    except (
        subprocess.TimeoutExpired,
        subprocess.CalledProcessError,
        JSONDecodeError,
    ) as error:
        raise BuckException("Querying buck for relative paths failed: {}".format(error))

    results = {}
    for project_path in project_paths:
        for target_data in owner_output.values():
            prefix = os.path.join(buck_root, target_data["buck.base_path"]) + os.sep
            suffix = project_path[len(prefix) :]
            if not project_path.startswith(prefix) or suffix not in target_data["srcs"]:
                continue

            if "buck.base_module" in target_data:
                base_path = os.path.join(*target_data["buck.base_module"].split("."))
            elif "base_module" in target_data:
                base_path = os.path.join(*target_data["base_module"].split("."))
            else:
                base_path = target_data["buck.base_path"]
            results[project_path] = os.path.join(base_path, target_data["srcs"][suffix])
            # Break after the first one because there might be multiple matches.
            break
    return results


def generate_source_directories(original_targets: Iterable[str]) -> Set[str]:
    original_targets = list(original_targets)
    targets_to_destinations = _normalize(original_targets)
    targets = [pair[0] for pair in targets_to_destinations]
    _build_targets(targets, original_targets)
    buck_out = _find_built_source_directories(targets_to_destinations)
    source_directories = buck_out.source_directories

    if buck_out.targets_not_found:
        message_targets = _map_normalized_targets_to_original(
            buck_out.targets_not_found, original_targets
        )

        raise BuckException(
            "Could not find link trees for:\n    `{}`.\n   "
            "See `{} --help` for more information.".format(
                "    \n".join(message_targets), sys.argv[0]
            )
        )

    return source_directories
