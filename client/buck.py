# Copyright 2004-present Facebook.  All rights reserved.

import functools
import glob
import json
import logging
import os
import subprocess
import sys
import tempfile
import threading
from collections import namedtuple
from json.decoder import JSONDecodeError
from logging import Logger
from typing import Dict, Iterable, List, NamedTuple, Optional, Set, Tuple

from .filesystem import find_root


LOG: Logger = logging.getLogger(__name__)


class BuckOut(NamedTuple):
    source_directories: Set[str]
    targets_not_found: Set[str]


class BuckException(Exception):
    pass


class BuckBuilder:
    def build(self, targets: Iterable[str]) -> Iterable[str]:
        """
            Build the given targets, and return a list of output directories
            containing the target output.
        """
        raise NotImplementedError


class FastBuckBuilder(BuckBuilder):
    def __init__(
        self,
        buck_root: str,
        output_directory: Optional[str] = None,
        buck_builder_binary: Optional[str] = None,
        buck_builder_target: Optional[str] = None,
        debug_mode: bool = False,
        buck_mode: Optional[str] = None,
    ) -> None:
        self._buck_root = buck_root
        self._output_directory: str = output_directory or tempfile.mkdtemp(
            prefix="pyre_tmp_"
        )
        self._buck_builder_binary = buck_builder_binary
        self._buck_builder_target = buck_builder_target
        self._debug_mode = debug_mode
        self._buck_mode = buck_mode
        self.conflicting_files: List[str] = []
        self.unsupported_files: List[str] = []

    def _get_builder_executable(self) -> str:
        builder_binary = self._buck_builder_binary
        if not self._debug_mode:
            if builder_binary is None:
                raise BuckException(
                    "--buck-builder-binary must be provided "
                    "if --buck-builder-debug is not enabled."
                )
            return builder_binary
        target = self._buck_builder_target
        if target is None:
            raise BuckException(
                "--buck-builder-target must be provided "
                "if --buck-builder-debug is enabled."
            )
        binary_relative_path = (
            subprocess.check_output(
                [
                    "buck",
                    "build",
                    "--show-output",
                    "//tools/pyre/facebook/fb_buck_project_builder",
                ],
                stderr=subprocess.DEVNULL,
            )
            .decode()
            .strip()
            .split(" ")[1]
        )
        return os.path.join(self._buck_root, binary_relative_path)

    def build(self, targets: Iterable[str]) -> List[str]:
        command = [
            self._get_builder_executable(),
            "-J-Djava.net.preferIPv6Addresses=true",
            "-J-Djava.net.preferIPv6Stack=true",
            "--buck_root",
            self._buck_root,
            "--output_directory",
            self._output_directory,
        ] + list(targets)
        if self._debug_mode:
            command.append("--debug")
        buck_mode = self._buck_mode
        if buck_mode:
            command.extend(["--mode", buck_mode])
        LOG.info("Buck builder command: `{}`".format(" ".join(command)))
        with subprocess.Popen(
            command,
            stdin=subprocess.DEVNULL,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            env={**os.environ, "NO_BUCKD": "1"},
        ) as buck_builder_process:
            # Java's logging conflicts with Python's logging, we capture the
            # logs and re-log them with python's logger.
            log_processor = threading.Thread(
                target=self._read_stderr,
                args=(buck_builder_process.stderr, logging.DEBUG),
            )
            log_processor.daemon = True
            log_processor.start()
            return_code = buck_builder_process.wait()
            # Wait until all stderr have been printed.
            log_processor.join()
            if return_code == 0:
                LOG.info("Finished building targets.")
                if self._debug_mode:
                    debug_output = json.load(buck_builder_process.stdout)
                    self.conflicting_files += debug_output["conflictingFiles"]
                    self.unsupported_files += debug_output["unsupportedFiles"]
                return [self._output_directory]
            else:
                raise BuckException(
                    "Could not build targets. Check the paths or run `buck clean`."
                )

    def _read_stderr(
        self, stream: Iterable[str], default_logging_section: int = logging.ERROR
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
    def __init__(self, build: bool = True) -> None:
        self._build = build

    def build(self, targets: Iterable[str]) -> Iterable[str]:
        """
            Shell out to buck to build the targets, then yield the paths to the
            link trees.
        """
        return generate_source_directories(targets, build=self._build)


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
            subprocess.check_output(
                command,
                stderr=subprocess.PIPE,
                timeout=600,
                env={**os.environ, "NO_BUCKD": "1"},
            )
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
                # pyre-fixme: command not always defined
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
        subprocess.check_output(
            command, stderr=subprocess.PIPE, env={**os.environ, "NO_BUCKD": "1"}
        )
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
    return find_root(path, ".buckconfig")


def query_buck_relative_paths(
    project_paths: Iterable[str], targets: Iterable[str]
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
    target_string = " ".join(targets)
    command = [
        "buck",
        "query",
        "--json",
        "--output-attribute",
        ".*",
        # This will get only those owner targets that are beneath our targets or
        # the dependencies of our targets.
        f"owner(%s) ^ deps(set({target_string}))",
        *project_paths,
    ]
    LOG.info(f"Running command: {command}")
    try:
        owner_output = json.loads(
            subprocess.check_output(
                command,
                timeout=30,
                stderr=subprocess.DEVNULL,
                env={**os.environ, "NO_BUCKD": "1"},
            )
            .decode()
            .strip()
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


def generate_source_directories(
    original_targets: Iterable[str], build: bool
) -> Set[str]:
    original_targets = list(original_targets)
    targets_to_destinations = _normalize(original_targets)
    targets = [pair[0] for pair in targets_to_destinations]
    if build:
        _build_targets(targets, original_targets)
    buck_out = _find_built_source_directories(targets_to_destinations)
    source_directories = buck_out.source_directories

    if buck_out.targets_not_found:
        if not build:
            # Build all targets to ensure buck doesn't remove some link trees as we go.
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
