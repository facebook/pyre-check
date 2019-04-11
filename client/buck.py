# Copyright 2004-present Facebook.  All rights reserved.

import functools
import glob
import json
import logging
import os
import subprocess
import sys
from collections import namedtuple
from typing import Dict, Iterable, List, Optional, Set, Tuple, cast  # noqa

from . import log
from .filesystem import BuckBuilder, find_root


LOG = logging.getLogger(__name__)
CACHE_PATH = ".pyre/buckcache.json"

BuckOut = namedtuple("BuckOut", "source_directories targets_not_found")


class BuckException(Exception):
    pass


class SimpleBuckBuilder(BuckBuilder):
    def __init__(self, build: bool = True, prompt: bool = True) -> None:
        self._build = build
        self._prompt = prompt

    def build(self, targets: Iterable[str]) -> Iterable[str]:
        """
            Shell out to buck to build the targets, then yield the paths to the
            link trees.
        """
        return generate_source_directories(
            targets, build=self._build, prompt=self._prompt
        )


def presumed_target_root(target):
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
        targets_to_destinations = (
            subprocess.check_output(command, stderr=subprocess.PIPE, timeout=600)
            .decode()
            .strip()
            .split("\n")
        )
        targets_to_destinations = cast(
            List[str], list(filter(bool, targets_to_destinations))
        )
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
    return find_root(path, ".buckconfig")


def resolve_relative_paths(paths: List[str]) -> Dict[str, str]:
    """
        Query buck to obtain a mapping from each absolute path to the relative
        location in the analysis directory.
    """
    buck_root = find_buck_root(os.getcwd())
    if buck_root is None:
        LOG.error(
            "Buck root couldn't be found. Returning empty analysis directory mapping."
        )
        return {}
    command = [
        "buck",
        "query",
        "--json",
        "--output-attribute",
        ".*",
        "owner(%s)",
        *paths,
    ]
    try:
        output = json.loads(
            subprocess.check_output(command, timeout=30, stderr=subprocess.DEVNULL)
            .decode()
            .strip()
        )
    except (
        subprocess.TimeoutExpired,
        subprocess.CalledProcessError,
        json.decoder.JSONDecodeError,
    ) as error:
        raise BuckException("Querying buck for relative paths failed: {}".format(error))
    # TODO(T40580762) we should use the owner name to determine which files are a
    # part of the pyre project
    results = {}
    for path in paths:
        # For each path, search for the target that owns it.
        for owner in output.values():
            prefix = os.path.join(buck_root, owner["buck.base_path"]) + os.sep

            if not path.startswith(prefix):
                continue

            suffix = path[len(prefix) :]

            if suffix not in owner["srcs"]:
                continue

            if "buck.base_module" in owner:
                base_path = os.path.join(*owner["buck.base_module"].split("."))
            else:
                base_path = owner["buck.base_path"]

            results[path] = os.path.join(base_path, owner["srcs"][suffix])
            break  # move on to next path
    return results


def generate_source_directories(
    original_targets: Iterable[str], build: bool, prompt: bool = True
) -> Set[str]:
    original_targets = list(original_targets)
    targets_to_destinations = _normalize(original_targets)
    targets = [pair[0] for pair in targets_to_destinations]
    if build:
        _build_targets(targets, original_targets)
    buck_out = _find_built_source_directories(targets_to_destinations)
    source_directories = buck_out.source_directories

    if buck_out.targets_not_found:
        if (not build) and not prompt or log.get_yes_no_input("Build target?"):
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
