# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import itertools
import logging
from pathlib import Path
from typing import Callable, List, NamedTuple, Optional


CONFIGURATION_FILE: str = ".pyre_configuration"
LOCAL_CONFIGURATION_FILE: str = ".pyre_configuration.local"
BINARY_NAME: str = "pyre.bin"
CLIENT_NAME: str = "pyre-client"
LOG_DIRECTORY: str = ".pyre"


LOG: logging.Logger = logging.getLogger(__name__)


def _find_parent_directory_containing(
    base: Path,
    target: str,
    predicate: Callable[[Path], bool],
    stop_search_after: Optional[int],
) -> Optional[Path]:
    resolved_base = base.resolve(strict=False)
    # Using `itertools.chain` to avoid expanding `resolve_base.parents` eagerly
    for i, candidate_directory in enumerate(
        itertools.chain([resolved_base], resolved_base.parents)
    ):
        candidate_path = candidate_directory / target
        try:
            if predicate(candidate_path):
                return candidate_directory
        except PermissionError:
            # We might not have sufficient permission to read the file/directory.
            # In that case, pretend the file doesn't exist.
            pass
        if stop_search_after is not None:
            if i >= stop_search_after:
                return None
    return None


def find_parent_directory_containing_file(
    base: Path,
    target: str,
    stop_search_after: Optional[int] = None,
) -> Optional[Path]:
    """
    Walk directories upwards from `base`, until the root directory is
    reached. At each step, check if the `target` file exist, and return
    it if found. Return None if the search is unsuccessful.

    We stop searching after checking `stop_search_after` parent
    directories of `base` if provided; this is mainly for testing.
    """

    def is_file(path: Path) -> bool:
        return path.is_file()

    return _find_parent_directory_containing(
        base,
        target,
        predicate=is_file,
        stop_search_after=stop_search_after,
    )


def find_global_root(base: Path) -> Optional[Path]:
    """Pyre always runs from the directory containing the nearest .pyre_configuration,
    if one exists."""
    return find_parent_directory_containing_file(base, CONFIGURATION_FILE)


def get_relative_local_root(
    global_root: Path, local_root: Optional[Path]
) -> Optional[str]:
    if local_root is None:
        return None
    else:
        try:
            return str(local_root.relative_to(global_root))
        except ValueError:
            # This happens when `local_root` is not prefixed by `global_root`
            return None


class FoundRoot(NamedTuple):
    global_root: Path
    local_root: Optional[Path] = None


def find_global_and_local_root(base: Path) -> Optional[FoundRoot]:
    """
    Walk directories upwards from `base` and try to find both the global and local
    pyre configurations.
    Return `None` if no global configuration is found.
    If a global configuration exists but no local configuration is found below it,
    return the path to the global configuration.
    If both global and local exist, return them as a pair.
    """
    found_global_root = find_parent_directory_containing_file(base, CONFIGURATION_FILE)
    if found_global_root is None:
        return None

    found_local_root = find_parent_directory_containing_file(
        base, LOCAL_CONFIGURATION_FILE
    )
    if found_local_root is None:
        return FoundRoot(found_global_root)

    # If the global configuration root is deeper than local configuration, ignore local.
    if found_local_root in found_global_root.parents:
        return FoundRoot(found_global_root)
    else:
        return FoundRoot(found_global_root, found_local_root)


def find_parent_directory_containing_directory(
    base: Path,
    target: str,
    stop_search_after: Optional[int] = None,
) -> Optional[Path]:
    """
    Walk directories upwards from base, until the root directory is
    reached. At each step, check if the target directory exist, and return
    it if found. Return None if the search is unsuccessful.

    We stop searching after checking `stop_search_after` parent
    directories of `base` if provided; this is mainly for testing.
    """

    def is_directory(path: Path) -> bool:
        return path.is_dir()

    return _find_parent_directory_containing(
        base,
        target,
        predicate=is_directory,
        stop_search_after=stop_search_after,
    )


def find_typeshed() -> Optional[Path]:
    current_directory = Path(__file__).parent

    # Prefer the typeshed we bundled ourselves (if any) to the one
    # from the environment.
    bundled_typeshed_relative_path = "pyre_check/typeshed/"
    bundled_typeshed = find_parent_directory_containing_directory(
        current_directory, bundled_typeshed_relative_path
    )
    if bundled_typeshed:
        return bundled_typeshed / bundled_typeshed_relative_path

    try:
        import typeshed  # pyre-fixme: Can't find module import typeshed

        return Path(typeshed.typeshed)
    except ImportError:
        LOG.debug("`import typeshed` failed, attempting a manual lookup")

    # This is a terrible, terrible hack.
    return find_parent_directory_containing_directory(current_directory, "typeshed/")


def find_typeshed_search_paths(typeshed_root: Path) -> List[Path]:
    """
    Given the root of typeshed, find all subdirectories in it that can be used
    as search paths for Pyre.
    """
    search_path = []
    third_party_root = typeshed_root / "stubs"
    third_party_subdirectories = (
        sorted(third_party_root.iterdir()) if third_party_root.is_dir() else []
    )
    for typeshed_subdirectory in itertools.chain(
        [typeshed_root / "stdlib"], third_party_subdirectories
    ):
        if typeshed_subdirectory.is_dir():
            search_path.append(typeshed_subdirectory)
    return search_path


def find_taint_models_directory() -> Optional[Path]:
    return find_parent_directory_containing_directory(
        Path(__file__).parent, "pyre_check/taint/"
    )
