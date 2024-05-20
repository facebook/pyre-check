# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
Defines an abstract interface representing a typeshed that allows
us to use different "backings" (e.g. in-memory for tests or raw
data pulled from github vs a local directory where we have one)
in most of our patching code.
"""


import abc
import contextlib
import pathlib
import tempfile

from typing import Dict, Iterable, Iterator, Mapping, Optional, Set


def _write_content_map_to_directory(
    content_map: Dict[pathlib.Path, str],
    root: pathlib.Path,
) -> None:
    """
    Write a map of relative_path: content to a root directly.
    """
    for path, content in content_map.items():
        full_path = root / path
        full_path.parent.mkdir(parents=True, exist_ok=True)
        full_path.write_text(content)


def write_content_map_to_directory(
    content_map: Dict[pathlib.Path, str],
    destination: pathlib.Path,
) -> None:
    """
    Write the given `Typeshed` into a directory rooted at `destination` on the filesystem.

    The `destination` directory is assumed to be nonexistent before this function gets
    invoked.
    """
    if destination.exists():
        raise ValueError(f"Cannot write to file that already exists: `{destination}`")
    with tempfile.TemporaryDirectory() as temporary_root_str:
        temporary_root = pathlib.Path(temporary_root_str)
        _write_content_map_to_directory(content_map, temporary_root)
        temporary_root.rename(destination)


class Typeshed(abc.ABC):
    """
    Representation of a collection of Python stub files.
    """

    @abc.abstractmethod
    def get_file_content(self, path: pathlib.Path) -> Optional[str]:
        """
        Return content of the given path, or `None` if the content is not available.
        Paths are all relative to typeshed root.
        This method is allowed to return `None` even for paths that come from the
        return value of `all_files()`. But such cases are expected to be rare.
        """
        raise NotImplementedError()

    @abc.abstractmethod
    def all_files(self) -> Iterable[pathlib.Path]:
        """
        Return paths to all contained files (directory excluded).
        Paths are all relative to typeshed root.
        Elements in the returned iterable is not guaranteed to follow any particular
        order.
        """
        raise NotImplementedError()


def write_to_directory(
    typeshed: Typeshed,
    destination: pathlib.Path,
) -> None:
    """
    Return a directory of relative paths to contents of files in the typeshed.
    Paths are all relative to typeshed root.
    """
    return write_content_map_to_directory(
        {
            path: content
            for path in typeshed.all_files()
            if (content := typeshed.get_file_content(path)) is not None
        },
        destination=destination,
    )


@contextlib.contextmanager
def _create_temporary_typeshed_directory(typeshed: Typeshed) -> Iterator[pathlib.Path]:
    with tempfile.TemporaryDirectory() as temporary_root:
        temporary_root_path = pathlib.Path(temporary_root)
        write_to_directory(typeshed, temporary_root_path)
        yield temporary_root_path


class MemoryBackedTypeshed(Typeshed):
    """
    A typeshed backed up by in-memory content. Essentially a wrapper around
    a dictonary from paths to their contents.
    This class is mostly useful for testing.
    """

    contents: Mapping[pathlib.Path, str]

    def __init__(self, contents: Mapping[pathlib.Path, str]) -> None:
        self.contents = contents

    def all_files(self) -> Iterable[pathlib.Path]:
        return self.contents.keys()

    def get_file_content(self, path: pathlib.Path) -> Optional[str]:
        return self.contents.get(path, None)


class DirectoryBackedTypeshed(Typeshed):
    """
    A typeshed backed up by a directory that lives on the filesystem.

    For simplicity, we assume that files in this directory remain unchanged. If
    the assumption does not hold, e.g. when files are added/removed/changed after
    the creation of a `DirectoryBacked` object, the behaviors of its methods
    become undefined.
    """

    root: pathlib.Path
    files: Set[pathlib.Path]

    def __init__(self, root: pathlib.Path) -> None:
        self.root = root
        self.files = {
            path.relative_to(root) for path in root.rglob("*") if path.is_file()
        }

    def all_files(self) -> Iterable[pathlib.Path]:
        return self.files

    def get_file_content(self, path: pathlib.Path) -> Optional[str]:
        return (self.root / path).read_text() if path in self.files else None


class PatchedTypeshed(Typeshed):
    """
    A typeshed backed up by another `Typeshed` object and a set of patch results
    that overwrite file contents in the base `Typeshed` object.

    Patches are specified as a dictionary from paths to either a `str` or `None`.
    When the value is a string, it serves as the new content for the corresponding
    file (or the content of a new file if the file did not exist before). When the
    value is `None`, it indicates that the corresponding file will be removed.
    """

    base: Typeshed
    updated_files: Dict[pathlib.Path, str]
    removed_files: Set[pathlib.Path]

    def __init__(
        self,
        base: Typeshed,
        patch_results: Mapping[pathlib.Path, Optional[str]],
    ) -> None:
        self.base = base
        self.updated_files = {
            path: content
            for path, content in patch_results.items()
            if content is not None
        }
        self.removed_files = {
            path for path, content in patch_results.items() if content is None
        }

    def all_files(self) -> Iterable[pathlib.Path]:
        return (
            set(self.base.all_files()) | self.updated_files.keys()
        ) - self.removed_files

    def get_file_content(self, path: pathlib.Path) -> Optional[str]:
        if path in self.removed_files:
            return None
        updated_content = self.updated_files.get(path, None)
        if updated_content is not None:
            return updated_content
        return self.base.get_file_content(path)
