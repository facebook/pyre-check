# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import abc
import pathlib
import zipfile

from typing import Iterable, Mapping, Optional, Set


class Typeshed(abc.ABC):
    """
    Representation of a collection of Python stub files.
    """

    @abc.abstractclassmethod
    def get_file_content(self, path: pathlib.Path) -> Optional[str]:
        """
        Return content of the given path, or `None` if the content is not available.
        Paths are all relative to typeshed root.
        This method is allowed to return `None` even for paths that come from the
        return value of `all_files()`. But such cases are expected to be rare.
        """
        raise NotImplementedError

    @abc.abstractclassmethod
    def all_files(self) -> Iterable[pathlib.Path]:
        """
        Return paths to all contained files (directory excluded).
        Paths are all relative to typeshed root.
        Elements in the returned iterable is not guaranteed to follow any particular
        order.
        """
        raise NotImplementedError


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


class FileBackedTypeshed(Typeshed):
    """
    A typeshed backed up by a directory that lives on the filesystem.

    For simplicity, we assume that files in this directory remain unchanged. If
    the assumption does not hold, e.g. when files are added/removed/changed after
    the creation of a `FileBackedTypeshed` object, the behaviors of its methods
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


class ZipBackedTypeshed(Typeshed):
    """
    A typeshed backed up by a zipball that lives on the filesystem.

    For simplicity, we assume that this zipfile remains unchanged. If the assumption
    does not hold, e.g. when this file gets added/removed/changed after the creation of
    the corresponding `ZipBackedTypeshed` object, the behaviors of its methods become
    undefined.
    """

    zip_file: zipfile.ZipFile

    def __init__(self, zip_file_path: pathlib.Path) -> None:
        self.zip_file = zipfile.ZipFile(zip_file_path)

    def all_files(self) -> Iterable[pathlib.Path]:
        return [
            pathlib.Path(zip_info.filename)
            for zip_info in self.zip_file.infolist()
            if not zip_info.is_dir()
        ]

    def get_file_content(self, path: pathlib.Path) -> Optional[str]:
        try:
            return self.zip_file.read(str(path)).decode("utf-8")
        except (KeyError, ValueError):
            return None
