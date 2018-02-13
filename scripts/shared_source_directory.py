# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import errno
import fcntl
import logging
import os
import shutil

from typing import (
    List,
    Optional,
)


LOG = logging.getLogger(__name__)

DIRECTORY_LIST_PATH = '.pyre/source_directories.txt'


def get_directory_name(suffix: Optional[str] = None) -> str:
    if suffix:
        return '.pyre/shared_source_directory_{}'.format(suffix)
    return '.pyre/shared_source_directory'


def missing(source_directories) -> Optional[List[str]]:
    try:
        existing_directories = set()
        missing_directories = []
        with open(DIRECTORY_LIST_PATH) as directories:
            fcntl.lockf(directories.fileno(), fcntl.LOCK_SH)
            for line in directories:
                existing_directories.add(line.strip())
            fcntl.lockf(directories.fileno(), fcntl.LOCK_UN)
            for source_directory in source_directories:
                if source_directory not in existing_directories:
                    missing_directories.append(source_directory)
        return missing_directories
    except (OSError, FileNotFoundError):
        # pessimistically return True if the file's not found.
        return None


def write_existing(source_directories: List[str]) -> None:
    # we need to create the .pyre/ directory if necessary, since the open
    # below will fail if it doesn't exist.
    try:
        os.mkdir('.pyre')
    except OSError:
        pass
    with open(DIRECTORY_LIST_PATH, 'w+') as directory_list:
        fcntl.lockf(directory_list, fcntl.LOCK_EX)
        for directory in source_directories:
            directory_list.write("{}\n".format(directory))
        fcntl.lockf(directory_list, fcntl.LOCK_UN)


def remove_list() -> None:
    try:
        os.remove(DIRECTORY_LIST_PATH)
    except Exception:
        pass


def merge(target_root: str, source_directories: List[str]) -> None:
    try:
        shutil.rmtree(target_root)
    except OSError:
        pass
    try:
        os.makedirs(os.path.dirname(target_root))
    except OSError:
        pass

    def merge_directory(source_directory):
        def add_file(path):
            merged_path = os.path.join(target_root, path)
            original_path = os.path.join(
                os.path.realpath(source_directory),
                path)
            if os.path.exists(merged_path):
                return
            directory = os.path.dirname(merged_path)
            if not os.path.exists(directory):
                os.makedirs(directory)
            if not path.endswith(".py") and not path.endswith(".pyi"):
                return
            try:
                os.symlink(original_path, merged_path)
            except OSError as error:
                if error.errno == errno.EEXIST:
                    os.unlink(merged_path)
                    os.symlink(original_path, merged_path)
                else:
                    LOG.error(str(error))

        for directory, _, files in os.walk(source_directory):
            files = [
                os.path.relpath(
                    os.path.join(directory, file), source_directory)
                for file in files
            ]
            for file in files:
                add_file(file)

    for directory in source_directories:
        merge_directory(directory)
