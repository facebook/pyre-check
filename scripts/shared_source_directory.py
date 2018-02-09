# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import fcntl
import os

from typing import (
    List,
    Optional,
)

directory_list_path = '.pyre/source_directories.txt'


def missing(source_directories) -> Optional[List[str]]:
    try:
        existing_directories = set()
        missing_directories = []
        with open(directory_list_path) as directories:
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
    with open(directory_list_path, 'w+') as directory_list:
        fcntl.lockf(directory_list, fcntl.LOCK_EX)
        for directory in source_directories:
            directory_list.write("{}\n".format(directory))
        fcntl.lockf(directory_list, fcntl.LOCK_UN)


def remove_list() -> None:
    try:
        os.remove(directory_list_path)
    except Exception:
        pass
