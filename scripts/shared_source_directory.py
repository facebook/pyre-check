# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import errno
import fcntl
import logging
import os
import shutil
import subprocess

from time import time
from typing import (
    List,
    Optional,
)

from . import log


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


def _is_empty(path: str) -> bool:
    try:
        return os.stat(path).st_size == 0
    except FileNotFoundError:
        return False


def merge(target_root: str, source_directories: List[str]) -> None:
    start = time()
    LOG.info("Merging %d source directories", len(source_directories))
    try:
        shutil.rmtree(target_root)
    except OSError:
        pass
    try:
        os.makedirs(os.path.dirname(target_root))
    except OSError:
        pass

    all_paths = {}
    def merge_directory(source_directory):
        output = subprocess.check_output([
            "find",
            source_directory,
            "-regextype",
            "posix-egrep",
            "-regex",
            ".*(py|pyi)",
        ])\
            .decode('utf-8')\
            .strip()
        for path in output.split('\n'):
            if path:
                # don't symlink empty __init__ files which might
                # override legitimate ones
                if not _is_empty(path):
                    relative_path = os.path.relpath(path, source_directory)
                    all_paths[relative_path] = path

    for directory in source_directories:
        merge_directory(os.path.realpath(directory))

    for relative_path, original_path in all_paths.items():
        merged_path = os.path.join(target_root, relative_path)
        directory = os.path.dirname(merged_path)
        try:
            os.makedirs(directory)
        except OSError:
            pass
        try:
            os.symlink(original_path, merged_path)
        except OSError as error:
            if error.errno == errno.EEXIST:
                os.unlink(merged_path)
                os.symlink(original_path, merged_path)
            else:
                LOG.error(str(error))

    stop = time()
    LOG.log(
        log.PERFORMANCE,
        "Merged source directories in %fs",
        stop - start)
