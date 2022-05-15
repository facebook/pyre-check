# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import builtins
import fcntl
import tempfile
import unittest
from unittest.mock import call, MagicMock, Mock, patch

from .. import filesystem
from ..filesystem import acquire_lock, expand_relative_path


class FilesystemTest(unittest.TestCase):
    @patch("fcntl.lockf")
    def test_acquire_lock(self, lock_file: Mock) -> None:
        (_, path) = tempfile.mkstemp()
        lockfile_file_descriptor = None
        with acquire_lock(path, blocking=False) as file_descriptor:
            lockfile_file_descriptor = file_descriptor

        with acquire_lock(path, blocking=True):
            pass
        lock_file.assert_has_calls(
            [
                call(lockfile_file_descriptor, fcntl.LOCK_EX | fcntl.LOCK_NB),
                call(lockfile_file_descriptor, fcntl.LOCK_UN),
                call(lockfile_file_descriptor, fcntl.LOCK_EX),
                call(lockfile_file_descriptor, fcntl.LOCK_UN),
            ]
        )

        def fail_on_exclusive(_, lock_kind):
            if lock_kind == fcntl.LOCK_EX | fcntl.LOCK_NB:
                raise OSError()
            return None

        lock_file.side_effect = fail_on_exclusive
        with self.assertRaises(OSError):
            with acquire_lock(path, blocking=False):
                pass

    @patch.object(builtins, "open")
    @patch.object(fcntl, "lockf")
    def test_acquire_lock__release_even_on_exception(
        self, lock_file: MagicMock, open_file: MagicMock
    ) -> None:
        class SomeException(Exception):
            pass

        with self.assertRaises(SomeException):
            with acquire_lock("foo.txt", blocking=True):
                raise SomeException

        file_descriptor = open_file().__enter__().fileno()
        lock_file.assert_has_calls(
            [call(file_descriptor, fcntl.LOCK_EX), call(file_descriptor, fcntl.LOCK_UN)]
        )

    def test_lock_command(self) -> None:
        self.assertEqual(
            filesystem._lock_command(blocking=True, is_shared_reader=True),
            fcntl.LOCK_SH,
        )
        self.assertEqual(
            filesystem._lock_command(blocking=True, is_shared_reader=False),
            fcntl.LOCK_EX,
        )
        self.assertEqual(
            filesystem._lock_command(blocking=False, is_shared_reader=True),
            fcntl.LOCK_SH | fcntl.LOCK_NB,
        )
        self.assertEqual(
            filesystem._lock_command(blocking=False, is_shared_reader=False),
            fcntl.LOCK_EX | fcntl.LOCK_NB,
        )

    def test_expand_relative_path__globs_are_unchanged(self) -> None:
        self.assertEqual(expand_relative_path("foo", "bar/*/baz"), "foo/bar/*/baz")
        self.assertEqual(
            expand_relative_path("dontcare", "/absolute/path/*/foo"),
            "/absolute/path/*/foo",
        )
