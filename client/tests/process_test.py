# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from pathlib import Path
from typing import Optional
from unittest.mock import MagicMock, patch

import psutil

from .. import process
from ..process import _register, get_process, get_processes


class ProcessTest(unittest.TestCase):
    # pyre-fixme[56]: Argument `tools.pyre.client.process` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(process, "remove_if_exists")
    @patch.object(Path, "mkdir")
    @patch.object(Path, "write_text")
    def test_register(
        self,
        write_text: MagicMock,
        make_directory: MagicMock,
        remove_if_exists: MagicMock,
    ) -> None:
        pid = 123
        pid_path = Path("/root/foo-123.pid")
        with _register(pid, pid_path):
            make_directory.assert_called_once_with(parents=True, exist_ok=True)
            write_text.assert_called_once_with("123")
        remove_if_exists.assert_called_once_with(str(pid_path))

    # pyre-fixme[56]: Argument `tools.pyre.client.process` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(process, "remove_if_exists")
    @patch.object(Path, "mkdir")
    @patch.object(Path, "write_text")
    def test_register_exception(
        self,
        write_text: MagicMock,
        make_directory: MagicMock,
        remove_if_exists: MagicMock,
    ) -> None:
        pid = 123
        pid_path = Path("/root/foo-123.pid")
        with self.assertRaises(Exception), _register(pid, pid_path):
            raise Exception
        remove_if_exists.assert_called_once_with(str(pid_path))

    # pyre-fixme[56]: Argument `psutil` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(psutil, "Process")
    @patch.object(Path, "read_text", return_value="123")
    def test_get_process(self, read_text: MagicMock, process_class: MagicMock) -> None:
        process = get_process("/root/some-monitor.pid")
        process_class.assert_called_once_with(123)
        self.assertIsNotNone(process)

    # pyre-fixme[56]: Argument `psutil` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(psutil, "Process")
    @patch.object(Path, "read_text")
    def test_get_process_file_not_found(
        self, read_text: MagicMock, process_class: MagicMock
    ) -> None:
        def failed_read_text() -> None:
            raise FileNotFoundError

        read_text.side_effect = failed_read_text
        process = get_process("/root/non-existent.pid")
        process_class.assert_not_called()
        self.assertIsNone(process)

    # pyre-fixme[56]: Argument `psutil` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(psutil, "Process")
    @patch.object(Path, "read_text", return_value="123")
    def test_get_process_error(
        self, read_text: MagicMock, process_class: MagicMock
    ) -> None:
        def failed_process_instance(pid: int) -> psutil.Process:
            raise psutil.Error

        process_class.side_effect = failed_process_instance
        process = get_process("/root/failed-process.pid")
        self.assertIsNone(process)
        process_class.assert_called_once_with(123)

    @patch.object(psutil, "Process")
    @patch.object(
        Path, "glob", return_value=["foo-123.pid", "foo-456.pid", "foo-789.pid"]
    )
    # pyre-fixme[56]: Argument `tools.pyre.client.process` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(process, "get_process")
    def test_get_processes(
        self, get_process: MagicMock, glob: MagicMock, process_class: MagicMock
    ) -> None:
        def _get_process(path: str) -> Optional[psutil.Process]:
            if path.endswith("3.pid"):
                return None
            return psutil.Process(123)

        get_process.side_effect = _get_process
        processes = get_processes(name="foo", log_directory="/root/")
        glob.assert_called_once_with("foo-*.pid")
        self.assertEqual(len(processes), 2)
        self.assertNotIn(None, processes)
