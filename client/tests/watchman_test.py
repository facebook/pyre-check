# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import os
import signal
import unittest
from contextlib import contextmanager
from pathlib import Path
from typing import Generator, Optional
from unittest.mock import MagicMock, patch

from .. import watchman
from ..configuration import Configuration
from ..process import Process
from ..watchman import Subscriber


@contextmanager
def send_sigint_to_self(
    path: str, blocking: bool
) -> Generator[Optional[int], None, None]:
    os.kill(os.getpid(), signal.SIGINT)
    yield


class SubscriberTest(unittest.TestCase):
    @patch.object(Process, "register_unique_process")
    @patch.object(watchman, "acquire_lock")
    @patch.object(os, "_exit")
    @patch.object(os, "close")
    @patch.object(os, "fork", return_value=0)
    # pyre-fixme[56]: Argument `os` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(os, "makedirs")
    def test_cleanup_on_sigint(
        self,
        makedirs: MagicMock,
        fork: MagicMock,
        close: MagicMock,
        exit: MagicMock,
        acquire_lock: MagicMock,
        register_unique_process: MagicMock,
    ) -> None:
        acquire_lock.side_effect = send_sigint_to_self

        class FooSubscriber(Subscriber):
            NAME = "foo_subscriber"

            def __init__(self, configuration: Configuration) -> None:
                super().__init__(base_path=".pyre/test", configuration=configuration)

            @staticmethod
            def is_alive(configuration: Configuration) -> bool:
                return True

            @property
            def _name(self) -> str:
                return self.NAME

        subscriber = FooSubscriber(configuration=Configuration("foo", Path("bar")))
        subscriber.daemonize()

        self.assertEqual(fork.call_count, 2)
        register_unique_process.assert_called_once()

    # pyre-fixme[56]: Argument `os` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(os, "kill")
    @patch.object(watchman.Path, "read_text")
    def test_stop_subscription(self, read_text: MagicMock, os_kill: MagicMock) -> None:
        read_text.return_value = "123"
        watchman.stop_subscriptions(".pyre/foo", "some_monitor")
        os_kill.assert_called_once_with(123, signal.SIGINT)

    # pyre-fixme[56]: Argument `os` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(os, "kill")
    @patch.object(watchman.Path, "read_text")
    def test_stop_subscription_handle_exception(
        self, read_text: MagicMock, os_kill: MagicMock
    ) -> None:
        read_text.side_effect = FileNotFoundError
        watchman.stop_subscriptions(".pyre/foo", "some_monitor")
        os_kill.assert_not_called()
