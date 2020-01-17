# Copyright (c) 2019-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import os
import signal
import sys
import unittest
from contextlib import contextmanager
from typing import Generator, Optional
from unittest.mock import MagicMock, patch

from .. import watchman_subscriber
from ..watchman_subscriber import WatchmanSubscriber


@contextmanager
def send_sigint_to_self(
    path: str, blocking: bool
) -> Generator[Optional[int], None, None]:
    os.kill(os.getpid(), signal.SIGINT)
    yield


class WatchmanSubscriberTest(unittest.TestCase):
    @patch.object(watchman_subscriber, "register_unique_process")
    @patch.object(watchman_subscriber, "remove_if_exists")
    @patch.object(watchman_subscriber, "acquire_lock")
    @patch.object(sys, "exit")
    @patch.object(os, "close")
    @patch.object(os, "fork", return_value=0)
    @patch.object(os, "makedirs")
    def test_cleanup_on_sigint(
        self,
        makedirs: MagicMock,
        fork: MagicMock,
        close: MagicMock,
        exit: MagicMock,
        acquire_lock: MagicMock,
        remove_if_exists: MagicMock,
        register_unique_process: MagicMock,
    ) -> None:
        acquire_lock.side_effect = send_sigint_to_self
        # pyre-fixme[41]: `_name` cannot be reassigned. It is a read-only property.
        WatchmanSubscriber._name = "foo_subscriber"
        subscriber = WatchmanSubscriber(".pyre/test")
        subscriber.daemonize()

        self.assertEqual(fork.call_count, 2)
        register_unique_process.assert_called_once()
        remove_if_exists.assert_any_call(
            os.path.join(".pyre", "test", "foo_subscriber.lock")
        )

    @patch.object(os, "kill")
    @patch.object(watchman_subscriber.Path, "read_text")
    def test_stop_subscriber(self, read_text: MagicMock, os_kill: MagicMock) -> None:
        read_text.return_value = "123"
        WatchmanSubscriber.stop_subscriber(".pyre/foo", "some_monitor")
        os_kill.assert_called_once_with(123, signal.SIGINT)

    @patch.object(os, "kill")
    @patch.object(watchman_subscriber.Path, "read_text")
    def test_stop_subscriber_handle_exception(
        self, read_text: MagicMock, os_kill: MagicMock
    ) -> None:
        read_text.side_effect = FileNotFoundError
        WatchmanSubscriber.stop_subscriber(".pyre/foo", "some_monitor")
        os_kill.assert_not_called()
