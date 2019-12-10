# Copyright (c) 2019-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe
import os
import signal
import sys
import unittest
from contextlib import contextmanager
from unittest.mock import patch

from .. import watchman_subscriber
from ..watchman_subscriber import WatchmanSubscriber


@contextmanager
def send_sigint_to_self(*args, **kwargs) -> None:
    os.kill(os.getpid(), signal.SIGINT)


class WatchmanSubscriberTest(unittest.TestCase):
    @patch.object(watchman_subscriber, "remove_if_exists")
    @patch.object(watchman_subscriber, "acquire_lock", side_effect=send_sigint_to_self)
    @patch.object(sys, "exit")
    @patch.object(os, "close")
    @patch.object(os, "fork", return_value=0)
    @patch.object(os, "makedirs")
    def test_cleanup_on_sigint(
        self, makedirs, fork, close, exit, acquire_lock, remove_if_exists
    ) -> None:
        # pyre-fixme[41]: `_name` cannot be reassigned. It is a read-only property.
        WatchmanSubscriber._name = "TEST"
        subscriber = WatchmanSubscriber(".pyre/TEST")
        subscriber.daemonize()

        remove_if_exists.assert_any_call(os.path.join(".pyre", "TEST", "TEST.pid"))
        remove_if_exists.assert_any_call(os.path.join(".pyre", "TEST", "TEST.lock"))
