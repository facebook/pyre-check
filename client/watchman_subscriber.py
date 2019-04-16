# Copyright (c) 2019-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import functools
import logging
import os
import signal
import sys
from multiprocessing import Event
from typing import Any, Dict, List, NamedTuple

from .filesystem import AnalysisDirectory, acquire_lock, remove_if_exists


LOG = logging.getLogger(__name__)  # type: logging.Logger


Subscription = NamedTuple(
    "Subscription", [("root", str), ("name", str), ("subscription", Dict[str, Any])]
)


class WatchmanSubscriber(object):
    def __init__(self, analysis_directory: AnalysisDirectory) -> None:
        self._base_path = os.path.join(
            analysis_directory.get_root(), ".pyre", self._name
        )  # type: str
        self._alive = True  # type: bool
        self._ready = Event()  # type: multiprocessing.synchronize.Event

    @property
    def _name(self) -> str:
        """
            A name to identify the subscriber. Used as the directory and file names
            for the log, lock, and pid files.
        """
        raise NotImplementedError

    @property
    def _subscriptions(self) -> List[Subscription]:
        """
            List of subscriptions
        """
        raise NotImplementedError

    def _handle_response(self, response: Dict[str, Any]) -> None:
        """
            Callback invoked when a message is received from watchman
        """
        raise NotImplementedError

    @property
    @functools.lru_cache(1)
    def _watchman_client(self) -> "pywatchman.client":  # noqa
        try:
            import pywatchman  # noqa

            # The client will block indefinitely when timeout is None.
            return pywatchman.client(timeout=None)
        except ImportError as exception:
            LOG.info("Not starting %s due to %s", self._name, str(exception))
            sys.exit(1)

    def _subscribe_to_watchman(self, subscription: Subscription) -> None:
        self._watchman_client.query(
            "subscribe", subscription.root, subscription.name, subscription.subscription
        )

    def _run(self) -> None:
        try:
            os.makedirs(self._base_path)
        except OSError:
            pass
        lock_path = os.path.join(self._base_path, "{}.lock".format(self._name))
        pid_path = os.path.join(self._base_path, "{}.pid".format(self._name))

        def cleanup() -> None:
            LOG.info("Cleaning up lock and pid files before exiting.")
            remove_if_exists(pid_path)
            remove_if_exists(lock_path)

        def interrupt_handler(_signal_number=None, _frame=None) -> None:
            LOG.info("Interrupt signal received.")
            cleanup()
            sys.exit(0)

        signal.signal(signal.SIGINT, interrupt_handler)

        # Die silently if unable to acquire the lock.
        with acquire_lock(lock_path, blocking=False):
            file_handler = logging.FileHandler(
                os.path.join(self._base_path, "%s.log" % self._name)
            )
            file_handler.setFormatter(
                logging.Formatter("%(asctime)s %(levelname)s %(message)s")
            )
            LOG.addHandler(file_handler)

            with open(pid_path, "w+") as pid_file:
                pid_file.write(str(os.getpid()))

            for subscription in self._subscriptions:
                self._subscribe_to_watchman(subscription)

            connection = self._watchman_client.recvConn
            if not connection:
                LOG.error("Connection to Watchman for %s not found", self._name)
                sys.exit(1)

            while self._alive:
                # This call is blocking, which prevents this loop from burning CPU.
                response = connection.receive()
                try:
                    if response["is_fresh_instance"]:
                        LOG.info(
                            "Ignoring initial watchman message for %s", response["root"]
                        )
                    else:
                        self._handle_response(response)
                    self._ready.set()  # At least one message has been received.
                except KeyError:
                    pass

            cleanup()

    def daemonize(self) -> None:
        """We double-fork here to detach the daemon process from the parent.
           If we were to just fork the child as a daemon, we'd have to worry about the
           parent process exiting zombifying the daemon."""
        if os.fork() == 0:
            pid = os.fork()
            if pid == 0:
                try:
                    # Closing the sys.stdout and stderr file descriptors here causes
                    # the program to crash when attempting to log.
                    os.close(sys.stdout.fileno())
                    os.close(sys.stderr.fileno())
                    self._run()
                    sys.exit(0)
                except Exception as exception:
                    LOG.info("Not running %s due to %s", self._name, str(exception))
                    sys.exit(1)
            else:
                sys.exit(0)
