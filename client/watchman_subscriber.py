# Copyright (c) 2019-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import functools
import logging
import multiprocessing
import os
import signal
import sys
from multiprocessing import Event
from pathlib import Path
from typing import Any, Dict, List, NamedTuple

from .filesystem import acquire_lock, remove_if_exists
from .process import register_unique_process


LOG: logging.Logger = logging.getLogger(__name__)


Subscription = NamedTuple(
    "Subscription", [("root", str), ("name", str), ("subscription", Dict[str, Any])]
)


class WatchmanSubscriber(object):
    def __init__(self, base_path: str) -> None:
        self._base_path: str = base_path
        self._alive: bool = True
        self._ready: multiprocessing.synchronize.Event = Event()

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

    @staticmethod
    def _compute_pid_path(base_path: str, name: str) -> str:
        return str(Path(base_path, f"{name}.pid"))

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
        self._watchman_client.query("watch", subscription.root)
        self._watchman_client.query(
            "subscribe", subscription.root, subscription.name, subscription.subscription
        )

    def _run(self) -> None:
        try:
            os.makedirs(self._base_path)
        except OSError:
            pass
        lock_path: str = os.path.join(self._base_path, "{}.lock".format(self._name))
        LOG.debug(f"WatchmanSubscriber: Trying to acquire lock file {lock_path}.")

        def cleanup() -> None:
            LOG.info("Cleaning up lock and pid files before exiting.")
            remove_if_exists(lock_path)

        def interrupt_handler(_signal_number=None, _frame=None) -> None:
            LOG.info("Interrupt signal received.")
            cleanup()
            sys.exit(0)

        signal.signal(signal.SIGINT, interrupt_handler)

        # Die silently if unable to acquire the lock.
        with acquire_lock(lock_path, blocking=False), (
            register_unique_process(
                os.getpid(), self._compute_pid_path(self._base_path, self._name)
            )
        ):
            LOG.debug("Acquired lock on %s", lock_path)
            file_handler = logging.FileHandler(
                os.path.join(self._base_path, "%s.log" % self._name), mode="w"
            )
            file_handler.setFormatter(
                logging.Formatter("%(asctime)s %(levelname)s %(message)s")
            )
            LOG.addHandler(file_handler)

            subscriptions = self._subscriptions
            for subscription in subscriptions:
                self._subscribe_to_watchman(subscription)

            if not subscriptions:
                LOG.info("No watchman roots to subscribe to.")

            connection = self._watchman_client.recvConn
            if not connection:
                LOG.error("Connection to Watchman for %s not found", self._name)
                sys.exit(1)

            while self._alive:
                # This call is blocking, which prevents this loop from burning CPU.
                response = connection.receive()
                if response.get("is_fresh_instance", False):
                    # TODO: is_fresh_instance can occur at any time, not just the first
                    # response.  Ignoring the initial response is fine, but if we
                    # receive a fresh instance response later we should assume that all
                    # files may have been changed.
                    LOG.info(
                        "Ignoring initial watchman message for %s",
                        response.get("root", "<no-root-found>"),
                    )
                else:
                    self._handle_response(response)
                self._ready.set()  # At least one message has been received.

            cleanup()

    def daemonize(self) -> None:
        """We double-fork here to detach the daemon process from the parent.
           If we were to just fork the child as a daemon, we'd have to worry about the
           parent process exiting zombifying the daemon."""
        LOG.debug("Daemonizing the %s.", self._name)
        if os.fork() == 0:
            pid = os.fork()
            if pid == 0:
                try:
                    LOG.propagate = False
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

    @staticmethod
    def stop_subscriber(base_path: str, subscriber_name: str) -> None:
        try:
            pid_path = Path(
                WatchmanSubscriber._compute_pid_path(base_path, subscriber_name)
            )
            pid = int(pid_path.read_text())
            os.kill(pid, signal.SIGINT)
            LOG.debug("Stopped the %s with pid %d.", subscriber_name, pid)
        except FileNotFoundError:
            LOG.debug(f"Could not stop the {subscriber_name} because it was not found.")
        except (OSError, ValueError) as exception:
            LOG.debug(
                f"Could not stop the {subscriber_name} "
                f"because of exception `{exception}`."
            )
