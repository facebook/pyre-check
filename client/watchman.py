# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import functools
import logging
import logging.handlers
import os
import signal
import sys
import time
from abc import abstractstaticmethod
from pathlib import Path
from typing import Any, Dict, List, NamedTuple

from .configuration import Configuration
from .filesystem import acquire_lock
from .process import Process


LOG: logging.Logger = logging.getLogger(__name__)

WAIT_TIME_IN_SECONDS = 10.0

Subscription = NamedTuple(
    "Subscription", [("root", str), ("name", str), ("subscription", Dict[str, Any])]
)


class WatchmanRestartedException(Exception):
    pass


class WatchmanSubscriberTimedOut(Exception):
    pass


def compute_pid_path(base_path: str, name: str) -> str:
    return str(Path(base_path, f"{name}.pid"))


class Subscriber:
    def __init__(self, base_path: str, configuration: Configuration) -> None:
        self._base_path: str = base_path
        self._configuration = configuration
        self._alive: bool = True
        self._ready: bool = False

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
    @abstractstaticmethod
    def is_alive(configuration: Configuration) -> bool:
        """This needs to be a static method. It is called by newly-spawned
        commands, which are in a different process."""
        ...

    def cleanup(self) -> None:
        pass

    @property
    @functools.lru_cache(1)
    def _watchman_client(self) -> "pywatchman.client":  # noqa
        try:
            import pywatchman  # noqa

            # The client will block indefinitely when timeout is None.
            return pywatchman.client(timeout=None)
        except ImportError as exception:
            LOG.info(f"Not starting {self._name} due to {exception}")
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
        LOG.debug(f"Subscriber: Trying to acquire lock file {lock_path}.")

        # Die silently if unable to acquire the lock.
        try:
            with acquire_lock(lock_path, blocking=False), (
                Process.register_unique_process(
                    os.getpid(), compute_pid_path(self._base_path, self._name)
                )
            ):
                LOG.debug(f"Acquired lock on {lock_path}")
                file_handler = logging.handlers.RotatingFileHandler(
                    os.path.join(self._base_path, f"{self._name}.log"),
                    mode="a",
                    # Keep at most 5 log files on disk
                    backupCount=4,
                    # Limit the size of each log file to 10MB
                    maxBytes=10 * 1000 * 1000,
                )
                file_handler.setFormatter(
                    logging.Formatter(
                        "%(asctime)s [PID %(process)d] %(levelname)s %(message)s"
                    )
                )
                LOG.addHandler(file_handler)

                subscriptions = self._subscriptions
                for subscription in subscriptions:
                    self._subscribe_to_watchman(subscription)

                if not subscriptions:
                    LOG.info("No watchman roots to subscribe to.")

                connection = self._watchman_client.recvConn
                if not connection:
                    LOG.error(
                        f"Connection to Watchman for {self._name} not found", self._name
                    )
                    sys.exit(1)

                while self._alive:
                    # This call is blocking, which prevents this loop from burning CPU.
                    response = connection.receive()
                    if response.get("is_fresh_instance", False):
                        if not self._ready:
                            root = response.get("root", "<no-root-found>")
                            self._ready = True
                            LOG.info(
                                f"Ignoring initial is_fresh_instance message for {root}"
                            )
                        else:
                            raise WatchmanRestartedException()
                    else:
                        self._handle_response(response)
        finally:
            LOG.info("Cleaning up monitor files before exiting...")
            self.cleanup()

    def _sleep_until_monitor_is_up(self) -> None:
        stop_time = time.time() + WAIT_TIME_IN_SECONDS
        LOG.info("Waiting for monitor to start up...")
        while time.time() < stop_time:
            if self.is_alive(self._configuration):
                return
            time.sleep(0.1)
        raise WatchmanSubscriberTimedOut

    def daemonize(self) -> None:
        """We double-fork here to detach the daemon process from the parent.
        If we were to just fork the child as a daemon, we'd have to worry about the
        parent process exiting zombifying the daemon."""
        LOG.debug(f"Daemonizing the {self._name}.")
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
                    os._exit(0)
                except Exception as exception:
                    LOG.info(f"Not running {self._name} due to {exception}")
                    os._exit(1)
                finally:
                    LOG.info(f"Not running {self._name} due to system exception")
                    os._exit(1)
            else:
                os._exit(0)

        self._sleep_until_monitor_is_up()


def stop_subscriptions(base_path: str, subscriber_name: str) -> None:
    try:
        pid_path = Path(compute_pid_path(base_path, subscriber_name))
        pid = int(pid_path.read_text())
        os.kill(pid, signal.SIGINT)
        LOG.debug(f"Stopped the {subscriber_name} with pid {pid}.")
    except FileNotFoundError:
        LOG.debug(f"Could not stop the {subscriber_name} because it was not found.")
    except (OSError, ValueError) as exception:
        LOG.debug(
            f"Could not stop the {subscriber_name} "
            + f"because of exception `{exception}`."
        )
