# pyre-strict
import functools
import logging
import os
import sys
from typing import Any, Dict, List, NamedTuple

from .filesystem import AnalysisDirectory, acquire_lock


LOG = logging.getLogger(__name__)  # type: logging.Logger


Subscription = NamedTuple(
    "Subscription", [("root", str), ("name", str), ("subscription", Dict[str, Any])]
)


class WatchmanSubscriber(object):
    def __init__(self, analysis_directory: AnalysisDirectory) -> None:
        self._base_path = os.path.join(
            analysis_directory.get_root(), ".pyre", self._class_name
        )  # type: str

    @property
    def _class_name(self) -> str:
        return self.__class__.__name__.lower()

    @property
    def _name(self) -> str:
        """
            A name to identify the subscriber
        """
        raise NotImplementedError

    @property
    def _subscriptions(self) -> List[Subscription]:
        """
            List of subscriptions
        """
        raise NotImplementedError

    # pyre-ignore: Dict[str, Any] allowed in strict on latest version
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

            return pywatchman.client(timeout=3600.0)
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
        lock_path = os.path.join(self._base_path, "%s.lock" % self._class_name)
        # Die silently if unable to acquire the lock.
        with acquire_lock(lock_path, blocking=False):
            file_handler = logging.FileHandler(
                os.path.join(self._base_path, "%s.log" % self._class_name)
            )
            file_handler.setFormatter(
                logging.Formatter("%(asctime)s %(levelname)s %(message)s")
            )
            LOG.addHandler(file_handler)

            pid_path = os.path.join(self._base_path, "%s.pid" % self._class_name)
            with open(pid_path, "w+") as pid_file:
                pid_file.write(str(os.getpid()))

            for subscription in self._subscriptions:
                self._subscribe_to_watchman(subscription)

            connection = self._watchman_client.recvConn
            if not connection:
                LOG.error("Connection to Watchman for %s not found", self._name)
                sys.exit(1)

            while True:
                # This call is blocking, which prevents this loop from burning CPU.
                response = connection.receive()
                try:
                    if response["is_fresh_instance"]:
                        LOG.info(
                            "Ignoring initial watchman message for %s", response["root"]
                        )
                    else:
                        self._handle_response(response)
                except KeyError:
                    pass

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
