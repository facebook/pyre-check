# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
import logging
import os
import sys

from . import stop
from ..filesystem import AnalysisDirectory, acquire_lock


LOG = logging.getLogger(__name__)


class Monitor:
    def __init__(
        self, arguments, configuration, analysis_directory: AnalysisDirectory
    ) -> None:
        self.arguments = arguments
        self.configuration = configuration
        self.analysis_directory = analysis_directory.get_root()
        self.watchman_client = None

    def _subscribe_to_watchman(self, root: str) -> None:
        name = "pyre_monitor_{}".format(os.path.basename(root))
        subscription = {
            "expression": [
                "allof",
                ["type", "f"],
                ["not", "empty"],
                ["suffix", "pyre_configuration.local"],
            ],
            "fields": ["name"],
        }
        self.watchman_client.query("subscribe", root, name, subscription)

    def _run(self) -> None:
        try:
            import pywatchman  # noqa

            self.watchman_client = pywatchman.client(timeout=3600.0)
        except ImportError as exception:
            LOG.info("Not starting monitor due to %s", str(exception))
            sys.exit(1)
        try:
            os.makedirs(os.path.join(self.analysis_directory, ".pyre/monitor"))
        except OSError:
            pass
        lock_path = os.path.join(self.analysis_directory, ".pyre/monitor/monitor.lock")
        # Die silently if unable to acquire the lock.
        with acquire_lock(lock_path, blocking=False):
            file_handler = logging.FileHandler(
                os.path.join(self.analysis_directory, ".pyre/monitor/monitor.log")
            )
            file_handler.setFormatter(
                logging.Formatter("%(asctime)s %(levelname)s %(message)s")
            )
            LOG.addHandler(file_handler)

            pid_path = os.path.join(
                self.analysis_directory, ".pyre/monitor/monitor.pid"
            )
            with open(pid_path, "w+") as pid_file:
                pid_file.write(str(os.getpid()))

            watched_roots = self.watchman_client.query("watch-list")["roots"]
            for root in watched_roots:
                self._subscribe_to_watchman(root)

            while True:
                # This call is blocking, which prevents this loop from burning CPU.
                response = self.watchman_client.recvConn.receive()
                try:
                    if response["is_fresh_instance"]:
                        LOG.info(
                            "Ignoring initial watchman message for %s", response["root"]
                        )
                    else:
                        LOG.info(
                            "Update to local configuration at %s",
                            os.path.join(response["root"], ",".join(response["files"])),
                        )
                        LOG.info("Stopping running pyre server.")
                        stop.Stop(
                            self.arguments, self.configuration, self.analysis_directory
                        ).run()
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
                    LOG.info("Not running pyre-monitor due to %s", str(exception))
                    sys.exit(1)
            else:
                sys.exit(0)
