#!/usr/bin/env fbpython
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import testslide

from ...language_server import daemon_connection

from ..daemon_query import DaemonQueryFailure

from ..daemon_query_failer import DaemonQueryNoOpFailer, RegexDaemonQueryFailer


class DaemonQueryAutoFailerTest(testslide.TestCase):
    def test_noop(self) -> None:
        # expected to be a no-op
        self.assertIsNone(DaemonQueryNoOpFailer().query_failure("/path"))


class DaemonQueryAutoFailerPatternTest(testslide.TestCase):
    def test_passes(self) -> None:
        daemonQueryAutoFailerPattern = RegexDaemonQueryFailer("/a/b/c/*")

        self.assertIsNone(daemonQueryAutoFailerPattern.query_failure("/path"))
        self.assertIsNone(
            daemonQueryAutoFailerPattern.query_failure("/a/b/something.py")
        )
        self.assertIsNone(
            daemonQueryAutoFailerPattern.query_failure("/a/b/something/something.py")
        )

        self.assertIsNone(
            daemonQueryAutoFailerPattern.query_connection_failure("/path")
        )
        self.assertIsNone(
            daemonQueryAutoFailerPattern.query_connection_failure("/a/b/something.py")
        )
        self.assertIsNone(
            daemonQueryAutoFailerPattern.query_connection_failure(
                "/a/b/something/something.py"
            )
        )

    def test_rejects(self) -> None:
        daemonQueryAutoFailerPattern = RegexDaemonQueryFailer("/a/b/c/*")

        self.assertEqual(
            daemonQueryAutoFailerPattern.query_failure("/a/b/c/something.py"),
            DaemonQueryFailure(
                "Not querying daemon for path: /a/b/c/something.py as matches regex: /a/b/c/*"
            ),
        )

        self.assertEqual(
            daemonQueryAutoFailerPattern.query_failure("/a/b/c/something/something.py"),
            DaemonQueryFailure(
                "Not querying daemon for path: /a/b/c/something/something.py as matches regex: /a/b/c/*"
            ),
        )

        self.assertEqual(
            daemonQueryAutoFailerPattern.query_connection_failure(
                "/a/b/c/something.py"
            ),
            daemon_connection.DaemonConnectionFailure(
                "Not querying daemon for path: /a/b/c/something.py as matches regex: /a/b/c/*"
            ),
        )

        self.assertEqual(
            daemonQueryAutoFailerPattern.query_connection_failure(
                "/a/b/c/something/something.py"
            ),
            daemon_connection.DaemonConnectionFailure(
                "Not querying daemon for path: /a/b/c/something/something.py as matches regex: /a/b/c/*"
            ),
        )
