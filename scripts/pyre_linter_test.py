# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import json
import os.path  # noqa
import subprocess
import unittest
from collections import OrderedDict
from unittest.mock import MagicMock, _patch, call, patch

from .pyre_linter import (
    __name__ as pyre_linter_name,
    _group_by_pyre_server,
    _lint_paths,
)


class PyreLinterTest(unittest.TestCase):
    @patch(f"{pyre_linter_name}._get_pyre_project", return_value="/root")
    @patch("os.chdir")
    @patch(
        "subprocess.run",
        return_value=MagicMock(
            stdout=json.dumps(
                {
                    "response": {
                        "errors": [
                            {
                                "line": 5,
                                "column": 4,
                                "path": "async_test.py",
                                "code": 1001,
                                "name": "Unawaited awaitable",
                                "description": "Unawaited description",
                                "long_description": "Unawaited long description",
                                "concise_description": "Unawaited concise",
                                "inference": {},
                                "define": "async_test.bar",
                            }
                        ]
                    }
                }
            ).encode()
        ),
    )
    def test_message_output_format(
        self, run: _patch, change_directory: _patch, get_pyre_project: _patch
    ) -> None:
        results = _lint_paths(["awaitable"], "/root", ["/root/async_test.py"])
        # pyre-ignore: Typeshed is missing the annotation.
        run.assert_called_once_with(
            ["pyre", "query", "run_check('awaitable', '/root/async_test.py')"],
            check=True,
            stdout=subprocess.PIPE,
            timeout=None,
        )
        self.assertEqual(
            [result._asdict() for result in results],
            [
                OrderedDict(
                    [
                        ("path", "/root/async_test.py"),
                        ("line", 5),
                        ("char", 4),
                        ("code", "PYRELINT"),
                        ("severity", "warning"),
                        ("name", "awaitable"),
                        ("original", None),
                        ("replacement", None),
                        ("description", "Unawaited description"),
                        ("bypassChangedLineFiltering", None),
                    ]
                )
            ],
        )

    @patch(
        "os.path.isfile",
        side_effect=lambda path: path
        in {
            "/a/.pyre_configuration",
            "/d/b/c/.pyre_configuration.local",
            "/d/b/.pyre_configuration.local",
            "/d/.pyre_configuration",
        },
    )
    def test_group_by_pyre_server(self, isfile: _patch) -> None:
        self.assertEqual(_group_by_pyre_server(["/nonexistent/a.py"]), {})
        self.assertEqual(_group_by_pyre_server(["/a/a.py"]), {"/a": ["/a/a.py"]})
        self.assertEqual(_group_by_pyre_server(["/d/b/c.py"]), {"/d/b": ["/d/b/c.py"]})
        self.assertEqual(
            _group_by_pyre_server(["/d/b/a/x.py"]), {"/d/b": ["/d/b/a/x.py"]}
        )
        self.assertEqual(
            _group_by_pyre_server(["/d/b/c/x.py"]), {"/d/b/c": ["/d/b/c/x.py"]}
        )
        self.assertEqual(
            _group_by_pyre_server(["/d/b/c/x.py", "/d/b/c/y.py", "/d/b/a/first.py"]),
            {"/d/b/c": ["/d/b/c/x.py", "/d/b/c/y.py"], "/d/b": ["/d/b/a/first.py"]},
        )

    # Test that the function handles timeouts gracefully.
    @patch("os.chdir")
    @patch("subprocess.run", side_effect=subprocess.TimeoutExpired("pyre", 42))
    def test_timeout(self, run: _patch, change_directory: _patch) -> None:
        self.assertEqual(
            _lint_paths(["awaitable"], "/root", ["/root/async_test.py"], 42), []
        )
        # pyre-ignore[16]: assert_called_once_with is missing from typeshed.
        run.assert_called_once_with(
            ["pyre", "query", "run_check('awaitable', '/root/async_test.py')"],
            check=True,
            stdout=subprocess.PIPE,
            timeout=42,
        )

    @patch(f"{pyre_linter_name}._get_pyre_project")
    @patch("os.chdir")
    @patch(
        "subprocess.run",
        return_value=MagicMock(
            stdout=json.dumps({"response": {"errors": []}}).encode()
        ),
    )
    def test_start_server(
        self, run: _patch, change_directory: _patch, get_project: _patch
    ) -> None:
        results = _lint_paths(
            ["awaitable"], "/root", ["/root/async_test.py"], start_server=True
        )
        # pyre-ignore: Typeshed is missing the annotation.
        run.assert_has_calls(
            [
                call(["pyre"], check=True, stdout=subprocess.DEVNULL),
                call(
                    ["pyre", "query", "run_check('awaitable', '/root/async_test.py')"],
                    check=True,
                    stdout=subprocess.PIPE,
                    timeout=None,
                ),
                call(["pyre", "stop"], check=True, stdout=subprocess.DEVNULL),
            ]
        )
        self.assertEqual([result._asdict() for result in results], [])
