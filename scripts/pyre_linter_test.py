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
from unittest.mock import MagicMock, _patch, patch

from .pyre_linter import _group_by_pyre_server, _lint_paths


class PyreLinterTest(unittest.TestCase):
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
    def test_message_output_format(self, run: _patch) -> None:
        results = _lint_paths(["awaitable"], "/root", ["/root/async_test.py"])
        # pyre-ignore: Typeshed is missing the annotation.
        run.assert_called_once_with(
            ["pyre", "query", "run_check('awaitable', '/root/async_test.py')"],
            check=True,
            stdout=subprocess.PIPE,
        )
        self.assertEqual(
            [result._asdict() for result in results],
            [
                OrderedDict(
                    [
                        ("path", "async_test.py"),
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
            "/b/c/.pyre_configuration.local",
            "/b/.pyre_configuration.local",
        },
    )
    def test_group_by_pyre_server(self, isfile: _patch) -> None:
        self.assertEqual(_group_by_pyre_server(["/nonexistent/a.py"]), {})
        self.assertEqual(_group_by_pyre_server(["/a/a.py"]), {"/a": ["/a/a.py"]})
        self.assertEqual(_group_by_pyre_server(["/b/c.py"]), {"/b": ["/b/c.py"]})
        self.assertEqual(_group_by_pyre_server(["/b/a/x.py"]), {"/b": ["/b/a/x.py"]})
        self.assertEqual(_group_by_pyre_server(["/b/c/x.py"]), {"/b/c": ["/b/c/x.py"]})
        self.assertEqual(
            _group_by_pyre_server(["/b/c/x.py", "/b/c/y.py", "/b/a/first.py"]),
            {"/b/c": ["/b/c/x.py", "/b/c/y.py"], "/b": ["/b/a/first.py"]},
        )
