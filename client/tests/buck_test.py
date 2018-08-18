# Copyright 2004-present Facebook.  All rights reserved.

import glob
import json
import subprocess
import unittest
from collections import OrderedDict, namedtuple
from unittest.mock import call, mock_open, patch

from .. import buck, log


BuckOut = namedtuple("BuckOut", "analysis_directories targets_not_found")


class BuckTest(unittest.TestCase):
    def test_presumed_target_root(self) -> None:
        self.assertEqual(
            buck.presumed_target_root("//path/directory/..."), "path/directory"
        )
        self.assertEqual(
            buck.presumed_target_root("/path/directory:target"), "path/directory"
        )

    def test_find_analysis_directories(self) -> None:
        trees = [
            "blah-vs_debugger#link-tree",
            "blah-blah#link-tree",
            "blah-interp#link-tree",
            "blah-ipython#link-tree",
        ]
        with patch.object(glob, "glob", return_value=trees) as glob_glob:
            self.assertEqual(
                buck._find_analysis_directories({"target": None}),
                BuckOut(["blah-blah#link-tree"], []),
            )
        with patch.object(glob, "glob") as glob_glob:
            buck._find_analysis_directories(
                {
                    "//path/targets:name": None,
                    "//path/targets:namelibrary": None,
                    "//path/...": None,
                }
            )
            glob_glob.assert_has_calls(
                [
                    call("buck-out/gen/path/targets/name#*link-tree"),
                    call("buck-out/gen/path/targets/namelibrary#*link-tree"),
                    call("buck-out/gen/path/...#*link-tree"),
                ],
                any_order=True,
            )

        with patch.object(glob, "glob", return_value=["new_tree"]) as glob_glob:
            found_trees = buck._find_analysis_directories(
                OrderedDict(
                    [
                        ("//path/targets:name", None),
                        ("//path/targets:namelibrary", None),
                        ("//path/targets:another", "buck-out/path/another"),
                        ("//path/...", None),
                    ]
                )
            )
            self.assertEqual(
                found_trees,
                BuckOut(["new_tree", "new_tree", "new_tree", "new_tree"], []),
            )

        with patch.object(glob, "glob", return_value=[]) as glob_glob:
            found_trees = buck._find_analysis_directories(
                OrderedDict(
                    [
                        ("//path/targets:name", None),
                        ("//path/targets:namelibrary", None),
                        ("//path/targets:another", "buck-out/path/another"),
                        ("//path/...", None),
                    ]
                )
            )
            self.assertEqual(
                found_trees,
                BuckOut(
                    [],
                    [
                        "//path/targets:name",
                        "//path/targets:namelibrary",
                        "//path/targets:another",
                        "//path/...",
                    ],
                ),
            )

        with patch.object(glob, "glob", return_value=[]) as glob_glob:
            found_trees = buck._find_analysis_directories(
                OrderedDict(
                    [
                        ("//path/targets:name", None),
                        ("//path/targets:namelibrary", ""),
                        ("//path/targets:another", ""),
                        ("//path/...", None),
                    ]
                )
            )
            self.assertEqual(
                found_trees, BuckOut([], ["//path/targets:name", "//path/..."])
            )

    @patch("%s.open" % buck.__name__, new_callable=mock_open, read_data="")
    def test_normalize(self, mock_open) -> None:
        with patch.object(json, "load", return_value={"target_path": ["a", "b"]}):
            with patch.object(json, "dump") as json_dump:
                with patch.object(subprocess, "check_output") as buck_targets:
                    buck_targets.return_value = "a\nb".encode("utf-8")
                    buck._normalize(["target_path"], use_cache=False)
                    buck._normalize(["target_path"], use_cache=True)
                    buck_targets.assert_has_calls(
                        [
                            call(
                                [
                                    "buck",
                                    "targets",
                                    "--show-output",
                                    "target_path",
                                    "--type",
                                    "python_binary",
                                    "python_test",
                                ],
                                stderr=subprocess.PIPE,
                                timeout=200,
                            )
                        ]
                    )

                    class Wildcard:
                        def __eq__(self, other):
                            return True

                    # Ignore the stream being written to.
                    json_dump.assert_has_calls(
                        [call({"target_path": ["a", "b"]}, Wildcard())]
                    )

    def test_build_targets(self) -> None:
        with patch.object(subprocess, "check_output") as buck_build:
            buck._build_targets(
                list(
                    {
                        "//t/...": {
                            "//t:subtarget": "subtarget destination",
                            "//t:subtarget2": "subtarget2 destination",
                        }
                    }.keys()
                )
            )
            buck_build.assert_called_once_with(
                ["buck", "build", "//t/..."], stderr=subprocess.PIPE
            )

    @patch.object(log, "get_yes_no_input", return_value=False)
    @patch.object(buck, "_normalize")
    @patch.object(buck, "_find_analysis_directories")
    def test_generate_analysis_directories(
        self, mock_find_analysis_directories, mock_normalize, mock_input
    ) -> None:
        mock_find_analysis_directories.return_value = BuckOut(  # noqa
            ["new_tree"], ["empty_target"]
        )

        with patch.object(buck, "_normalize") as mock_normalize:
            with self.assertRaises(buck.BuckException):
                buck.generate_analysis_directories(["target"], build=False, prompt=True)
                buck.generate_analysis_directories(
                    ["target1", "target2"], build=False, prompt=True
                )
                mock_normalize.assert_has_calls(
                    [call("target"), call("target1"), call("target2")]
                )

        mock_find_analysis_directories.return_value = BuckOut(["new_tree"], [])  # noqa
        with patch.object(buck, "_normalize") as mock_normalize:
            with patch.object(buck, "_build_targets") as mock_build:
                buck.generate_analysis_directories(["target"], build=True, prompt=True)
                mock_build.assert_not_called()
