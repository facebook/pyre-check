# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest

from sapp.analysis_output import AnalysisOutput
from sapp.base_parser import ParseType
from sapp.pysa_taint_parser import Parser


class TestParser(unittest.TestCase):
    def setUp(self) -> None:
        self.parser = Parser()
        self.json_file = "tools/pyre/facebook/pysa/integration_test/results.json"
        self.results = None

    def test_basic(self):
        entry = self._get_issue("hotpot.admin_views.query")
        self.assertRegex(entry.get("filename"), "hotpot/admin_views.py")
        self.assertEqual(entry.get("line"), 41)
        self.assertEqual(entry.get("start"), 44)
        self.assertEqual(entry.get("end"), 70)
        self.assertEqual(entry.get("code"), 5001)
        self.assertEqual(entry.get("initial_sources"), {("UserControlled", 0)})
        self.assertEqual(entry.get("final_sinks"), {("RemoteCodeExecution", 0)})
        self.assertEqual(entry.get("postconditions")[0]["callee"], "leaf")
        self.assertEqual(
            entry.get("preconditions")[0]["callee"],
            "hotpot.helper.try_execute_hotpot_query",
        )

    def _load_results(self):
        if not self.results:
            self.results = self.parser.parse(AnalysisOutput.from_file(self.json_file))
        return self.results

    def _get_issue(self, name):
        entries = self._load_results()
        for entry in entries:
            if entry.get("type") == ParseType.ISSUE:
                return entry
        self.assertFalse("No issue for callable with name {} found".format(name))
