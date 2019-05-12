from unittest import TestCase

from ..pipeline import Pipeline
from ..warning_code_filter import WarningCodeFilter


class TestWarningCodeFilter(TestCase):
    def setUp(self) -> None:
        self.warning_code_filter = WarningCodeFilter({6000})

    def test_filter_codes(self):
        dict_entries = {"issues": [{"code": 6000}, {"code": 6001}, {"code": 6002}]}
        output, _ = Pipeline([self.warning_code_filter]).run(dict_entries)

        self.assertEqual(len(output["issues"]), 1)
        self.assertEqual(output["issues"][0], {"code": 6000})
