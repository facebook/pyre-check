from unittest import TestCase

from tools.sapp.base_parser import ParseType
from tools.sapp.pipeline import Pipeline
from tools.sapp.warning_code_filter import WarningCodeFilter


class TestWarningCodeFilter(TestCase):
    def setUp(self) -> None:
        self.warning_code_filter = WarningCodeFilter({6000})

    def test_filter_codes(self):
        tuple_entries = [
            (ParseType.ISSUE, "key1", {"code": 6000}),
            (ParseType.ISSUE, "key2", {"code": 6001}),
            (ParseType.ISSUE, "key2", {"code": 6002}),
        ]
        input = (tuple_entries, None)
        output, _ = Pipeline([self.warning_code_filter]).run(input)
        output_entries = list(output[0])

        self.assertEqual(len(output_entries), 1)
        self.assertEqual(output_entries[0], (ParseType.ISSUE, "key1", {"code": 6000}))
