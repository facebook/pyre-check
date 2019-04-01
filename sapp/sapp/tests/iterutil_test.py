from unittest import TestCase

from ..iterutil import split_every


class UtilsTest(TestCase):
    def test_split_every(self):
        self.assertEqual(
            list(split_every(2, range(10))), [[0, 1], [2, 3], [4, 5], [6, 7], [8, 9]]
        )
