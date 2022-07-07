# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import testslide

from ..exceptions import InvalidConfiguration
from ..extension import Element


class ElementTest(testslide.TestCase):
    def test_from_json(self) -> None:
        def assert_extension_equal(input: object, expected: Element) -> None:
            self.assertEqual(
                Element.from_json(input),
                expected,
            )

        def assert_extension_raises(input: object) -> None:
            with self.assertRaises(InvalidConfiguration):
                Element.from_json(input)

        assert_extension_raises({})
        assert_extension_raises({"derp": 42})
        assert_extension_equal(".pyi", Element(suffix=".pyi"))
        assert_extension_equal(
            {"suffix": ".pyi", "include_suffix_in_module_qualifier": True},
            Element(suffix=".pyi", include_suffix_in_module_qualifier=True),
        )
        assert_extension_raises(
            {"suffix": 42, "include_suffix_in_module_qualifier": True},
        )
        assert_extension_raises(
            {"suffix": ".pyi", "include_suffix_in_module_qualifier": []},
        )
