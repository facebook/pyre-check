# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import Callable, Mapping, TypeVar

import testslide

from ..patch import (
    Action,
    action_from_json,
    AddAction,
    AddPosition,
    DeleteAction,
    DeleteImportAction,
    Patch,
    QualifiedName,
    ReadPatchException,
    ReplaceAction,
)


class PatchTest(testslide.TestCase):
    def test_qualified_name(self) -> None:
        def assert_name_preserved(name: str) -> None:
            self.assertEqual(QualifiedName.from_string(name).to_string(), name)

        assert_name_preserved("")
        assert_name_preserved("foo")
        assert_name_preserved("foo.bar")
        assert_name_preserved("foo.bar.baz")

        self.assertTrue(QualifiedName.from_string("").is_empty())
        self.assertFalse(QualifiedName.from_string("foo").is_empty())


T = TypeVar("T")
U = TypeVar("U")


class PatchReaderTest(testslide.TestCase):
    def _assert_parsed(self, input: T, parser: Callable[[T], U], expected: U) -> None:
        self.assertEqual(parser(input), expected)

    def _assert_not_parsed(self, input: T, parser: Callable[[T], U]) -> None:
        with self.assertRaises(ReadPatchException):
            parser(input)

    def assert_parsed_parent(self, input: object, expected: QualifiedName) -> None:
        self._assert_parsed(input, QualifiedName.from_json, expected)

    def assert_not_parsed_parent(self, input: object) -> None:
        self._assert_not_parsed(input, QualifiedName.from_json)

    def test_read_parent(self) -> None:
        self.assert_parsed_parent("foo", expected=QualifiedName(["foo"]))
        self.assert_parsed_parent("foo.bar", expected=QualifiedName(["foo", "bar"]))
        self.assert_not_parsed_parent(42)
        self.assert_not_parsed_parent([])
        self.assert_not_parsed_parent({})
        self.assert_not_parsed_parent(False)

    def assert_parsed_add_position(self, input: object, expected: AddPosition) -> None:
        self._assert_parsed(input, AddPosition.from_json, expected)

    def assert_not_parsed_add_position(self, input: object) -> None:
        self._assert_not_parsed(input, AddPosition.from_json)

    def test_read_add_position(self) -> None:
        self.assert_parsed_add_position("top", expected=AddPosition.TOP_OF_SCOPE)
        self.assert_parsed_add_position("bottom", expected=AddPosition.BOTTOM_OF_SCOPE)
        self.assert_not_parsed_add_position(42)
        self.assert_not_parsed_add_position([])
        self.assert_not_parsed_add_position({})
        self.assert_not_parsed_add_position(False)

    def assert_parsed_action(
        self, input: Mapping[str, object], expected: Action
    ) -> None:
        self._assert_parsed(input, action_from_json, expected)

    def assert_not_parsed_action(self, input: Mapping[str, object]) -> None:
        self._assert_not_parsed(input, action_from_json)

    def test_read_add_action(self) -> None:
        self.assert_parsed_action(
            {"action": "add", "content": "derp"}, AddAction(content="derp")
        )
        self.assert_parsed_action(
            {"action": "add", "content": "derp", "position": "top"},
            AddAction(content="derp", position=AddPosition.TOP_OF_SCOPE),
        )
        self.assert_not_parsed_action({"doom": "eternal"})
        self.assert_not_parsed_action({"action": "doom"})
        self.assert_not_parsed_action({"action": "add", "doom": "eternal"})

    def test_read_delete_action(self) -> None:
        self.assert_parsed_action(
            {"action": "delete", "name": "derp"}, DeleteAction(name="derp")
        )
        self.assert_not_parsed_action({"doom": "eternal"})
        self.assert_not_parsed_action({"action": "doom"})
        self.assert_not_parsed_action({"action": "delete", "doom": "eternal"})

    def test_read_delete_import_action(self) -> None:
        self.assert_parsed_action(
            {"action": "delete_import", "name": "derp"}, DeleteImportAction(name="derp")
        )
        self.assert_not_parsed_action({"doom": "eternal"})
        self.assert_not_parsed_action({"action": "doom"})
        self.assert_not_parsed_action({"action": "delete_import", "doom": "eternal"})

    def test_read_replace_action(self) -> None:
        self.assert_parsed_action(
            {"action": "replace", "name": "doom", "content": "BFG"},
            ReplaceAction(name="doom", content="BFG"),
        )
        self.assert_not_parsed_action({"doom": "eternal"})
        self.assert_not_parsed_action({"action": "doom"})
        self.assert_not_parsed_action({"action": "replace", "doom": "eternal"})
        self.assert_not_parsed_action({"action": "replace", "name": "doom"})
        self.assert_not_parsed_action({"action": "replace", "content": "BFG"})

    def assert_parsed_patch(self, input: object, expected: Patch) -> None:
        self._assert_parsed(input, Patch.from_json, expected)

    def assert_not_parsed_patch(self, input: object) -> None:
        self._assert_not_parsed(input, Patch.from_json)

    def test_read_patch(self) -> None:
        self.assert_parsed_patch(
            {"parent": "foo.bar", "action": "delete", "name": "doom"},
            Patch(
                parent=QualifiedName(["foo", "bar"]),
                action=DeleteAction(name="doom"),
            ),
        )
        self.assert_parsed_patch(
            {"action": "delete", "name": "doom"},
            Patch(
                action=DeleteAction(name="doom"), parent=QualifiedName.from_string("")
            ),
        )
        self.assert_not_parsed_patch(42)
        self.assert_not_parsed_patch([])
        self.assert_not_parsed_patch(False)
        self.assert_not_parsed_patch({})
        self.assert_not_parsed_patch({"parent": "foo.bar"})
