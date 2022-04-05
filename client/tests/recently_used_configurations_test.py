# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import builtins
import json
import os
import unittest
from pathlib import Path
from unittest.mock import call, MagicMock, patch

from .. import filesystem, recently_used_configurations, terminal


class RecentlyUsedConfigurationsTest(unittest.TestCase):
    @patch.object(Path, "write_text")
    @patch.object(
        recently_used_configurations.Cache,
        "_load_items_from_file",
        return_value=["bar", "baz"],
    )
    # pyre-fixme[56]: Argument `tools.pyre.client.filesystem` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(filesystem, "acquire_lock")
    def test_put(
        self,
        acquire_lock: MagicMock,
        load_items_from_file: MagicMock,
        write_text: MagicMock,
    ) -> None:
        recently_used_configurations.Cache(Path("/.pyre")).put("foo")
        write_text.assert_called_once_with('["foo", "bar", "baz"]')
        acquire_lock.assert_called_once_with(
            "/.pyre/recently-used-local-configurations.lock", blocking=False
        )

    @patch.object(Path, "write_text")
    @patch.object(recently_used_configurations.Cache, "_load_items_from_file")
    # pyre-fixme[56]: Argument `tools.pyre.client.filesystem` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(filesystem, "acquire_lock", side_effect=OSError)
    def test_put__lock_not_acquired(
        self,
        acquire_lock: MagicMock,
        load_items_from_file: MagicMock,
        write_text: MagicMock,
    ) -> None:
        recently_used_configurations.Cache(Path("/.pyre")).put("foo")
        write_text.assert_not_called()

    @patch.object(
        recently_used_configurations.Cache,
        "_load_items_from_file",
        return_value=["bar", "baz"],
    )
    # pyre-fixme[56]: Argument `tools.pyre.client.filesystem` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(filesystem, "acquire_lock")
    def test_get_all_items(
        self, acquire_lock: MagicMock, load_items_from_file: MagicMock
    ) -> None:
        self.assertEqual(
            recently_used_configurations.Cache(Path("/.pyre")).get_all_items(),
            ["bar", "baz"],
        )
        acquire_lock.assert_called_once_with(
            "/.pyre/recently-used-local-configurations.lock", blocking=False
        )

    @patch.object(recently_used_configurations.Cache, "_load_items_from_file")
    # pyre-fixme[56]: Argument `tools.pyre.client.filesystem` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(filesystem, "acquire_lock", side_effect=OSError)
    def test_get_all_items__lock_not_acquired(
        self, acquire_lock: MagicMock, load_items_from_file: MagicMock
    ) -> None:
        self.assertEqual(
            recently_used_configurations.Cache(Path("/.pyre")).get_all_items(), []
        )
        acquire_lock.assert_called_once_with(
            "/.pyre/recently-used-local-configurations.lock", blocking=False
        )

    @patch.object(Path, "read_text", return_value='["bar", "baz"]')
    def test_load_items_from_file(self, read_text: MagicMock) -> None:
        self.assertEqual(
            recently_used_configurations.Cache(Path("/.pyre"))._load_items_from_file(),
            ["bar", "baz"],
        )

    @patch.object(Path, "read_text", side_effect=FileNotFoundError)
    def test_load_items_from_file__no_existing_file(self, read_text: MagicMock) -> None:
        self.assertEqual(
            recently_used_configurations.Cache(Path("/.pyre"))._load_items_from_file(),
            [],
        )

    # pyre-fixme[56]: Argument `json` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(json, "loads", side_effect=json.JSONDecodeError("foo", "bar", 0))
    @patch.object(Path, "read_text")
    def test_load_items_from_file__json_error(
        self, read_text: MagicMock, json_loads: MagicMock
    ) -> None:
        self.assertEqual(
            recently_used_configurations.Cache(Path("/.pyre"))._load_items_from_file(),
            [],
        )

    def test_add_recently_used_configuration(self) -> None:
        self.assertEqual(
            recently_used_configurations.Cache._add_recent_item("foo", []), ["foo"]
        )
        self.assertEqual(
            recently_used_configurations.Cache._add_recent_item(
                "foo", ["bar", "foo", "baz"]
            ),
            ["foo", "bar", "baz"],
        )
        self.assertEqual(
            recently_used_configurations.Cache._add_recent_item(
                "foo",
                [
                    f"bar{x}"
                    for x in range(recently_used_configurations.MAXIMUM_RECENT_ITEMS)
                ],
            ),
            [
                "foo",
                *[
                    f"bar{x}"
                    for x in range(
                        recently_used_configurations.MAXIMUM_RECENT_ITEMS - 1
                    )
                ],
            ],
        )

    # pyre-fixme[56]: Argument `os` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(os, "remove")
    def test_delete_cache(self, remove: MagicMock) -> None:
        recently_used_configurations.Cache(
            Path("/.pyre"), file_base_name="some-file-name"
        ).delete()
        remove.assert_has_calls(
            [
                call(str(Path("/.pyre") / "some-file-name.lock")),
                call(str(Path("/.pyre") / "some-file-name.json")),
            ]
        )

    @patch.object(terminal, "is_capable", return_value=True)
    # pyre-fixme[56]: Argument `builtins` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(builtins, "input", return_value="1")
    def test_prompt_user_for_local_root(
        self, input: MagicMock, is_capable: MagicMock
    ) -> None:
        self.assertEqual(
            recently_used_configurations.prompt_user_for_local_root(
                ["foo", "bar", "baz"]
            ),
            "bar",
        )

    @patch.object(terminal, "is_capable", return_value=True)
    # pyre-fixme[56]: Argument `builtins` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(builtins, "input", return_value="")
    def test_prompt_user_for_local_root__pressed_enter(
        self, input: MagicMock, is_capable: MagicMock
    ) -> None:
        self.assertEqual(
            recently_used_configurations.prompt_user_for_local_root(
                ["foo", "bar", "baz"]
            ),
            "foo",
        )

    @patch.object(terminal, "is_capable", return_value=True)
    # pyre-fixme[56]: Argument `builtins` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(builtins, "input", side_effect=ValueError)
    def test_prompt_user_for_local_root__interrupt(
        self, input: MagicMock, is_capable: MagicMock
    ) -> None:
        self.assertEqual(
            recently_used_configurations.prompt_user_for_local_root(
                ["foo", "bar", "baz"]
            ),
            None,
        )

    @patch.object(terminal, "is_capable", return_value=True)
    # pyre-fixme[56]: Argument `builtins` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(builtins, "input", side_effect=KeyboardInterrupt)
    def test_prompt_user_for_local_root__exception(
        self, input: MagicMock, is_capable: MagicMock
    ) -> None:
        self.assertEqual(
            recently_used_configurations.prompt_user_for_local_root(
                ["foo", "bar", "baz"]
            ),
            None,
        )

    @patch.object(terminal, "is_capable", return_value=False)
    # pyre-fixme[56]: Argument `builtins` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(builtins, "input", return_value="42")
    def test_prompt_user_for_local_root__not_terminal(
        self, input: MagicMock, is_capable: MagicMock
    ) -> None:
        self.assertEqual(
            recently_used_configurations.prompt_user_for_local_root(
                ["foo", "bar", "baz"]
            ),
            None,
        )
