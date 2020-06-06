# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import builtins
import json
import os
import unittest
from pathlib import Path
from unittest.mock import MagicMock, call, patch

from .. import filesystem, recently_used_configurations
from ..recently_used_configurations import (
    RECENTLY_USED_LOCAL_CONFIGURATIONS_FILE,
    RECENTLY_USED_LOCAL_CONFIGURATIONS_LOCK,
)


class RecentlyUsedConfigurationsTest(unittest.TestCase):
    @patch.object(Path, "write_text")
    @patch.object(
        recently_used_configurations,
        "_load_recently_used_configurations",
        return_value=["bar", "baz"],
    )
    @patch.object(filesystem, "acquire_lock")
    def test_log_recently_used(
        self,
        acquire_lock: MagicMock,
        load_recently_used_configurations: MagicMock,
        write_text: MagicMock,
    ) -> None:
        recently_used_configurations.log_as_recently_used("foo", Path("/.pyre/"))
        write_text.assert_called_once_with('["foo", "bar", "baz"]')
        acquire_lock.assert_called_once_with(
            "/.pyre/recently-used-local-configurations.lock", blocking=False
        )

    @patch.object(Path, "write_text")
    @patch.object(recently_used_configurations, "_load_recently_used_configurations")
    @patch.object(filesystem, "acquire_lock", side_effect=OSError)
    def test_log_recently_used__lock_not_acquired(
        self,
        acquire_lock: MagicMock,
        load_recently_used_configurations: MagicMock,
        write_text: MagicMock,
    ) -> None:
        recently_used_configurations.log_as_recently_used("foo", Path("/.pyre/"))
        write_text.assert_not_called()

    @patch.object(
        recently_used_configurations,
        "_load_recently_used_configurations",
        return_value=["bar", "baz"],
    )
    @patch.object(filesystem, "acquire_lock")
    def test_get_recently_used(
        self, acquire_lock: MagicMock, load_recently_used_configurations: MagicMock
    ) -> None:
        self.assertEqual(
            recently_used_configurations.get_recently_used_configurations(
                Path("/.pyre/")
            ),
            ["bar", "baz"],
        )
        acquire_lock.assert_called_once_with(
            "/.pyre/recently-used-local-configurations.lock", blocking=False
        )

    @patch.object(recently_used_configurations, "_load_recently_used_configurations")
    @patch.object(filesystem, "acquire_lock", side_effect=OSError)
    def test_get_recently_used__lock_not_acquired(
        self, acquire_lock: MagicMock, load_recently_used_configurations: MagicMock
    ) -> None:
        self.assertEqual(
            recently_used_configurations.get_recently_used_configurations(
                Path("/.pyre/")
            ),
            [],
        )
        acquire_lock.assert_called_once_with(
            "/.pyre/recently-used-local-configurations.lock", blocking=False
        )

    @patch.object(filesystem, "acquire_lock")
    def test_log_recently_used__no_local_configuration(
        self, acquire_lock: MagicMock
    ) -> None:
        recently_used_configurations.log_as_recently_used(None, Path("/.pyre/"))
        acquire_lock.assert_not_called()

    @patch.object(Path, "read_text", return_value='["bar", "baz"]')
    def test_load_recently_used_configurations(self, read_text: MagicMock) -> None:
        self.assertEqual(
            recently_used_configurations._load_recently_used_configurations(
                Path("/.pyre")
            ),
            ["bar", "baz"],
        )

    @patch.object(Path, "read_text", side_effect=FileNotFoundError)
    def test_load_recently_used_configurations__no_existing_file(
        self, read_text: MagicMock
    ) -> None:
        self.assertEqual(
            recently_used_configurations._load_recently_used_configurations(
                Path("/.pyre")
            ),
            [],
        )

    @patch.object(json, "loads", side_effect=json.JSONDecodeError("foo", "bar", 0))
    @patch.object(Path, "read_text")
    def test_load_recently_used_configurations__json_error(
        self, read_text: MagicMock, json_loads: MagicMock
    ) -> None:
        self.assertEqual(
            recently_used_configurations._load_recently_used_configurations(
                Path("/.pyre")
            ),
            [],
        )

    def test_add_recently_used_configuration(self) -> None:
        self.assertEqual(
            recently_used_configurations._add_recently_used_configuration("foo", []),
            ["foo"],
        )
        self.assertEqual(
            recently_used_configurations._add_recently_used_configuration(
                "foo", ["bar", "foo", "baz"]
            ),
            ["foo", "bar", "baz"],
        )
        self.assertEqual(
            recently_used_configurations._add_recently_used_configuration(
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

    @patch.object(os, "remove")
    def test_delete_cache(self, remove: MagicMock) -> None:
        recently_used_configurations.delete_cache(Path("/.pyre"))
        remove.assert_has_calls(
            [
                call(str(Path("/.pyre") / RECENTLY_USED_LOCAL_CONFIGURATIONS_LOCK)),
                call(str(Path("/.pyre") / RECENTLY_USED_LOCAL_CONFIGURATIONS_FILE)),
            ]
        )

    @patch.object(builtins, "input", return_value="1")
    def test_prompt_user_for_local_root(self, input: MagicMock) -> None:
        self.assertEqual(
            recently_used_configurations.prompt_user_for_local_root(
                ["foo", "bar", "baz"]
            ),
            "bar",
        )

    @patch.object(builtins, "input", return_value="")
    def test_prompt_user_for_local_root__pressed_enter(self, input: MagicMock) -> None:
        self.assertEqual(
            recently_used_configurations.prompt_user_for_local_root(
                ["foo", "bar", "baz"]
            ),
            "foo",
        )

    @patch.object(builtins, "input", side_effect=ValueError)
    def test_prompt_user_for_local_root__interrupt(self, input: MagicMock) -> None:
        self.assertEqual(
            recently_used_configurations.prompt_user_for_local_root(
                ["foo", "bar", "baz"]
            ),
            None,
        )

    @patch.object(builtins, "input", side_effect=KeyboardInterrupt)
    def test_prompt_user_for_local_root__exception(self, input: MagicMock) -> None:
        self.assertEqual(
            recently_used_configurations.prompt_user_for_local_root(
                ["foo", "bar", "baz"]
            ),
            None,
        )
