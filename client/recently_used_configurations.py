# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
import logging
import os
import sys
from pathlib import Path
from typing import Generic, List, Optional, TypeVar

from . import filesystem, terminal


LOG: logging.Logger = logging.getLogger(__name__)
MAXIMUM_RECENT_ITEMS = 10


T = TypeVar("T")


class Cache(Generic[T]):
    def __init__(
        self,
        cache_directory: Path,
        file_base_name: str = "recently-used-local-configurations",
    ) -> None:
        self._file_path: Path = cache_directory / f"{file_base_name}.json"
        self._lock_path: Path = cache_directory / f"{file_base_name}.lock"

    @staticmethod
    def _add_recent_item(new_item: T, existing_items: List[T]) -> List[T]:
        updated_list = [
            new_item,
            *(item for item in existing_items if item != new_item),
        ]
        return updated_list[:MAXIMUM_RECENT_ITEMS]

    def _load_items_from_file(self) -> List[T]:
        items = []
        try:
            items = json.loads(self._file_path.read_text())
        except FileNotFoundError:
            LOG.debug(f"No existing file `{str(self._file_path)}`.")
        except json.JSONDecodeError:
            LOG.debug(f"Error when loading json from `{str(self._file_path)}`")
        return items

    def put(self, new_item: T) -> None:
        try:
            with filesystem.acquire_lock(str(self._lock_path), blocking=False):
                existing_items = self._load_items_from_file()
                updated_items = self._add_recent_item(new_item, existing_items)
                self._file_path.write_text(json.dumps(updated_items))
        except OSError:
            LOG.debug(
                f"Failed to acquire lock `{str(self._lock_path)}`. "
                + "Not adding to recently-used items cache."
            )

    def get_all_items(self) -> List[T]:
        try:
            with filesystem.acquire_lock(str(self._lock_path), blocking=False):
                return self._load_items_from_file()
        except OSError:
            LOG.debug(
                f"Failed to acquire lock `{str(self._lock_path)}`. "
                + "Returning empty list of recently-used items."
            )
            return []

    def delete(self) -> None:
        try:
            os.remove(str(self._lock_path))
            os.remove(str(self._file_path))
        except OSError as error:
            LOG.debug(
                f"Error while trying to delete recently-used cache files: {error}."
            )


def prompt_user_for_local_root(local_roots: List[str]) -> Optional[str]:
    if not terminal.is_capable(sys.stdin):
        return None

    def _default_indicator(index: int) -> str:
        return " [default]" if index == 0 else ""

    local_root_choices = [
        f"{i}. {local_root}{_default_indicator(i)}"
        for i, local_root in enumerate(local_roots)
    ]
    prompt = (
        "Enter the number of the local root you want to use "
        + "(`Enter` for the default; any other key to quit): "
    )
    message = "\n".join(
        # pyre-fixme[6]: Expected `Iterable[typing_extensions.Literal[str]]` for 1st ...
        (
            "Would you like to run the command with a local root?",
            "",
            "Recently-used local roots:",
            *local_root_choices,
            prompt,
        )
    )

    LOG.info(message)

    try:
        user_input = input()
        index = 0 if user_input == "" else int(user_input)
        return local_roots[index]
    except (Exception, KeyboardInterrupt):
        LOG.error("No valid local root chosen. Quitting.")
        return None
