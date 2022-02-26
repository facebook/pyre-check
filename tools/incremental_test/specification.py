# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
import logging
from abc import ABC, abstractmethod
from contextlib import contextmanager
from dataclasses import dataclass
from pathlib import Path
from typing import Any, ContextManager, Dict, Iterator, List

from .environment import Environment


LOG: logging.Logger = logging.getLogger(__name__)


class InvalidSpecificationException(Exception):
    pass


class RepositoryState(ABC):
    @abstractmethod
    def activate_sandbox(self, environment: Environment) -> ContextManager[Path]:
        raise NotImplementedError

    @abstractmethod
    def to_json(self) -> Dict[str, Any]:
        raise NotImplementedError

    @staticmethod
    def from_json(input_json: Dict[str, Any]) -> "RepositoryState":
        try:
            kind = input_json["kind"]
            if kind == "hg":
                return HgRepositoryState(
                    repository=Path(input_json["repository"]),
                    commit_hash=input_json["commit_hash"],
                )
            elif kind == "file":
                files = input_json["files"]
                if not isinstance(files, dict):
                    raise InvalidSpecificationException(
                        "File repository must be specified as dicts"
                    )
                return FileRepositoryState(files)
            elif kind == "updated":
                base = input_json["base"]
                updates = input_json["updates"]
                if not isinstance(updates, list):
                    raise InvalidSpecificationException(
                        "Updates must be specified as lists"
                    )
                return UpdatedRepositoryState(
                    RepositoryState.from_json(base),
                    [RepositoryUpdate.from_json(update) for update in updates],
                )
            else:
                raise InvalidSpecificationException(
                    "Cannot create RepositoryState due to unrecognized kind"
                )
        except KeyError as key:
            raise InvalidSpecificationException(
                f"Cannot create RespositoryState due to missing field '{key}'"
            )
        except TypeError as error:
            raise InvalidSpecificationException(
                f"Cannot create RespositoryState due to invalid path: {error}"
            )


class RepositoryUpdate(ABC):
    @abstractmethod
    def to_json(self) -> Dict[str, Any]:
        raise NotImplementedError

    @abstractmethod
    def update_steps(self) -> List["SingleUpdate"]:
        raise NotImplementedError

    @staticmethod
    def from_json(input_json: Dict[str, Any]) -> "RepositoryUpdate":
        try:
            kind = input_json["kind"]
            if kind == "hg":
                return HgRepositoryUpdate(commit_hash=input_json["commit_hash"])
            elif kind == "patch":
                return PatchRepositoryUpdate(
                    patch=input_json["patch"],
                    patch_flags=input_json.get("patch_flags", ""),
                )
            elif kind == "file":
                changes = input_json.get("changes", {})
                removals = input_json.get("removals", [])
                if not isinstance(changes, dict):
                    raise InvalidSpecificationException(
                        "File changes must be specified as dicts"
                    )
                if not isinstance(removals, list):
                    raise InvalidSpecificationException(
                        "File removals must be specified as lists"
                    )
                if len(changes) == 0 and len(removals) == 0:
                    raise InvalidSpecificationException("No file change is given")
                return FileRepositoryUpdate(changes=changes, removals=removals)
            elif kind == "batch":
                updates = input_json["updates"]
                if not isinstance(updates, list):
                    raise InvalidSpecificationException(
                        "Batch updates must be specified as lists"
                    )
                parsed_updates: List[SingleUpdate] = []
                for update in updates:
                    parsed_update = RepositoryUpdate.from_json(update)
                    parsed_updates.extend(parsed_update.update_steps())
                return BatchRepositoryUpdate(parsed_updates)
            else:
                raise InvalidSpecificationException(
                    "Cannot create RepositoryUpdate due to unrecognized kind"
                )
        except KeyError as key:
            raise InvalidSpecificationException(
                f"Cannot create RepositoryUpdate due to missing field '{key}'"
            )


class SingleUpdate(RepositoryUpdate):
    @abstractmethod
    def update(self, environment: Environment, working_directory: Path) -> None:
        raise NotImplementedError

    def update_steps(self) -> List["SingleUpdate"]:
        return [self]


@dataclass(frozen=True)
class HgRepositoryState(RepositoryState):
    repository: Path
    commit_hash: str

    def to_json(self) -> Dict[str, Any]:
        return {
            "kind": "hg",
            "repository": str(self.repository),
            "commit_hash": self.commit_hash,
        }

    def get_working_directory(self) -> Path:
        return self.repository

    @contextmanager
    def _do_prepare(self, environment: Environment) -> Iterator[Path]:
        # Save the original commit hash.
        hg_output = environment.checked_run(
            working_directory=self.repository, command="hg whereami"
        ).stdout.strip()
        original_commit_hash = hg_output if len(hg_output) > 0 else None
        LOG.debug(f"Original hg commit = {original_commit_hash}")

        environment.checked_run(
            working_directory=self.repository,
            command=f"hg update --clean {self.commit_hash}",
        )
        yield self.repository

        # Discard all changes and revert to the original commit hash.
        if original_commit_hash is not None:
            environment.checked_run(
                working_directory=self.repository,
                command=f"hg update --clean {original_commit_hash}",
            )

    def activate_sandbox(self, environment: Environment) -> ContextManager[Path]:
        return self._do_prepare(environment)


@dataclass
class FileRepositoryState(RepositoryState):
    files: Dict[str, str]

    def to_json(self) -> Dict[str, Any]:
        return {"kind": "file", "files": self.files}

    @contextmanager
    def _do_prepare(self, environment: Environment) -> Iterator[Path]:
        # Grab a temporary directory as the local root
        temporary_directory = environment.checked_run(
            working_directory=Path("."), command="mktemp -d"
        ).stdout.strip()
        root = Path(temporary_directory)
        LOG.debug(f"Using temporary directory {temporary_directory} as local root")

        watched = False
        try:
            # Write all files under the local root.
            all_files = {
                ".watchmanconfig": "{}",
                # Note that --binary and --typeshed still needs to be set in pyre flags.
                ".pyre_configuration": '{ "source_directories": [ "." ] }',
                **self.files,
            }
            FileRepositoryUpdate(changes=all_files, removals=[]).update(
                environment, root
            )

            # Watchman uses the "error" field instead of return code to signal errors
            watchman_output = environment.checked_run(
                working_directory=root, command="watchman watch ."
            ).stdout
            if "error" in json.loads(watchman_output):
                raise RuntimeError(
                    f"`watchman watch` invocation failed with output:\n{watchman_output}"
                )
            watched = True
            yield root
        finally:
            # Clean up the files we've written.
            if watched:
                environment.checked_run(
                    working_directory=root, command="watchman watch-del ."
                )
            environment.checked_run(
                working_directory=Path("."), command=f"rm -rf {temporary_directory}"
            )

    def activate_sandbox(self, environment: Environment) -> ContextManager[Path]:
        return self._do_prepare(environment)


@dataclass
class UpdatedRepositoryState(RepositoryState):
    base: RepositoryState
    updates: List[RepositoryUpdate]

    def to_json(self) -> Dict[str, Any]:
        return {
            "kind": "updated",
            "base": self.base.to_json(),
            "updates": [update.to_json() for update in self.updates],
        }

    @contextmanager
    def _do_prepare(self, environment: Environment) -> Iterator[Path]:
        with self.base.activate_sandbox(environment) as sandbox_root:
            for update in self.updates:
                single_updates = update.update_steps()
                for single_update in single_updates:
                    single_update.update(environment, sandbox_root)
            yield sandbox_root

    def activate_sandbox(self, environment: Environment) -> ContextManager[Path]:
        return self._do_prepare(environment)


@dataclass(frozen=True)
class HgRepositoryUpdate(SingleUpdate):
    commit_hash: str

    def update(self, environment: Environment, working_directory: Path) -> None:
        environment.checked_run(
            working_directory=working_directory,
            command=f"hg update --clean {self.commit_hash}",
        )

    def to_json(self) -> Dict[str, Any]:
        return {"kind": "hg", "commit_hash": self.commit_hash}


@dataclass(frozen=True)
class PatchRepositoryUpdate(SingleUpdate):
    patch: str
    patch_flags: str

    def update(self, environment: Environment, working_directory: Path) -> None:
        environment.checked_run(
            working_directory=working_directory,
            command=f"patch {self.patch_flags}",
            stdin=self.patch,
        )

    def to_json(self) -> Dict[str, Any]:
        return {"kind": "patch", "patch": self.patch, "patch_flags": self.patch_flags}


@dataclass(frozen=True)
class FileRepositoryUpdate(SingleUpdate):
    changes: Dict[str, str]
    removals: List[str]

    def update(self, environment: Environment, working_directory: Path) -> None:
        for handle, content in self.changes.items():
            # Need to create parent directory if it doesn't exist
            parent_path = Path(handle).parent
            if not parent_path == Path("."):
                environment.checked_run(
                    working_directory=working_directory,
                    command=f"mkdir -p {parent_path}",
                )
            environment.checked_run(
                working_directory=working_directory,
                command=f"tee {handle}",
                stdin=content,
            )
        for handle in self.removals:
            environment.checked_run(
                working_directory=working_directory, command=f"rm -f {handle}"
            )

    def to_json(self) -> Dict[str, Any]:
        return {"kind": "file", "changes": self.changes, "removals": self.removals}


@dataclass(frozen=True)
class BatchRepositoryUpdate(RepositoryUpdate):
    _updates: List[SingleUpdate]

    def to_json(self) -> Dict[str, Any]:
        return {
            "kind": "batch",
            "updates": [update.to_json() for update in self._updates],
        }

    def update_steps(self) -> List[SingleUpdate]:
        return self._updates


@dataclass(frozen=True)
class Specification:
    old_state: RepositoryState
    new_state: RepositoryUpdate

    pyre_check_pyre_options: str = ""
    pyre_check_options: str = ""
    pyre_start_pyre_options: str = ""
    pyre_start_options: str = ""
    pyre_stop_pyre_options: str = ""
    pyre_stop_options: str = ""
    pyre_incremental_pyre_options: str = ""
    pyre_incremental_options: str = ""

    def to_json(self) -> Dict[str, Any]:
        result: Dict[str, Any] = {
            "old_state": self.old_state.to_json(),
            "new_state": self.new_state.to_json(),
        }
        if len(self.pyre_check_pyre_options) > 0:
            result["pyre_check_pyre_options"] = self.pyre_check_pyre_options
        if len(self.pyre_check_options) > 0:
            result["pyre_check_options"] = self.pyre_check_options
        if len(self.pyre_start_pyre_options) > 0:
            result["pyre_start_pyre_options"] = self.pyre_start_pyre_options
        if len(self.pyre_start_options) > 0:
            result["pyre_start_options"] = self.pyre_start_options
        if len(self.pyre_stop_pyre_options) > 0:
            result["pyre_stop_pyre_options"] = self.pyre_stop_pyre_options
        if len(self.pyre_stop_options) > 0:
            result["pyre_stop_options"] = self.pyre_stop_options
        if len(self.pyre_incremental_pyre_options) > 0:
            result["pyre_incremental_pyre_options"] = self.pyre_incremental_pyre_options
        if len(self.pyre_incremental_options) > 0:
            result["pyre_incremental_options"] = self.pyre_incremental_options
        return result

    @staticmethod
    def from_json(input_json: Dict[str, Any]) -> "Specification":
        try:
            return Specification(
                old_state=RepositoryState.from_json(input_json["old_state"]),
                new_state=RepositoryUpdate.from_json(input_json["new_state"]),
                pyre_check_pyre_options=input_json.get("pyre_check_pyre_options", ""),
                pyre_check_options=input_json.get("pyre_check_options", ""),
                pyre_start_pyre_options=input_json.get("pyre_start_pyre_options", ""),
                pyre_start_options=input_json.get("pyre_start_options", ""),
                pyre_stop_pyre_options=input_json.get("pyre_stop_pyre_options", ""),
                pyre_stop_options=input_json.get("pyre_stop_options", ""),
                pyre_incremental_pyre_options=input_json.get(
                    "pyre_incremental_pyre_options", ""
                ),
                pyre_incremental_options=input_json.get("pyre_incremental_options", ""),
            )
        except KeyError as key:
            raise InvalidSpecificationException(
                f"Cannot create Specification due to missing field '{key}'"
            )
