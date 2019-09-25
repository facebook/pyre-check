# pyre-strict

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
            else:
                raise InvalidSpecificationException(
                    f"Cannot create RepositoryState due to unrecognized kind"
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
    def update(self, environment: Environment, working_directory: Path) -> None:
        raise NotImplementedError

    @abstractmethod
    def to_json(self) -> Dict[str, Any]:
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
            else:
                raise InvalidSpecificationException(
                    f"Cannot create RepositoryUpdate due to unrecognized kind"
                )
        except KeyError as key:
            raise InvalidSpecificationException(
                f"Cannot create RepositoryUpdate due to missing field '{key}'"
            )


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
            working_directory=self.repository, command=f"hg whereami"
        ).stdout.strip()
        original_commit_hash = hg_output if len(hg_output) > 0 else None
        LOG.debug(f"Original hg commit = {original_commit_hash}")

        environment.checked_run(
            working_directory=self.repository, command=f"hg update {self.commit_hash}"
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


@dataclass(frozen=True)
class HgRepositoryUpdate(RepositoryUpdate):
    commit_hash: str

    def update(self, environment: Environment, working_directory: Path) -> None:
        environment.checked_run(
            working_directory=working_directory, command=f"hg update {self.commit_hash}"
        )

    def to_json(self) -> Dict[str, Any]:
        return {"kind": "hg", "commit_hash": self.commit_hash}


@dataclass(frozen=True)
class PatchRepositoryUpdate(RepositoryUpdate):
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
class FileRepositoryUpdate(RepositoryUpdate):
    changes: Dict[str, str]
    removals: List[str]

    def update(self, environment: Environment, working_directory: Path) -> None:
        for handle, content in self.changes.items():
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
class Specification:
    old_state: RepositoryState
    new_state: RepositoryUpdate

    pyre_check_pyre_options: str
    pyre_check_options: str
    pyre_start_pyre_options: str
    pyre_start_options: str
    pyre_incremental_pyre_options: str
    pyre_incremental_options: str

    def to_json(self) -> Dict[str, Any]:
        return {
            "old_state": self.old_state.to_json(),
            "new_state": self.new_state.to_json(),
            "pyre_check_pyre_options": self.pyre_check_pyre_options,
            "pyre_check_options": self.pyre_check_options,
            "pyre_start_pyre_options": self.pyre_start_pyre_options,
            "pyre_start_options": self.pyre_start_options,
            "pyre_incremental_pyre_options": self.pyre_incremental_pyre_options,
            "pyre_incremental_options": self.pyre_incremental_options,
        }

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
                pyre_incremental_pyre_options=input_json.get(
                    "pyre_incremental_pyre_options", ""
                ),
                pyre_incremental_options=input_json.get("pyre_incremental_options", ""),
            )
        except KeyError as key:
            raise InvalidSpecificationException(
                f"Cannot create Specification due to missing field '{key}'"
            )
