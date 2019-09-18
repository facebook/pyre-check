# pyre-strict

from abc import ABC, abstractmethod
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict

from .environment import Environment


class InvalidSpecificationException(Exception):
    pass


class RepositoryState(ABC):
    @abstractmethod
    def prepare(self, environment: Environment) -> None:
        raise NotImplementedError

    @abstractmethod
    def get_working_directory(self) -> Path:
        raise NotImplementedError

    @abstractmethod
    def to_json(self) -> Dict[str, Any]:
        raise NotImplementedError

    @staticmethod
    def from_json(input_json: Dict[str, Any]) -> "RepositoryState":
        try:
            kind = input_json["kind"]
            if kind == "base":
                return BaseRepositoryState(
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


class BaseRepositoryState(RepositoryState):
    _repository: Path
    _commit_hash: str

    def __init__(self, repository: Path, commit_hash: str) -> None:
        self._repository = repository
        self._commit_hash = commit_hash

    def __eq__(self, other: object) -> bool:
        if isinstance(other, BaseRepositoryState):
            return (
                self._repository == other._repository
                and self._commit_hash == other._commit_hash
            )
        return NotImplemented

    def __hash__(self) -> int:
        return hash((self._repository, self._commit_hash))

    def __repr__(self) -> str:
        return f"BaseRepositoryState({self._repository}, {self._commit_hash})"

    def to_json(self) -> Dict[str, Any]:
        return {
            "kind": "base",
            "repository": str(self._repository),
            "commit_hash": str(self._commit_hash),
        }

    def prepare(self, environment: Environment) -> None:
        environment.checked_run(
            working_directory=self._repository, command=f"hg update {self._commit_hash}"
        )

    def get_working_directory(self) -> Path:
        return self._repository


@dataclass(frozen=True)
class Specification:
    old_state: RepositoryState
    new_state: RepositoryState

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
                new_state=RepositoryState.from_json(input_json["new_state"]),
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
