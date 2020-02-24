# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import json
import os
from glob import glob
from typing import IO, Any, Dict, Iterable, NamedTuple, Optional

from .sharded_files import ShardedFile


METADATA_GLOB = "*metadata.json"


# pyre-fixme[2]: Parameter annotation cannot contain `Any`.
# pyre-fixme[8]: Attribute has type `Dict[int, typing.Any]`; used as
#  `Dict[Variable[_KT], Variable[_VT]]`.
class Metadata(NamedTuple):
    analysis_root: str
    repo_root: Optional[str] = None
    repository_name: Optional[str] = None
    tool: Optional[str] = None
    analysis_tool_version: Optional[str] = None
    commit_hash: Optional[str] = None
    job_instance: Optional[int] = None
    project: Optional[str] = None
    # Mapping from code to rule metadata.
    # pyre-ignore: we don't have a shape for rules yet.
    rules: Dict[int, Any] = {}

    @property
    def root(self) -> str:
        return self.repo_root or self.analysis_root


class AnalysisOutputError(Exception):
    pass


class AnalysisOutput(object):
    """Represents one of various ways the analysis output can be specified.
    A file name, a file handle, or a sharded file pattern.
    Access to the output is provided via generators that provide file handles
    to the diagnostics json (issues), or the summary json (pre and post).
    """

    def __init__(
        self,
        *,
        directory: Optional[str] = None,
        filename_spec: Optional[str] = None,
        file_handle: Optional[IO[str]] = None,
        metadata: Optional[Metadata] = None,
        tool: Optional[str] = None,
    ) -> None:
        self.directory = directory
        self.filename_spec = filename_spec
        self.file_handle = file_handle
        self.metadata = metadata
        self.tool = tool

        if not filename_spec and file_handle and hasattr(file_handle, "name"):
            self.filename_spec = file_handle.name

    def __str__(self) -> str:
        if self.directory:
            return f"AnalysisOutput({repr(self.directory)})"

        return f"AnalysisOutput({repr(self.filename_spec)})"

    @classmethod
    def from_str(cls, identifier: str) -> "AnalysisOutput":
        if os.path.isdir(identifier):
            return cls.from_directory(identifier)
        elif os.path.isfile(identifier):
            return cls.from_file(identifier)
        elif os.path.isdir(os.path.dirname(identifier)) and "@" in os.path.basename(
            identifier
        ):
            return cls.from_file(identifier)
        else:
            raise AnalysisOutputError(f"Unrecognized identifier `{identifier}`")

    @classmethod
    def from_directory(cls, directory: str) -> "AnalysisOutput":
        metadata = {}
        for file in glob(os.path.join(directory, METADATA_GLOB)):
            with open(file) as f:
                metadata.update(json.load(f))

        if "filename_spec" in metadata:
            filename_spec = os.path.join(
                directory, os.path.basename(metadata["filename_spec"])
            )
        else:
            # Legacy
            filename_spec = os.path.join(
                directory, os.path.basename(metadata["filenames"][0])
            )

        repo_root = metadata.get("repo_root")
        analysis_root = metadata["root"]

        rules = {rule["code"]: rule for rule in metadata.get("rules", [])}

        return cls(
            directory=directory,
            filename_spec=filename_spec,
            metadata=Metadata(
                analysis_tool_version=metadata["version"],
                commit_hash=metadata.get("commit"),
                analysis_root=analysis_root,
                repo_root=repo_root,
                job_instance=metadata.get("job_instance"),
                tool=metadata.get("tool"),
                repository_name=metadata.get("repository_name"),
                project=metadata.get("project"),
                rules=rules,
            ),
        )

    @classmethod
    def from_file(cls, file_name: str) -> "AnalysisOutput":
        # """Pass in either a single file name or a sharded file pattern.
        # Performs early validation by 1) opening the file if it is a single file,
        # or 2) computing and checking the file shards.
        # """
        return cls(filename_spec=file_name)

    @classmethod
    def from_handle(cls, file_handle: IO[str]) -> "AnalysisOutput":
        return cls(file_handle=file_handle)

    def file_handles(self) -> Iterable[IO[str]]:
        """Generates all file handles represented by the analysis.
        Callee owns file handle and closes it when the next is yielded or the
        generator ends.
        """
        if self.file_handle:
            # pyre-fixme[7]: Expected `Iterable[IO[str]]` but got
            #  `Generator[Optional[IO[str]], None, None]`.
            yield self.file_handle
            # pyre-fixme[16]: `Optional` has no attribute `close`.
            self.file_handle.close()
            self.file_handle = None
        else:
            for name in self.file_names():
                with open(name, "r") as f:
                    yield f

    def file_names(self) -> Iterable[str]:
        """Generates all file names that are used to generate file_handles.
        """
        if self.is_sharded():
            yield from ShardedFile(self.filename_spec).get_filenames()
        elif self.filename_spec:
            # pyre-fixme[7]: Expected `Iterable[str]` but got
            #  `Generator[Optional[str], None, None]`.
            yield self.filename_spec

    def is_sharded(self) -> bool:
        if self.filename_spec:
            # pyre-fixme[16]: `Optional` has no attribute `__getitem__`.
            return "@" in self.filename_spec
        else:
            return False
