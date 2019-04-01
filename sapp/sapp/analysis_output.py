#!/usr/bin/env python3

from argparse import ArgumentTypeError
from typing import Optional

from .sharded_files import ShardedFile


class AnalysisOutput(object):
    """Represents one of various ways the analysis output can be specified.
    A file name, a file handle, or a sharded file pattern.
    Access to the output is provided via generators that provide file handles
    to the diagnostics json (issues), or the summary json (pre and post).
    """

    single_file: Optional[str] = None
    file_shards: Optional[ShardedFile] = None
    file_handle = None
    # Canonical name for the AnalysisOutput, either the file name, the file
    # pattern, or the name associated with the file handle, or None.
    name = None

    @classmethod
    def from_file(cls, file_name: str):
        """Pass in either a single file name or a sharded file pattern.
        Performs early validation by 1) opening the file if it is a single file,
        or 2) computing and checking the file shards.
        """
        result = cls()
        result.name = file_name
        if "@" in file_name:
            try:
                result.file_shards = ShardedFile(file_name)
            except ValueError as e:
                raise ArgumentTypeError("can't open '{}': {}".format(file_name, e))
        else:
            result.single_file = file_name
            try:
                with open(file_name, "r"):
                    # Just check validity.
                    pass
            except OSError as e:
                raise ArgumentTypeError("can't open '{}': {}".format(file_name, e))
        return result

    @classmethod
    def from_handle(cls, file_handle):
        result = cls()
        result.name = file_handle.name if hasattr(file_handle, "name") else None
        result.file_handle = file_handle
        return result

    def file_handles(self):
        """Generates all file handles represented by the analysis.
        Callee owns file handle and closes it when the next is yielded or the
        generator ends.
        """
        if self.file_handle:
            yield self.file_handle
            self.file_handle.close()
        elif self.single_file:
            with open(self.single_file, "r") as f:
                yield f
        elif self.file_shards:
            for shard in self.file_shards.get_filenames():
                with open(shard, "r") as f:
                    yield f

    def file_names(self):
        """Generates all file names that are used to generate file_handles.
        """
        if self.file_shards:
            for name in self.file_shards.get_filenames():
                yield name
        elif self.name:
            yield name

    def is_sharded(self):
        return self.file_shards is not None
