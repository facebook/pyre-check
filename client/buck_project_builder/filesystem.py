# Copyright (c) 2019-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import io
import os
import subprocess
import urllib.parse
import urllib.request
import zipfile
from typing import Iterable, List, Mapping, NamedTuple, Optional

from ..filesystem import get_filesystem


Glob = NamedTuple("Glob", [("patterns", List[str]), ("exclude", List[str])])


class Sources:
    def __init__(
        self,
        files: Optional[Mapping[str, str]] = None,
        globs: Optional[List[Glob]] = None,
    ) -> None:
        # Files are given as a mapping from source location to output location.
        self.files = files or {}  # type: Mapping[str, str]
        self.globs = globs or []  # type: List[Glob]


def resolve_source_mapping(
    source_directory: str, output_directory: str, sources: Sources
) -> Mapping[str, str]:
    """
        Returns a mapping from absolute source path to absolute output path as specified
        by the sources object. Files are not guaranteed to exist.
    """
    result = {
        os.path.join(source_directory, source_file): os.path.join(
            output_directory, output_file
        )
        for source_file, output_file in sources.files.items()
    }

    filesystem = get_filesystem()
    for glob in sources.globs:
        matches = filesystem.list(source_directory, glob.patterns, exclude=glob.exclude)
        result.update(
            {
                os.path.join(source_directory, match): os.path.join(
                    output_directory, match
                )
                for match in matches
            }
        )
    return result


def build_thrift_stubs(
    buck_root: str,
    thrift_sources: Iterable[str],
    output_directory: str,
    include_json_converters: bool = False,
) -> str:
    command = [
        "thrift",
        "--gen",
        "mstch_pyi{}".format(":json" if include_json_converters else ""),
        "-I",
        ".",
        "--templates",
        "thrift/compiler/generate/templates",
        "-o",
        output_directory,
    ]
    for thrift_source in thrift_sources:
        subprocess.call(
            command + [thrift_source], stderr=subprocess.PIPE, cwd=buck_root
        )
    return os.path.join(output_directory, "gen-py")


def download_and_extract_zip_file(url: str, output_directory: str) -> None:
    parsed_url = urllib.parse.urlparse(url)
    if parsed_url.netloc != "pypi.facebook.com":
        raise ValueError(
            "Cannot download file `{}`. "
            "It comes from an untrusted location.".format(url)
        )
    with urllib.request.urlopen(url) as response:
        buffer = io.BytesIO(response.read())
        with zipfile.ZipFile(buffer) as zip_file:
            zip_file.extractall(output_directory)
