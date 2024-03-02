# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import dataclasses
import json
import pathlib
import textwrap
from typing import Mapping

from . import typeshed


@dataclasses.dataclass(frozen=True)
class GlobSource:
    def to_string(self) -> str:
        return 'glob(["**/*"])'


@dataclasses.dataclass(frozen=True)
class MappedSource:
    mapping: Mapping[pathlib.Path, pathlib.Path] = dataclasses.field(
        default_factory=dict
    )

    def to_string(self) -> str:
        return json.dumps({str(k): str(v) for k, v in self.mapping.items()})


@dataclasses.dataclass(frozen=True)
class BuckFileGroup:
    name: str
    sources: GlobSource | MappedSource

    def to_string(self) -> str:
        return textwrap.dedent(
            f"""
            buck_filegroup(
                name = "{self.name}",
                srcs = {self.sources.to_string()},
            )
            """  # noqa: B907
        )


@dataclasses.dataclass(frozen=True)
class BuckFileContent:
    original_filegroup: BuckFileGroup
    flattened_filegroup: BuckFileGroup

    def to_string(self) -> str:
        preamble = textwrap.dedent(
            """
            # This file is generated. Do not edit manually.
            # See https://www.internalfb.com/intern/staticdocs/pyre/docs/fb/typeshed/
            load("@fbcode_macros//build_defs:native_rules.bzl", "buck_filegroup")

            oncall("pyre")
            """
        )
        return "\n".join(
            [
                preamble,
                self.original_filegroup.to_string(),
                self.flattened_filegroup.to_string(),
            ]
        )


def _strip_toplevel(path: pathlib.Path) -> pathlib.Path:
    parts = path.parts
    if len(parts) <= 1:
        raise RuntimeError(
            f"Unable to strip top-level directory from `{path}`. "
            "Please double-check if the typeshed directory structure is correct. "
            "It is expected that `stdlib/` and `stubs/` directories are presented under "
            "Typeshed root."
        )
    else:
        return pathlib.Path(*parts[1:])


def generate_mapped_source(typeshed: typeshed.Typeshed) -> MappedSource:
    return MappedSource(
        mapping={_strip_toplevel(path): path for path in typeshed.all_files()}
    )


def generate_buck_file(typeshed: typeshed.Typeshed) -> BuckFileContent:
    return BuckFileContent(
        original_filegroup=BuckFileGroup(name="original", sources=GlobSource()),
        flattened_filegroup=BuckFileGroup(
            name="flattened", sources=generate_mapped_source(typeshed)
        ),
    )


def write_buck_file_to_directory(
    content: BuckFileContent, destination: pathlib.Path, filename: str = "TARGETS"
) -> None:
    """
    Write the given Buck file into a directory rooted at `destination` on the filesystem.

    The `destination` directory is assumed to exist before this function gets invoked.
    """
    (destination / filename).write_text(content.to_string())
