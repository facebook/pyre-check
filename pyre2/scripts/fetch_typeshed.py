#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
Tools for pulling an upstream typeshed archive from github, cleaning out
irrelevant data, and producing artifacts that can be bundled into Pyre2 executable.
"""

import argparse
import dataclasses
import datetime
import hashlib
import io
import json
import logging
import pathlib
import shutil
import tarfile
import urllib.request

LOG: logging.Logger = logging.getLogger(__name__)


@dataclasses.dataclass(frozen=True)
class TypeshedEntry:
    path: pathlib.Path
    data: bytes


@dataclasses.dataclass(frozen=True)
class FetchMetadata:
    url: str
    sha256: str
    timestamp: float


def get_default_typeshed_url() -> str:
    commit_hash = json.loads(
        urllib.request.urlopen(
            "https://api.github.com/repos/python/typeshed/commits/main"
        )
        .read()
        .decode("utf-8")
    )["sha"]
    LOG.info(f"Found typeshed main at commit {commit_hash}")
    return f"https://api.github.com/repos/python/typeshed/tarball/{commit_hash}"


def get_typeshed_url(specified_url: str | None) -> str:
    if specified_url is not None:
        return specified_url
    LOG.info("Typeshed URL not specified. Trying to auto-determine it...")
    try:
        return get_default_typeshed_url()
    except Exception as e:
        raise RuntimeError(
            "Cannot determine the default typeshed URL. "
            + "Please manually specify one with `--url` argument. "
            + "If the download still fails, please check network connectivity."
        ) from e


def fetch_as_tarfile(
    url: str,
) -> tuple[tarfile.TarFile, FetchMetadata]:
    LOG.info(f"Fetch typeshed tarball from URL `{url}`...")
    data = urllib.request.urlopen(url).read()
    metadata = FetchMetadata(
        url=url,
        sha256=hashlib.sha256(data).hexdigest(),
        timestamp=datetime.datetime.now().timestamp(),
    )
    return (
        tarfile.open(
            # Buffer the url data stream in memory so seeking in tarfile is possible
            fileobj=io.BytesIO(data),
            mode="r:gz",
        ),
        metadata,
    )


def should_include_member(info: tarfile.TarInfo) -> bool:
    if info.isdir():
        return False

    path = pathlib.Path(info.name)
    parts = path.parts

    # There are quite a few config files and some test directories in
    # typeshed that we don't need - only pull in the directories that
    # actually contain stubs.
    if len(parts) < 2:
        return False

    # The typeshed repository contains two stub roots, which are different:
    # - The "stdlib" directory contains stubs for the standard library, and
    #   for stdlib typeshed is the correct way to get them.
    # - The "stubs" directory contains stubs for third-party libraries, which
    #   are mirrorored into various `xyz-stubs` packages on PyPI.
    # We only want to vendor the stdlib stubs.
    if parts[1] != "stdlib":
        return False

    # Only Python source files are interesting to us
    if path.suffix not in (".py", ".pyi"):
        return False

    # Skip Python 2-only stubs; Pyre no longer supports Python 2.
    if "@python2" in parts:
        return False

    # Skip stub tests
    if "@tests" in parts:
        return False

    return True


def relative_path_for_stdlib(info: tarfile.TarInfo) -> pathlib.Path:
    """
    Convert a filename within a typeshed tarball into a path relative to the
    top of typeshed/stdlib.
    """
    return pathlib.Path(*pathlib.Path(info.name).parts[2:])


def trim_typeshed(input_tar: tarfile.TarFile) -> list[TypeshedEntry]:
    LOG.info("Trimming down typeshed tarfile...")
    entries = (
        (
            relative_path_for_stdlib(member),
            input_tar.extractfile(member),
        )
        for member in input_tar.getmembers()
        if should_include_member(member)
    )
    return [
        TypeshedEntry(path, reader.read())
        for path, reader in entries
        if reader is not None
    ]


def write_typeshed(output_path: pathlib.Path, entries: list[TypeshedEntry]) -> None:
    LOG.info("Clearing output directory...")
    shutil.rmtree(output_path, ignore_errors=True)
    LOG.info("Writing trimmed typeshed to disk...")
    for entry in entries:
        file_path = output_path / entry.path
        file_path.parent.mkdir(parents=True, exist_ok=True)
        file_path.write_bytes(entry.data)


def write_metadata(output_path: pathlib.Path, metadata: FetchMetadata) -> None:
    output_path.write_text(
        json.dumps(
            {
                "url": metadata.url,
                "sha256": metadata.sha256,
                "update_time": datetime.datetime.fromtimestamp(
                    metadata.timestamp
                ).isoformat(sep=" ", timespec="seconds"),
            },
            indent=2,
        )
    )


def run(specified_url: str | None, output_dir: pathlib.Path) -> None:
    if not output_dir.exists():
        raise RuntimeError(f"Output path `{output_dir}` does not exist")
    if not output_dir.is_dir():
        raise RuntimeError(f"Output path `{output_dir}` is not a directory")
    url = get_typeshed_url(specified_url)
    typeshed_tarfile, metadata = fetch_as_tarfile(url)
    entries = trim_typeshed(typeshed_tarfile)
    write_typeshed(output_dir / "typeshed", entries)
    write_metadata(output_dir / "typeshed_metadata.json", metadata)


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Fetch typeshed from a given URL and trim out files that are not needed for Pyre2 bundling."
    )
    parser.add_argument(
        "-u",
        "--url",
        help="The URL to download from. If not specified, the trunk files on the typeshed main branch will be used.",
    )
    parser.add_argument(
        "-o",
        "--output",
        type=pathlib.Path,
        required=True,
        help="The directory to write the downloaded typeshed to.",
    )
    args = parser.parse_args()
    # @lint-ignore FIXIT1: OSS scripts cannot take on fb-internal dependency
    logging.basicConfig(
        format="%(levelname)s %(asctime)s: %(message)s",
        level=logging.INFO,
    )
    run(args.url, args.output)


if __name__ == "__main__":
    main()
