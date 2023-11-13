# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
Tools for pulling an upstream typeshed zip archive from github, cleaning out
irrelevant data, and producing either a typeshed.Typeshed object or a
directory.
"""
from __future__ import annotations

import io

import json
import logging
import pathlib
import shutil
import urllib.request
import zipfile

from . import typeshed


LOG: logging.Logger = logging.getLogger(__name__)
LATEST: str = "LATEST"


def get_default_typeshed_url() -> str:
    commit_hash = json.loads(
        urllib.request.urlopen(
            "https://api.github.com/repos/python/typeshed/commits/main"
        )
        .read()
        .decode("utf-8")
    )["sha"]
    LOG.info(f"Found typeshed main at commit {commit_hash}")
    return f"https://api.github.com/repos/python/typeshed/zipball/{commit_hash}"


def get_typeshed_url(specified_url: str) -> str:
    if specified_url != LATEST:
        return specified_url
    LOG.info("Typeshed URL not specified. Trying to auto-determine it...")
    default_url = get_default_typeshed_url()
    if default_url is None:
        raise RuntimeError(
            "Cannot determine the default typeshed URL. "
            + "Please manually specify one with `--url` argument. "
            + "If the download still fails, please check network connectivity."
        )
    return default_url


def fetch_as_zipped_bytes(
    url: str,
) -> zipfile.ZipFile:
    raw_bytes = io.BytesIO()
    with urllib.request.urlopen(url) as response:
        shutil.copyfileobj(response, raw_bytes)
    return zipfile.ZipFile(raw_bytes)


def should_include_zipinfo(info: zipfile.ZipInfo) -> bool:
    if info.is_dir():
        return False

    parts = pathlib.Path(info.filename).parts

    # There are quite a few config files and some test directories in
    # typeshed that we don't need - only pull in the directories that
    # actually contain stubs.
    if len(parts) < 2 or parts[1] not in ("stubs", "stdlib"):
        return False

    # Bypass txt and toml files - these files just make our vendored directory
    # bigger for no benefit, and some of them violate lint rules (e.g. no trailing
    # newline).
    if parts[-1].endswith(".txt") or parts[-1].endswith(".toml"):
        return False

    # Skip Python 2-only stubs; Pyre no longer supports Python 2.
    if "@python2" in parts:
        return False

    return True


def relative_path_for_zipinfo(info: zipfile.ZipInfo) -> pathlib.Path:
    """
    Convert a filename within a zipped typeshed into a path relative to the
    top of the typeshed repository.
    """
    return pathlib.Path(*pathlib.Path(info.filename).parts[1:])


def fetch_as_typeshed(
    url: str,
) -> typeshed.Typeshed:
    url = get_typeshed_url(url)
    with fetch_as_zipped_bytes(url) as zipped_bytes:
        contents = {
            relative_path_for_zipinfo(info): zipped_bytes.read(info).decode("utf-8")
            for info in zipped_bytes.infolist()
            if should_include_zipinfo(info)
        }
        contents[pathlib.Path("source_url")] = f"{url}\n"
        return typeshed.MemoryBackedTypeshed(contents)


def fetch_as_directory(
    url: str,
    target: pathlib.Path,
) -> None:
    upstream_typeshed = fetch_as_typeshed(url)
    typeshed.write_to_directory(
        typeshed=upstream_typeshed,
        target=target,
    )
