# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
This module provides the CLI we use for pulling new upstream typesheds
and vendoring them, which includes some logic both for cleaning and patching.
"""
from __future__ import annotations

import logging

import pathlib
import shutil
import sys

import click

from . import patching, upstream


@click.group()
def pyre_typeshed_patcher() -> None:
    pass


@pyre_typeshed_patcher.command()
@click.option(
    "--url",
    type=str,
    default=upstream.LATEST,
    help=(
        f"The URL to pull from. If {upstream.LATEST}, "
        "we find the most recent upstream version."
    ),
)
@click.option(
    "--destination",
    type=str,
    required=True,
    help="The output directory in which to dump the upsteam typeshed.",
)
def fetch_upstream(
    url: str,
    destination: str,
) -> None:
    """Pulls an upstream typeshed."""
    upstream.fetch_as_directory(
        url=url,
        destination=pathlib.Path(destination),
    )
    print(f"Finished writing upstream typeshed to {destination}")


@pyre_typeshed_patcher.command()
@click.option(
    "--source-root",
    type=str,
    required=True,
    help="Directory of the source (original) typeshed",
)
@click.option(
    "--relative-path",
    type=str,
    required=True,
    help="Relative path of the stub file to patch",
)
@click.option(
    "--patch-specs",
    type=str,
    default=None,
    help="Path to the patch specs toml file",
)
@click.option(
    "--show-diff/--no-show-diff",
    type=bool,
    default=True,
    help="Print diff to stderr?",
)
def patch_one_file(
    source_root: str,
    relative_path: str,
    patch_specs: str,
    show_diff: bool,
) -> None:
    """
    Patch a single file in a source typeshed.

    Write a diff of the patch result to stderr and the patched file to stdout.
    """
    patching.patch_one_file_entrypoint(
        source_root=pathlib.Path(source_root),
        relative_path=pathlib.Path(relative_path),
        patch_specs_toml=pathlib.Path(patch_specs),
        show_diff=show_diff,
    )


@pyre_typeshed_patcher.command()
@click.option(
    "--source-root",
    type=str,
    required=True,
    help="Directory of the source (original) typeshed",
)
@click.option(
    "--patch-specs",
    type=str,
    default=None,
    help="Path to the patch specs toml file",
)
@click.option(
    "--destination-root",
    type=str,
    required=True,
    help="The directory in which to write the patched typeshed stubs",
)
@click.option(
    "--diffs-directory",
    type=str,
    required=True,
    help="Optional directory to drop diffs of patched stub files.",
)
@click.option(
    "--overwrite/--no-overwrite",
    type=bool,
    default=False,
    help="Overwrite the destination if it exists",
)
def patch_typeshed_directory(
    source_root: str,
    patch_specs: str,
    destination_root: str,
    diffs_directory: str | None,
    overwrite: bool,
) -> None:
    def handle_overwrite_directory(directory: pathlib.Path) -> None:
        if directory.exists():
            if overwrite:
                shutil.rmtree(directory)
            else:
                raise RuntimeError(
                    f"Refusing to overwrite existing {directory}. "
                    "Use --overwrite to overwrite a directory, remove any existing file"
                )

    if diffs_directory is not None:
        diffs_directory_path = pathlib.Path(diffs_directory)
        handle_overwrite_directory(diffs_directory_path)
    else:
        diffs_directory_path = None

    destination_root_path = pathlib.Path(destination_root)
    handle_overwrite_directory(destination_root_path)

    return patching.patch_typeshed_directory(
        source_root=pathlib.Path(source_root),
        patch_specs_toml=pathlib.Path(patch_specs),
        destination_root=destination_root_path,
        diffs_directory=diffs_directory_path,
    )


def main() -> None:
    logging.basicConfig(
        format="%(levelname)s %(asctime)s %(name)s: %(message)s",
        level=logging.INFO,
    )
    pyre_typeshed_patcher(sys.argv[1:])


if __name__ == "__main__":
    main()  # pragma: no cover
