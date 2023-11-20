# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
This module provides the CLI we use for pulling new upstream typesheds
and vendoring them, which includes some logic both for cleaning and patching.
"""
from __future__ import annotations

import pathlib
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
    "--target",
    type=str,
    required=True,
    help="The output directory in which to dump the upsteam typeshed.",
)
def fetch_upstream(
    url: str,
    target: str,
) -> None:
    """Pulls an upstream typeshed."""
    upstream.fetch_as_directory(
        url=url,
        target=pathlib.Path(target),
    )
    print(f"Finished writing upstream typeshed to {target}")


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
    "--source",
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
    "--target",
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
    help="Overwrite the target if it exists",
)
def patch_typeshed_directory(
    source: str,
    patch_specs: str,
    target: str,
    diffs_directory: str | None,
    overwrite: bool,
) -> None:
    return patching.patch_typeshed_directory(
        source=pathlib.Path(source),
        patch_specs_toml=pathlib.Path(patch_specs),
        target=pathlib.Path(target),
        diffs_directory=pathlib.Path(diffs_directory) if diffs_directory else None,
        overwrite=overwrite,
    )


if __name__ == "__main__":
    pyre_typeshed_patcher(sys.argv[1:])
