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

from . import upstream


@click.group()
def pyre_typeshed_patcher() -> None:
    pass


@pyre_typeshed_patcher.command()
@click.option(
    "--url",
    type=str,
    default=upstream.LATEST,
    help=(
        "The URL to pull from. If {upstream.LATEST}, "
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


if __name__ == "__main__":
    pyre_typeshed_patcher(sys.argv[1:])
