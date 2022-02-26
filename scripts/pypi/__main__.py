# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
from pathlib import Path

from pypi.build_pypi_package import build_pypi_package


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Build a PyPi Package.")
    parser.add_argument("--typeshed-path", type=Path, required=True)
    parser.add_argument("--version", type=str, required=True)
    parser.add_argument("--nightly", action="store_true")

    arguments = parser.parse_args()
    build_pypi_package(
        pyre_directory=Path(__file__).resolve().parent.parent.parent,
        typeshed_path=arguments.typeshed_path,
        version=arguments.version,
        nightly=arguments.nightly,
    )
