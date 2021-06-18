# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging
import sys
from pathlib import Path

from ..api.connection import PyreConnection
from ..client.find_directories import find_global_and_local_root, FoundRoot

LOG: logging.Logger = logging.getLogger(__name__)


def _create_pyre_connection(
    roots: FoundRoot,
) -> PyreConnection:

    if roots.local_root is not None:
        return PyreConnection(roots.local_root)
    return PyreConnection(roots.global_root)


def main() -> None:
    logging.basicConfig(
        format="[%(asctime)s][%(levelname)s]: %(message)s", level=logging.INFO
    )

    roots = find_global_and_local_root(Path("."))
    if roots is None:
        LOG.error(f"Failed to find global Pyre configuration for {Path().absolute()}.")
        sys.exit(1)

    try:
        with _create_pyre_connection(roots) as pyre_connection:
            LOG.info(
                f"Server is up: {pyre_connection.server_initialized} at "
                f"{pyre_connection.pyre_directory}"
            )
    except Exception as exception:
        LOG.error(f"Pyre server raised an exception: {exception}")


if __name__ == "__main__":
    main()
