# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import hashlib
from pathlib import Path


def get_socket_path(root: Path, log_directory: Path) -> Path:
    """
    Determine where the server socket file is located. We can't directly use
    `log_directory` because of the ~100 character length limit on Unix socket
    file paths.

    Implementation needs to be kept in sync with the `socket_path_of` function
    in `pyre/new_server/start.ml`.
    """
    log_path_digest = hashlib.md5(
        str(log_directory.resolve(strict=False)).encode("utf-8")
    ).hexdigest()
    return root / f"pyre_server_{log_path_digest}.sock"
