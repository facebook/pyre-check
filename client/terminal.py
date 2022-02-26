# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import os
import sys
from typing import TextIO


def is_capable(file: TextIO = sys.stderr) -> bool:
    """
    Determine whether we are connected to a capable terminal.
    """
    if not os.isatty(file.fileno()):
        return False
    terminal = os.getenv("TERM", "dumb")
    # Hardcoded list of non-capable terminals.
    return terminal not in ["dumb", "emacs"]
