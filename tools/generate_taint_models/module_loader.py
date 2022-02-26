# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import ast
import glob
import logging
import os
from typing import Iterator, Optional


LOG: logging.Logger = logging.getLogger(__name__)


def load_module(module_path: str) -> Optional[ast.Module]:
    try:
        with open(module_path, "r") as file:
            parsed = ast.parse(file.read())
            if not isinstance(parsed, ast.Module):
                return None
            return parsed
    except (FileNotFoundError, SyntaxError) as error:
        LOG.warning(f"Could not load `{module_path}`: {str(error)}")
    return None


def find_all_paths(root: str) -> Iterator[str]:
    for path in glob.glob(root + "/**/*.py", recursive=True):
        # Stubs take precedence if both module.py and module.pyi exist.
        stub_path = f"{path}i"
        if os.path.exists(stub_path):
            path = stub_path

        yield path
