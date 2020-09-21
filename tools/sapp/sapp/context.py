# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

from typing import List, NamedTuple, Optional, Type

import click

from .db import DB
from .pipeline.base_parser import BaseParser


class Context(NamedTuple):
    database: DB
    parser_class: Type[BaseParser]
    repository: Optional[str]
    ipython_extensions: List[str] = []


# pyre-fixme[5]: Global expression must be annotated.
pass_context = click.make_pass_decorator(Context, ensure=True)
