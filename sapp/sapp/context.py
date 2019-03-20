# pyre-strict

from typing import NamedTuple, Optional, Type

import click
from sapp.base_parser import BaseParser
from sapp.db import DB


class Context(NamedTuple):
    database: DB
    parser_class: Type[BaseParser]
    repository: Optional[str]


# pyre-fixme[5]: Global expression must be annotated.
pass_context = click.make_pass_decorator(Context, ensure=True)
