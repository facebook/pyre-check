# pyre-strict

from typing import NamedTuple, Optional

import click
from sapp.db import DB


class Context(NamedTuple):
    repository: Optional[str]
    database: DB


pass_context = click.make_pass_decorator(Context, ensure=True)
