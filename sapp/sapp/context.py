# pyre-strict

from typing import NamedTuple, Optional

import click
from sapp.db import DBType


class Context(NamedTuple):
    repository: Optional[str]
    database_name: str
    database_engine: DBType


pass_context = click.make_pass_decorator(Context, ensure=True)
