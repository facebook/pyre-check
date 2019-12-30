# pyre-unsafe

from typing import Any

from django.db.backends.base.base import BaseDatabaseWrapper
from django.db.utils import ConnectionHandler

def close_old_connections(**kwargs: Any) -> None: ...
def reset_queries(**kwargs: Any) -> None: ...

transaction: Any

connections: ConnectionHandler

connection: BaseDatabaseWrapper

class OperationalError(Exception): ...
