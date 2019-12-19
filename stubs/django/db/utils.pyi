# pyre-unsafe

from typing import Iterable

from django.db.backends.base.base import BaseDatabaseWrapper

class ConnectionHandler:
    def __init__(self, databases=None): ...
    def __getitem__(self, alias) -> BaseDatabaseWrapper: ...
    def close_all(self): ...
    def all(self) -> Iterable[BaseDatabaseWrapper]: ...

class Error(Exception):
    pass

class DatabaseError(Error):
    pass

class IntegrityError(DatabaseError):
    pass
