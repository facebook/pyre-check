# pyre-unsafe

from django.db.backends.base.base import BaseDatabaseWrapper

class DatabaseWrapper(BaseDatabaseWrapper):
    pass

class CursorWrapper:
    def __init__(self, cursor): ...
    def execute(self, query, args=None): ...
    def executemany(self, query, args): ...
