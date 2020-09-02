# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

from django.db.backends.base.base import BaseDatabaseWrapper

class DatabaseWrapper(BaseDatabaseWrapper):
    pass

class CursorWrapper:
    def __init__(self, cursor): ...
    def execute(self, query, args=None): ...
    def executemany(self, query, args): ...
