# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

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
