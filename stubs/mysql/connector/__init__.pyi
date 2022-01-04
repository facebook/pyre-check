# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import Any

from mysql.connector.abstracts import MySQLConnectionAbstract as MySQLConnectionAbstract
from mysql.connector.connection import MySQLConnection as MySQLConnection
from mysql.connector.connection_cext import CMySQLConnection as CMySQLConnection

HAVE_CEXT: bool

def connect(*args: Any, **kwargs: Any) -> MySQLConnectionAbstract: ...

Connect = connect
