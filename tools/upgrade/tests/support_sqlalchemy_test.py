# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from pathlib import Path

from ..commands.support_sqlalchemy import (
    SupportSqlalchemy,
    _dequalify,
    _dequalify_sqlalchemy_type,
)


class SupportSqlalchemyTest(unittest.TestCase):
    def test_get_sqlalchemy_errors(self) -> None:
        errors = {
            "test1.py": [
                {
                    "path": "test1.py",
                    "description": "Missing attribute annotation [4]: Attribute"
                    " `repository` of class `FBRun` has type"
                    " `sqlalchemy.sql.schema.Column[str]`"
                    " but no type is specified.",
                    "code": 4,
                },
                {"path": "test1.py", "description": "something else", "code": 4},
                {
                    "path": "test1.py",
                    "description": "Missing attribute annotation [4]: Attribute"
                    " `repository` of class `FBRun` has type"
                    " `sqlalchemy.sql.schema.Column[str]`"
                    " but no type is specified.",
                    "code": 4,
                },
            ],
            "test2.py": [
                {
                    "path": "test2.py",
                    "description": "Missing attribute annotation [4]: Attribute"
                    " `repository` of class `FBRun` has type"
                    " `sqlalchemy.sql.schema.Column[str]`"
                    " but no type is specified.",
                    "code": 4,
                }
            ],
            "test3.py": [
                {"path": "test3.py", "description": "something else", "code": 4}
            ],
        }
        self.assertEqual(
            SupportSqlalchemy._get_sqlalchemy_errors(errors, filter_paths=None),
            {
                Path("test1.py"): [
                    {
                        "path": "test1.py",
                        "description": "Missing attribute annotation [4]: Attribute"
                        " `repository` of class `FBRun` has type"
                        " `sqlalchemy.sql.schema.Column[str]`"
                        " but no type is specified.",
                        "code": 4,
                    },
                    {
                        "path": "test1.py",
                        "description": "Missing attribute annotation [4]: Attribute"
                        " `repository` of class `FBRun` has type"
                        " `sqlalchemy.sql.schema.Column[str]`"
                        " but no type is specified.",
                        "code": 4,
                    },
                ],
                Path("test2.py"): [
                    {
                        "path": "test2.py",
                        "description": "Missing attribute annotation [4]: Attribute"
                        " `repository` of class `FBRun` has type"
                        " `sqlalchemy.sql.schema.Column[str]`"
                        " but no type is specified.",
                        "code": 4,
                    }
                ],
            },
        )

        self.assertEqual(
            SupportSqlalchemy._get_sqlalchemy_errors(
                errors, filter_paths=[Path("test1.py")]
            ),
            {
                Path("test1.py"): [
                    {
                        "path": "test1.py",
                        "description": "Missing attribute annotation [4]: Attribute"
                        " `repository` of class `FBRun` has type"
                        " `sqlalchemy.sql.schema.Column[str]`"
                        " but no type is specified.",
                        "code": 4,
                    },
                    {
                        "path": "test1.py",
                        "description": "Missing attribute annotation [4]: Attribute"
                        " `repository` of class `FBRun` has type"
                        " `sqlalchemy.sql.schema.Column[str]`"
                        " but no type is specified.",
                        "code": 4,
                    },
                ]
            },
        )

    def test_dequalify_sqlalchemy_type(self) -> None:
        self.assertEqual(
            _dequalify_sqlalchemy_type(
                {
                    "path": "test1.py",
                    "description": "something",
                    "code": 4,
                    "inference": {
                        "annotation": (
                            "sqlalchemy.sql.schema.Column[typing.Optional[int]]"
                        )
                    },
                }
            ),
            {
                "path": "test1.py",
                "description": "something",
                "code": 4,
                "inference": {"annotation": "Column[typing.Optional[int]]"},
            },
        )

    def test_dequalify(self) -> None:
        self.assertEqual(_dequalify("int"), "int")
        self.assertEqual(_dequalify("sql.Column[int]"), "Column[int]")
        self.assertEqual(_dequalify("sqlalchemy.sql.schema.Column[int]"), "Column[int]")
        self.assertEqual(
            _dequalify("sqlalchemy.sql.schema.Column[typing.Optional[int]]"),
            "Column[typing.Optional[int]]",
        )
