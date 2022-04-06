# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from pathlib import Path

from ..commands.support_sqlalchemy import SupportSqlalchemy


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
