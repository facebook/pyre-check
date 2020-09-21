# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import json
import textwrap
import unittest
from typing import Dict, List, Optional
from unittest.mock import call, patch

from .. import UserError, errors
from ..ast import UnstableAST
from ..errors import (
    Errors,
    PartialErrorSuppression,
    SkippingGeneratedFileException,
    _suppress_errors,
)


unittest.util._MAX_LENGTH = 200


class ErrorsTest(unittest.TestCase):
    def test_from_json(self) -> None:
        self.assertEqual(
            Errors.from_json('[{ "path": "test.py", "key": "value" }]'),
            Errors([{"path": "test.py", "key": "value"}]),
        )
        with patch(
            "sys.stdin.read", return_value='[{ "path": "test.py", "key": "value" }]'
        ):
            self.assertEqual(
                Errors.from_stdin(), Errors([{"path": "test.py", "key": "value"}])
            )

        self.assertEqual(
            Errors.from_json(
                json.dumps(
                    [
                        {"path": "test.py", "key": "value", "code": 1},
                        {"path": "test.py", "key": "value", "code": 2},
                    ]
                ),
                only_fix_error_code=1,
            ),
            Errors([{"path": "test.py", "key": "value", "code": 1}]),
        )
        with patch(
            "sys.stdin.read",
            return_value=json.dumps(
                [
                    {"path": "test.py", "key": "value", "code": 1},
                    {"path": "test.py", "key": "value", "code": 2},
                ]
            ),
        ):
            self.assertEqual(
                Errors.from_stdin(only_fix_error_code=1),
                Errors([{"path": "test.py", "key": "value", "code": 1}]),
            )

        with self.assertRaises(UserError):
            Errors.from_json('[{ "path": "test.py", "key": "value" }')

    @patch.object(errors.Path, "read_text", return_value="")
    @patch.object(errors.Path, "write_text")
    def test_suppress(self, path_write_text, path_read_text) -> None:
        # Test run on multiple files.
        with patch(f"{errors.__name__}._suppress_errors", return_value="<transformed>"):
            Errors(
                [
                    {
                        "path": "path.py",
                        "line": 1,
                        "concise_description": "Error [1]: description",
                    },
                    {
                        "path": "other.py",
                        "line": 2,
                        "concise_description": "Error [2]: description",
                    },
                ]
            ).suppress()
            path_read_text.assert_has_calls([call(), call()])
            path_write_text.assert_has_calls(
                [call("<transformed>"), call("<transformed>")]
            )

        with patch(f"{errors.__name__}._suppress_errors", side_effect=UnstableAST()):
            with self.assertRaises(PartialErrorSuppression) as context:
                Errors(
                    [
                        {
                            "path": "path.py",
                            "line": 1,
                            "concise_description": "Error [1]: description",
                        },
                        {
                            "path": "other.py",
                            "line": 2,
                            "concise_description": "Error [2]: description",
                        },
                    ]
                ).suppress()
            self.assertEqual(
                set(context.exception.unsuppressed_paths), {"path.py", "other.py"}
            )

    def assertSuppressErrors(
        self,
        errors: Dict[int, List[Dict[str, str]]],
        input: str,
        expected_output: str,
        *,
        custom_comment: Optional[str] = None,
        max_line_length: Optional[int] = None,
        truncate: bool = False,
        unsafe: bool = False,
    ) -> None:
        def _normalize(input: str) -> str:
            return textwrap.dedent(input).strip().replace("FIXME", "pyre-fixme")

        self.assertEqual(
            _suppress_errors(
                _normalize(input),
                errors,
                custom_comment,
                max_line_length,
                truncate,
                unsafe,
            ),
            _normalize(expected_output),
        )

    def test_suppress_errors(self) -> None:
        self.assertSuppressErrors(
            {},
            """
            def foo() -> None: pass
            """,
            """
            def foo() -> None: pass
            """,
        )

        # Basic error suppression
        self.assertSuppressErrors(
            {1: [{"code": "1", "description": "description"}]},
            """
            def foo() -> None: pass
            """,
            """
            # FIXME[1]: description
            def foo() -> None: pass
            """,
        )

        # Indentation is correct.
        self.assertSuppressErrors(
            {2: [{"code": "1", "description": "description"}]},
            """
            def foo() -> None:
                pass
            """,
            """
            def foo() -> None:
                # FIXME[1]: description
                pass
            """,
        )

        # We skip generated files.
        with self.assertRaises(SkippingGeneratedFileException):
            _suppress_errors("@" "generated", {})

        # Custom message.
        self.assertSuppressErrors(
            {1: [{"code": "1", "description": "description"}]},
            """
            def foo() -> None: pass
            """,
            """
            # FIXME[1]: T1234
            def foo() -> None: pass
            """,
            custom_comment="T1234",
        )

        # Existing Comment
        self.assertSuppressErrors(
            {2: [{"code": "1", "description": "description"}]},
            """
            # comment
            def foo() -> None: pass
            """,
            """
            # comment
            # FIXME[1]: description
            def foo() -> None: pass
            """,
        )

        self.assertSuppressErrors(
            {1: [{"code": "0", "description": "description"}]},
            """
            def foo() -> None: # FIXME[1]
                # comment
                pass
            """,
            """
            def foo() -> None:
                # comment
                pass
            """,
        )

        # Multiple Errors
        self.assertSuppressErrors(
            {
                1: [{"code": "1", "description": "description"}],
                2: [{"code": "2", "description": "description"}],
            },
            """
            def foo() -> None:
                pass
            """,
            """
            # FIXME[1]: description
            def foo() -> None:
                # FIXME[2]: description
                pass
            """,
        )

        self.assertSuppressErrors(
            {
                1: [
                    {"code": "1", "description": "description"},
                    {"code": "2", "description": "description"},
                ]
            },
            """
            def foo() -> None: pass
            """,
            """
            # FIXME[1]: description
            # FIXME[2]: description
            def foo() -> None: pass
            """,
        )

        # Line length limit
        self.assertSuppressErrors(
            {1: [{"code": "1", "description": "description"}]},
            """
            def foo() -> None: pass
            """,
            """
            # FIXME[1]:
            #  description
            def foo() -> None: pass
            """,
            max_line_length=20,
        )

        # Remove unused ignores.
        self.assertSuppressErrors(
            {1: [{"code": "0", "description": "description"}]},
            """
            # FIXME[0]: ignore
            def foo() -> None: pass
            """,
            """
            def foo() -> None: pass
            """,
        )
        self.assertSuppressErrors(
            {1: [{"code": "0", "description": "description"}]},
            """
            # FIXME[0]: ignore
            #  over multple lines
            def foo() -> None: pass
            """,
            """
            def foo() -> None: pass
            """,
        )
        self.assertSuppressErrors(
            {1: [{"code": "0", "description": "description"}]},
            """
            # FIXME[0]: ignore
            #  over multple lines
            # FIXME[1]: description
            def foo() -> None: pass
            """,
            """
            # FIXME[1]: description
            def foo() -> None: pass
            """,
        )
        self.assertSuppressErrors(
            {1: [{"code": "0", "description": "description"}]},
            """
            def foo() -> None: pass  # FIXME[0]: ignore
            """,
            """
            def foo() -> None: pass
            """,
        )
        self.assertSuppressErrors(
            {
                1: [{"code": "0", "description": "description"}],
                2: [{"code": "0", "description": "description"}],
            },
            """
            # FIXME[1]: ignore
            # FIXME[2]: ignore
            def foo() -> None: pass
            """,
            """
            def foo() -> None: pass
            """,
        )
        self.assertSuppressErrors(
            {
                1: [
                    {"code": "0", "description": "description"},
                    {"code": "2", "description": "new error"},
                ]
            },
            """
            def foo() -> None: pass  # FIXME[1]
            """,
            """
            # FIXME[2]: new error
            def foo() -> None: pass
            """,
        )
        self.assertSuppressErrors(
            {
                1: [
                    {"code": "2", "description": "new error"},
                    {"code": "0", "description": "description"},
                ]
            },
            """
            def foo() -> None: pass  # FIXME[1]
            """,
            """
            # FIXME[2]: new error
            def foo() -> None: pass
            """,
        )

        # Truncate long comments.
        self.assertSuppressErrors(
            {1: [{"code": "1", "description": "description"}]},
            """
            def foo() -> None: pass
            """,
            """
            # FIXME[1]: descr...
            def foo() -> None: pass
            """,
            max_line_length=25,
            truncate=True,
        )

        self.assertSuppressErrors(
            {
                1: [
                    {
                        "code": "1",
                        "description": "this description takes up over four lines \
                        of content when it is split, given the max line length",
                    }
                ]
            },
            """
            def foo() -> None: pass
            """,
            """
            # FIXME[1]: this ...
            def foo() -> None: pass
            """,
            max_line_length=25,
        )

        # Line breaks.
        self.assertSuppressErrors(
            {
                3: [{"code": "1", "description": "description"}],
                4: [{"code": "2", "description": "description"}],
            },
            """
            def foo() -> None:
                x = something + \\
                        error() + \\
                        error()  # unrelated comment
            """,
            """
            def foo() -> None:
                x = (something +
                        # FIXME[1]: description
                        error() +
                        # FIXME[2]: description
                        error())  # unrelated comment
            """,
        )

        self.assertSuppressErrors(
            {3: [{"code": "1", "description": "description"}]},
            """
            def foo() -> None:
                x, y, z = \\
                    error()
            """,
            """
            def foo() -> None:
                x, y, z = (
                    # FIXME[1]: description
                    error())
            """,
        )

        self.assertSuppressErrors(
            {3: [{"code": "1", "description": "description"}]},
            """
            def foo() -> None:
                del \\
                    error
            """,
            """
            def foo() -> None:
                del (
                    # FIXME[1]: description
                    error)
            """,
        )

        self.assertSuppressErrors(
            {3: [{"code": "1", "description": "description"}]},
            """
            def foo() -> None:
                assert \\
                    test
            """,
            """
            def foo() -> None:
                assert (
                    # FIXME[1]: description
                    test)
            """,
        )

        self.assertSuppressErrors(
            {3: [{"code": "1", "description": "description"}]},
            """
            def foo() -> None:
                assert test + \\
                    test2
            """,
            """
            def foo() -> None:
                assert (test +
                    # FIXME[1]: description
                    test2)
            """,
        )

        self.assertSuppressErrors(
            {3: [{"code": "1", "description": "description"}]},
            """
            def foo() -> None:
                raise \\
                    Exception()
            """,
            """
            def foo() -> None:
                raise (
                    # FIXME[1]: description
                    Exception())
            """,
        )

        self.assertSuppressErrors(
            {3: [{"code": "1", "description": "description"}]},
            """
            def foo() -> None:
                return a + \\
                    error
            """,
            """
            def foo() -> None:
                return (a +
                    # FIXME[1]: description
                    error)
            """,
        )

        self.assertSuppressErrors(
            {3: [{"code": "1", "description": "description"}]},
            """
            def foo() -> None:
                return \\
                    error
            """,
            """
            def foo() -> None:
                return (
                    # FIXME[1]: description
                    error)
            """,
        )

    def test_suppress_errors__long_class_name(self) -> None:
        self.assertSuppressErrors(
            {
                1: [
                    {
                        "code": "1",
                        "description": "This is a \
                        really.long.class.name.exceeding.twenty.five.Characters",
                    }
                ]
            },
            """
            def foo() -> None: pass
            """,
            """
            # FIXME[1]: This ...
            def foo() -> None: pass
            """,
            max_line_length=25,
        )
