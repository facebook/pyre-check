# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import json
import textwrap
import unittest
from typing import Dict, List, Optional, Tuple
from unittest.mock import call, patch

from .. import UserError, errors
from ..ast import UnstableAST
from ..errors import (
    Errors,
    PartialErrorSuppression,
    SkippingGeneratedFileException,
    SkippingUnparseableFileException,
    _map_line_to_start_of_range,
    _get_unused_ignore_codes,
    _line_ranges_spanned_by_format_strings,
    _remove_unused_ignores,
    _relocate_errors,
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

    def test_paths_to_errors(self) -> None:
        errors = Errors(
            [
                {"path": "test1.py", "key": "value", "code": 1},
                {"path": "test2.py", "key": "value", "code": 2},
                {"path": "test1.py", "key": "value", "code": 3},
            ]
        )
        self.assertEqual(
            errors.paths_to_errors,
            {
                "test1.py": [
                    {"code": 1, "key": "value", "path": "test1.py"},
                    {"code": 3, "key": "value", "path": "test1.py"},
                ],
                "test2.py": [{"code": 2, "key": "value", "path": "test2.py"}],
            },
        )

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

    def test_get_unused_ignore_codes(self) -> None:
        self.assertEqual(
            _get_unused_ignore_codes(
                [
                    {
                        "code": "0",
                        "description": "The `pyre-ignore[1, 9]` or `pyre-fixme[1, 9]` "
                        + "comment is not suppressing type errors, please remove it.",
                    }
                ]
            ),
            [1, 9],
        )
        self.assertEqual(
            _get_unused_ignore_codes(
                [
                    {
                        "code": "0",
                        "description": "The `pyre-ignore[1, 9]` or `pyre-fixme[1, 9]` "
                        + "comment is not suppressing type errors, please remove it.",
                    },
                    {
                        "code": "0",
                        "description": "The `pyre-ignore[2]` or `pyre-fixme[2]` "
                        + "comment is not suppressing type errors, please remove it.",
                    },
                ]
            ),
            [1, 2, 9],
        )
        self.assertEqual(
            _get_unused_ignore_codes(
                [
                    {
                        "code": "1",
                        "description": "The `pyre-ignore[1, 9]` or `pyre-fixme[1, 9]` "
                        + "comment is not suppressing type errors, please remove it.",
                    }
                ]
            ),
            [],
        )

    @patch.object(errors, "_get_unused_ignore_codes")
    def test_remove_unused_ignores(self, get_unused_ignore_codes) -> None:
        get_unused_ignore_codes.return_value = [1, 3, 4]
        self.assertEqual(
            _remove_unused_ignores("# pyre-fixme[1, 2, 3, 4]: Comment", []),
            "# pyre-fixme[2]: Comment",
        )

        get_unused_ignore_codes.return_value = [1, 2, 3, 4]
        self.assertEqual(
            _remove_unused_ignores("# pyre-fixme[1, 2, 3, 4]: Comment", []), ""
        )

        get_unused_ignore_codes.return_value = [1]
        self.assertEqual(
            _remove_unused_ignores("# pyre-fixme[2, 3, 4]: Comment", []),
            "# pyre-fixme[2, 3, 4]: Comment",
        )

        get_unused_ignore_codes.return_value = [1, 2]
        self.assertEqual(_remove_unused_ignores("# pyre-fixme: Comment", []), "")

        get_unused_ignore_codes.return_value = [1, 2]
        self.assertEqual(
            _remove_unused_ignores(
                "# Unrelated comment. # pyre-fixme[1, 2]: Comment", []
            ),
            "# Unrelated comment.",
        )

        get_unused_ignore_codes.return_value = [1, 3, 4]
        self.assertEqual(
            _remove_unused_ignores("# pyre-fixme    [1, 2, 3, 4]: Comment", []),
            "# pyre-fixme    [2]: Comment",
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

        # Avoid duplicate error messages
        self.assertSuppressErrors(
            {
                1: [
                    {"code": "1", "description": "description 1"},
                    {"code": "2", "description": "description duplicate"},
                    {"code": "2", "description": "description duplicate"},
                    {"code": "1", "description": "description 2"},
                ]
            },
            """
            def foo() -> None: pass
            """,
            """
            # FIXME[1]: description 1
            # FIXME[2]: description duplicate
            # FIXME[1]: description 2
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

        # Skip files with parse errors.
        with self.assertRaises(SkippingUnparseableFileException):
            _suppress_errors(
                "input", {1: [{"code": "404", "description": "description"}]}
            )

        # Skip generated files.
        with self.assertRaises(SkippingGeneratedFileException):
            _suppress_errors("# @" "generated", {})

        # Do not check for generated files with --unsafe.
        try:
            _suppress_errors(
                "# @" "generated",
                {},
                custom_comment=None,
                max_line_length=None,
                truncate=False,
                unsafe=True,
            )
        except SkippingGeneratedFileException:
            self.fail("Unexpected `SkippingGeneratedFileException` exception.")

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

        # Remove unused ignores by error code.
        self.assertSuppressErrors(
            {
                1: [
                    {
                        "code": "0",
                        "description": "The `pyre-ignore[1]` or `pyre-fixme[1]` "
                        + "comment is not suppressing type errors, please remove it.",
                    }
                ]
            },
            """
            def foo() -> None: pass  # FIXME[1, 2]
            """,
            """
            def foo() -> None: pass  # FIXME[2]
            """,
        )

        self.assertSuppressErrors(
            {
                1: [
                    {
                        "code": "0",
                        "description": "The `pyre-ignore[1, 3]` or `pyre-fixme[1, 3]` "
                        + "comment is not suppressing type errors, please remove it.",
                    }
                ]
            },
            """
            # FIXME[1, 2, 3]
            # Continuation comment.
            def foo() -> None: pass
            """,
            """
            # FIXME[2]
            # Continuation comment.
            def foo() -> None: pass
            """,
        )

        self.assertSuppressErrors(
            {
                1: [
                    {
                        "code": "0",
                        "description": "The `pyre-ignore[1, 3]` or `pyre-fixme[1, 3]` "
                        + "comment is not suppressing type errors, please remove it.",
                    }
                ]
            },
            """
            # FIXME[1, 2, 3]: Comment[Comment]
            def foo() -> None: pass
            """,
            """
            # FIXME[2]: Comment[Comment]
            def foo() -> None: pass
            """,
        )

        self.assertSuppressErrors(
            {
                1: [
                    {
                        "code": "0",
                        "description": "The `pyre-ignore[1, 3]` or `pyre-fixme[1, 3]` "
                        + "comment is not suppressing type errors, please remove it.",
                    }
                ]
            },
            """
            # FIXME[1, 3]
            # Continuation comment.
            def foo() -> None: pass
            """,
            """
            def foo() -> None: pass
            """,
        )

        self.assertSuppressErrors(
            {
                1: [
                    {
                        "code": "0",
                        "description": "The `pyre-ignore[1, 3]` or `pyre-fixme[1, 3]` "
                        + "comment is not suppressing type errors, please remove it.",
                    }
                ],
                2: [{"code": "4", "description": "Description."}],
            },
            """
            # FIXME[1, 2, 3]
            def foo() -> None: pass
            """,
            """
            # FIXME[2]
            # FIXME[4]: Description.
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

        # Line breaks without errors.
        self.assertSuppressErrors(
            {},
            """
            def foo() -> None:
                \"\"\"
                Random line break that won't parse.
                /!\\
                Text.
                \"\"\"
                pass
            """,
            """
            def foo() -> None:
                \"\"\"
                Random line break that won't parse.
                /!\\
                Text.
                \"\"\"
                pass
            """,
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

    def test_suppress_errors__format_string(self) -> None:
        self.assertSuppressErrors(
            {
                4: [
                    {
                        "code": "42",
                        "description": "Some error",
                    }
                ],
                5: [
                    {
                        "code": "42",
                        "description": "Some error",
                    },
                    {
                        "code": "43",
                        "description": "Some error",
                    },
                ],
            },
            """
            def foo() -> None:
                f\"\"\"
                foo
                {1 + "hello"}
                {"world" + int("a")}
                bar
                \"\"\"
            """,
            """
            def foo() -> None:
                # FIXME[42]: Some error
                # FIXME[43]: Some error
                f\"\"\"
                foo
                {1 + "hello"}
                {"world" + int("a")}
                bar
                \"\"\"
            """,
        )
        self.assertSuppressErrors(
            {
                4: [
                    {
                        "code": "42",
                        "description": "Some error 1",
                    },
                    {
                        "code": "42",
                        "description": "Some error 2",
                    },
                ],
            },
            """
            def foo() -> None:
                f\"\"\"
                foo
                {1 + "hello"}
                {"world" + int("a")}
                bar
                \"\"\"
            """,
            """
            def foo() -> None:
                # FIXME[42]: Some error 1
                # FIXME[42]: Some error 2
                f\"\"\"
                foo
                {1 + "hello"}
                {"world" + int("a")}
                bar
                \"\"\"
            """,
        )

    def assertLinesSpanned(
        self, source: str, expected_lines: List[Tuple[int, int]]
    ) -> None:
        self.assertEqual(
            list(
                _line_ranges_spanned_by_format_strings(textwrap.dedent(source)).values()
            ),
            expected_lines,
        )

    def test_lines_spanned_by_format_strings(self) -> None:
        self.assertLinesSpanned(
            """
            def foo() -> None:
                f\"\"\"
                foo
                {1 + "hello"}
                bar
                \"\"\"

                f\"\"\"
                bar
                \"\"\"
            """,
            [(3, 7), (9, 11)],
        )
        self.assertLinesSpanned(
            """
            def foo() -> None:
                f"{1 + "hello"}"
            """,
            [(3, 3)],
        )
        # Skip checking of format strings in case libcst barfs on the parsing.
        self.assertLinesSpanned(
            """
            def cannot_parse()
            """,
            [],
        )

    def test_map_line_to_start_of_range(self) -> None:
        self.assertEqual(
            _map_line_to_start_of_range([(3, 3), (3, 5), (9, 13)]),
            {3: 3, 4: 3, 5: 3, 9: 9, 10: 9, 11: 9, 12: 9, 13: 9},
        )
        self.assertEqual(
            _map_line_to_start_of_range([]),
            {},
        )
        # Intervals shouldn't overlap, but if they do, we will prefer the earlier one.
        self.assertEqual(
            _map_line_to_start_of_range([(3, 5), (4, 6)]),
            {3: 3, 4: 3, 5: 3, 6: 4},
        )

    def test_relocate_errors(self) -> None:
        errors = {
            1: [
                {"code": "1", "description": "description"},
                {"code": "2", "description": "description"},
            ],
            2: [
                {"code": "3", "description": "description"},
                {"code": "4", "description": "description"},
            ],
            3: [
                {"code": "5", "description": "description"},
                {"code": "6", "description": "description"},
            ],
        }
        self.assertEqual(
            _relocate_errors(
                errors,
                {},
            ),
            errors,
        )
        self.assertEqual(
            _relocate_errors(
                errors,
                {2: 1, 3: 1},
            ),
            {
                1: [
                    {"code": "1", "description": "description"},
                    {"code": "2", "description": "description"},
                    {"code": "3", "description": "description"},
                    {"code": "4", "description": "description"},
                    {"code": "5", "description": "description"},
                    {"code": "6", "description": "description"},
                ],
            },
        )
        self.assertEqual(
            _relocate_errors(
                errors,
                {1: 1, 2: 2, 3: 2},
            ),
            {
                1: [
                    {"code": "1", "description": "description"},
                    {"code": "2", "description": "description"},
                ],
                2: [
                    {"code": "3", "description": "description"},
                    {"code": "4", "description": "description"},
                    {"code": "5", "description": "description"},
                    {"code": "6", "description": "description"},
                ],
            },
        )
