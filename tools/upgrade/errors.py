# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
TODO(T132414938) Add a module-level docstring
"""


import itertools
import json
import logging
import re
import sys
from collections import defaultdict
from pathlib import Path
from typing import Any, cast, Dict, Iterable, List, Optional, Tuple, Union

import libcst
import libcst.matchers as libcst_matchers

from . import ast, UserError


LOG: logging.Logger = logging.getLogger(__name__)
MAX_LINES_PER_FIXME: int = 4


PyreError = Dict[str, Any]
PathsToErrors = Dict[Path, List[PyreError]]
LineRange = Tuple[int, int]
LineToErrors = Dict[int, List[Dict[str, str]]]


class LineBreakTransformer(libcst.CSTTransformer):
    def leave_SimpleWhitespace(
        self,
        original_node: libcst.SimpleWhitespace,
        updated_node: libcst.SimpleWhitespace,
    ) -> Union[libcst.SimpleWhitespace, libcst.ParenthesizedWhitespace]:
        whitespace = original_node.value.replace("\\", "")
        if "\n" in whitespace:
            first_line = libcst.TrailingWhitespace(
                whitespace=libcst.SimpleWhitespace(
                    value=whitespace.split("\n")[0].rstrip()
                ),
                comment=None,
                newline=libcst.Newline(),
            )
            last_line = libcst.SimpleWhitespace(value=whitespace.split("\n")[1])
            return libcst.ParenthesizedWhitespace(
                first_line=first_line, empty_lines=[], indent=True, last_line=last_line
            )
        return updated_node

    @staticmethod
    def basic_parenthesize(
        node: libcst.CSTNode,
        whitespace: Optional[libcst.BaseParenthesizableWhitespace] = None,
    ) -> libcst.CSTNode:
        if not hasattr(node, "lpar"):
            return node
        if whitespace:
            return node.with_changes(
                lpar=[libcst.LeftParen(whitespace_after=whitespace)],
                rpar=[libcst.RightParen()],
            )
        return node.with_changes(lpar=[libcst.LeftParen()], rpar=[libcst.RightParen()])

    def leave_Assert(
        self, original_node: libcst.Assert, updated_node: libcst.Assert
    ) -> libcst.Assert:
        test = updated_node.test
        message = updated_node.msg
        comma = updated_node.comma
        if not test:
            return updated_node
        if message and isinstance(comma, libcst.Comma):
            message = LineBreakTransformer.basic_parenthesize(
                message, comma.whitespace_after
            )
            comma = comma.with_changes(
                whitespace_after=libcst.SimpleWhitespace(
                    value=" ",
                )
            )
        assert_whitespace = updated_node.whitespace_after_assert
        if isinstance(assert_whitespace, libcst.ParenthesizedWhitespace):
            return updated_node.with_changes(
                test=LineBreakTransformer.basic_parenthesize(test, assert_whitespace),
                msg=message,
                comma=comma,
                whitespace_after_assert=libcst.SimpleWhitespace(value=" "),
            )
        return updated_node.with_changes(
            test=LineBreakTransformer.basic_parenthesize(test),
            msg=message,
            comma=comma,
        )

    def leave_Assign(
        self, original_node: libcst.Assign, updated_node: libcst.Assign
    ) -> libcst.Assign:
        assign_value = updated_node.value
        assign_whitespace = updated_node.targets[-1].whitespace_after_equal
        if libcst_matchers.matches(
            assign_whitespace, libcst_matchers.ParenthesizedWhitespace()
        ):
            adjusted_target = updated_node.targets[-1].with_changes(
                whitespace_after_equal=libcst.SimpleWhitespace(value=" ")
            )
            updated_targets = list(updated_node.targets[:-1])
            updated_targets.append(adjusted_target)
            return updated_node.with_changes(
                targets=tuple(updated_targets),
                value=LineBreakTransformer.basic_parenthesize(
                    assign_value, assign_whitespace
                ),
            )
        return updated_node.with_changes(
            value=LineBreakTransformer.basic_parenthesize(assign_value)
        )

    def leave_AnnAssign(
        self, original_node: libcst.AnnAssign, updated_node: libcst.AnnAssign
    ) -> libcst.AnnAssign:
        assign_value = updated_node.value
        equal = updated_node.equal
        if not isinstance(equal, libcst.AssignEqual):
            return updated_node
        assign_whitespace = equal.whitespace_after
        updated_value = (
            LineBreakTransformer.basic_parenthesize(assign_value, assign_whitespace)
            if assign_value
            else None
        )
        if libcst_matchers.matches(
            assign_whitespace, libcst_matchers.ParenthesizedWhitespace()
        ):
            updated_equal = equal.with_changes(
                whitespace_after=libcst.SimpleWhitespace(value=" ")
            )

            return updated_node.with_changes(
                equal=updated_equal,
                value=updated_value,
            )
        return updated_node.with_changes(value=updated_value)

    def leave_Del(
        self, original_node: libcst.Del, updated_node: libcst.Del
    ) -> libcst.Del:
        delete_target = updated_node.target
        delete_whitespace = updated_node.whitespace_after_del
        if isinstance(delete_whitespace, libcst.ParenthesizedWhitespace):
            return updated_node.with_changes(
                target=LineBreakTransformer.basic_parenthesize(
                    delete_target, delete_whitespace
                ),
                whitespace_after_del=libcst.SimpleWhitespace(value=" "),
            )
        return updated_node.with_changes(
            target=LineBreakTransformer.basic_parenthesize(delete_target)
        )

    def leave_Raise(
        self, original_node: libcst.Raise, updated_node: libcst.Raise
    ) -> libcst.Raise:
        exception = updated_node.exc
        if not exception:
            return updated_node
        raise_whitespace = updated_node.whitespace_after_raise
        if isinstance(raise_whitespace, libcst.ParenthesizedWhitespace):
            return updated_node.with_changes(
                exc=LineBreakTransformer.basic_parenthesize(
                    exception, raise_whitespace
                ),
                whitespace_after_raise=libcst.SimpleWhitespace(value=" "),
            )
        return updated_node.with_changes(
            exc=LineBreakTransformer.basic_parenthesize(exception)
        )

    def leave_Return(
        self, original_node: libcst.Return, updated_node: libcst.Return
    ) -> libcst.Return:
        return_value = updated_node.value
        if not return_value:
            return updated_node
        return_whitespace = updated_node.whitespace_after_return
        if isinstance(return_whitespace, libcst.ParenthesizedWhitespace):
            return updated_node.with_changes(
                value=LineBreakTransformer.basic_parenthesize(
                    return_value, return_whitespace
                ),
                whitespace_after_return=libcst.SimpleWhitespace(value=" "),
            )
        return updated_node.with_changes(
            value=LineBreakTransformer.basic_parenthesize(return_value)
        )


class PartialErrorSuppression(Exception):
    def __init__(self, message: str, unsuppressed_paths: List[str]) -> None:
        super().__init__(message)
        self.unsuppressed_paths: List[str] = unsuppressed_paths


def error_path(error: Dict[str, Any]) -> str:
    return error["path"]


class Errors:
    @classmethod
    def empty(cls) -> "Errors":
        return cls([])

    @staticmethod
    def from_json(
        json_string: str,
        only_fix_error_code: Optional[int] = None,
        from_stdin: bool = False,
    ) -> "Errors":
        try:
            errors = json.loads(json_string)
            return Errors(_filter_errors(errors, only_fix_error_code))
        except json.decoder.JSONDecodeError:
            if from_stdin:
                raise UserError(
                    "Received invalid JSON as input. "
                    "If piping from `pyre check` be sure to use `--output=json`."
                )
            else:
                raise UserError(
                    "Encountered invalid output when checking for pyre errors: "
                    f"`{json_string}`."
                )

    @staticmethod
    def from_stdin(only_fix_error_code: Optional[int] = None) -> "Errors":
        input = sys.stdin.read()
        return Errors.from_json(input, only_fix_error_code, from_stdin=True)

    def __init__(self, errors: List[Dict[str, Any]]) -> None:
        self.errors: List[Dict[str, Any]] = errors

    def __len__(self) -> int:
        return len(self.errors)

    def __eq__(self, other: "Errors") -> bool:
        return self.errors == other.errors

    @property
    def paths_to_errors(self) -> Dict[str, List[PyreError]]:
        return {
            path: list(errors)
            for path, errors in itertools.groupby(
                sorted(self.errors, key=error_path), key=error_path
            )
        }

    def suppress(
        self,
        comment: Optional[str] = None,
        max_line_length: Optional[int] = None,
        truncate: bool = False,
        unsafe: bool = False,
    ) -> None:
        unsuppressed_paths_and_exceptions = []

        for path_to_suppress, errors in self.paths_to_errors.items():
            LOG.info("Processing `%s`", path_to_suppress)
            try:
                path = Path(path_to_suppress)
                input = path.read_text()
                output = _suppress_errors(
                    input,
                    _build_error_map(errors),
                    comment,
                    max_line_length
                    if max_line_length and max_line_length > 0
                    else None,
                    truncate,
                    unsafe,
                )
                path.write_text(output)
            except SkippingGeneratedFileException:
                LOG.warning(f"Skipping generated file at {path_to_suppress}")
            except LineBreakParsingException:
                LOG.warning(
                    f"Skipping file with unparseable line breaks at {path_to_suppress}"
                )
            except (ast.UnstableAST, SyntaxError) as exception:
                unsuppressed_paths_and_exceptions.append((path_to_suppress, exception))

        if unsuppressed_paths_and_exceptions:
            exception_messages = "\n".join(
                f"{path} - {str(exception)}"
                for path, exception in unsuppressed_paths_and_exceptions
            )
            raise PartialErrorSuppression(
                "Could not fully suppress errors due to the following exceptions: "
                f"{exception_messages}\n Run with `--unsafe` to suppress anyway.",
                [path for path, _ in unsuppressed_paths_and_exceptions],
            )


def _filter_errors(
    errors: List[Dict[str, Any]], only_fix_error_code: Optional[int] = None
) -> List[Dict[str, Any]]:
    if only_fix_error_code is not None:
        errors = [error for error in errors if error["code"] == only_fix_error_code]
    return errors


def _remove_comment_preamble(lines: List[str]) -> None:
    # Deprecated: leaving remove logic until live old-style comments are cleaned up.
    while lines:
        old_line = lines.pop()
        new_line = re.sub(r"# pyre: .*$", "", old_line).rstrip()
        if old_line == "" or new_line != "":
            # The preamble has ended.
            lines.append(new_line)
            return


def _add_error_to_line_break_block(lines: List[str], errors: List[List[str]]) -> None:
    # Gather unbroken lines.
    line_break_block = [lines.pop() for _ in range(0, len(errors))]
    line_break_block.reverse()

    # Transform line break block to use parenthesis.
    indent = len(line_break_block[0]) - len(line_break_block[0].lstrip())
    line_break_block = [line[indent:] for line in line_break_block]
    statement = "\n".join(line_break_block)
    transformed_statement = libcst.Module([]).code_for_node(
        cast(
            libcst.CSTNode,
            libcst.parse_statement(statement).visit(LineBreakTransformer()),
        )
    )
    transformed_lines = transformed_statement.split("\n")
    transformed_lines = [" " * indent + line for line in transformed_lines]

    # Add to lines.
    for line, comment in zip(transformed_lines, errors):
        lines.extend(comment)
        lines.append(line)


def _split_across_lines(
    comment: str, indent: int, max_line_length: Optional[int]
) -> List[str]:
    if not max_line_length or len(comment) <= max_line_length:
        return [comment]

    comment = comment.lstrip()
    available_columns = max_line_length - indent - len("#  ")

    buffered_line = ""
    result = []
    prefix = " " * indent
    for token in comment.split():
        if buffered_line and (
            len(buffered_line) + len(token) + len(" ") > available_columns
        ):
            # This new token would make the line exceed the limit,
            # hence terminate what we have accumulated.
            result.append(("{}{}".format(prefix, buffered_line)).rstrip())
            # The first line already has a comment token on it, so don't prefix #. For
            # the rest, we need to add the comment symbol manually.
            prefix = "{}#  ".format(" " * indent)
            buffered_line = ""

        buffered_line = buffered_line + token + " "

    result.append(("{}{}".format(prefix, buffered_line)).rstrip())
    return result


class SkippingGeneratedFileException(Exception):
    pass


class LineBreakParsingException(Exception):
    pass


def _str_to_int(digits: str) -> Optional[int]:
    try:
        return int(digits)
    except ValueError:
        return None


def _get_unused_ignore_codes(errors: List[Dict[str, str]]) -> List[int]:
    unused_ignore_codes: List[int] = []
    ignore_errors = [error for error in errors if error["code"] == "0"]
    for error in ignore_errors:
        match = re.search(
            r"The `pyre-ignore\[(.*?)\]` or `pyre-fixme\[.*?\]`", error["description"]
        )
        if match:
            unused_ignore_codes.extend(
                int_code
                for int_code in (
                    _str_to_int(code.strip()) for code in match.group(1).split(",")
                )
                if int_code is not None
            )
    unused_ignore_codes.sort()
    return unused_ignore_codes


def _remove_unused_ignores(line: str, errors: List[Dict[str, str]]) -> str:
    unused_ignore_codes = _get_unused_ignore_codes(errors)
    match = re.search(r"pyre-(ignore|fixme) *\[([0-9, ]+)\]", line)
    stripped_line = re.sub(r"# *pyre-(ignore|fixme).*$", "", line).rstrip()
    if not match:
        return stripped_line

    # One or more codes are specified in the ignore comment.
    # Remove only the codes that are erroring as unused.
    ignore_codes_string = match.group(2)
    ignore_codes = [
        int(code.strip()) for code in ignore_codes_string.split(",") if code != ""
    ]
    remaining_ignore_codes = set(ignore_codes) - set(unused_ignore_codes)
    if len(remaining_ignore_codes) == 0 or len(unused_ignore_codes) == 0:
        return stripped_line
    else:
        return line.replace(
            ignore_codes_string,
            ", ".join([str(code) for code in remaining_ignore_codes]),
        )


def _line_ranges_spanned_by_format_strings(
    source: str,
) -> Dict[libcst.CSTNode, LineRange]:
    def _code_range_to_line_range(
        code_range: libcst._position.CodeRange,
    ) -> LineRange:
        return code_range.start.line, code_range.end.line

    try:
        wrapper = libcst.metadata.MetadataWrapper(libcst.parse_module(source))
    except libcst._exceptions.ParserSyntaxError as exception:
        # NOTE: This should not happen. If a file is unparseable for libcst, it
        # would probably have been unparseable for Pyre as well. In that case,
        # we would not have raised a 404 parse error and not reached here in the
        # first place. Still, catch the exception and just skip the special
        # handling of format strings.
        LOG.warning(
            "Not moving out fixmes from f-strings because"
            f" libcst failed to parse the file: {exception}"
        )
        return {}

    position_map = wrapper.resolve(libcst.metadata.PositionProvider)
    return {
        format_string: _code_range_to_line_range(position_map[format_string])
        for format_string in libcst_matchers.findall(
            wrapper.module, libcst_matchers.FormattedString()
        )
    }


def _map_line_to_start_of_range(line_ranges: List[LineRange]) -> Dict[int, int]:
    target_line_map = {}
    for start, end in reversed(line_ranges):
        for line in range(start, end + 1):
            target_line_map[line] = start
    return target_line_map


class LineBreakBlock:
    error_comments: List[List[str]]
    opened_expressions: int
    is_active: bool

    def __init__(self) -> None:
        self.error_comments = []
        self.opened_expressions = 0
        self.is_active = False

    def ready_to_suppress(self) -> bool:
        # Line break block has been filled and then ended; errors can be applied.
        return not self.is_active and len(self.error_comments) > 0

    def process_line(self, line: str, error_comments: List[str]) -> None:
        comment_free_line = line.split("#")[0].rstrip()
        if not self.is_active:
            # Check if line break block is beginning.
            self.is_active = comment_free_line.endswith("\\")
            if self.is_active:
                self.error_comments.append(error_comments)
            return

        # Check if line break block is ending.
        self.error_comments.append(error_comments)
        if comment_free_line.endswith("\\"):
            return
        if comment_free_line.endswith("("):
            self.opened_expressions += 1
            return
        if comment_free_line.endswith(")"):
            self.opened_expressions -= 1
        self.is_active = self.opened_expressions > 0


def _lines_after_suppressing_errors(
    lines: List[str],
    errors: Dict[int, List[Dict[str, str]]],
    custom_comment: Optional[str],
    max_line_length: Optional[int],
    truncate: bool,
) -> List[str]:
    new_lines = []
    removing_pyre_comments = False
    line_break_block = LineBreakBlock()
    in_multi_line_string = False
    for index, line in enumerate(lines):
        if removing_pyre_comments:
            stripped = line.lstrip()
            if stripped.startswith("#") and not re.match(
                r"# *pyre-(ignore|fixme).*$", stripped
            ):
                continue
            else:
                removing_pyre_comments = False
        number = index + 1
        if line.startswith("#") and re.match(r"# *@manual=.*$", line):
            # Apply suppressions for lines following @manual to current line.
            errors[number] = errors[number + 1]
            del errors[number + 1]

        # Deduplicate errors
        error_mapping = {
            error["code"] + error["description"]: error
            for error in errors.get(number, [])
        }
        relevant_errors = list(error_mapping.values())

        if any(error["code"] == "0" for error in relevant_errors):
            replacement = _remove_unused_ignores(line, relevant_errors)
            if replacement == "":
                removing_pyre_comments = True
                _remove_comment_preamble(new_lines)
                continue
            else:
                line = replacement

        indent = len(line) - len(line.lstrip(" "))
        comments = [
            line
            for error in relevant_errors
            for line in _error_to_fixme_comment_lines(
                error, indent, truncate, max_line_length, custom_comment
            )
        ]

        # Handle suppressions in line break blocks.
        line_break_block.process_line(line, comments)
        if line_break_block.ready_to_suppress():
            new_lines.append(line)
            try:
                line_break_block_errors = line_break_block.error_comments
                if sum(len(errors) for errors in line_break_block_errors) > 0:
                    _add_error_to_line_break_block(new_lines, line_break_block_errors)
            except libcst.ParserSyntaxError as exception:
                raise LineBreakParsingException(exception)
            line_break_block = LineBreakBlock()
            continue

        # Handle suppressions around multi-line strings.
        contains_multi_line_string_token = line.count('"""') % 2 != 0
        if contains_multi_line_string_token:
            is_end_of_multi_line_string = in_multi_line_string
            in_multi_line_string = not in_multi_line_string
        else:
            is_end_of_multi_line_string = False

        if is_end_of_multi_line_string and len(comments) > 0:
            # Use a simple same-line suppression for errors on a multi-line string close
            error_codes = [
                error["code"] for error in relevant_errors if error["code"] != "0"
            ]
            line = line + "  # pyre-fixme[{}]".format(", ".join(error_codes))
            new_lines.append(line)
            continue

        # Add suppression comments.
        if not line_break_block.is_active and len(comments) > 0:
            LOG.info(
                "Adding comment%s on line %d: %s",
                "s" if len(comments) > 1 else "",
                number,
                " \n".join(comments),
            )
            new_lines.extend(comments)
        new_lines.append(line)

    return new_lines


def _relocate_errors(
    errors: LineToErrors, target_line_map: Dict[int, int]
) -> LineToErrors:
    relocated = defaultdict(list)
    for line, errors in errors.items():
        target_line = target_line_map.get(line)
        if target_line is None or target_line == line:
            target_line = line
        else:
            LOG.info(
                f"Relocating the following fixmes from line {line}"
                f" to line {target_line} because line {line} is within"
                f" a multi-line format string:\n{errors}"
            )

        relocated[target_line].extend(errors)
    return relocated


def _relocate_errors_inside_format_strings(
    errors: LineToErrors, source: str
) -> LineToErrors:
    def _expression_to_string(expression: libcst.BaseExpression) -> str:
        return libcst.Module(
            [libcst.SimpleStatementLine([libcst.Expr(expression)])]
        ).code.strip()

    format_string_line_ranges = _line_ranges_spanned_by_format_strings(source)
    if len(format_string_line_ranges) == 0:
        return errors

    log_lines = ["Lines spanned by format strings:"]
    for format_string, line_range in format_string_line_ranges.items():
        # pyre-fixme[6]: Expected BaseExpression but got CSTNode.
        log_lines.append(f"{_expression_to_string(format_string)}: {line_range}")
    LOG.debug("\n".join(log_lines))

    return _relocate_errors(
        errors, _map_line_to_start_of_range(list(format_string_line_ranges.values()))
    )


def _find_first_non_comment_line_for_unparseable_file(lines: List[str]) -> List[str]:
    first_empty_line = 0
    for index, line in enumerate(lines):
        stripped_line = line.lstrip()
        if not stripped_line.startswith("#"):
            first_empty_line = index
            break
    return (
        lines[:first_empty_line]
        + ["# pyre-ignore-all-errors[404]"]
        + lines[first_empty_line:]
    )


def _suppress_errors(
    input: str,
    errors: LineToErrors,
    custom_comment: Optional[str] = None,
    max_line_length: Optional[int] = None,
    truncate: bool = False,
    unsafe: bool = False,
) -> str:
    if not unsafe and "@" "generated" in input:
        raise SkippingGeneratedFileException()

    lines: List[str] = input.split("\n")

    # Do not suppress parse errors.
    if any(
        error["code"] == "404" for error_list in errors.values() for error in error_list
    ):
        new_lines = _find_first_non_comment_line_for_unparseable_file(lines)
        output = "\n".join(new_lines)
        return output

    errors = _relocate_errors_inside_format_strings(errors, input)

    new_lines = _lines_after_suppressing_errors(
        lines, errors, custom_comment, max_line_length, truncate
    )

    output = "\n".join(new_lines)
    if not unsafe:
        ast.check_stable(input, output)
    return output


def _error_to_fixme_comment_lines(
    error: Dict[str, Any],
    indent: int,
    truncate: bool,
    max_line_length: Optional[int],
    custom_comment: Optional[str],
) -> List[str]:
    if error["code"] == "0":
        return []

    description = custom_comment if custom_comment else error["description"]
    comment = "{}# pyre-fixme[{}]: {}".format(" " * indent, error["code"], description)

    if not max_line_length:
        return [comment]

    truncated_comment = comment[: (max_line_length - 3)] + "..."
    split_comment_lines = _split_across_lines(comment, indent, max_line_length)
    should_truncate = (
        truncate
        or len(split_comment_lines) > MAX_LINES_PER_FIXME
        or any(len(line) > max_line_length for line in split_comment_lines)
    )
    return [truncated_comment] if should_truncate else split_comment_lines


def _build_error_map(
    errors: Iterable[Dict[str, Any]]
) -> Dict[int, List[Dict[str, str]]]:
    error_map = defaultdict(lambda: [])
    for error in errors:
        if error["concise_description"]:
            description = error["concise_description"]
        else:
            description = error["description"]
        match = re.search(r"\[(\d+)\]: (.*)", description)
        if match:
            error_map[error["line"]].append(
                {"code": match.group(1), "description": match.group(2)}
            )
    return error_map
