# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import ast

from tools.pyre.source.errpy import ffi_python


def _do_format_ast_with_indentation(to_format: str, indent: int) -> str:
    """Function will transform an AST provided as a single line string into a
    multi-line formatted AST which is easier to read and diff over.

    e.g. for code: `a=8`
    Single line AST:
     "Module(body=[Assign(targets=[Name(id='a', ctx=Store(), lineno=1, col_offset=0, end_lineno=1, end_col_offset=1)], value=Constant(value=8, kind=None, lineno=1, col_offset=2, end_lineno=1, end_col_offset=3), type_comment=None, lineno=1, col_offset=0, end_lineno=1, end_col_offset=3)], type_ignores=[])"

    After call of _do_format_ast_with_indentation:

    Module(
    body=[
     Assign(
      targets=[
       Name(
        id='a',
        ctx=Store(),
        lineno=1,
        col_offset=0,
        end_lineno=1,
        end_col_offset=1)],
      value=Constant(
       value=8,
       lineno=1,
       col_offset=4,
       end_lineno=1,
       end_col_offset=5),
      lineno=1,
      col_offset=0,
      end_lineno=1,
      end_col_offset=5),
     type_comment=None,
     lineno=1,
     col_offset=0,
     end_lineno=1,
     end_col_offset=3)
     ],
    type_ignores=[]
    )
    """
    formatted = []
    current_indent = 0
    next_line_newline = False
    inside_string = None
    next_char_escape = False
    for char in to_format:
        if inside_string:
            if next_char_escape:
                formatted.append(char)
                next_char_escape = False
                continue
            if inside_string == char:
                inside_string = None
            else:
                if "\\" == char:
                    next_char_escape = True

            formatted.append(char)
            continue
        elif char in ("'", '"'):
            inside_string = char
            formatted.append(char)
            continue

        if char == " ":
            continue

        if next_line_newline and char not in (")", "]"):
            formatted.append("\n")
            formatted.append(" " * current_indent)
            next_line_newline = False

        formatted.append(char)
        if char == "(" or char == "[":
            current_indent += indent
            next_line_newline = True
        elif char == ")" or char == "]":
            current_indent -= indent
            next_line_newline = False
        elif char == ",":
            next_line_newline = True

    return "".join(formatted)


def format_ast_with_indentation(ast: str, indent: int = 1) -> str:
    components = ast.split("\n")
    # ast should be the first line, others may be pretty printed code output
    to_format = components[0]

    formatted = _do_format_ast_with_indentation(to_format, indent)

    if len(components) > 1:
        return formatted + "\n" + "\n".join(components[1:])
    else:
        return formatted


def get_cpython_ast(
    source: str,
    raise_exception: bool = False,
    pretty_print: bool = True,
) -> str:
    try:
        ast_inst = ast.parse(source)
        ast_dump = ast.dump(ast_inst, include_attributes=True)
        if pretty_print:
            return ast_dump + "\n" + ast.unparse(ast_inst)
        else:
            return ast_dump
    except BaseException as e:
        if raise_exception:
            raise e
        return "Invalid CPython Syntax: " + str(e)


def run_errpy(
    source: str, just_pretty_print_ast: bool = False
) -> tuple[tuple[str, str], bool]:
    """returns: (result, did_errpy_panic)"""
    try:
        if just_pretty_print_ast:
            return (
                ffi_python.py_parse_module_print_ast_pretty_print_only(source),
                False,
            )
        ast, errors = ffi_python.py_parse_module_print_ast_and_pretty_print(source)
        return ((ast, errors), False)
    except BaseException as e:
        return (("ERRPY Failure: " + str(e), ""), True)


def run_errpy_ast_only(source: str) -> tuple[tuple[str, str], bool]:
    """returns: (result, did_errpy_panic)"""
    try:
        ast, errors = ffi_python.py_parse_module_print_ast(source)
        return (
            (ast, errors),
            False,
        )
    except BaseException as e:
        return (("ERRPY Failure: " + str(e), ""), True)
