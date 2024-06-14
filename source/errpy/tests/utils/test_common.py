# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import json
import os
import traceback
import unittest
from os.path import exists
from typing import List, Optional, Tuple

import tools.pyre.source.errpy.tests.utils.ast_utils as ast_utils

EXPECTED_FAILS_POSTFIX: str = ".expect_fails"
RESOURCES_DIR: str = "tools/pyre/source/errpy/tests/test_resources/"
UNIT_TESTS_DIR: str = "unit_tests/"
PRETTY_PRINTER_TESTS_DIR: str = "pretty_printer_tests/"
ERROR_RECOVERY_TESTS_DIR: str = "error_recovery/"
ERROR_RECOVERY_SPECIFIC_TESTS_DIR: str = "error_recovery_specific_tests/"
INVALID_SYNTAX_TESTS_DIR: str = "invalid_syntax_tests/"
EXPECTED_RESULTS_POSTFIX: str = "_expected_results"
EXPECTED_RESULTS_POSTFIX_NEW: str = ".new"

TEST_CONFIG_FNAME: str = RESOURCES_DIR + "test_config.json"

TEST_ERRPY_RESULTS_NEWFILE_CONFIG_KEY: str = "TEST_ERRPY_RESULTS_NEWFILE"

WRITE_EXPECTED_RESULTS_NEWFILE: bool = False


def load_test_config() -> None:
    global WRITE_EXPECTED_RESULTS_NEWFILE
    try:
        with open(TEST_CONFIG_FNAME) as fobj:
            config = fobj.read()
            params = json.loads(config)
            WRITE_EXPECTED_RESULTS_NEWFILE = params[
                TEST_ERRPY_RESULTS_NEWFILE_CONFIG_KEY
            ]

    except Exception:
        print("Cannot load_test_config due to: {}".format(traceback.format_exc()))


load_test_config()


def read_code(
    fname: str, flavour: str = UNIT_TESTS_DIR, return_if_not_exist: Optional[str] = None
) -> str:
    to_open = RESOURCES_DIR + flavour + fname

    if not exists(to_open) and return_if_not_exist is not None:
        return return_if_not_exist

    with open(to_open) as fobj:
        return fobj.read()


def write_file(fname: str, to_write: str, flavour: str = UNIT_TESTS_DIR) -> None:
    with open(RESOURCES_DIR + flavour + fname, "w") as fobj:
        fobj.write(to_write)


def make_results_dir(dirname: str, flavour: str = UNIT_TESTS_DIR) -> str:
    full_dir_path = RESOURCES_DIR + flavour + dirname + "/"
    os.makedirs(full_dir_path, exist_ok=True)
    return dirname + "/"


def format_side_by_side(left_input: str, right_input: str) -> str:
    """Print existing and replacement text side by side"""
    ret = ""

    width = max(len(x) for x in left_input.split("\n")) + 5

    leftx = "Input:\n" + "".join(["-"] * width) + "\n" + left_input
    rightx = "ERRPY Recovered AST:\n" + "".join(["-"] * width) + "\n" + right_input

    left = [x[0 : min(len(x), width)].ljust(width) for x in leftx.split("\n")]
    right = [x[0 : min(len(x), width)].ljust(width) for x in rightx.split("\n")]

    for i in range(len(left)):
        ret += left[i]
        ret += "| "
        if i < len(right):
            ret += right[i]
        ret += "\n"
    return ret


class ASTTestCommon(unittest.TestCase):
    def check_vs_expects_file(
        self,
        test_dir: str,
        failed_for: str,
        expected_results_fname: str,
        got_results: str,
    ) -> None:
        """
        This method supports 'expect' style tests where we compare the output of a test, 'got_results' against what was expected which is persisted
        in a file 'expected_results_fname'.

        Three things can happen wihtin this flow:
         - If there is no expected results already persisted in 'expected_results_fname' then this is created using the obtained 'got_results' results
           and the calling unit test fails.
         - If the two test outputs differ (obtained and expected) then a new 'expected_results_fname' file is committed to disk and the test fails.
         - If the two test outputs match then this part of the test passes, horray!.
        """

        got_results = (
            "@" + "generated\n\n" + got_results
        )  # help diff review tool undestand this to be generated

        expected_results = None
        try:
            expected_results = read_code(expected_results_fname, flavour=test_dir)
        except Exception:
            write_file(expected_results_fname, got_results, flavour=test_dir)
            self.fail(
                "No '%s' file defined, so created a new one! Check this document and ensure results match expectation. Future runs will treat contents as correct test result"
                % (expected_results_fname)
            )

        # great now we can check the results against what's expected
        if expected_results != got_results:
            expected_results = ast_utils.format_ast_with_indentation(expected_results)
            got_results = ast_utils.format_ast_with_indentation(got_results)

            if WRITE_EXPECTED_RESULTS_NEWFILE:
                new_results_fname = (
                    expected_results_fname + EXPECTED_RESULTS_POSTFIX_NEW
                )
                print(
                    "test config variable: '{}' is set in '{}'. Writing results to new file: '{}' (for diffing etc)".format(
                        TEST_ERRPY_RESULTS_NEWFILE_CONFIG_KEY,
                        TEST_CONFIG_FNAME,
                        new_results_fname,
                    )
                )
                try:
                    write_file(new_results_fname, got_results, flavour=test_dir)
                    print(f"new file write of: {new_results_fname} complete")
                except Exception:
                    print(
                        "new file write failed due to: {}".format(
                            traceback.format_exc()
                        )
                    )

            print("Test for: %s failed:\n" % (failed_for))

            self.fail("Test for: %s failed" % (failed_for))

    def splitmany_test_cases(
        self, many_fname: str, flavour: str
    ) -> List[Tuple[str, str]]:
        all_tests_Split = read_code(many_fname, flavour).split("##")
        sanitized_test_cases = []
        for caze in all_tests_Split:
            caze = caze.strip()
            lines = caze.split("\n")
            code_title = lines[0].strip()
            test_body = "\n".join(lines[1:]).strip()
            sanitized_test_cases.append((code_title, test_body))
        return sanitized_test_cases

    def check_many_cases_in_file(
        self, many_fname: str, flavour: str = UNIT_TESTS_DIR
    ) -> list[tuple[str, str]]:
        """Test many code blocks delimiatated by ##'s in many_fname
        We expect all code blocks in test resource to be cases syntatically compliant with 3.10 syntax
        May OPTIONALLY provide '.expect_fails' postfixed file for tests which are expected to fail - which specifies
        a newline delimitated list of test cases expected to fail - normally expected to be empty but a
        handy tool during development process"""

        failed_cases_file = many_fname + EXPECTED_FAILS_POSTFIX
        failed_cases_file_contents = read_code(
            failed_cases_file, flavour, return_if_not_exist=""
        )

        expect_fails = []
        if failed_cases_file_contents:
            expect_fails = [
                x.strip() for x in failed_cases_file_contents.split("\n") if x
            ]

        expect_fails_set = set(expect_fails)
        fails = []
        unexpected_fails_postfix = []

        # split into individual tests
        sanitized_test_cases = self.splitmany_test_cases(many_fname, flavour)

        for code_title, test_body in sanitized_test_cases:
            expected_ast = ast_utils.get_cpython_ast(test_body).strip()
            (got_ast, errors), _ = ast_utils.run_errpy(test_body)

            if errors:
                got_ast += errors
            got_ast = got_ast.strip()
            if expected_ast != got_ast:
                got_ast = ast_utils.format_ast_with_indentation(got_ast)
                expected_ast = ast_utils.format_ast_with_indentation(expected_ast)

                fails.append(code_title)
                if code_title not in expect_fails_set:
                    unexpected_fails_postfix += [code_title]
                    print("test code: %s failed..." % code_title)
                    print("\n\ntest fail\n")
                    print("Result:\n" + got_ast)
                    print("\nExpect:\n" + expected_ast)
                    print("\n\n")

        if unexpected_fails_postfix:
            failed_instances = "Unexpected failures:\n%s\n" % "\n".join(
                sorted(unexpected_fails_postfix)
            )
            print(failed_instances)
            self.fail(
                "\n%s out of %s tests failed, with %s being unexpected in: %s%s- %s"
                % (
                    len(fails),
                    len(sanitized_test_cases) - 1,
                    len(unexpected_fails_postfix),
                    many_fname,
                    (
                        " (add these to `{}{}`) ".format(
                            many_fname, EXPECTED_FAILS_POSTFIX
                        )
                    ),
                    unexpected_fails_postfix,
                )
            )
        else:
            self.assertEqual(
                sorted(fails),
                sorted(expect_fails),
                "Failed cases don't match expectation defined in: " + failed_cases_file,
            )

        return sanitized_test_cases
