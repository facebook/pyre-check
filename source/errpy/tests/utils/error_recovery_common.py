# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict


import traceback
from dataclasses import astuple, dataclass
from difflib import unified_diff
from typing import Callable, Iterator, List, Optional

import tools.pyre.source.errpy.tests.utils.ast_utils as ast_utils
from tools.pyre.source.errpy.tests.utils.test_common import (
    ASTTestCommon,
    ERROR_RECOVERY_SPECIFIC_TESTS_DIR,
    ERROR_RECOVERY_TESTS_DIR,
    EXPECTED_RESULTS_POSTFIX,
    EXPECTED_RESULTS_POSTFIX_NEW,
    format_side_by_side,
    make_results_dir,
    read_code,
    TEST_CONFIG_FNAME,
    TEST_ERRPY_RESULTS_NEWFILE_CONFIG_KEY,
    WRITE_EXPECTED_RESULTS_NEWFILE,
    write_file,
)


@dataclass
class ErrorRecoveryTestIteratorStep:
    """Class for holding each iteration step of the iterator to move
    through the test case input."""

    character_number: int
    current_character: str
    did_errpy_panic: bool
    current_body: str
    got_ast: str
    errors: str
    last_few_chars_input: list[str]
    diff: str


def validate_code_and_agument_errors(pprint_ast: str, errors: str) -> str:
    """We validate the input pprint_ast with CPython and report if there
    is at least one syntax error. The errors input parameter is updated
    in place with any errors"""
    try:
        ast_utils.get_cpython_ast(pprint_ast, raise_exception=True)
    except BaseException as e:
        message = "Output AST is not valid according to CPython: " + str(e)
        if errors:
            errors = errors + message
        else:
            errors = message
    return errors


class ErrorRecoveryCommon(ASTTestCommon):
    def compare_recovered_ast_many(
        self,
        many_fname: str,
        test_dir: str = ERROR_RECOVERY_SPECIFIC_TESTS_DIR,
    ) -> None:
        """Iterate through series of inputs delimited by ## and check pprint ast
        of each against expected recovered pprint ast"""

        # split input into individual tests
        # create directory for holding test results if not one exists already
        # generate expected results in approperiate files
        # gather test failures together for overall testcase failure
        expected_results_fname = many_fname + EXPECTED_RESULTS_POSTFIX

        got_results = ""
        for code_title, test_body in self.splitmany_test_cases(many_fname, test_dir):
            if code_title.startswith("#"):
                continue  # skip copywrite notice

            (pprint_ast, errors), _ = ast_utils.run_errpy(test_body, True)
            errors = validate_code_and_agument_errors(pprint_ast, errors)
            if errors:
                pprint_ast += errors
            got_results += f"##{code_title}\n{pprint_ast.strip()}\n\n"

        self.check_vs_expects_file(
            test_dir, many_fname, expected_results_fname, got_results
        )

    def check_error_recovery_char_by_char(self, many_fname: str) -> None:
        def generator(
            test_body: str, skip_comments_and_whitespace: bool
        ) -> Iterator[ErrorRecoveryTestIteratorStep]:
            character_number = -1
            last_few_chars_input = []
            current_body = ""
            in_comment = False
            for current_character in test_body:
                # we progress char by char
                character_number += 1
                last_few_chars_input.append(current_character)
                if len(last_few_chars_input) > 30:
                    last_few_chars_input.pop(0)

                if skip_comments_and_whitespace:
                    # probably don't want to skip for CST
                    current_body += current_character
                    if current_character == " ":
                        continue

                    if in_comment:
                        if current_character == "\n":
                            in_comment = False
                        else:
                            continue
                    if current_character == "#":
                        in_comment = True
                        continue

                (got_ast, errors), did_errpy_panic = ast_utils.run_errpy(
                    current_body.strip(), True
                )
                got_ast = got_ast.strip()

                errors = validate_code_and_agument_errors(got_ast, errors)

                # show diff of input code and error recovered outcome

                diff = "".join(
                    unified_diff(
                        got_ast.splitlines(keepends=True),
                        current_body.splitlines(keepends=True),
                    )
                )

                yield ErrorRecoveryTestIteratorStep(
                    character_number,
                    current_character,
                    did_errpy_panic,
                    current_body,
                    got_ast,
                    errors,
                    last_few_chars_input,
                    diff,
                )

        self._check_error_recovery(many_fname, ".tailtest.", generator)

    def make_progressive_test_generator(
        self, insert_chars: Optional[str] = None
    ) -> Callable[[str, bool], Iterator[ErrorRecoveryTestIteratorStep]]:
        def generator(
            test_body: str, skip_comments_and_whitespace: bool
        ) -> Iterator[ErrorRecoveryTestIteratorStep]:
            last_few_chars_input = []
            in_comment = False
            for character_number, current_character in enumerate(test_body):
                # we progress char by char
                last_few_chars_input.append(current_character)
                if len(last_few_chars_input) > 30:
                    last_few_chars_input.pop(0)

                if skip_comments_and_whitespace:
                    # probably don't want to skip for CST
                    if current_character == " ":
                        continue

                    if in_comment:
                        if current_character == "\n":
                            in_comment = False
                        else:
                            continue
                    if current_character == "#":
                        in_comment = True
                        continue

                if insert_chars is not None:
                    current_body = (
                        test_body[:character_number]
                        + insert_chars
                        + test_body[character_number:]
                    )
                else:
                    # remove nth test
                    current_body = (
                        test_body[:character_number] + test_body[character_number + 1 :]
                    )

                region_of_interest = current_body[
                    max(character_number - 10, 0) : character_number + 10
                ]

                (got_ast, errors), did_errpy_panic = ast_utils.run_errpy(
                    current_body.strip(), True
                )
                got_ast = got_ast.strip()

                errors = validate_code_and_agument_errors(got_ast, errors)

                # show diff of input code and error recovered outcome

                diff = "".join(
                    unified_diff(
                        got_ast.splitlines(keepends=True),
                        current_body.splitlines(keepends=True),
                    )
                )

                yield ErrorRecoveryTestIteratorStep(
                    character_number,
                    region_of_interest,
                    did_errpy_panic,
                    current_body,
                    got_ast,
                    errors,
                    last_few_chars_input,
                    diff,
                )

        return generator

    def check_error_recovery_insert_keyword(self, many_fname: str) -> None:
        self._check_error_recovery(
            many_fname,
            ".insert_keyword.",
            self.make_progressive_test_generator(" def "),
        )

    def check_error_recovery_insert_garbage(self, many_fname: str) -> None:
        self._check_error_recovery(
            many_fname,
            ".insert_garbage.",
            self.make_progressive_test_generator(" garbage "),
        )

    def check_error_recovery_insert_whitespace(self, many_fname: str) -> None:
        self._check_error_recovery(
            many_fname,
            ".insert_whitespace.",
            self.make_progressive_test_generator(" "),
        )

    def check_error_recovery_nth_removed(self, many_fname: str) -> None:
        self._check_error_recovery(
            many_fname, ".nth_removed.", self.make_progressive_test_generator()
        )

    def _check_error_recovery(
        self,
        many_fname: str,
        test_name: str,
        generator: Callable[[str, bool], Iterator[ErrorRecoveryTestIteratorStep]],
        flavour: str = ERROR_RECOVERY_TESTS_DIR,
        skip_comments_and_whitespace: bool = True,
    ) -> None:
        """Runs a series of tests to investigate error recovery in the following scenarios for the given input:
        1. Progressively building up the input to the parser char by char and seeing how much of an AST can be built for any particular input. - check_error_recovery_char_by_char
        2. Inserting a whitespace ` ` character char by char on the overall input. - check_error_recovery_insert_whitespace
        3. Trying the input with nth character from the input stream removed. - check_error_recovery_nth_removed

        the scenarious of implemented as generators operating over the given input

        We must ensure that:
        1. ERRPY doesn't panic,
        2. ERRPY doesn't produce an empty AST
        3. and we must also ensure that the AST output is as desired in order to satisfy the requirements of the client's
        e.g. supporting dot completion, partial function argument narrowing, etc"""
        # basic 'sanity check' to ensue final state of input will produce a valid AST before continuing
        sanitized_test_cases = self.check_many_cases_in_file(
            many_fname, flavour=flavour
        )

        # split input into individual tests
        # create directory for holding test results if not one exists already
        # generate expected results in approperiate files
        # gather test failures together for overall testcase failure
        expected_results_dir = make_results_dir(
            many_fname + test_name + EXPECTED_RESULTS_POSTFIX, flavour
        )

        fail_cases = []

        for code_title, test_body in sanitized_test_cases:
            if code_title.startswith("#"):
                continue  # skip copywrite notice

            # we prevoiusly validated that the input code is syntatically valid.
            # now we ensure that the input code for the test is formatted as par the
            # implicit formatting scheme of our ast pretty printer - which outputs
            # Python code from the AST.
            # We do this by writing over the test_body with the output of ERRPPY
            (test_body, _), _ = ast_utils.run_errpy(test_body, True)

            expected_results_fname = (
                expected_results_dir
                + code_title.replace(" ", "-")
                + EXPECTED_RESULTS_POSTFIX
            )

            expected_results = None
            try:
                expected_results = read_code(expected_results_fname, flavour=flavour)
            except Exception:
                pass

            results = ""  # help diff review tool undestand this to be generated
            did_errpy_panics = []  # we want to avoid these
            empty_asts = []  # this is usually a bad thing

            for iteration in generator(test_body, skip_comments_and_whitespace):
                (
                    character_number,
                    current_character,
                    did_errpy_panic,
                    current_body,
                    got_ast,
                    got_ast_errors,
                    last_few_chars_input,
                    diff,
                ) = astuple(iteration)
                if diff.strip() == "":
                    diff = "No difference - AST matches, fully recovered!"

                results += "{}. Input. char of interest: '{}':\n\n{}\n{}\nERRPY Recovered AST diff:\n{}\n\n===============================================================================\n".format(
                    character_number,
                    "\\n" if current_character == "\n" else current_character,
                    format_side_by_side(current_body, got_ast),
                    got_ast_errors,
                    diff,
                )
                if did_errpy_panic:
                    did_errpy_panics.append(
                        "{}.->{}<-".format(
                            character_number, "".join(last_few_chars_input)
                        )
                    )

                if got_ast == "":
                    empty_asts.append(
                        "{}.->{}<-".format(
                            character_number, "".join(last_few_chars_input)
                        )
                    )

            if empty_asts:
                results = "Errpy makes empty AST (weird) with input prior to and including:\n{}\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n{}".format(
                    "\n\n".join(empty_asts),
                    results,
                )

            if did_errpy_panics:
                results = "Errpy panics (really bad) with input prior to and including:\n{}\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n{}".format(
                    "\n\n".join(did_errpy_panics),
                    results,
                )

            results = (
                "@" + "generated\n\n" + results
            )  # help diff review tool undestand this to be generated

            self._compare_results(
                code_title,
                expected_results,
                results,
                expected_results_fname,
                flavour,
                fail_cases,
            )

        if fail_cases:
            self.fail("Following subtests failed: %s" % (fail_cases))

    def _compare_results(
        self,
        code_title: str,
        expected_results: Optional[str],
        results: str,
        expected_results_fname: str,
        flavour: str,
        fail_cases: List[str],
    ) -> None:
        if expected_results is None:
            write_file(expected_results_fname, results, flavour=flavour)
            print(
                "No '%s' file defined, so created a new one! Check this document and ensure results match expectation. Future runs will treat contents as correct test result"
                % (expected_results_fname)
            )
            fail_cases.append(code_title)
        else:
            # great now we can check the results against what's expected
            if expected_results != results:
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
                        write_file(new_results_fname, results, flavour=flavour)
                        print("new file write complete")
                    except Exception:
                        print(
                            "new file write failed due to: {}".format(
                                traceback.format_exc()
                            )
                        )

                print("test code: %s failed..." % code_title)
                print("\n\ntest fail\n")

                diff = unified_diff(
                    results.splitlines(keepends=True),
                    expected_results.splitlines(keepends=True),
                )
                print("".join(diff), end="")

                print("\n\n")

                fail_cases.append(code_title)
