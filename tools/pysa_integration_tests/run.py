# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
A generic script to run integration tests.
"""

# pyre-strict
import argparse
import logging
import os
import sys
from pathlib import Path
from typing import List, Optional, TYPE_CHECKING

# This script is meant to be run from the command line.
if TYPE_CHECKING:
    import tools.pyre.tools.pysa_integration_tests.runner_lib as test_runner_lib
else:
    import runner_lib as test_runner_lib  # @manual=//tools/pyre/tools/pysa_integration_tests:runner_lib

LOG: logging.Logger = logging.getLogger(__name__)


def main(
    *,
    working_directory: Path,
    filter_issues: bool,
    skip_model_verification: bool,
    run_from_source: bool,
    passthrough_args: Optional[List[str]],
    save_results_to: Optional[Path],
    typeshed: Optional[Path],
    compact_ocaml_heap: bool,
    check_invariants: bool,
    require_pyre_env: bool,
    ignore_positions: bool,
    write_actual_results_on_failure: bool,
    use_pyrefly: bool,
) -> None:
    """
    Entry point function to run a full end-to-end integration test.
    """
    logging.basicConfig(
        level=logging.DEBUG,
        format="%(asctime)s %(levelname)s %(message)s",
    )

    # Verify `PYRE_BINARY` is set
    if require_pyre_env and "PYRE_BINARY" not in os.environ:
        LOG.error(
            "Required environment variable `PYRE_BINARY` is not set. "
            "`make all` from this script's directory will "
            "automatically set `PYRE_BINARY`"
        )
        sys.exit(1)

    LOG.info("Running in `%s`", working_directory)
    pysa_results = test_runner_lib.run_pysa(
        passthrough_args=passthrough_args,
        skip_model_verification=skip_model_verification,
        run_from_source=run_from_source,
        save_results_to=save_results_to,
        typeshed=typeshed,
        compact_ocaml_heap=compact_ocaml_heap,
        check_invariants=check_invariants,
        use_pyrefly=use_pyrefly,
        working_directory=working_directory,
    )

    test_result_directory = (
        save_results_to if save_results_to is not None else working_directory
    )
    test_runner_lib.compare_to_expected_json(
        actual_results=pysa_results,
        test_directory=working_directory,
        test_result_directory=test_result_directory,
        filter_issues=filter_issues,
        ignore_positions=ignore_positions,
        write_actual_results_on_failure=write_actual_results_on_failure,
        use_pyrefly=use_pyrefly,
    )


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Run integeration tests")

    parser.add_argument("--working-directory", type=Path, help="Test working directory")
    parser.add_argument(
        "--filter-issues",
        action="store_true",
        help="Filter out issues with a rule that is not present in the function name",
    )
    parser.add_argument(
        "--skip-model-verification", action="store_true", help="Skip model verification"
    )
    parser.add_argument(
        "--run-from-source",
        action="store_true",
        help="Run pysa from source with the open source setup",
    )
    parser.add_argument(
        "--require-pyre-env",
        action="store_true",
        help="Require the PYRE_BINARY environment variable to be set",
    )
    parser.add_argument(
        "--ignore-positions",
        action="store_true",
        help="Ignore positions when comparing expected results",
    )
    parser.add_argument(
        "--write-actual-results-on-failure",
        action="store_true",
        help="Dump result.actual to stdout on failure. Useful for updating based on CI results.",
    )
    parser.add_argument(
        "--passthrough-args",
        nargs="+",
        help="Additional parameters to pass to `pyre analyze`",
    )
    parser.add_argument(
        "-s",
        "--save-results-to",
        type=Path,
        help="Directory to write analysis results to. Default: output is not saved",
    )
    parser.add_argument(
        "--compact-ocaml-heap",
        action="store_true",
        default=False,
        help="Compact OCaml heap during the analysis to save memory",
    )
    parser.add_argument(
        "--check-invariants",
        action="store_true",
        default=False,
        help="Check abstract domain invariants when running the analysis",
    )
    parser.add_argument(
        "--typeshed",
        type=Path,
        help="Path to the typeshed to use",
    )
    parser.add_argument(
        "--use-pyrefly",
        action="store_true",
        default=False,
        help="Use pyrefly as the type checker, instead of pyre1",
    )

    parsed: argparse.Namespace = parser.parse_args()
    main(
        working_directory=parsed.working_directory or Path(os.getcwd()),
        filter_issues=parsed.filter_issues,
        skip_model_verification=parsed.skip_model_verification,
        run_from_source=parsed.run_from_source,
        passthrough_args=parsed.passthrough_args,
        save_results_to=parsed.save_results_to,
        typeshed=parsed.typeshed,
        compact_ocaml_heap=parsed.compact_ocaml_heap,
        check_invariants=parsed.check_invariants,
        require_pyre_env=parsed.require_pyre_env,
        ignore_positions=parsed.ignore_positions,
        write_actual_results_on_failure=parsed.write_actual_results_on_failure,
        use_pyrefly=parsed.use_pyrefly,
    )
