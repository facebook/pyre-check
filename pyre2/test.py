#!/usr/bin/env fbpython
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
Test that everything works well
"""

from __future__ import annotations
import os
import signal
import subprocess
import sys
import time
from contextlib import contextmanager
from enum import Enum
from pathlib import Path
from typing import Any, ContextManager, Generator, Iterable


class Colors(Enum):
    # Copied from https://stackoverflow.com/questions/287871/how-to-print-colored-text-to-the-terminal
    HEADER = "\033[95m"
    OKBLUE = "\033[94m"
    OKCYAN = "\033[96m"
    OKGREEN = "\033[92m"
    WARNING = "\033[93m"
    FAIL = "\033[91m"
    ENDC = "\033[0m"
    BOLD = "\033[1m"
    UNDERLINE = "\033[4m"


def print_running(msg: str) -> None:
    print(Colors.OKGREEN.value + "Running " + msg + Colors.ENDC.value)


@contextmanager
def timing() -> Generator[None, None, None]:
    start = time.time()
    yield
    duration = time.time() - start
    print(f"Finished in {duration:.2f} seconds.")


def run(
    args: Iterable[str],
    capture_output: bool = False,
) -> subprocess.CompletedProcess[str]:
    """
    Runs a command (args) in a new process.
    If the command fails, raise CalledProcessError.
    If the command passes, return CompletedProcess.
    If capture_output is False, print to the console, otherwise record it as CompletedProcess.stdout/stderr.
    If error is specified, print error on stderr when there is a CalledProcessError.
    """
    # On Ci stderr gets out of order with stdout. To avoid this, we need to flush stdout/stderr first.
    sys.stdout.flush()
    sys.stderr.flush()
    try:
        # @lint-ignore FIXIT1 NoUnsafeExecRule
        result = subprocess.run(
            tuple(args),
            # We'd like to use the capture_output argument,
            # but that isn't available in Python 3.6 which we use on Windows
            stdout=subprocess.PIPE if capture_output else sys.stdout,
            stderr=subprocess.PIPE if capture_output else sys.stderr,
            check=True,
            encoding="utf-8",
        )
        return result
    except subprocess.CalledProcessError as e:
        # Print the console info if we were capturing it
        if capture_output:
            print(e.stdout, file=sys.stdout)
            print(e.stderr, file=sys.stderr)
        sys.exit(1)


def rustfmt() -> None:
    print_running("arc f")
    run(["arc", "f"])


def clippy() -> None:
    print_running("arc rust-clippy ...")
    run(
        [
            "arc",
            "rust-clippy",
            "...",
            "--reuse-current-config",
        ]
    )


def test() -> None:
    if "SANDCASTLE_NONCE" in os.environ:
        print("Skipping tests on CI because they're already scheduled.")
        return

    print_running("buck2 test kind('rust_test|rust_library', ...)")
    res = run(
        [
            "buck2",
            "uquery",
            "kind('rust_test|rust_library', ...)",
            "--reuse-current-config",
        ],
        capture_output=True,
    )
    tests = [line.strip() for line in res.stdout.splitlines()]
    run(
        ["buck2", "test", "--reuse-current-config"]
        + tests
        + ["--", "--run-disabled", "--return-zero-on-skips"]
    )


def conformance() -> None:
    print_running("conformance_output_script ...")
    run(
        [
            "buck2",
            "run",
            "--reuse-current-config",
            "conformance:conformance_output_script",
            "--",
            "./conformance/third_party",
        ]
    )


def main() -> None:
    # Change to the target_determinator directory
    script_dir = Path(__file__).parent.absolute()
    os.chdir(str(script_dir))

    with timing():
        rustfmt()
    with timing():
        clippy()
    with timing():
        test()
    with timing():
        conformance()


def invoke_main() -> None:
    try:
        main()
    except KeyboardInterrupt:
        # no stack trace on interrupt
        sys.exit(signal.SIGINT)


if __name__ == "__main__":
    # Do not add code here, it won't be run. Add them to the function called below.
    invoke_main()  # pragma: no cover
