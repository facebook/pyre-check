#!/usr/bin/env fbpython
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import ast
import time

from tools.pyre.source.errpy import ffi_python

RESOURCES_DIR = "errpy/benchmark/benchmark_resources/"
NANO_TO_MS = 1e-6


class BenchmarkRunner:
    """For each benchmark return the time in nanoseconds to invoke the parser
    on 'iterations' number of times with 'warm_up_cycles' iterations as
    warm ups. Note that for ERRPY 'warm_up_cycles' is less relevant as
    its not JIT compiled (but then CPython isnt currently either)"""

    def __init__(
        self, source: str, warm_up_cycles: int = 1000, iterations: int = 100000
    ) -> None:
        self.source = source
        self.warm_up_cycles = warm_up_cycles
        self.iterations = iterations

    def run_cpython(self) -> int:
        source = self.source
        for _ in range(self.warm_up_cycles):
            ast.parse(source)

        iters = range(self.iterations)
        # WARNING: be careful about code put between tick and toc as this can
        # affect the benchmarking
        tick = time.time_ns()
        for _ in iters:
            ast.parse(source)
        toc = time.time_ns()
        return toc - tick

    def run_errpy(self) -> int:
        source = self.source
        for _ in range(self.warm_up_cycles):
            ffi_python.py_parse_module(source)

        iters = range(self.iterations)
        # WARNING: be careful about code put between tick and toc as this can
        # affect the benchmarking
        tick = time.time_ns()
        for _ in iters:
            ffi_python.py_parse_module(source)
        toc = time.time_ns()
        return toc - tick


def main() -> int:
    """DOC_STRING"""
    source_filename = RESOURCES_DIR + "simple.pytest"

    with open(source_filename) as fobj:
        source = fobj.read()

    runner = BenchmarkRunner(source)
    ns_cpython = runner.run_cpython()
    ns_errpy = runner.run_errpy()

    warm_up = runner.warm_up_cycles
    iterations = runner.iterations

    print(
        "Cpython vs Errpy benchmark complete! ({} warm ups, {} iterations each)".format(
            warm_up, iterations
        )
    )
    print(
        "============================================================================\n"
    )
    print(
        "CPython: {} ns || {:.2f} ms per run of sample input".format(
            ns_cpython, ns_cpython / iterations * NANO_TO_MS
        )
    )
    print(
        "Errpy:   {} ns || {:.2f} ms per run of sample input\n".format(
            ns_errpy, ns_errpy / iterations * NANO_TO_MS
        )
    )
    if ns_errpy > ns_cpython:
        pct = (ns_errpy - ns_cpython) / ns_cpython * 100.0
        print("Errpy is slower than CPython by: {:.2f} % \n".format(pct))
    else:
        pct = (ns_cpython - ns_errpy) / ns_errpy * 100.0
        print("CPython is slower than Errpy by: {:.2f} % \n".format(pct))

    return 0


if __name__ == "__main__":
    main()
