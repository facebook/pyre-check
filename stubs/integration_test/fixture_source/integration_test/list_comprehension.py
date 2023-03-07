# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa

from typing import List

from integration_test.taint import source, sink


class Sink:
    def run(self, command: str) -> str:
        sink(command)
        return ""


def take_input() -> None:
    sinks: List[Sink] = [Sink()]
    result = [s.run(source()) for s in sinks]


def inductive_comprehension_sink(arguments: List[str]) -> None:
    command = "  ".join(argument.lower() for argument in arguments)
    sink(command)


def eval_via_comprehension_sink() -> None:
    inductive_comprehension_sink(source())
