# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import Protocol


class P:
    def __call__(self, arg: str) -> str:
        ...


def returns_p() -> P:
    ...


p: P = returns_p()


def foo() -> str:
    return p("a")


class CallableProtocol(Protocol):
    def __call__(self, arg: str) -> str:
        ...


def returns_callable_protocol() -> CallableProtocol:
    ...


def bar() -> str:
    p = returns_callable_protocol()
    return p("b")
