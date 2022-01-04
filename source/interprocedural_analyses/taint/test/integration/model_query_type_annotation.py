# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import Annotated, Dict, List


def test1_f1(taint_1: int, taint_2: str, taint_3: float):
    pass


def test1_f2(no_taint_1: List[str], no_taint_2: int):
    pass


class Test2_T1:
    ...


class Test2_T2:
    ...


class Test2_Foo:
    ...


class Test2_C:
    def test2_f1(self, taint_1: Test2_T1, taint_2: Test2_T2, no_taint_1: Test2_Foo):
        # self should not be tainted
        pass

    def test2_f2(self, taint_1: Dict[int, Test2_T1]):
        # self should not be tainted
        pass

    def test2_f3(self, no_taint_1: int, no_taint_2: str):
        # self should not be tainted
        pass


def test3_f1(
    taint_1: Annotated[str, "foo"], no_taint_1: str, taint_2: Annotated[int, "bar"]
):
    pass


def test3_f2(no_taint_1: List[Annotated[str, "foo"]], no_taint_2: int):
    pass


def test4_taint_1(x) -> str:
    pass


def test4_no_taint_1(x) -> int:
    pass


class Test5_T1:
    ...


class Test5_T2:
    ...


class Test5_Foo:
    ...


def test5_taint_1(x) -> Test5_T1:
    pass


def test5_taint_2(x) -> Test5_T2:
    pass


def test5_no_taint_1(x) -> Test5_Foo:
    pass


class Test6_C:
    def test6_taint_1(self, x) -> Annotated[str, "foo"]:
        pass

    def test6_taint_2(self, x) -> Annotated[List[str], "bar"]:
        pass

    def test6_no_taint_1(self, x) -> str:
        pass


class Test7_C:
    taint_1: int = 0
    taint_2: int = 0
    no_taint_1: List[int] = []
    no_taint_2: str = ""


class Test8_C:
    taint_1: str = ""
    taint_2: List[str] = []
    no_taint_1: List[int] = []
    no_taint_2: int = 0


class Test9_C:
    taint_1: Annotated[str, "foo"] = ""
    taint_2: Annotated[int, "bar"] = 0
    no_taint_1: List[int] = []
    no_taint_2: int = 0
