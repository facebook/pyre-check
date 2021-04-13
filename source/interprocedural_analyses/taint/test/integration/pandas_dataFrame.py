# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import List, overload


class Series:
    def __init__(self) -> None:
        pass


class DataFrame:
    def __init__(self) -> None:
        pass

    @overload
    def __getitem__(self, key: str) -> Series:
        pass

    @overload
    def __getitem__(self, key: List[str]) -> "DataFrame":
        pass

    def __getitem__(self, key):
        return None

    @overload
    def __setitem__(self, key: str, newvalue: Series) -> None:
        pass

    @overload
    def __setitem__(self, key: List[str], newvalue: "DataFrame") -> None:
        pass

    def __setitem__(self, key, newvalue):
        return None


def sink(arg: DataFrame):
    pass


def source() -> DataFrame:
    pass


def clear_df() -> DataFrame:
    pass


def issue1():
    df = source()
    df["a"] = df["b"]
    sink(df)


def issue2():
    df = source()
    df2 = df[["a", "b"]]
    sink(df2)


def issue3():
    df = source()
    df2 = clear_df()
    df2["a"] = df["b"]
    sink(df2)


def issue4():
    df = source()
    df2 = clear_df()
    df2[["b", "a"]] = df[["a", "b"]]
    sink(df2)


def issue5():
    df = source()
    var = "a"
    df["b"] = df[var]
    sink(df)
