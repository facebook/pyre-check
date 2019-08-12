# @nolint

from enum import Enum


class CustomEnum(Enum):
    TRACKED_FIELD = "A"
    UNTRACKED_field = "B"
    untracked_field = "C"


def tracked_index():
    d = {}
    d[CustomEnum.TRACKED_FIELD] = __test_source()
    return d[CustomEnum.TRACKED_FIELD]


def untracked_index_a():
    d = {}
    d[CustomEnum.untracked_field] = __test_source()
    return d[CustomEnum.untracked_field]


def untracked_index_b():
    d = {}
    d[CustomEnum.UNTRACKED_field] = __test_source()
    return d[CustomEnum.UNTRACKED_field]


CONSTANT_A = "A"
CONSTANT_B = {"a": "b"}
untracked_constant = "1"


def tracked_constant_A():
    d = {}
    d[CONSTANT_A] = __test_source()
    return d[CONSTANT_A]


def tracked_constant_B():
    d = {}
    d[CONSTANT_B] = __test_source()
    return d[CONSTANT_B]


def test_untracked_constant():
    d = {}
    d[untracked_constant] = __test_source()
    return d[untracked_constant]
