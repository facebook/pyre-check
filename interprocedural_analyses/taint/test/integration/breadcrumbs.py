# flake8: noqa


def int_source() -> int:
    return __test_source()


def float_source() -> float:
    return __test_source()


def bool_source() -> bool:
    return __test_source()


def int_parameter(x, y: int):
    __test_sink(y)


def float_parameter(x, y: float):
    __test_sink(y)


def bool_parameter(x, y: bool):
    __test_sink(y)
