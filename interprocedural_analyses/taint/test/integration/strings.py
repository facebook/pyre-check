# flake8: noqa
def concatenate_lhs(source: str):
    return source + "A"


def concatenate_rhs(source: str):
    return "A" + source


def bad_1():
    a = concatenate_lhs(__test_source())
    __test_sink(a)


def bad_2():
    a = concatenate_rhs(__test_source())
    __test_sink(a)


def either(b: bool):
    if b:
        a = concatenate_lhs(__test_source())
    else:
        a = concatenate_rhs(__test_source())
    __test_sink(a)


def maybe_lhs(b: bool):
    if b:
        a = concatenate_lhs(__test_source())
    else:
        a = __test_source()
    __test_sink(a)


def maybe_rhs(b: bool):
    if b:
        a = __test_source()
    else:
        a = concatenate_rhs(__test_source())
    __test_sink(a)
