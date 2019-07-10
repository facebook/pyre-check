# @nolint
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
