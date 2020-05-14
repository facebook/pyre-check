from builtins import __test_sink, __test_source


def bar():
    return __test_source()


def qux(arg):
    __test_sink(arg)


def bad(ok, arg):
    qux(arg)


def some_source():
    return bar()


def match_flows():
    x = some_source()
    bad(5, x)
