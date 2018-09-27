# @nolint


def bar():
    return __testSource()


def qux(arg):
    __testSink(arg)


def bad(arg):
    qux(arg)


def some_source():
    return bar()


def match_flows():
    x = some_source()
    bad(x)
