# flake8: noqa


def source():
    return 0


def source_with_inferred():
    a = source()
    return a


def inferred_is_propagated():
    return source_with_inferred()


def inferred_sink(taint_left, taint_right, taint_without_feature, untainted):
    __test_sink(taint_left)
    __test_sink(taint_right)
    __test_sink(taint_without_feature)


def sink_is_propagated(argument):
    inferred_sink(argument, None, None, None)


def taint_in_taint_out(arg):
    return arg


def tito_and_sink(arg):
    __test_sink(arg)
    return arg


def tito_is_propagated(arg):
    return taint_in_taint_out(arg)


def attach_without_tito(arg):
    return 0


def no_tito(arg):
    return attach_without_tito(arg)
