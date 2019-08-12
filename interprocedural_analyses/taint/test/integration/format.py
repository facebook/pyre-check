# @nolint


def source_via_format():
    taint = __test_source()
    return f"{taint} is bad"


def tito_via_format(arg1, arg2, arg3):
    return f"{arg1} and {arg2} but not arg3"


def sink_via_format(arg):
    __test_sink(f"{arg}")
