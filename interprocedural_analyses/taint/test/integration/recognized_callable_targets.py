from builtins import __test_sink, __test_source, to_callable_target


@to_callable_target
def callable_target(arg):
    __test_sink(arg)


def test_callable_target():
    x = __test_source()
    callable_target.async_schedule(x)
