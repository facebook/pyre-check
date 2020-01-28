# flake8: noqa


def foo():
    def inner():
        x = __test_source()
        __test_sink(x)

    def inner_with_model():
        return __test_source()
