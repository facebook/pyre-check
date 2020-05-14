from builtins import __test_sink, __test_source


class StaticClass:
    @staticmethod
    def sink(oops):
        __test_sink(oops)


def test(source):
    return StaticClass.sink(source)


def run_test(source):
    test(__test_source())
