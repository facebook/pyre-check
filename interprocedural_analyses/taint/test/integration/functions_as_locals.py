# flake8: noqa


def foo(arg):
    __test_sink(arg)


def foo_as_local():
    x = __test_source()
    f = foo
    foo(x)
    f(x)


def local_tito(arg):
    f = foo
    f(arg)


class C:
    def m(self, arg):
        __test_sink(arg)


def local_function_with_method_sink(c: C):
    f = c.m
    x = __test_source()
    c.m(x)
    f(x)


def method_tito(c: C, arg):
    f = c.m
    f(arg)
