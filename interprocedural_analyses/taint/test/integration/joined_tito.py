# flake8: noqa


class Wrapper:
    def __init__(self, a, b):
        self.a = a
        self.b = b


class C:
    def __init__(self, wrapper: Wrapper) -> None:
        self.x = wrapper
        self.y = wrapper.b


def y_is_benign():
    wrapper = Wrapper(a=__test_source(), b=0)
    c = C(wrapper)
    __test_sink(c.y)
