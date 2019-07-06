# @nolint


def test_from_1_to_0():
    x = 0
    change_arg0(x, __test_source())
    return x


def test_from_0_to_1():
    y = 0
    change_arg1(__test_source(), y)
    return y


def test_from_1_to_0_nested():
    x = {}
    change_arg0(x.foo, __test_source())
    return x.foo


def test_from_1_to_0_nested_distinct():
    x = {}
    change_arg0(x.foo, __test_source())
    return x.bar


def test_list_append():
    l = MyList()
    l.append(__test_source())
    return l


def wrapper_from_1_to_0(x, y):
    change_arg0(x, y)


def wrapper_from_0_to_1(x, y):
    change_arg1(x, y)


def to_sink_via_side_effect(y):
    x = {}
    change_arg0(x, y)
    __test_sink(x)


# Mocks that have models


def change_arg0(arg0, arg1):
    ...


def change_arg1(arg0, arg1):
    ...


class MyList:
    def append(self, arg):
        pass
