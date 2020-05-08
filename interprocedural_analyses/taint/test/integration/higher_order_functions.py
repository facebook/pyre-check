def goes_to_sink(arg):
    __test_sink(arg)


def has_tito(arg):
    return arg


def higher_order_function(f, arg):
    f(arg)


def test_higher_order_function():
    higher_order_function(goes_to_sink, __test_source())


class C:
    def method_to_sink(self, arg):
        __test_sink(arg)

    def self_to_sink(self):
        __test_sink(self)


def higher_order_method(c: C, arg):
    higher_order_function(c.method_to_sink, arg)


def test_higher_order_method():
    higher_order_method(C(), __test_source())


def test_higher_order_method_self():
    c: C = __test_source()
    higher_order_function(c.self_to_sink)


def higher_order_function_and_sink(f, arg):
    f(arg)
    __test_sink(arg)


def test_higher_order_function_and_sink():
    higher_order_function_and_sink(goes_to_sink, __test_source())


def test_higher_order_tito(x):
    return higher_order_function(has_tito, x)


def apply(f, x):
    return f(x)


def source_through_tito():
    x = __test_source()
    y = apply(has_tito, x)
    return y
