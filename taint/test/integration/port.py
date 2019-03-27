# @nolint


def source_field():
    result = {}
    result.a = __test_source()
    return result


def sink_field(arg):
    __test_sink(arg.a)


def match_flows():
    x = source_field()
    sink_field(x)


def star_arg(x, *data, **kwargs):
    sink_field(data[1])


def star_arg_wrapper(x, *data, **kwargs):
    star_arg(x, *data, **kwargs)


def match_star_arg_with_star():
    data = [0, source_field(), 2]
    star_arg_wrapper("a", *data)


def match_star_arg_directly():
    star_arg_wrapper("a", "b", source_field(), "d")


def star_star_arg(x, **kwargs):
    sink_field(kwargs["arg"])


def star_star_arg_wrapper(x, **kwargs):
    star_star_arg(x, **kwargs)


def match_star_star_arg_with_star():
    data = {"a": 0, "arg": source_field()}
    star_star_arg_wrapper("a", **data)


def match_star_star_arg_directly():
    star_star_arg_wrapper("a", "b", arg=source_field())


class Foo:
    # @cached_property
    def some_source():
        return __test_source()


def refer_to_method_as_field(foo: Foo):
    # This comes up in Instagram due to @cached_property decorators
    taint = foo.some_source
    __test_sink(taint)
