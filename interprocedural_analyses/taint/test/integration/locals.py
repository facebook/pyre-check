# flake8: noqa

from builtins import __test_sink, __test_source


def named_sink(x):
    __test_sink(x)


def locals_to_sink():
    # No issue before assigning.
    __test_sink(locals()["x"])
    x = __test_source()

    __test_sink(locals()["x"])
    __test_sink(locals()["y"])

    # We properly handle named parameters through `**`.
    named_sink(**locals())


# Note the limitation in the model - we won't track that `x` flows to a sink..
def source_parameter_to_sink(x, y):
    __test_sink(locals()["x"])
    __test_sink(locals()["y"])
