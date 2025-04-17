# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source


def tito(arg):
    return arg


def transform_x(arg):
    pass


def transform_y(arg):
    pass


def transform_z(arg):
    pass


def transform_yz(arg):
    arg1 = tito(arg)
    arg2 = transform_y(arg1)
    arg3 = tito(arg2)
    arg4 = transform_z(arg3)
    return arg4


def nested_transform_x(arg):
    return transform_x(arg)


def double_nested_transform_x(arg):
    return nested_transform_x(arg)


def sequential_tito_forward():
    x0 = _test_source()
    x1 = nested_transform_x(x0)
    x2 = tito(x1)
    x3 = transform_yz(x2)
    return x3


def sequential_tito_backward(arg):
    arg1 = nested_transform_x(arg)
    arg2 = tito(arg1)
    arg3 = transform_yz(arg2)
    _test_sink(arg3)


def branch_tito_forward(b):
    x0 = _test_source()
    if b:
        x1 = nested_transform_x(x0)
    else:
        x1 = nested_transform_x(x0)
        x1 = transform_y(x1)
    return x1


def branch_tito_backward(arg, b):
    if b:
        arg1 = nested_transform_x(arg)
    else:
        arg1 = transform_y(arg)
    _test_sink(arg1)


def loop_tito_forward():
    x0 = _test_source()
    while 1:
        x0 = transform_x(x0)
    return x0


def loop_tito_backward(arg):
    while 1:
        arg = transform_x(arg)
    _test_sink(arg)


def transform_nested_x_y(arg):
    return transform_y(nested_transform_x(arg))


def source_transform_nested_x_y():
    return transform_nested_x_y(_test_source())


def extra_trace_sink_as_origin():
    source = _test_source()
    source_x = transform_x(source)
    _test_sink(source_x)


class TransformBase:
    def transform(self, arg):
        return arg


class OverrideTransform(TransformBase):
    def transform(self, arg):
        return transform_x(arg)


def extra_trace_through_override(o: TransformBase):
    source = _test_source()
    source_x = o.transform(source)
    _test_sink(source_x)


# ExtraTraceSink is propagated as a sink, while LocalReturn is propagated as tito.
# That means they can be collapsed differently, given the heuristics for tito and sinks are different.
# That might lead to a T:LocalReturn tito being present while the T:ExtraTraceSink is missing,
# which usually means this is a false positive.

def non_deterministic_int() -> int:
    ...


def tito_collapse(x: str) -> str:
    ...


# self[transformed_key] -> TransformX -> LocalReturn
def tito_transform_attribute(self):
    if non_deterministic_int() > 10:
        return transform_x(self['transformed_key'])
    else:
        return tito_collapse(self['transformed_key'])


# Tito self -> TransformX -> LocalReturn
# but TransformX:ExtraTraceSink on self[transformed_key]
def mismatching_tito_extra_sink(self):
    if non_deterministic_int() > 10:
        taint = tito_transform_attribute(self)
    elif non_deterministic_int() > 10:
        taint = self['a']
    elif non_deterministic_int() > 10:
        taint = self['b']
    elif non_deterministic_int() > 10:
        taint = self['c']
    elif non_deterministic_int() > 10:
        taint = self['d']
    elif non_deterministic_int() > 10:
        taint = self['e']
    elif non_deterministic_int() > 10:
        taint = self['f']
    return taint


# Tito self -> TransformX -> LocalReturn but no TransformX:ExtraTraceSink
def transform_tito_with_missing_extra_sink(self):
    self['transformed_key'] = ''
    return mismatching_tito_extra_sink(self)


def no_issue_via_transform_tito_with_missing_extra_sink(arg):
    x = _test_source()
    y = transform_tito_with_missing_extra_sink(x)
    _test_sink(y)


def issue_via_transfom_captured_variable():
    x = _test_source()

    # With model capture(x) -> TransformX -> LocalReturn
    def nested_function():
        return x

    _test_sink(nested_function())
