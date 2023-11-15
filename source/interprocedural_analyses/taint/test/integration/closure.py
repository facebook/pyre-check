# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source


def nonlocal_closure_read_reduction():
    x = _test_source()
    z = _test_source()

    def inner(z):
        _test_sink(x)
        _test_sink(z)
    inner(z)


def taint_propagation():
    x = _test_source()
    z = _test_source()
    wrapper_for_taint_propagation(x, z)


def wrapper_for_taint_propagation(x, z):
    def inner(z):
        _test_sink(x)
        _test_sink(z)
    inner(z)


def taint_propagation_hof():
    x = _test_source()
    z = _test_source()
    wrapper_for_taint_propagation_hof(x, z)


def wrapper_for_taint_propagation_hof(x, z):
    def inner(z):
        _test_sink(x)
        _test_sink(z)

    higher_order_function(inner, z)


def higher_order_function(f, z):
    f(z)


def parameter_order_swap(x, y, z):
    def inner():
        _test_sink(y)
        _test_sink(x)
        _test_sink(z)
    inner()


def parameter_order_swap_different_variable_names(x, y, z):
    a, b, c = x, y, z

    def inner():
        _test_sink(b)
        _test_sink(a)
        _test_sink(c)
    inner()


def nonlocal_closure_multiple_reads():
    x = _test_source()

    def conditional_read(conditional):
        if conditional:
            _test_sink(x)
        else:
            _test_sink(0)
    conditional_read()

    def overread():
        y = x
        y = 0
        _test_sink(y)
    overread()

    x = 0
    conditional_read()


class Object:
    pass


def closure():
    obj = Object()

    def source():
        obj.x = _test_source()

    def sink():
        _test_sink(obj.x)

    return source, sink


def closure_flow():
    # TODO(T168869049): False Negative
    source, sink = closure()
    source()
    sink()


def closure_no_flow():
    source, sink = closure()
    sink()
    source()


def nonlocal_closure_write_reduction():
    x = ""
    z = ""

    def source():
        nonlocal x
        x = _test_source()
        return _test_source()

    z = source()
    _test_sink(x)
    _test_sink(z)


def nonlocal_closure_reduction():
    obj1 = ""

    def source():
        nonlocal obj1
        obj1 = _test_source()
        obj2 = _test_source()
        return obj2

    def sink(obj2):
        _test_sink(obj1)
        _test_sink(obj2)

    obj2 = source()
    sink(obj2)


def nonlocal_closure_multiple_writes():
    x = 1

    def conditional_write(condition):
        nonlocal x
        if condition:
            x = _test_source()
        else:
            x = 0
    # Note: The truthiness of the conditional isn't actually used
    conditional_write(True)
    _test_sink(x)

    def overwrite():
        nonlocal x
        x = _test_source()
        x = 0
    x = 1
    overwrite()
    _test_sink(x)


def nonlocal_closure_flow():
    obj = ""

    def source():
        nonlocal obj
        obj = _test_source()

    def sink():
        _test_sink(obj)

    source()
    sink()


def nonlocal_closure_no_flow():
    obj = ""

    def source():
        nonlocal obj
        obj = _test_source()

    def sink():
        _test_sink(obj)

    sink()
    source()


def nonlocal_closure_inner_flow():
    obj = ""

    def flow():
        nonlocal obj
        obj = _test_source()
        _test_sink(obj)

    flow()


def nonlocal_closure_obscure():
    obj = ""

    def source():
        nonlocal obj
        obj = _test_source()

    def sink():
        _test_sink(obj)

    return source, sink


def nonlocal_obscure_flow():
    # TODO(T168868830): FN due to not knowing the returned functions match
    # the models of functions defined in nonlocal_closure_obscure
    source, sink = nonlocal_closure_obscure()
    source()
    sink()


def nonlocal_obscure_no_flow():
    source, sink = nonlocal_closure_obscure()
    sink()
    source()


def nonlocal_closure_nested_flow():
    outer = ""

    def source1():
        inner = ""
        def source2():
            def source3():
                nonlocal inner
                inner = _test_source()
            source3()
            _test_sink(inner)
        source2()
        _test_sink(inner)
        nonlocal outer
        outer = inner

    source1()
    _test_sink(outer)


def nonlocal_closure_wrapper_flow():
    obj = ""

    def source():
        nonlocal obj
        obj = _test_source()

    def wrapper():
        source()

    wrapper()
    _test_sink(obj) # TODO(T169118550): FN


def _test_source2(): ...


def nonlocal_closure_conditional_write():
    obj = _test_source()

    def conditional_write(cond):
        nonlocal obj
        if cond:
            obj = _test_source2()

    def clear():
        nonlocal obj
        obj = 0

    conditional_write(True)
    _test_sink(obj)

    obj = 0
    obj = _test_source()
    clear()
    # TODO(T169657906): [FP] Overwrite taint on nonlocal writes
    _test_sink(obj)
