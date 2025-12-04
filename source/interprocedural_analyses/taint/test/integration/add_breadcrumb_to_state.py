# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from pysa import _test_sink, _test_source


# Annotated with `@AddBreadcrumbToState(Via[add_breadcrumb_to_state])`
def add_breadcrumb_to_state() -> bool:
    return True


def test_if_issue():
    x = _test_source()

    _test_sink(x)  # Issue without breadcrumb

    if add_breadcrumb_to_state():
        _test_sink(x)  # Issue with breadcrumb

    _test_sink(x)  # Issue with breadrcumb


def test_if_source_within_condition():
    x = _test_source()

    if add_breadcrumb_to_state():
        return x

    return None


def test_if_source_outside_condition():
    x = _test_source()

    if add_breadcrumb_to_state():
        return None

    return x  # Source will have the breadcrumb


def test_if_sink_within_condition(x):
    if add_breadcrumb_to_state():
        _test_sink(x)


def test_if_sink_outside_condition(x):
    if add_breadcrumb_to_state():
        return

    _test_sink(x)  # Sink will have the breadcrumb.


def add_breadcrumb_to_state_wrapper() -> bool:
    return add_breadcrumb_to_state()


def test_wrapper():
    x = _test_source()

    # Currently, add_breadcrumb_to_state is not propagated.
    if add_breadcrumb_to_state_wrapper():
        _test_sink(x)  # Issue without breadcrumb


class BreadcrumbOnEnter:
    def __init__(self):
        pass

    # Annotated with `@AddBreadcrumbToState(Via[add_breadcrumb_to_state])`
    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_value, traceback):
        return


def test_add_breadcrumb_context_manager():
    x = _test_source()
    with BreadcrumbOnEnter():
        _test_sink(x)
