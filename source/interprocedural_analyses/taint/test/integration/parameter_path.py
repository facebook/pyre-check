# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source


def source_on_first():
    return 1, 0


def source_on_second():
    return 0, 1


def source_on_0_1():
    return ((0, 1), (0, 0))


def issue_only_with_source_first():
    issue, no_issue = source_on_first()
    _test_sink(issue)
    _test_sink(no_issue)


def issue_only_with_source_second():
    no_issue, issue = source_on_second()
    _test_sink(no_issue)
    _test_sink(issue)


def issue_only_with_source_nested_first():
    first, second = source_on_0_1()
    a, issue = first
    c, d = second
    _test_sink(issue)
    _test_sink(a)
    _test_sink(c)
    _test_sink(d)
    return source_on_0_1()


def source_on_key_a():
    return {"a": 1}


def issue_only_with_source_key_a():
    d = source_on_key_a()
    _test_sink(d["a"])
    _test_sink(d["b"])


def source_on_member_a():
    ...


def issue_with_source_member():
    x = source_on_member_a()
    _test_sink(x.a)
    _test_sink(x.b)


def sink_on_first(arg):
    return


def sink_on_second(arg):
    return


def sink_on_0_1(arg):
    return


def issue_only_with_sink_first():
    sink_on_first(arg=(_test_source(), 0))
    sink_on_first(arg=(0, _test_source()))


def issue_only_with_sink_second():
    sink_on_second(arg=(_test_source(), 0))
    sink_on_second(arg=(0, _test_source()))


def issue_only_with_sink_nested_first():
    sink_on_0_1(arg=((_test_source(), 0), (0, 0)))
    sink_on_0_1(arg=((0, _test_source()), (0, 0)))
    sink_on_0_1(arg=((0, 0), (_test_source(), 0)))
    sink_on_0_1(arg=((0, 0), (0, _test_source())))


def sink_on_key_a(arg):
    return


def sink_on_member_a(arg):
    return


def issue_only_with_sink_key_a():
    sink_on_key_a({"a": _test_source(), "b": 0})
    sink_on_key_a({"a": 0, "b": _test_source()})


def issue_with_sink_member():
    x = object()
    x.a = _test_source()
    sink_on_member_a(x)

    y = object()
    y.b = _test_source()
    sink_on_member_a(y)


def tito_from_first(arg):
    return


def tito_from_second(arg):
    return


def issue_tito_from_first():
    _test_sink(tito_from_first(arg=(_test_source(), 0)))
    _test_sink(tito_from_first(arg=(0, _test_source())))


def issue_tito_from_second():
    _test_sink(tito_from_second(arg=(_test_source(), 0)))
    _test_sink(tito_from_second(arg=(0, _test_source())))


def tito_from_first_to_second(arg):
    return


def issue_tito_first_to_second():
    _test_sink(tito_from_first_to_second(arg=(_test_source(), 0))[0])
    _test_sink(tito_from_first_to_second(arg=(0, _test_source()))[0])
    _test_sink(tito_from_first_to_second(arg=(_test_source(), 0))[1])
    _test_sink(tito_from_first_to_second(arg=(0, _test_source()))[1])


def tito_from_b_to_a(arg):
    return


def issue_tito_b_to_a():
    _test_sink(tito_from_b_to_a({"a": _test_source(), "b": 0})["a"])
    _test_sink(tito_from_b_to_a({"a": 0, "b": _test_source()})["a"])
    _test_sink(tito_from_b_to_a({"a": _test_source(), "b": 0})["b"])
    _test_sink(tito_from_b_to_a({"a": 0, "b": _test_source()})["b"])


def tito_from_a_to_self_b(self, arg):
    return


def issue_tito_from_a_to_self_b():
    x = {}
    tito_from_a_to_self_b(x, {"a": _test_source(), "b": 0})
    _test_sink(x["a"])
    _test_sink(x["b"])

    x = {}
    tito_from_a_to_self_b(x, {"a": 0, "b": _test_source()})
    _test_sink(x["a"])
    _test_sink(x["b"])


def complex_tito(arg):
    return


def issue_complex_tito():
    _test_sink(complex_tito({"a": {"x": {_test_source(): 0}}})["foo"])  # Issue.
    _test_sink(complex_tito({"a": {"x": {0: _test_source()}}})["foo"])
    _test_sink(complex_tito({"a": {"x": {_test_source(): 0}}})["bar"])
    _test_sink(complex_tito({"a": {"x": {0: _test_source()}}})["bar"])
    _test_sink(complex_tito({"b": {"x": {_test_source(): 0}}})["foo"])
    _test_sink(complex_tito({"b": {"x": {0: _test_source()}}})["foo"])
    _test_sink(complex_tito({"b": {"x": {_test_source(): 0}}})["bar"])
    _test_sink(complex_tito({"b": {"x": {0: _test_source()}}})["bar"])
