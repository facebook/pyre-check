# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import Union
import random


class Node:
    def __init__(self, id) -> None:
        self.id = id

    def send(self, vc) -> None: ...

    @classmethod
    def get(cls, id) -> "Node":
        return cls(id)


def user_controlled_input():
    return "evil"


def permissive_context():
    return 0


def combine_tainted_user_and_dangerous_vc():
    id = user_controlled_input()
    vc = permissive_context()
    Node.get(id).send(vc)


def demonstrate_triggered_context(vc):
    id = user_controlled_input()
    Node.get(id).send(vc)


def demonstrate_triggered_input(id):
    vc = permissive_context()
    Node.get(id).send(vc)


def issue_with_triggered_input():
    id = user_controlled_input()
    demonstrate_triggered_input(id)


def issue_with_triggered_context():
    vc = permissive_context()
    demonstrate_triggered_context(vc)


def no_issue_with_wrong_label():
    vc = permissive_context()
    demonstrate_triggered_input(vc)


def test_other_input():
    return "other"


def combines_tests_and_context(test, vc):
    return None


def a_source():
    return None


def b_source():
    return None


def issue_with_test_a_and_b():
    combines_tests_and_context(a_source(), permissive_context())
    combines_tests_and_context(b_source(), permissive_context())


def a_sink(arg):
    return


def b_sink(arg):
    return


def transform_t(arg):
    return


def sanitize_source_a_tito(arg):
    return arg


def sanitize_source_b_tito(arg):
    return arg


def sanitize_sink_a_tito(arg):
    return arg


def no_issue_with_transform():
    x = a_source()
    y = transform_t(x)
    combines_tests_and_context(y, permissive_context())


def no_sink_with_transform(x):
    y = transform_t(x)
    combines_tests_and_context(a_source(), y)


def issue_with_sanitizer():
    x = a_source()
    y = sanitize_sink_a_tito(x)
    combines_tests_and_context(y, permissive_context())


def no_sink_with_sanitizer(x):
    y = sanitize_source_b_tito(sanitize_source_a_tito(x))
    combines_tests_and_context(y, permissive_context())


def user_controlled_input_wrapper():
    return user_controlled_input()


def demonstrate_triggered_context_more_hops(vc):
    # More hops enable showing the source trace as a subtrace
    id = user_controlled_input_wrapper()
    Node.get(id).send(vc)


def issue_with_triggered_context_more_hops():
    vc = permissive_context()
    demonstrate_triggered_context_more_hops(vc)


class A:
    def multi_sink(self, user_controlled, permissive_context):
        pass


class B:
    def multi_sink(self, user_controlled, permissive_context):
        pass


def muliple_main_issues_1(a_or_b: Union[A, B]):
    # Due to multiple potential callables at the same call site
    a_or_b.multi_sink(user_controlled_input(), permissive_context())


def muliple_main_issues_2():
    # Due to joining the issue handles from multiple call sites
    vc = permissive_context()
    multiple_triggered_context(vc)


def multiple_triggered_context(vc):
    id1 = user_controlled_input()
    Node.get(id1).send(vc)
    id2 = user_controlled_input()
    Node.get(id2).send(vc)


def wrapper_node_get_send(id, vc):
    Node.get(id).send(vc)


def issue_with_wrapper_node_get_send():
    id = user_controlled_input()
    vc = permissive_context()
    wrapper_node_get_send(id, vc)


def wrapper_node_get_send_triggered_context(vc):
    id = user_controlled_input()
    # We should see a triggered partial sink here
    wrapper_node_get_send(id, vc)


def issue_with_wrapper_node_get_send_triggered_context():
    vc = permissive_context()
    wrapper_node_get_send_triggered_context(vc)


def wrapper_node_send(id, vc):
    # Expect no partial sink, because we need two partial sinks on different
    # parameters to file an issue
    id = 0
    Node.get(id).send(vc)


def wrapper_combined_node_get_send(combined):
    # Expect no partial sink, because we need two partial sinks on different
    # parameters to file an issue
    id = None
    vc = None
    if random.random() > 0.5:
        id = combined
    else:
        vc = combined
    Node.get(id).send(vc)


def wrapper_mismatched_partial_sinks(id, vc):
    # Expect no partial sink, because the two partial sinks are from different
    # call sites.
    Node.get(id).send(0)
    Node.get(0).send(vc)


# Share both partial sink kinds in multiple rules
def c_source(): ...


def d_source(): ...


def e_source(): ...


def multi_sink_share_both_sinks(x, y): ...


def demonstrate_triggered_c_from_d_and_e(x):
    multi_sink_share_both_sinks(x, d_source())
    multi_sink_share_both_sinks(x, e_source())


def issue_with_triggered_c_from_d_and_e():
    # Should see two issues
    demonstrate_triggered_c_from_d_and_e(c_source())


def demonstrate_triggered_c_from_d(x):
    multi_sink_share_both_sinks(x, d_source())


def issue_with_triggered_c_from_d():
    # Should see one issue
    demonstrate_triggered_c_from_d(c_source())


def demonstrate_triggered_c_from_d_or_e(x):
    if random.random() > 0.5:
        multi_sink_share_both_sinks(x, d_source())
    else:
        multi_sink_share_both_sinks(x, e_source())


def issue_with_triggered_c_from_d_or_e():
    # Should see two issues
    demonstrate_triggered_c_from_d_or_e(c_source())


def demonstrate_triggered_d_and_e(y):
    multi_sink_share_both_sinks(c_source(), y)


def issue_with_triggered_d_and_e():
    demonstrate_triggered_d_and_e(d_source())
    demonstrate_triggered_d_and_e(e_source())


def combine_c_d_and_c_e():
    multi_sink_share_both_sinks(c_source(), d_source())
    multi_sink_share_both_sinks(c_source(), e_source())
