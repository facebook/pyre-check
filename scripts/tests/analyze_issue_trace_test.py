#!/usr/bin/env fbpython
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest

from ..analyze_issue_trace import CallGraph


CALL_GRAPH = {
    "test.nested_run.do_the_thing": ["Obj{test.glob}", "Ovr{list::append}"],
    "test.$toplevel": [
        "Obj{test.glob}",
        "object.__init__",
        "object.__new__",
        "test.main",
        "str.__eq__",
        "str.__ne__",
    ],
    "test.call_multi_path4": ["test.multi_path2", "test.multi_path4"],
    "test.cycle_call1": ["test.cycle_call2"],
    "test.cycle_call2": ["test.cycle_call3"],
    "test.cycle_call3": ["test.cycle_call1"],
    "test.get_these": [
        "test.call_multi_path4",
        "test.cycle_call1",
        "test.immediate_examples",
        "test.leak_globals_by_passing_in",
        "test.leak_globals_by_transitive_call",
        "test.long_chain1",
        "test.multi_path1",
        "test.nested_run",
        "test.self_call",
        "test.transitive_call_accessing_globals",
    ],
    "test.immediate_examples": ["Obj{test.glob}", "Ovr{list::append}"],
    "test.leak_globals_by_passing_in": [
        "Obj{test.glob}",
        "test.transitive_call_with_globals_passed_in",
    ],
    "test.leak_globals_by_transitive_call": ["test.transitive_call_accessing_globals"],
    "test.long_chain1": ["test.long_chain2"],
    "test.long_chain2": ["test.long_chain3"],
    "test.long_chain3": ["test.long_chain4"],
    "test.long_chain4": ["print"],
    "test.main": [
        "Ovr{test.MyClass::this_one_too}",
        "test.get_these",
        "test.this_one_shouldnt_be_found",
    ],
    "test.multi_path1": ["test.multi_path2", "test.multi_path3"],
    "test.multi_path2": ["test.multi_path3", "test.multi_path4"],
    "test.multi_path3": ["test.multi_path4"],
    "test.multi_path4": ["print"],
    "test.nested_run": ["test.nested_run.do_the_thing"],
    "test.self_call": ["test.self_call"],
    "test.this_one_shouldnt_be_found": ["Obj{test.glob}", "Ovr{list::append}"],
    "test.transitive_call_accessing_globals": ["Obj{test.glob}", "Ovr{list::append}"],
    "test.transitive_call_with_globals_passed_in": ["Ovr{list::append}"],
    "test.MySubclass.this_one_too": ["Obj{test.glob}", "Ovr{list::append}"],
}


class AnalyzeIssueTraceTest(unittest.TestCase):
    def test_load_call_graph_happy_path(self) -> None:
        call_graph = {
            "my_module.my_function": [
                "something_that.my_function_calls",
                "builtins.print",
                "my_module.my_function",
            ],
            "something_that.my_function_calls": ["int.__str__"],
        }

        CallGraph.validate_call_graph(call_graph)
        result = CallGraph.json_to_call_graph(call_graph)

        self.assertEqual(len(result), 2)
        self.assertSetEqual(result["something_that.my_function_calls"], {"int.__str__"})
        self.assertSetEqual(
            result["my_module.my_function"],
            {"something_that.my_function_calls", "builtins.print"},
        )

    def test_load_call_graph_bad_root(self) -> None:
        call_graph = ["1234"]

        try:
            CallGraph.validate_call_graph(call_graph)
            self.fail("should have thrown")
        except ValueError:
            pass

    def test_load_call_graph_bad_callers(self) -> None:
        call_graph = {"caller": 1234}

        try:
            CallGraph.validate_call_graph(call_graph)
            self.fail("should have thrown")
        except ValueError:
            pass

    def test_load_call_graph_bad_callees(self) -> None:
        call_graph = {"caller": [1, 2, 3]}

        try:
            CallGraph.validate_call_graph(call_graph)
            self.fail("should have thrown")
        except ValueError:
            pass

    def test_create_dependency_graph(self) -> None:
        call_graph = {
            "parent.function.one": {
                "child_function.one",
                "child_function.two",
                "child_function.three",
            },
            "parent.function.two": {
                "child_function.one",
                "child_function.two",
            },
            "child_function.one": {"child_function.two"},
        }

        expected_dependency_graph = {
            "child_function.one": {
                "parent.function.one",
                "parent.function.two",
            },
            "child_function.two": {
                "parent.function.one",
                "parent.function.two",
                "child_function.one",
            },
            "child_function.three": {
                "parent.function.one",
            },
        }

        actual_dependency_graph = CallGraph.create_dependency_graph(call_graph)

        self.assertSetEqual(
            set(actual_dependency_graph), set(expected_dependency_graph)
        )
        for callee, expected_callers in expected_dependency_graph.items():
            with self.subTest(f"Callee: {callee}"):
                actual_callers = actual_dependency_graph[callee]

                self.assertSetEqual(actual_callers, expected_callers)
