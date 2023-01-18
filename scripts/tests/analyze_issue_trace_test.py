#!/usr/bin/env fbpython
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest

from ..analyze_issue_trace import CallGraph


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

    def test_find_trace_to_parent_simple_path(self) -> None:
        call_graph = CallGraph(
            {
                "function.start": ["function.one"],
                "function.one": ["function.two", "unrelated.call"],
                "function.two": ["function.three"],
                "function.three": ["print"],
                "unrelated.call": ["int"],
            },
            ["function.start"],
        )

        trace = call_graph.find_shortest_trace_to_entrypoint("print")

        self.assertIsNotNone(trace)
        self.assertListEqual(
            trace,
            [
                "print",
                "function.three",
                "function.two",
                "function.one",
                "function.start",
            ],
        )

    def test_find_trace_to_parent_no_path(self) -> None:
        call_graph = CallGraph(
            {
                "function.start": ["function.one"],
                "function.one": ["function.two"],
                "function.two": ["function.three"],
                "function.three": ["print"],
            },
            ["function.one"],
        )

        trace = call_graph.find_shortest_trace_to_entrypoint(
            "this_function_does_not_exist",
        )

        self.assertIsNotNone(trace)
        self.assertListEqual(trace, [])

    def test_find_trace_to_parent_multi_path(self) -> None:
        call_graph = CallGraph(
            {
                "function.start": ["function.one"],
                "function.one": ["function.two_a", "function.two_b"],
                "function.two_a": ["function.three"],
                "function.two_b": ["function.two_b.extra_call"],
                "function.two_b.extra_call": ["function.three"],
                "function.three": ["print"],
            },
            ["function.start"],
        )

        trace = call_graph.find_shortest_trace_to_entrypoint("print")

        self.assertIsNotNone(trace)
        self.assertListEqual(
            trace,
            [
                "print",
                "function.three",
                "function.two_a",
                "function.one",
                "function.start",
            ],
        )

    def test_find_trace_to_parent_incomplete_call_graph(self) -> None:
        # this function tests a call graph whose keys are not fully represented
        # in values and vice-versa
        call_graph = CallGraph(
            {
                "function.start": ["function.one"],
                "function.one": ["function.two", "unrelated.call_1"],
                "function.two": ["function.three"],
                "function.three": ["print"],
                "unrelated.call_2": ["int"],
            },
            ["function.start"],
        )

        trace = call_graph.find_shortest_trace_to_entrypoint("print")

        self.assertIsNotNone(trace)
        self.assertListEqual(
            trace,
            [
                "print",
                "function.three",
                "function.two",
                "function.one",
                "function.start",
            ],
        )

    def test_find_trace_to_parent_cycle_from_bottom(self) -> None:
        call_graph = CallGraph(
            {
                "function.start": ["function.one"],
                "function.one": ["function.two", "unrelated.call_1"],
                "function.two": ["function.three"],
                "function.three": ["function.one", "print"],
                "unrelated.call_2": ["int"],
            },
            ["function.start"],
        )

        trace = call_graph.find_shortest_trace_to_entrypoint("print")

        self.assertIsNotNone(trace)
        self.assertListEqual(
            trace,
            [
                "print",
                "function.three",
                "function.two",
                "function.one",
                "function.start",
            ],
        )

    def test_find_trace_to_parent_cycle_from_top(self) -> None:
        call_graph = CallGraph(
            {
                "function.start": ["function.one"],
                "function.one": ["function.two", "unrelated.call_1"],
                "function.two": ["function.three"],
                "function.three": ["function.one", "print"],
                "unrelated.call_2": ["int"],
            },
            ["function.start"],
        )

        trace = call_graph.find_shortest_trace_to_entrypoint("function.one")

        self.assertIsNotNone(trace)
        self.assertListEqual(
            trace,
            ["function.one", "function.start"],
        )

    def test_find_trace_to_parent_self_call(self) -> None:
        call_graph = CallGraph(
            {
                "function.start": ["function.one"],
                "function.one": ["function.one", "function.two"],
                "function.two": ["print"],
            },
            ["function.start"],
        )

        trace = call_graph.find_shortest_trace_to_entrypoint("print")

        self.assertIsNotNone(trace)
        self.assertListEqual(
            trace,
            ["print", "function.two", "function.one", "function.start"],
        )

    def test_find_trace_to_parent_start_is_entrypoint(self) -> None:
        call_graph = CallGraph(
            {
                "function.start": ["function.start"],
            },
            ["function.start"],
        )

        trace = call_graph.find_shortest_trace_to_entrypoint("function.start")

        self.assertIsNotNone(trace)
        self.assertListEqual(
            trace,
            ["function.start"],
        )

    def test_find_trace_to_parent_multiple_valid_entrypoints(self) -> None:
        call_graph = CallGraph(
            {
                "function.start": ["function.one"],
                "function.one": ["function.two", "unrelated.call_1"],
                "function.two": ["function.three"],
                "function.three": ["function.one", "print"],
                "unrelated.call_2": ["int"],
            },
            ["function.start", "function.one"],
        )

        trace = call_graph.find_shortest_trace_to_entrypoint("print")

        self.assertIsNotNone(trace)
        self.assertListEqual(
            trace,
            ["print", "function.three", "function.two", "function.one"],
        )

    def test_validate_entrypoints_file_happy_path(self) -> None:
        entrypoints_list = ["my.entrypoint.one", "doesnt.exist"]
        call_graph = CallGraph({"my.entrypoint.one": ["print"]}, entrypoints_list)

        self.assertSetEqual(call_graph.entrypoints, {"my.entrypoint.one"})

    def test_validate_entrypoints_file_bad_root(self) -> None:
        entrypoints_list = {"not_a_list": True}

        try:
            CallGraph({"my.entrypoint.one": ["print"]}, entrypoints_list)
            self.fail("should have thrown")
        except ValueError:
            pass

    def test_validate_entrypoints_file_bad_list_elements(self) -> None:
        entrypoints_list = [True, 1]

        try:
            CallGraph({"my.entrypoint.one": ["print"]}, entrypoints_list)
            self.fail("should have thrown")
        except ValueError:
            pass
