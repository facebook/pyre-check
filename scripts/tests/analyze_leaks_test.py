#!/usr/bin/env fbpython
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest

from ..analyze_leaks import CallGraph


class AnalyzeIssueTraceTest(unittest.TestCase):
    def test_load_pysa_call_graph_happy_path(self) -> None:
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

    def test_load_pyre_call_graph_happy_path(self) -> None:
        call_graph = {
            "my_module.my_function": [
                {
                    "keys_we_dont_need": [1, 2, 3],
                    "target": "something_that.my_function_calls",
                },
                {"target": "builtins.print"},
                {"direct_target": "my_module.my_function"},
            ],
            "something_that.my_function_calls": [{"direct_target": "int.__str__"}],
        }

        call_graph = CallGraph.validate_call_graph(call_graph)
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

    def test_load_call_graph_bad_callees_dict_keys(self) -> None:
        call_graph = {"caller": {"callee": 123}}

        try:
            CallGraph.validate_call_graph(call_graph)
            self.fail("should have thrown")
        except ValueError:
            pass

    def test_load_call_graph_bad_callees_dict_target(self) -> None:
        call_graph = {"caller": {"target": 123}}

        try:
            CallGraph.validate_call_graph(call_graph)
            self.fail("should have thrown")
        except ValueError:
            pass

    def test_load_call_graph_bad_callees_dict_direct_target(self) -> None:
        call_graph = {"caller": {"direct_target": 123}}

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

    def test_get_all_callees_empty(self) -> None:
        entrypoints_list = []
        call_graph = CallGraph({"f1": ["f2", "f3"], "f2": ["f1"]}, entrypoints_list)

        callees = call_graph.get_all_callees()

        self.assertEqual(callees, set())

    def test_get_all_callees_f1(self) -> None:
        entrypoints_list = ["f1"]
        call_graph = CallGraph(
            {"f1": ["f2", "f3"], "f2": ["f1"], "f3": ["f3"]}, entrypoints_list
        )

        callees = call_graph.get_all_callees()

        self.assertEqual(callees, {"f1", "f2", "f3"})

    def test_get_all_callees_f2(self) -> None:
        entrypoints_list = ["f2"]
        call_graph = CallGraph(
            {"f1": ["f2", "f3"], "f2": ["f1"], "f3": ["f3"]}, entrypoints_list
        )

        callees = call_graph.get_all_callees()

        self.assertEqual(callees, {"f1", "f2", "f3"})

    def test_get_all_callees_f3(self) -> None:
        entrypoints_list = ["f3"]
        call_graph = CallGraph(
            {"f1": ["f2", "f3"], "f2": ["f1"], "f3": ["f3"]}, entrypoints_list
        )

        callees = call_graph.get_all_callees()

        self.assertEqual(callees, {"f3"})

    def test_get_all_callees_multiple(self) -> None:
        entrypoints_list = ["f1", "f4"]
        call_graph = CallGraph(
            {
                "f1": ["f2", "f3"],
                "f2": ["f1"],
                "f3": ["f3"],
                "f4": ["f5"],
                "f5": ["print"],
                "f6": [],
            },
            entrypoints_list,
        )

        callees = call_graph.get_all_callees()

        self.assertEqual(callees, {"f1", "f2", "f3", "f4", "f5", "print"})

    def test_prepare_issues_for_query(self) -> None:
        callees = ["f1", "f2", "f3"]

        result_query = CallGraph.prepare_issues_for_query(callees)
        expected_query = "batch(global_leaks(f1), global_leaks(f2), global_leaks(f3))"

        self.assertEqual(result_query, expected_query)

    def test_analyze_pyre_query_results(self) -> None:
        example_pyre_stdout = """
        {
            "response": [
                {"error": "we failed to find your callable"},
                {
                    "response": {
                        "errors": [{"error_msg": "found an error for you", "location": "your_location"}]
                    }
                },
                {"error": "we failed to find your callable 2"},
                {
                    "response": {
                        "errors": [{"error_msg": "found an error for you2", "location": "your_location2"}]
                    }
                }
            ]
        }
        """

        results = CallGraph.analyze_pyre_query_results(example_pyre_stdout)
        expected_results = {
            "global_leaks": [
                {"error_msg": "found an error for you", "location": "your_location"},
                {"error_msg": "found an error for you2", "location": "your_location2"},
            ],
            "errors": [
                "we failed to find your callable",
                "we failed to find your callable 2",
            ],
        }

        print(results)
        print(expected_results)

        self.assertEqual(results, expected_results)

    def test_analyze_pyre_query_results_non_json(self) -> None:
        example_pyre_stdout = """
            this is not a valid response
        """

        try:
            CallGraph.analyze_pyre_query_results(example_pyre_stdout)
            self.fail("should have thrown")
        except RuntimeError:
            pass

    def test_analyze_pyre_query_results_not_top_level_dict(self) -> None:
        example_pyre_stdout = """
            ["this is a list"]
        """

        try:
            CallGraph.analyze_pyre_query_results(example_pyre_stdout)
            self.fail("should have thrown")
        except RuntimeError:
            pass

    def test_analyze_pyre_query_results_no_response_present(self) -> None:
        example_pyre_stdout = """
            {"not a response": 1}
        """

        try:
            CallGraph.analyze_pyre_query_results(example_pyre_stdout)
            self.fail("should have thrown")
        except RuntimeError:
            pass

    def test_analyze_pyre_query_results_response_not_a_list(self) -> None:
        example_pyre_stdout = """
            {"response": 1}
        """

        try:
            CallGraph.analyze_pyre_query_results(example_pyre_stdout)
            self.fail("should have thrown")
        except RuntimeError:
            pass

    def test_analyze_pyre_query_results_response_batch_response_not_a_dict(
        self,
    ) -> None:
        example_pyre_stdout = """
            {
                "response": [
                    123
                ]
            }
        """

        try:
            CallGraph.analyze_pyre_query_results(example_pyre_stdout)
            self.fail("should have thrown")
        except RuntimeError:
            pass

    def test_analyze_pyre_query_results_response_batch_response_no_nested_error_or_response(
        self,
    ) -> None:
        example_pyre_stdout = """
            {
                "response": {
                    "not_error": 123,
                    "not_response": 456
                }
            }
        """

        try:
            CallGraph.analyze_pyre_query_results(example_pyre_stdout)
            self.fail("should have thrown")
        except RuntimeError:
            pass
