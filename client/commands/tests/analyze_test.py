# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import tempfile
from pathlib import Path
from typing import Iterable, Tuple

import testslide

from ... import (
    backend_arguments,
    command_arguments,
    configuration,
    frontend_configuration,
)
from ...configuration import search_path
from ...tests import setup
from .. import analyze


class ArgumentTest(testslide.TestCase):
    def test_serialize_arguments(self) -> None:
        def assert_serialized(
            arguments: analyze.Arguments, items: Iterable[Tuple[str, object]]
        ) -> None:
            serialized = arguments.serialize()
            for key, value in items:
                if key not in serialized:
                    self.fail(f"Cannot find key `{key}` in serialized arguments")
                else:
                    self.assertEqual(value, serialized[key])

        assert_serialized(
            analyze.Arguments(
                base_arguments=backend_arguments.BaseArguments(
                    log_path="/log",
                    global_root="/project",
                    source_paths=backend_arguments.SimpleSourcePath(
                        [search_path.SimpleElement("source")]
                    ),
                ),
                dump_call_graph="/call-graph",
                dump_model_query_results="/model-query",
                find_missing_flows="obscure",
                maximum_model_source_tree_width=10,
                maximum_model_sink_tree_width=11,
                maximum_model_tito_tree_width=12,
                maximum_tree_depth_after_widening=4,
                maximum_return_access_path_width=13,
                maximum_return_access_path_depth_after_widening=5,
                maximum_tito_collapse_depth=6,
                maximum_tito_positions=40,
                maximum_overrides_to_analyze=50,
                maximum_tito_depth=5,
                maximum_trace_length=4,
                no_verify=True,
                verify_dsl=True,
                repository_root="/root",
                rule_filter=[1, 2],
                source_filter=["X"],
                sink_filter=["Y", "Z"],
                transform_filter=["T"],
                save_results_to="/output/results.json",
                output_format="sharded-json",
                strict=True,
                taint_model_paths=["/taint/models"],
                use_cache=True,
                build_cache_only=True,
                check_invariants=True,
                limit_entrypoints=True,
                compact_ocaml_heap=True,
                saved_state_arguments=command_arguments.PysaSavedStateArguments(
                    watchman_root=Path("/root"),
                    project_name="my_project",
                    preset="some_preset",
                    cache_critical_files=["*.py"],
                ),
                compute_coverage=True,
                higher_order_call_graph_max_iterations=10,
                maximum_target_depth=4,
                maximum_parameterized_targets_at_call_site=1000,
            ),
            [
                ("log_path", "/log"),
                ("global_root", "/project"),
                ("source_paths", {"kind": "simple", "paths": ["source"]}),
                ("dump_call_graph", "/call-graph"),
                ("dump_model_query_results", "/model-query"),
                ("find_missing_flows", "obscure"),
                ("infer_self_tito", True),
                ("infer_argument_tito", False),
                ("maximum_model_source_tree_width", 10),
                ("maximum_model_sink_tree_width", 11),
                ("maximum_model_tito_tree_width", 12),
                ("maximum_tree_depth_after_widening", 4),
                ("maximum_return_access_path_width", 13),
                ("maximum_return_access_path_depth_after_widening", 5),
                ("maximum_tito_collapse_depth", 6),
                ("maximum_tito_positions", 40),
                ("maximum_overrides_to_analyze", 50),
                ("maximum_tito_depth", 5),
                ("maximum_trace_length", 4),
                ("no_verify", True),
                ("verify_dsl", True),
                ("repository_root", "/root"),
                ("rule_filter", [1, 2]),
                ("source_filter", ["X"]),
                ("sink_filter", ["Y", "Z"]),
                ("transform_filter", ["T"]),
                ("save_results_to", "/output/results.json"),
                ("output_format", "sharded-json"),
                ("strict", True),
                ("taint_model_paths", ["/taint/models"]),
                ("use_cache", True),
                ("build_cache_only", True),
                ("check_invariants", True),
                ("limit_entrypoints", True),
                ("compact_ocaml_heap", True),
                (
                    "saved_state",
                    {
                        "watchman_root": "/root",
                        "project_name": "my_project",
                        "preset": "some_preset",
                        "cache_critical_files": ["*.py"],
                    },
                ),
                ("compute_coverage", True),
                ("higher_order_call_graph_max_iterations", 10),
                ("maximum_target_depth", 4),
                ("maximum_parameterized_targets_at_call_site", 1000),
            ],
        )

    def test_create_analyze_arguments(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            setup.ensure_directories_exists(
                root_path,
                [".pyre", "blocks", "search", "taint_models", "local/src"],
            )
            setup.write_configuration_file(
                root_path,
                {
                    "ignore_all_errors": ["blocks", "nonexistent"],
                    "exclude": ["exclude"],
                    "extensions": [".ext"],
                    "workers": 42,
                    "search_path": ["search"],
                    "optional_search_path": ["nonexistent"],
                },
            )
            setup.write_configuration_file(
                root_path, {"source_directories": ["src"]}, relative="local"
            )

            analyze_configuration = frontend_configuration.OpenSource(
                configuration.create_configuration(
                    command_arguments.CommandArguments(
                        local_configuration="local",
                        dot_pyre_directory=root_path / ".pyre",
                        strict=True,
                    ),
                    root_path,
                )
            )

            self.assertEqual(
                analyze.create_analyze_arguments(
                    analyze_configuration,
                    command_arguments.AnalyzeArguments(
                        debug=True,
                        dump_call_graph="/call-graph",
                        dump_model_query_results="/model-query",
                        find_missing_flows=command_arguments.MissingFlowsKind.TYPE,
                        infer_self_tito=False,
                        infer_argument_tito=True,
                        maximum_model_source_tree_width=10,
                        maximum_model_sink_tree_width=11,
                        maximum_model_tito_tree_width=12,
                        maximum_tree_depth_after_widening=4,
                        maximum_return_access_path_width=13,
                        maximum_return_access_path_depth_after_widening=5,
                        maximum_tito_collapse_depth=6,
                        maximum_tito_positions=40,
                        maximum_overrides_to_analyze=50,
                        maximum_tito_depth=5,
                        maximum_trace_length=4,
                        no_verify=True,
                        verify_dsl=True,
                        repository_root="/root",
                        rule=[1, 2],
                        source=["X"],
                        sink=["Y", "Z"],
                        transform=["T"],
                        save_results_to="/result.json",
                        output_format=command_arguments.TaintOutputFormat.SHARDED_JSON,
                        taint_models_path=[str(root_path / "taint_models")],
                        use_cache=True,
                        build_cache_only=True,
                        check_invariants=True,
                        limit_entrypoints=True,
                        compact_ocaml_heap=True,
                        saved_state_arguments=command_arguments.PysaSavedStateArguments(
                            watchman_root=Path("/root"),
                            project_name="test_project",
                            preset="some_preset",
                            cache_critical_files=["*.py"],
                        ),
                        compute_coverage=True,
                        higher_order_call_graph_max_iterations=10,
                        maximum_target_depth=4,
                        maximum_parameterized_targets_at_call_site=1000,
                    ),
                ),
                analyze.Arguments(
                    base_arguments=backend_arguments.BaseArguments(
                        log_path=str(root_path / ".pyre/local"),
                        global_root=str(root_path),
                        checked_directory_allowlist=[
                            str(root_path / "local/src"),
                        ],
                        checked_directory_blocklist=[
                            str(root_path / "blocks"),
                            str(root_path / "nonexistent"),
                        ],
                        debug=True,
                        excludes=[
                            "exclude",
                        ],
                        extensions=[".ext"],
                        relative_local_root="local",
                        number_of_workers=42,
                        parallel=True,
                        python_version=analyze_configuration.get_python_version(),
                        search_paths=[
                            search_path.SimpleElement(str(root_path / "search"))
                        ],
                        source_paths=backend_arguments.SimpleSourcePath(
                            [search_path.SimpleElement(str(root_path / "local/src"))]
                        ),
                    ),
                    dump_call_graph="/call-graph",
                    dump_model_query_results="/model-query",
                    find_missing_flows="type",
                    infer_self_tito=False,
                    infer_argument_tito=True,
                    maximum_model_source_tree_width=10,
                    maximum_model_sink_tree_width=11,
                    maximum_model_tito_tree_width=12,
                    maximum_tree_depth_after_widening=4,
                    maximum_return_access_path_width=13,
                    maximum_return_access_path_depth_after_widening=5,
                    maximum_tito_collapse_depth=6,
                    maximum_tito_positions=40,
                    maximum_overrides_to_analyze=50,
                    maximum_tito_depth=5,
                    maximum_trace_length=4,
                    no_verify=True,
                    verify_dsl=True,
                    repository_root="/root",
                    rule_filter=[1, 2],
                    source_filter=["X"],
                    sink_filter=["Y", "Z"],
                    transform_filter=["T"],
                    save_results_to="/result.json",
                    output_format="sharded-json",
                    strict=True,
                    taint_model_paths=[str(root_path / "taint_models")],
                    use_cache=True,
                    build_cache_only=True,
                    check_invariants=True,
                    limit_entrypoints=True,
                    compact_ocaml_heap=True,
                    saved_state_arguments=command_arguments.PysaSavedStateArguments(
                        watchman_root=Path("/root"),
                        project_name="test_project",
                        preset="some_preset",
                        cache_critical_files=["*.py"],
                    ),
                    compute_coverage=True,
                    higher_order_call_graph_max_iterations=10,
                    maximum_target_depth=4,
                    maximum_parameterized_targets_at_call_site=1000,
                ),
            )
