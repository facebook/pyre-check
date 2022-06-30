# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import tempfile
from pathlib import Path
from typing import Iterable, Tuple

import testslide

from ... import command_arguments, configuration
from ...configuration import search_path
from ...tests import setup
from .. import backend_arguments, frontend_configuration
from ..analyze import Arguments, create_analyze_arguments


class ArgumentTest(testslide.TestCase):
    def test_serialize_arguments(self) -> None:
        def assert_serialized(
            arguments: Arguments, items: Iterable[Tuple[str, object]]
        ) -> None:
            serialized = arguments.serialize()
            for key, value in items:
                if key not in serialized:
                    self.fail(f"Cannot find key `{key}` in serialized arguments")
                else:
                    self.assertEqual(value, serialized[key])

        assert_serialized(
            Arguments(
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
                inline_decorators=True,
                maximum_tito_depth=5,
                maximum_trace_length=4,
                no_verify=True,
                verify_dsl=True,
                repository_root="/root",
                rule_filter=[1, 2],
                save_results_to="/output/results.json",
                strict=True,
                taint_model_paths=["/taint/models"],
                use_cache=True,
            ),
            [
                ("log_path", "/log"),
                ("global_root", "/project"),
                ("source_paths", {"kind": "simple", "paths": ["source"]}),
                ("dump_call_graph", "/call-graph"),
                ("dump_model_query_results", "/model-query"),
                ("find_missing_flows", "obscure"),
                ("inline_decorators", True),
                ("maximum_tito_depth", 5),
                ("maximum_trace_length", 4),
                ("no_verify", True),
                ("verify_dsl", True),
                ("repository_root", "/root"),
                ("rule_filter", [1, 2]),
                ("save_results_to", "/output/results.json"),
                ("strict", True),
                ("taint_model_paths", ["/taint/models"]),
                ("use_cache", True),
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
                    "search_path": ["search", "nonexistent"],
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
                create_analyze_arguments(
                    analyze_configuration,
                    command_arguments.AnalyzeArguments(
                        debug=True,
                        dump_call_graph="/call-graph",
                        dump_model_query_results="/model-query",
                        find_missing_flows=command_arguments.MissingFlowsKind.TYPE,
                        inline_decorators=True,
                        maximum_tito_depth=5,
                        maximum_trace_length=4,
                        no_verify=True,
                        verify_dsl=True,
                        repository_root="/root",
                        rule=[1, 2],
                        save_results_to="/result.json",
                        taint_models_path=[str(root_path / "taint_models")],
                        use_cache=True,
                    ),
                ),
                Arguments(
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
                    inline_decorators=True,
                    maximum_tito_depth=5,
                    maximum_trace_length=4,
                    no_verify=True,
                    verify_dsl=True,
                    repository_root="/root",
                    rule_filter=[1, 2],
                    save_results_to="/result.json",
                    strict=True,
                    taint_model_paths=[str(root_path / "taint_models")],
                    use_cache=True,
                ),
            )
