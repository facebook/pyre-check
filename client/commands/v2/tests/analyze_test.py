# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import Iterable, Tuple

import testslide

from .... import configuration
from .. import backend_arguments
from ..analyze import (
    Arguments,
)


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
                        [configuration.SimpleSearchPathElement("source")]
                    ),
                ),
                dump_call_graph=True,
                dump_model_query_results=True,
                find_missing_flows="obscure",
                inline_decorators=True,
                maximum_tito_depth=5,
                maximum_trace_length=4,
                no_verify=True,
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
                ("dump_call_graph", True),
                ("dump_model_query_results", True),
                ("find_missing_flows", "obscure"),
                ("inline_decorators", True),
                ("maximum_tito_depth", 5),
                ("maximum_trace_length", 4),
                ("no_verify", True),
                ("repository_root", "/root"),
                ("rule_filter", [1, 2]),
                ("save_results_to", "/output/results.json"),
                ("strict", True),
                ("taint_model_paths", ["/taint/models"]),
                ("use_cache", True),
            ],
        )
