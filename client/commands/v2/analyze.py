# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import dataclasses
import logging
from typing import Optional, Sequence, Dict, Any

from ... import commands, command_arguments, configuration as configuration_module
from . import backend_arguments, remote_logging

LOG: logging.Logger = logging.getLogger(__name__)


@dataclasses.dataclass(frozen=True)
class Arguments:
    """
    Data structure for configuration options the backend analyze command can recognize.
    Need to keep in sync with `pyre/command/newAnalyzeCommand.ml`
    """

    base_arguments: backend_arguments.BaseArguments

    dump_call_graph: bool = False
    dump_model_query_results: bool = False
    find_missing_flows: Optional[str] = None
    inline_decorators: bool = False
    maximum_tito_depth: Optional[int] = None
    maximum_trace_length: Optional[int] = None
    no_verify: bool = False
    repository_root: Optional[str] = None
    rule_filter: Optional[Sequence[int]] = None
    save_results_to: Optional[str] = None
    strict: bool = False
    taint_model_paths: Sequence[str] = dataclasses.field(default_factory=list)
    use_cache: bool = False

    def serialize(self) -> Dict[str, Any]:
        find_missing_flows = self.find_missing_flows
        maximum_tito_depth = self.maximum_tito_depth
        maximum_trace_length = self.maximum_trace_length
        repository_root = self.repository_root
        rule_filter = self.rule_filter
        save_results_to = self.save_results_to
        return {
            **self.base_arguments.serialize(),
            "dump_call_graph": self.dump_call_graph,
            "dump_model_query_results": self.dump_model_query_results,
            **(
                {}
                if find_missing_flows is None
                else {"find_missing_flows": find_missing_flows}
            ),
            "inline_decorators": self.inline_decorators,
            **(
                {}
                if maximum_tito_depth is None
                else {"maximum_tito_depth": maximum_tito_depth}
            ),
            **(
                {}
                if maximum_trace_length is None
                else {"maximum_trace_length": maximum_trace_length}
            ),
            "no_verify": self.no_verify,
            **({} if repository_root is None else {"repository_root": repository_root}),
            **({} if rule_filter is None else {"rule_filter": rule_filter}),
            **({} if save_results_to is None else {"save_results_to": save_results_to}),
            "strict": self.strict,
            "taint_model_paths": self.taint_model_paths,
            "use_cache": self.use_cache,
        }


def run_analyze(
    configuration: configuration_module.Configuration,
    analyze_arguments: command_arguments.AnalyzeArguments,
) -> commands.ExitCode:
    LOG.warning("Coming soon...")
    return commands.ExitCode.SUCCESS


@remote_logging.log_usage(command_name="analyze")
def run(
    configuration: configuration_module.Configuration,
    analyze_arguments: command_arguments.AnalyzeArguments,
) -> commands.ExitCode:
    try:
        return run_analyze(configuration, analyze_arguments)
    except Exception as error:
        raise commands.ClientException(
            f"Exception occured during pyre analyze: {error}"
        ) from error
