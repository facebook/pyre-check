#!/usr/bin/env python3

import logging
from typing import Tuple

from sapp.pipeline import PipelineStep, Summary
from sapp.trace_graph import TraceGraph
from sapp.trimmed_trace_graph import TrimmedTraceGraph


log = logging.getLogger()


class TrimTraceGraph(PipelineStep[TraceGraph, TraceGraph]):
    def run(self, input: TraceGraph, summary: Summary) -> Tuple[TraceGraph, Summary]:
        if summary.get("affected_files") is None:
            summary["graph"] = input  # used by ranker
            return input, summary

        log.info("Trimming graph to affected files.")
        trimmed_graph = TrimmedTraceGraph(
            summary["affected_files"], summary.get("affected_issues_only", False)
        )
        trimmed_graph.populate_from_trace_graph(input)

        summary["graph"] = trimmed_graph  # used by ranker
        return trimmed_graph, summary
