#!/usr/bin/env python3

import logging
from typing import Tuple

from .pipeline import PipelineStep, Summary
from .trace_graph import TraceGraph
from .trimmed_trace_graph import TrimmedTraceGraph


log = logging.getLogger("sapp")


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
