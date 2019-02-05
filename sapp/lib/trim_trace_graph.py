#!/usr/bin/env python3

import logging
from typing import Tuple

from tools.sapp.pipeline import PipelineStep, Summary
from tools.sapp.trace_graph import TraceGraph
from tools.sapp.trimmed_trace_graph import TrimmedTraceGraph


log = logging.getLogger()


class TrimTraceGraph(PipelineStep[TraceGraph, TraceGraph]):
    def run(
        self, input: TraceGraph, summary: Summary = None
    ) -> Tuple[TraceGraph, Summary]:
        self.summary = summary or {}

        if self.summary.get("affected_files") is None:
            return input, self.summary

        log.info("Trimming graph to affected files.")
        trimmed_graph = TrimmedTraceGraph(
            self.summary["affected_files"], self.summary.get("affected_issues_only")
        )
        trimmed_graph.populate_from_trace_graph(input)
        return trimmed_graph, summary
