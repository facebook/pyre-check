# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
#
# Enables navigating traces with sinks back to the source kinds that can reach it

import logging
from collections import defaultdict, deque
from typing import Dict, List, Set, Tuple

from .models import IssueInstance, SharedTextKind, TraceFrame, TraceKind
from .pipeline import PipelineStep, Summary
from .trace_graph import TraceGraph


log = logging.getLogger("sapp")

FrameID = int
SinkToSourceKindMap = Dict[int, Set[int]]
SinkToSourceKindDepthMap = Dict[int, Dict[int, int]]


class PropagateSourceKindsToSinks(  # pyre-fixme[13]
    PipelineStep[TraceGraph, TraceGraph]
):
    """For all issues propagate source kinds to all reachable frames leading to
    sinks."""

    def __init__(self) -> None:
        super().__init__()
        self.summary: Summary
        self.graph: TraceGraph
        # pyre-fixme[8]
        self.visited: Dict[FrameID, SinkToSourceKindDepthMap] = defaultdict(
            lambda: defaultdict(lambda: defaultdict(lambda: 999999))
        )

    def _subtract_kinds(
        self,
        depth: int,
        kind_map: SinkToSourceKindMap,
        to_remove: SinkToSourceKindDepthMap,
    ) -> SinkToSourceKindMap:
        result = {}
        for sink_kind, source_kinds in kind_map.items():
            if sink_kind in to_remove:
                source_depths = to_remove[sink_kind]
                source_kinds = {
                    source_kind
                    for source_kind in source_kinds
                    if source_depths.get(source_kind, depth + 1) > depth
                }
            if len(source_kinds) > 0:
                result[sink_kind] = source_kinds
        return result

    def _update_visited(
        self, frame_id: FrameID, depth: int, kind_map: SinkToSourceKindMap
    ) -> None:
        for sink_kind, source_kinds in kind_map.items():
            visited_frame = self.visited[frame_id]
            for source_kind in source_kinds:
                visited_frame[sink_kind][source_kind] = depth

    def _propagate_source_kinds(self, instance: IssueInstance):
        """Propagate the source kinds of this issue instance to all reachable sink
        traces."""
        graph = self.graph

        initial_frames = graph.get_issue_instance_trace_frames(instance)
        initial_source_frames = [
            frame for frame in initial_frames if frame.kind == TraceKind.POSTCONDITION
        ]
        initial_sink_frames = [
            frame for frame in initial_frames if frame.kind == TraceKind.PRECONDITION
        ]
        source_kind_list = [
            graph.get_trace_frame_leaf_ids_by_kind(frame, SharedTextKind.source)
            for frame in initial_source_frames
        ]
        if len(source_kind_list) == 0:
            return

        source_kinds = set.union(*source_kind_list)
        if len(source_kinds) == 0:
            return
        self._propagate_kinds_along_traces(initial_sink_frames, source_kinds)

    def _propagate_kinds_along_traces(
        self, start_frames: List[TraceFrame], new_kinds: Set[int]
    ) -> None:
        graph = self.graph

        queue = deque(
            [
                (
                    start_frame,
                    {
                        sink_id: new_kinds
                        for sink_id in graph.get_trace_frame_leaf_ids(start_frame)
                    },
                    0,
                )
                for start_frame in start_frames
            ]
        )
        while len(queue) > 0:
            frame, kind_map, depth = queue.popleft()
            if len(kind_map) == 0:
                continue

            frame_id = frame.id.local_id
            if frame_id in self.visited:
                kind_map = self._subtract_kinds(depth, kind_map, self.visited[frame_id])
                if len(kind_map) == 0:
                    continue

            self._update_visited(frame_id, depth, kind_map)

            next_frames = [
                (next_frame, graph.get_trace_frame_leaf_ids(frame))
                for next_frame in self.graph.get_trace_frames_from_caller(
                    frame.kind, frame.callee_id, frame.callee_port
                )
            ]
            queue.extend(
                (
                    frame,
                    {
                        sink: sources
                        for sink, sources in kind_map.items()
                        if sink in sink_kinds
                    },
                    depth + 1,
                )
                for (frame, sink_kinds) in next_frames
            )

    def run(self, input: TraceGraph, summary: Summary) -> Tuple[TraceGraph, Summary]:
        graph = input
        self.summary = summary
        self.graph = graph

        log.info("Propagating source kinds to sinks")

        for instance in graph.get_issue_instances():
            self._propagate_source_kinds(instance)

        # Create new source assocs based on the visited results
        source_count = 0
        trace_frame_count = 0
        for trace_frame_id, kind_map in self.visited.items():
            trace_frame_count += 1
            trace_frame = graph.get_trace_frame_from_id(trace_frame_id)
            for source_depths in kind_map.values():
                for source_kind, depth in source_depths.items():
                    graph.add_trace_frame_leaf_by_local_id_assoc(
                        trace_frame, source_kind, depth
                    )
                    source_count += 1

        log.info(
            f"Added {source_count} source kinds to {trace_frame_count} trace frames"
        )

        return graph, summary
