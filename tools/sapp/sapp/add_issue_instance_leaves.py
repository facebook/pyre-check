# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging
from collections import deque
from typing import Tuple

from .models import DBID, SharedText, SharedTextKind
from .pipeline import PipelineStep, Summary
from .trace_graph import TraceGraph


log = logging.getLogger("sapp")


class AddIssueInstanceLeaves(PipelineStep[TraceGraph, TraceGraph]):
    """For all issues with a given code, adds a leaf at the issue frame and adds
    the relevant trace_frame_leaf_assocs. The depth increases as the trace
    frames get further away (in the caller->callee direction) from the issue
    frame. Useful for queries in the callee->caller direction."""

    def __init__(self, code: int, leaf_name: str, leaf_kind: SharedTextKind) -> None:
        super().__init__()
        self.code = code
        self.leaf_name = leaf_name
        self.leaf_kind = leaf_kind

    def run(self, input: TraceGraph, summary: Summary) -> Tuple[TraceGraph, Summary]:
        graph = input

        # Get all the issue instances within this category
        instances = [
            instance
            for instance in graph.get_issue_instances()
            if self.code == graph.get_issue(instance.issue_id).code
        ]

        # Add all the trace frame ids for all the issue instances
        trace_frames = []
        for instance in instances:
            trace_frames.extend(graph.get_issue_instance_trace_frames(instance))

        # Explore forward (caller -> callee; issue -> leaf)
        queue = deque([(frame, 0) for frame in trace_frames])
        depth_by_frame_id = {}
        while len(queue) > 0:
            trace_frame, depth = queue.popleft()
            trace_frame_id = trace_frame.id.local_id

            # Skip repeat frames unless we arrived at them by a shorter path.
            if (
                trace_frame_id in depth_by_frame_id
                and depth >= depth_by_frame_id[trace_frame_id]
            ):
                continue
            else:
                # Record the minimum depth.
                depth_by_frame_id[trace_frame_id] = depth

            # Iterate forward
            queue.extend(
                (next_frame, depth + 1)
                for next_frame in graph.get_next_trace_frames(trace_frame)
            )

        # Create new leaves based on these depths
        leaf = graph.get_shared_text(self.leaf_kind, self.leaf_name)
        if leaf is None:
            leaf = SharedText.Record(
                id=DBID(), contents=self.leaf_name, kind=self.leaf_kind
            )
            graph.add_shared_text(leaf)

        # Add the assoc to the leaf
        log.info(
            'Adding %d "%s" leaves from issues with code %d...',
            len(depth_by_frame_id),
            self.leaf_name,
            self.code,
        )
        for trace_frame_id, depth in depth_by_frame_id.items():
            trace_frame = graph.get_trace_frame_from_id(trace_frame_id)
            graph.add_trace_frame_leaf_assoc(trace_frame, leaf, depth)

        return graph, summary
