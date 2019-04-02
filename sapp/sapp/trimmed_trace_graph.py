#!/usr/bin/env python3

from typing import Any, Iterable, List, Optional, Set, Tuple

from .models import SharedTextKind, TraceFrame, TraceKind
from .trace_graph import TraceGraph


class TrimmedTraceGraph(TraceGraph):
    """Represents a trimmed graph that is constructed from a bigger TraceGraph
    based on issues that have traces involving a set of affected files or
    directories.
    """

    def __init__(
        self, affected_files: List[str], affected_issues_only: bool = False
    ) -> None:
        """Creates an empty TrimmedTraceGraph.
        """
        super().__init__()
        self._affected_files = affected_files
        self._affected_issues_only = affected_issues_only
        self._visited_trace_frame_ids: Set[int] = set()

    def populate_from_trace_graph(self, graph: TraceGraph) -> None:
        """Populates this graph from the given one based on affected_files
        """
        # Track which trace frames have been visited as we populate the full
        # traces of the graph.
        self._visited_trace_frame_ids: Set[int] = set()

        self._populate_affected_issues(graph)

        if not self._affected_issues_only:
            # Finds issues from the conditions and saves them.
            # Also saves traces that have been trimmed to the affected
            # conditions.
            self._populate_issues_from_affected_trace_frames(graph)

            # Traces populated above may be missing all traces because
            # _populate_issues_from_affected_trace_frames only populates
            # traces that reach the affected conditions in one direction. We
            # may need to populate traces in other directions too.
            #
            # For example:
            #
            # Issue_x reaches affected_file_x via postcondition_x (forward
            # trace, i.e. trace leading to source). None of its backward
            # traces (leading to sinks) reach the affected files.
            #
            # _populate_issues_from_affected_trace_frames would have copied its
            # forward traces and trimmed it to those reaching postcondition_x.
            # We cannot blindly populate all forward traces in this case as
            # branches not leading to postcondition_x are unnecessary.
            #
            # However, in this specific example, all backward traces are needed
            # to give a complete picture of which sinks the issue reaches.
            # The following ensures that.
            for instance_id in self._issue_instances.keys():
                first_hop_ids = self._issue_instance_trace_frame_assoc[instance_id]
                fwd_trace_ids = {
                    tf_id
                    for tf_id in first_hop_ids
                    if self._trace_frames[tf_id].kind == TraceKind.POSTCONDITION
                }
                bwd_trace_ids = {
                    tf_id
                    for tf_id in first_hop_ids
                    if self._trace_frames[tf_id].kind == TraceKind.PRECONDITION
                }

                if len(fwd_trace_ids) == 0:
                    self._populate_issue_trace(
                        graph, instance_id, TraceKind.POSTCONDITION
                    )

                if len(bwd_trace_ids) == 0:
                    self._populate_issue_trace(
                        graph, instance_id, TraceKind.PRECONDITION
                    )

    def _populate_affected_issues(self, graph: TraceGraph) -> None:
        """Populates the trimmed graph with issues whose locations are in
        affected_files based on data in the input graph. Since these issues
        exist in the affected files, all traces are copied as well.
        """
        affected_instance_ids = [
            instance.id.local_id
            for instance in graph._issue_instances.values()
            if self._is_filename_prefixed_with(instance.filename, self._affected_files)
        ]

        for instance_id in affected_instance_ids:
            if instance_id in self._issue_instances:
                continue
            self._populate_issue_and_traces(graph, instance_id)

    def _get_sink_names(self, graph: TraceGraph, instance_id: int) -> Set[str]:
        kind: SharedTextKind = SharedTextKind.SINK
        sinks = graph.get_issue_instance_shared_texts(instance_id, kind)
        return {sink.contents for sink in sinks}

    def _get_source_names(self, graph: TraceGraph, instance_id: int) -> Set[str]:
        kind: SharedTextKind = SharedTextKind.SOURCE
        sources = graph.get_issue_instance_shared_texts(instance_id, kind)
        return {source.contents for source in sources}

    def _get_leaf_names(self, graph: TraceGraph, instance_id: int) -> Set[str]:
        return self._get_source_names(graph, instance_id).union(
            self._get_sink_names(graph, instance_id)
        )

    def _get_leaf_names_from_pairs(
        self, graph: TraceGraph, leaf_pairs: Set[Tuple[int, Any]]
    ) -> Set[str]:
        return {
            leaf.contents
            for leaf in (graph._shared_texts[leaf_id] for (leaf_id, _) in leaf_pairs)
        }

    def _populate_issues_from_affected_trace_frames(self, graph: TraceGraph) -> None:
        """TraceFrames found in affected_files should be reachable via some
        issue instance. Follow traces of graph to find them and
        populate this TrimmedGraph with it.
        """

        initial_trace_frames = [
            trace_frame
            for trace_frame in graph._trace_frames.values()
            if self._is_filename_prefixed_with(
                trace_frame.filename, self._affected_files
            )
        ]

        self._populate_issues_from_affected_conditions(
            initial_trace_frames,
            graph,
            lambda trace_frame_id: (
                graph._trace_frame_issue_instance_assoc[trace_frame_id]
            ),
            lambda trace_frame: (
                [
                    graph._trace_frames[trace_frame_id]
                    for trace_frame_id in graph._trace_frames_rev_map[
                        (trace_frame.caller, trace_frame.caller_port)
                    ]
                    if graph._trace_frames[trace_frame_id].kind == trace_frame.kind
                ]
            ),
            lambda instance_id: (self._get_leaf_names(graph, instance_id)),
            lambda trace_frame_id: (
                self._get_leaf_names_from_pairs(
                    graph, graph._trace_frame_leaf_assoc[trace_frame_id]
                )
            ),
            lambda instance, trace_frame: (
                self.add_issue_instance_trace_frame_assoc(instance, trace_frame)
            ),
            lambda trace_frame_id: (
                self._add_trace_frame(graph, graph._trace_frames[trace_frame_id])
            ),
            lambda initial_trace_frame_ids: (
                self._populate_trace(graph, initial_trace_frame_ids)
            ),
        )

    def _populate_issues_from_affected_conditions(
        self,
        initial_conditions,
        graph: TraceGraph,
        get_issue_instances_from_condition_id,
        get_condition_parent,
        get_instance_leaves,
        get_condition_leaves,
        add_instance_condition_assoc,
        add_condition,
        add_traces,
    ) -> None:
        """Helper for populating reachable issue instances from the initial
        pre/postconditions. Also populates conditions/traces reachable from
        these instances. Traces are populated only in the direction this is
        called from: i.e. if initial_conditions are preconditions, only the
        backward trace is populated.

        Params:
        initial_conditions: The initial collection of pre/postconditions to
        start searching for issues from.

        graph: The trace graph to search for issues. Nodes/edges in this graph
        will be copied over to the local state

        get_issue_instances_from_condition_id: Function that returns all
        issue instances associated with a given a pre/postcondition id.

        get_condition_parent: Function that returns the parent condition of
        a given pre/postcondition. Given a pre/postcondition, p, its parent p',
        is the pre/postcondition that calls it, i.e. p.caller = p'.callee

        get_instance_leaves: Function that returns a collection of leaf names
        associated with the given issue instance ID.

        get_condition_leaves: Function that returns a collection of leaf names
        associated with the given (pre/post)condition ID.

        add_instance_condition_assoc: Function that takes in the issue
        instance and condition and adds the assoc between them.

        add_condition: Function that adds a condition to the graph given its
        id. This must add all the condition's assocs with the leaves because
        we don't filter out any condition-leaf assocs.

        add_traces: Function that takes a list of initial conditions and
        adds all conditions reachable from these to the graph.
        """
        visited: Set[int] = set()
        que = list(zip(initial_conditions, initial_conditions))

        while len(que) > 0:
            condition, initial_condition = que.pop()
            cond_id = condition.id.local_id
            if cond_id in visited:
                continue
            visited.add(cond_id)

            # Found instance(s) related to the current condition. Yay.
            # This instance may have been found before, but process it again
            # anyway because we need to add the assoc with this condition.
            for instance_id in get_issue_instances_from_condition_id(cond_id):
                # Check if the leaves (sources/sinks) of the issue reach
                # the same leaves as the initial conditions. The issue is
                # relevant only if the conditions intersect.
                instance = graph._issue_instances[instance_id]
                issue_leaves = set(get_instance_leaves(instance.id.local_id))
                initial_leaves = set(
                    get_condition_leaves(initial_condition.id.local_id)
                )
                leaves = issue_leaves.intersection(initial_leaves)
                if len(leaves) > 0:
                    if instance_id not in self._issue_instances:
                        self._populate_issue(graph, instance_id)
                    add_instance_condition_assoc(instance, condition)

            # Conditions that call this may have originated from other issues,
            # keep searching for parent conditions leading to this one.
            que.extend(
                [
                    (cond, initial_condition)
                    for cond in get_condition_parent(condition)
                    if cond.id.local_id not in visited
                ]
            )

        # Add traces leading out from initial_conditions, and all visited
        # conditions leading back towards the issues.
        initial_condition_ids = [
            condition.id.local_id for condition in initial_conditions
        ]
        add_traces(initial_condition_ids)
        for condition_id in visited:
            add_condition(condition_id)

    def _populate_issue_and_traces(self, graph: TraceGraph, instance_id: int) -> None:
        """ Copies an issue over from the given trace graph, including all its
        traces and assocs.
        """
        self._populate_issue(graph, instance_id)
        self._populate_issue_trace(graph, instance_id)

    def _populate_issue_trace(
        self, graph: TraceGraph, instance_id: int, kind: Optional[TraceKind] = None
    ) -> None:
        trace_frame_ids = list(graph._issue_instance_trace_frame_assoc[instance_id])
        instance = graph._issue_instances[instance_id]
        filtered_ids = []
        for trace_frame_id in trace_frame_ids:
            frame = graph._trace_frames[trace_frame_id]
            if not kind or kind == frame.kind:
                self.add_issue_instance_trace_frame_assoc(instance, frame)
                filtered_ids.append(trace_frame_id)
        self._populate_trace(graph, filtered_ids)

    def _populate_issue(self, graph: TraceGraph, instance_id: int) -> None:
        """Adds an issue to the trace graph along with relevant information
        pertaining to the issue (e.g. instance, fix_info, sources/sinks)
        The issue is identified by its corresponding instance's ID in the input
        trace graph.
        """
        instance = graph._issue_instances[instance_id]
        issue = graph._issues[instance.issue_id.local_id]
        self._populate_shared_text(graph, instance.message_id)
        self._populate_shared_text(graph, instance.filename_id)
        self._populate_shared_text(graph, instance.callable_id)

        self.add_issue_instance(instance)
        self.add_issue(issue)

        if instance_id in graph._issue_instance_fix_info:
            issue_fix_info = graph._issue_instance_fix_info[instance_id]
            self.add_issue_instance_fix_info(instance, issue_fix_info)

        for shared_text_id in graph._issue_instance_shared_text_assoc[instance_id]:
            shared_text = graph._shared_texts[shared_text_id]
            if shared_text_id not in self._shared_texts:
                self.add_shared_text(shared_text)
            self.add_issue_instance_shared_text_assoc(instance, shared_text)

    def _populate_trace(self, graph: TraceGraph, trace_frame_ids: List[int]) -> None:
        """ Populates (from the given trace graph) the forward and backward
        traces reachable from the given traces (including input trace frames).
        Make sure to respect trace kind in successors
        """
        while len(trace_frame_ids) > 0:
            trace_frame_id = trace_frame_ids.pop()
            if trace_frame_id in self._visited_trace_frame_ids:
                continue

            trace_frame = graph._trace_frames[trace_frame_id]
            self._add_trace_frame(graph, trace_frame)
            self._visited_trace_frame_ids.add(trace_frame_id)

            key = (trace_frame.callee, trace_frame.callee_port)
            trace_frame_ids.extend(
                [
                    trace_frame_id
                    for trace_frame_id in graph._trace_frames_map[key]
                    if trace_frame_id not in self._visited_trace_frame_ids
                    and graph._trace_frames[trace_frame_id].kind == trace_frame.kind
                ]
            )

    def _add_trace_frame(self, graph: TraceGraph, trace_frame: TraceFrame) -> None:
        """ Copies the trace frame from 'graph' to this (self) graph.
        Also copies all the trace_frame-leaf assocs since we don't
        know which ones are needed until we know the issue that reaches it
        """
        trace_frame_id = trace_frame.id.local_id
        self.add_trace_frame(trace_frame)
        self._populate_shared_text(graph, trace_frame.filename_id)
        self._populate_shared_text(graph, trace_frame.caller_id)
        self._populate_shared_text(graph, trace_frame.callee_id)
        for (leaf_id, depth) in graph._trace_frame_leaf_assoc[trace_frame_id]:
            leaf = graph._shared_texts[leaf_id]
            if leaf_id not in self._shared_texts:
                self.add_shared_text(leaf)
            self.add_trace_frame_leaf_assoc(trace_frame, leaf, depth)

    @staticmethod
    def _is_filename_prefixed_with(filename: str, prefixes: Iterable[str]) -> bool:
        return any([filename.startswith(p) for p in prefixes])

    def _populate_shared_text(self, graph, id) -> None:
        text = graph._shared_texts[id.local_id]
        if text.id.local_id not in self._shared_texts:
            self.add_shared_text(text)
