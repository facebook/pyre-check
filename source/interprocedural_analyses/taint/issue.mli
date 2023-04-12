(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Statement
open Domains
open Interprocedural

module Flow : sig
  type t = {
    source_taint: ForwardTaint.t;
    sink_taint: BackwardTaint.t;
  }
  [@@deriving show]

  val bottom : t

  val is_bottom : t -> bool

  val join : t -> t -> t
end

module LocationSet : Stdlib.Set.S with type elt = Location.WithModule.t

type t = {
  flow: Flow.t;
  handle: IssueHandle.t;
  locations: LocationSet.t;
  (* Only used to create the Pyre errors. *)
  define: Ast.Statement.Define.t Ast.Node.t;
}

val canonical_location : t -> Location.WithModule.t

val to_json
  :  taint_configuration:TaintConfiguration.Heap.t ->
  expand_overrides:OverrideGraph.SharedMemory.t option ->
  is_valid_callee:
    (port:AccessPath.Root.t -> path:Abstract.TreeDomain.Label.path -> callee:Target.t -> bool) ->
  filename_lookup:(Reference.t -> string option) ->
  t ->
  Yojson.Safe.t

val to_error : taint_configuration:TaintConfiguration.Heap.t -> t -> Error.t

(* A map from triggered sink kinds (which is a string) to the handles of the flows that are detected
   when creating these triggered sinks. The issue handles will be propagated in the backward
   analysis. A triggered sink here means we must find its matching source, in order to file an issue
   for a multi-source rule. *)
module TriggeredSinkHashMap : sig
  type t

  val create : unit -> t

  val is_empty : t -> bool

  val mem : t -> Sinks.partial_sink -> bool

  val find : t -> Sinks.partial_sink -> IssueHandleSet.t option
end

(* A map from locations to a backward taint of triggered sinks.
 * This is used to store triggered sinks found in the forward analysis,
 * and propagate them up in the backward analysis. *)
module TriggeredSinkLocationMap : sig
  type t

  val create : unit -> t

  val add : t -> location:Location.t -> taint:BackwardState.t -> unit

  val get : t -> location:Location.t -> BackwardState.t
end

(* Accumulate flows and generate issues. *)
module Candidates : sig
  type issue = t

  type t

  val create : unit -> t

  (* Check for issues in flows from the `source_tree` to the `sink_tree`, updating
   * issue `candidates`. *)
  val check_flow
    :  t ->
    location:Location.WithModule.t ->
    sink_handle:IssueHandle.Sink.t ->
    source_tree:ForwardState.Tree.t ->
    sink_tree:BackwardState.Tree.t ->
    unit

  (* Check for issues for combined source rules.
   * For flows where both sources are present, this adds the flow to issue `candidates`.
   * If only one source is present, this creates a triggered sink in `triggered_sinks_for_call`.
   *)
  val check_triggered_flows
    :  t ->
    taint_configuration:TaintConfiguration.Heap.t ->
    triggered_sinks_for_call:TriggeredSinkHashMap.t ->
    location:Location.WithModule.t ->
    sink_handle:IssueHandle.Sink.t ->
    source_tree:ForwardState.Tree.t ->
    sink_tree:BackwardState.Tree.t ->
    define:Define.t Node.t ->
    unit

  val generate_issues
    :  t ->
    taint_configuration:TaintConfiguration.Heap.t ->
    define:Define.t Node.t ->
    issue IssueHandle.SerializableMap.t
end

module MultiSource : sig
  type issue = t

  (* Whether an issue is related with a multi-source rule. *)
  val is_multi_source : issue -> bool

  (* When the combination of the given issue alongwith other issues that it relates to (under the
     same partial sink kind) constitutes all issues required for reporting an issue of the
     corresponding multi-source rule, return a pair of the partial sink kind and the related issues.
     Fact: There exists a valid issue for a multi-source rule iff. there exists a non-empty set of
     related issues (under a partial sink kind). *)
  val find_related_issues
    :  taint_configuration:TaintConfiguration.Heap.t ->
    issue_handle_map:t IssueHandle.SerializableMap.t ->
    issue ->
    issue list Sinks.Map.t

  val is_main_issue : sink:Sinks.t -> taint_configuration:TaintConfiguration.Heap.t -> issue -> bool

  val get_first_sink_hops
    :  main_issue_location:Location.WithModule.t ->
    filename_lookup:(Reference.t -> string option) ->
    issue ->
    ExtraTraceFirstHop.Set.t

  val get_first_source_hops
    :  main_issue_location:Location.WithModule.t ->
    filename_lookup:(Reference.t -> string option) ->
    issue ->
    ExtraTraceFirstHop.Set.t

  val attach_extra_traces
    :  source_traces:ExtraTraceFirstHop.Set.t ->
    sink_traces:ExtraTraceFirstHop.Set.t ->
    issue ->
    issue
end
