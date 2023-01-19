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

(* A map from triggered sink kinds (which is a string) to the triggered sink taints to propagate in
   the backward analysis. For a multi-source rule, triggered sinks do not mean we have found the
   issue, because the other sources are still missing. *)
module TriggeredSinkHashMap : sig
  type t

  val create : unit -> t

  val is_empty : t -> bool

  val mem : t -> Sinks.partial_sink -> bool

  val add
    :  t ->
    triggered_sink:Sinks.partial_sink ->
    extra_trace:Domains.ExtraTraceFirstHop.t ->
    issue_handles:Domains.IssueHandleSet.t ->
    unit

  val find : t -> Sinks.partial_sink -> BackwardTaint.t option
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
    issue list
end
