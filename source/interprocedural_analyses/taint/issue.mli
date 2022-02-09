(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
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

  val is_bottom : t -> bool

  val join : t -> t -> t
end

module Flows : sig
  type t = Flow.t list [@@deriving show]
end

module Candidate : sig
  type t = {
    flows: Flows.t;
    location: Location.WithModule.t;
  }

  val join : t -> t -> t
end

(* A unique identifier that represents the first sink of an issue. *)
module SinkHandle : sig
  type t =
    | Call of {
        callee: Target.t;
        index: int;
        parameter: AccessPath.Root.t;
      }
    | Global of {
        callee: Target.t;
        index: int;
      }
    | Return
    | LiteralStringSink of Sinks.t
    | ConditionalTestSink of Sinks.t

  val make_call : call_target:CallGraph.CallTarget.t -> root:AccessPath.Root.t -> t

  val make_global : call_target:CallGraph.CallTarget.t -> t
end

module SinkTreeWithHandle : sig
  type t = {
    sink_tree: BackwardState.Tree.t;
    handle: SinkHandle.t;
  }

  val filter_bottom : t list -> t list

  (* Discard handles, join sink trees into a single tree. *)
  val join : t list -> BackwardState.Tree.t
end

type t = {
  code: int;
  flow: Flow.t;
  location: Location.WithModule.t;
  (* Only used to create the Pyre errors. *)
  define: Ast.Statement.Define.t Ast.Node.t;
}

module TriggeredSinks : sig
  type t = String.Hash_set.t
end

val generate_source_sink_matches
  :  location:Location.WithModule.t ->
  sink_handle:SinkHandle.t ->
  source_tree:ForwardState.Tree.t ->
  sink_tree:BackwardState.Tree.t ->
  Candidate.t

val generate_issues : define:Define.t Node.t -> Candidate.t -> t list

val to_json : filename_lookup:(Reference.t -> string option) -> Target.t -> t -> Yojson.Safe.json

val generate_error : t -> Error.t

val code_metadata : unit -> Yojson.Safe.json

(* Will modify the triggered_sinks data structure, adding the newly triggered sinks. *)
val compute_triggered_sinks
  :  triggered_sinks:TriggeredSinks.t ->
  location:Location.WithModule.t ->
  sink_handle:SinkHandle.t ->
  source_tree:ForwardState.Tree.t ->
  sink_tree:BackwardState.Tree.t ->
  Sinks.partial_sink list * Candidate.t list

val source_can_match_rule : Sources.t -> bool

val sink_can_match_rule : Sinks.t -> bool
