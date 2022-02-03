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

module Flow : sig
  type t = {
    source_taint: ForwardTaint.t;
    sink_taint: BackwardTaint.t;
  }
  [@@deriving show]
end

module Flows : sig
  type t = Flow.t list [@@deriving show]
end

module Candidate : sig
  type t = {
    flows: Flows.t;
    location: Location.WithModule.t;
  }
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
  source_tree:ForwardState.Tree.t ->
  sink_tree:BackwardState.Tree.t ->
  Candidate.t

val generate_issues : define:Define.t Node.t -> Candidate.t -> t list

val to_json
  :  filename_lookup:(Reference.t -> string option) ->
  Interprocedural.Target.t ->
  t ->
  Yojson.Safe.json

val generate_error : t -> Interprocedural.Error.t

val code_metadata : unit -> Yojson.Safe.json

(* Will modify the triggered_sinks data structure, adding the newly triggered sinks. *)
val compute_triggered_sinks
  :  triggered_sinks:TriggeredSinks.t ->
  location:Location.WithModule.t ->
  source_tree:ForwardState.Tree.t ->
  sink_tree:BackwardState.Tree.t ->
  Sinks.partial_sink list * Candidate.t list

val source_can_match_rule : Sources.t -> bool

val sink_can_match_rule : Sinks.t -> bool
