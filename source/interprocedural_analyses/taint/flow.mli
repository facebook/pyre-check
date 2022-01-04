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

type flow = {
  source_taint: ForwardTaint.t;
  sink_taint: BackwardTaint.t;
}
[@@deriving show]

type flows = flow list [@@deriving show]

type candidate = {
  flows: flows;
  location: Location.WithModule.t;
}

type features = {
  breadcrumbs: Features.BreadcrumbSet.t;
  first_indices: Features.FirstIndexSet.t;
  first_fields: Features.FirstFieldSet.t;
}

type flow_state = {
  matched: flows;
  rest: flows;
}

type issue = {
  code: int;
  flow: flow;
  features: features;
  issue_location: Location.WithModule.t;
  (* Only used to create the Pyre errors. *)
  define: Ast.Statement.Define.t Ast.Node.t;
}

type triggered_sinks = String.Hash_set.t

val generate_source_sink_matches
  :  location:Location.WithModule.t ->
  source_tree:ForwardState.Tree.t ->
  sink_tree:BackwardState.Tree.t ->
  candidate

val generate_issues : define:Define.t Node.t -> candidate -> issue list

val to_json
  :  filename_lookup:(Reference.t -> string option) ->
  Interprocedural.Target.t ->
  issue ->
  Yojson.Safe.json

val generate_error : issue -> Interprocedural.Error.t

val code_metadata : unit -> Yojson.Safe.json

(* Will modify the triggered_sinks data structure, adding the newly triggered sinks. *)
val compute_triggered_sinks
  :  triggered_sinks:triggered_sinks ->
  location:Location.WithModule.t ->
  source_tree:ForwardState.Tree.t ->
  sink_tree:BackwardState.Tree.t ->
  Sinks.partial_sink list * candidate list

val source_can_match_rule : Sources.t -> bool

val sink_can_match_rule : Sinks.t -> bool
