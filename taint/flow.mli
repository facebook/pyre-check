(* Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast
open Statement
open Domains

type flow = {
  source_taint: ForwardTaint.t;
  sink_taint: BackwardTaint.t;
}
[@@deriving sexp]

type flows = flow list [@@deriving sexp]

type candidate = {
  flows: flows;
  location: Location.t;
}
[@@deriving sexp]

type flow_state = {
  matched: flows;
  rest: flows;
}
[@@deriving sexp]

type issue = {
  code: int;
  flow: flow;
  issue_location: Location.t;
  (* Only used to create the Pyre errors. *)
  define: Ast.Statement.Define.t Ast.Node.t;
}
[@@deriving sexp]

val partition_flows
  :  ?sources:(Sources.t -> bool) ->
  ?sinks:(Sinks.t -> bool) ->
  flows ->
  flow_state

val generate_source_sink_matches
  :  location:Location.t ->
  source_tree:ForwardState.Tree.t ->
  sink_tree:BackwardState.Tree.t ->
  candidate

val generate_issues : define:Define.t Node.t -> candidate -> issue list

val to_json
  :  environment:Analysis.Environment.t ->
  Interprocedural.Callable.t ->
  issue ->
  Yojson.Safe.json

val generate_error : issue -> Interprocedural.Error.t

val code_metadata : unit -> Yojson.Safe.json
