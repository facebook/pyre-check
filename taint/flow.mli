(** Copyright (c) 2018-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Statement
open Domains


type flow = {
  source_taint: ForwardTaint.t;
  sink_taint: BackwardTaint.t;
}
[@@deriving sexp]

type flows = flow list
[@@deriving sexp]

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

val partition_flows:
  ?sources: (Sources.t -> bool)
  -> ?sinks: (Sinks.t -> bool)
  -> flows
  -> flow_state

val generate_source_sink_matches:
  location: Location.t
  -> source_tree: ForwardState.access_path_tree
  -> sink_tree: BackwardState.access_path_tree
  -> candidate

val generate_errors:
  define: Define.t Node.t
  -> candidate
  -> Interprocedural.Error.t list
