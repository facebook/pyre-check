(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Domains

module Backward : sig
  type model = {
    taint_in_taint_out: BackwardState.t;
    sink_taint: BackwardState.t;
  }
  [@@deriving show]

  val empty : model
end

module Forward : sig
  type model = { source_taint: ForwardState.t } [@@deriving show]

  val empty : model
end

module Mode : sig
  type sanitize_sources =
    | AllSources
    | SpecificSources of Sources.t list

  type sanitize_sinks =
    | AllSinks
    | SpecificSinks of Sinks.t list

  type sanitize_tito =
    | AllTito
    | SpecificTito of {
        sanitized_tito_sources: Sources.t list;
        sanitized_tito_sinks: Sinks.t list;
      }
  [@@deriving show, compare, eq]

  type sanitize = {
    sources: sanitize_sources option;
    sinks: sanitize_sinks option;
    tito: sanitize_tito option;
  }
  [@@deriving show, eq]

  type t =
    | SkipAnalysis (* Don't analyze at all *)
    | Sanitize of sanitize
    (* Analyze, but throw away inferred model *)
    | Normal
  [@@deriving show, eq]

  val join : t -> t -> t
end

type call_model = {
  forward: Forward.model;
  backward: Backward.model;
  mode: Mode.t;
}

val is_empty_model : call_model -> bool

val empty_skip_model : call_model (* Skips analysis *)

type result = Flow.issue list

include
  Interprocedural.Result.ANALYSIS_RESULT_WITH_REGISTRATION
    with type result := result
     and type call_model := call_model

val model_to_json
  :  filename_lookup:(Ast.Reference.t -> string option) ->
  Interprocedural.Callable.t ->
  call_model ->
  Yojson.Safe.t
