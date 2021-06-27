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

module Sanitize : sig
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

  type t = {
    sources: sanitize_sources option;
    sinks: sanitize_sinks option;
    tito: sanitize_tito option;
  }
  [@@deriving show, eq]

  val empty : t

  val is_empty : t -> bool

  val join : t -> t -> t
end

module Mode : sig
  type t =
    | Obscure
    | SkipAnalysis (* Don't analyze at all *)
    | SkipDecoratorWhenInlining
    | SkipOverrides
  [@@deriving show, compare]
end

module ModeSet : sig
  type t [@@deriving show]

  val singleton : Mode.t -> t

  val empty : t

  val is_empty : t -> bool

  val add : Mode.t -> t -> t

  val remove : Mode.t -> t -> t

  val contains : Mode.t -> t -> bool

  val join : t -> t -> t
end

type call_model = {
  forward: Forward.model;
  backward: Backward.model;
  sanitize: Sanitize.t;
  modes: ModeSet.t;
}

val is_empty_model : with_modes:ModeSet.t -> call_model -> bool

val empty_skip_model : call_model (* Skips analysis *)

val should_externalize_model : call_model -> bool

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

val decorators_to_skip
  :  Interprocedural.Result.model_t Interprocedural.Callable.Map.t ->
  Ast.Reference.Set.t
