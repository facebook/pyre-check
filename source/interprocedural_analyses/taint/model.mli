(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Pyre
open Ast
open Analysis
open Interprocedural

type t = {
  call_target: Target.t;
  model: TaintResult.call_model;
}
[@@deriving show]

exception InvalidModel of string

val get_callsite_model
  :  resolution:Resolution.t ->
  call_target:[< Target.t ] ->
  arguments:Expression.Call.Argument.t list ->
  t

module GlobalModel : sig
  type t

  val get_source : t -> Domains.ForwardState.Tree.t

  val get_sink : t -> Domains.BackwardState.Tree.t

  val get_tito : t -> Domains.BackwardState.Tree.t

  val get_sanitize : t -> Domains.Sanitize.t

  val get_modes : t -> TaintResult.ModeSet.t

  val is_sanitized : t -> bool
end

val get_global_model
  :  resolution:Resolution.t ->
  location:Location.WithModule.t ->
  expression:Expression.t ->
  GlobalModel.t

val get_model_sources : paths:Path.t list -> (Path.t * string) list

val infer_class_models
  :  environment:TypeEnvironment.ReadOnly.t ->
  TaintResult.call_model Target.Map.t

val is_obscure : TaintResult.call_model -> bool

val remove_obscureness : TaintResult.call_model -> TaintResult.call_model

val remove_sinks : TaintResult.call_model -> TaintResult.call_model

val add_obscure_sink
  :  resolution:Resolution.t ->
  call_target:[< Target.t ] ->
  TaintResult.call_model ->
  TaintResult.call_model

(* Create a symbolic callable representing an unknown callee at a call site. *)
val unknown_callee : location:Location.WithModule.t -> call:Expression.expression -> Target.t

(* Register a model with sinks on all parameters for a symbolic callable that
 * represents an unknown callee, in order to find missing flows. *)
val register_unknown_callee_model : [< Target.t ] -> unit

(* Apply sanitizers on a given model at the end of the forward and backward analysis. *)
val apply_sanitizers : TaintResult.call_model -> TaintResult.call_model
