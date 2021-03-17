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
  is_obscure: bool;
  call_target: Callable.t;
  model: TaintResult.call_model;
}
[@@deriving show]

exception InvalidModel of string

val get_callsite_model
  :  resolution:Resolution.t ->
  call_target:[< Callable.t ] ->
  arguments:Expression.Call.Argument.t list ->
  t

val get_global_sink_model
  :  resolution:Resolution.t ->
  location:Location.WithModule.t ->
  expression:Expression.t ->
  Domains.BackwardState.Tree.t option

val get_global_tito_model_and_mode
  :  resolution:Resolution.t ->
  expression:Expression.t ->
  Domains.BackwardState.Tree.t option * TaintResult.Mode.t option

val global_is_sanitized : resolution:Resolution.t -> expression:Expression.t -> bool

val get_model_sources : paths:Path.t list -> (Path.t * string) list

val infer_class_models
  :  environment:TypeEnvironment.ReadOnly.t ->
  TaintResult.call_model Callable.Map.t

val remove_sinks : TaintResult.call_model -> TaintResult.call_model

val add_obscure_sink
  :  resolution:Resolution.t ->
  call_target:[< Callable.t ] ->
  TaintResult.call_model ->
  TaintResult.call_model

(* Create a symbolic callable representing an unknown callee at a call site. *)
val unknown_callee : location:Location.WithModule.t -> call:Expression.expression -> Callable.t

(* Register a model with sinks on all parameters for a symbolic callable that
 * represents an unknown callee, in order to find missing flows. *)
val register_unknown_callee_model : [< Callable.t ] -> unit
