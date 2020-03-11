(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

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
  :  call_target:[< Callable.t ] ->
  arguments:Expression.Call.Argument.t list ->
  t

val get_global_sink_model
  :  resolution:Resolution.t ->
  location:Location.WithModule.t ->
  expression:Expression.t ->
  Domains.BackwardState.Tree.t option

val get_global_tito_model
  :  resolution:Resolution.t ->
  expression:Expression.t ->
  Domains.BackwardState.Tree.t option

val get_model_sources : paths:Path.t list -> (Path.t * string) list

val infer_class_models
  :  environment:TypeEnvironment.ReadOnly.t ->
  TaintResult.call_model Callable.Map.t
