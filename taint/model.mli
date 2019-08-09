(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast
open Analysis
open Interprocedural

type t = {
  is_obscure: bool;
  call_target: Callable.t;
  model: TaintResult.call_model;
}
[@@deriving show, sexp]

exception InvalidModel of string

(* Exposed for testing. *)
val demangle_class_attribute : string -> string

val get_callsite_model
  :  call_target:[< Callable.t ] ->
  arguments:Expression.Call.Argument.t list ->
  t

val get_global_sink_model
  :  resolution:Resolution.t ->
  location:Location.t ->
  expression:Expression.t ->
  Domains.BackwardState.Tree.t option

val parse
  :  resolution:Resolution.t ->
  ?path:PyrePath.t ->
  source:string ->
  configuration:Configuration.t ->
  TaintResult.call_model Callable.Map.t ->
  TaintResult.call_model Callable.Map.t
