(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Analysis
open Expression
open Interprocedural


type t = {
  is_obscure: bool;
  call_target: Callable.t;
  model: TaintResult.call_model;
}
[@@deriving show, sexp]

exception InvalidModel of string

val create
  : resolution: Resolution.t
  -> ?verify: bool
  -> model_source: string
  -> unit
  -> t list

val get_callsite_model
  :  resolution: Resolution.t
  -> call_target: [<Callable.t]
  -> arguments: Argument.t list
  -> t

val parse
  :  resolution: Resolution.t
  -> source:string
  -> TaintResult.call_model Callable.Map.t
  -> TaintResult.call_model Callable.Map.t
