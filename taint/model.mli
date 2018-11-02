(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Analysis
open Expression
open Interprocedural


type t = {
  call_target: Callable.t;
  model: TaintResult.call_model;
}
[@@deriving show, sexp]

val create: resolution: Resolution.t -> model_source: string -> t list Or_error.t

val get_callsite_model
  :  resolution: Resolution.t
  -> call_target: [<Callable.t]
  -> arguments: Argument.t list
  -> Interprocedural.Result.model_t option
