(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

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

val create
  : resolution: Resolution.t
  -> ?verify: bool
  -> model_source: string
  -> unit
  -> t list Or_error.t

val get_callsite_model
  :  resolution: Resolution.t
  -> call_target: [<Callable.t]
  -> arguments: Argument.t list
  -> t
