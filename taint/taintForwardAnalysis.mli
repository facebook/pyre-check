(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Analysis
open TaintDomains
open Statement


type forward_model = {
  define_name: Ast.Statement.Access.t;
  source_taint: ForwardState.t;
}
[@@deriving show]


module FixpointState : sig

  type t = {
    taint: ForwardState.t;
    models: forward_model list;
  }

  val create: unit -> t

  val show_models: t option -> string

  include Fixpoint.State with type t := t
end


module Analyzer : Fixpoint.Fixpoint with type state := FixpointState.t


val run: Define.t -> forward_model option
