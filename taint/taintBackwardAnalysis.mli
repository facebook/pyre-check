(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Statement
open Analysis
open TaintDomains


type backward_model = {
  taint_in_taint_out: BackwardState.t;
  backward_taint: BackwardState.t;
}
[@@deriving show]


module FixpointState : sig

  type t = {
    taint: BackwardState.t;
    models: backward_model list;
  }

  val create: unit -> t

  val show_models: t option -> string

  include Fixpoint.State with type t := t
end


module Analyzer : Fixpoint.Fixpoint with type state = FixpointState.t


val run: Define.t -> backward_model option
