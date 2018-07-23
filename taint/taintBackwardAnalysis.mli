(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Statement
open Analysis
open TaintDomains


module FixpointState : sig
  type t = { taint: BackwardState.t }
  val create: unit -> t
  include Fixpoint.State with type t := t
end

module Analyzer : Fixpoint.Fixpoint with type state = FixpointState.t

val run: Define.t -> TaintResult.Backward.model
