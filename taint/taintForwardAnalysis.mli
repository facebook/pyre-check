(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Analysis
open TaintDomains
open Statement


module Model : sig
  type t = {
    define_name: Ast.Statement.Access.t;
    source_taint: ForwardState.t;
  }
  [@@deriving show]

  val create: Define.t list -> Resolution.t -> t
end


module FixpointState : sig

  type t = {
    taint: ForwardState.t;
    models: Model.t list;
  }

  val create: unit -> t

  val show_models: t option -> string

  include Fixpoint.State with type t := t
end


module Analyzer : Fixpoint.Fixpoint with type state := FixpointState.t


val run: Cfg.t -> FixpointState.t option
