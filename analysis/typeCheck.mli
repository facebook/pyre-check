(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Expression

module Error = PyreError

module State : sig
  type t
  [@@deriving eq, show]

  type coverage = {
    full: int;
    partial: int;
    untyped: int;
  }

  val create
    :  environment:(module Environment.Reader)
    -> annotations:(Access.t * Annotation.t) list
    -> define:Statement.define Node.t
    -> ?lookup: Lookup.t
    -> unit
    -> t

  val errors
    :  (int * int list) list
    -> Configuration.t
    -> t
    -> Error.t list

  val coverage: t -> coverage

  val initial_forward
    : ?lookup: Lookup.t
    -> (module Environment.Reader)
    -> Statement.define Node.t
    -> t

  val initial_backward
    :  environment:(module Environment.Reader)
    -> Statement.define Node.t
    -> forward:t
    -> t

  include Fixpoint.State with type t := t
end

module Fixpoint : Fixpoint.Fixpoint with type state := State.t

type result = {
  errors: Error.t list;
  lookup: Lookup.t option;
}

val check
  :  Configuration.t
  -> (module Environment.Reader)
  -> Source.t
  -> result
