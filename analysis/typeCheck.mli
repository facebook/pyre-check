(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Expression

module Error = PyreError

module Coverage : sig
  type t = {
    full: int;
    partial: int;
    untyped: int;
    ignore: int;
  }

  val create_empty: t

  val full: t -> int
  val partial: t -> int
  val untyped: t -> int
  val ignore: t -> int

  val sum: t -> t -> t
end

module State : sig
  type t
  [@@deriving eq, show]

  val create
    :  ?configuration: Configuration.t
    -> environment: (module Environment.Handler)
    -> annotations: (Access.t * Annotation.t) list
    -> define: Statement.Define.t Node.t
    -> ?lookup: Lookup.t
    -> unit
    -> t

  val errors: t -> Error.t list

  val coverage: t -> Coverage.t

  val initial_forward
    :  ?configuration: Configuration.t
    -> ?lookup: Lookup.t
    -> (module Environment.Handler)
    -> Statement.Define.t Node.t
    -> t

  val initial_backward
    :  ?configuration: Configuration.t
    -> environment: (module Environment.Handler)
    -> Statement.Define.t Node.t
    -> forward:t
    -> t

  include Fixpoint.State with type t := t
end

module Fixpoint : Fixpoint.Fixpoint with type state := State.t

type result = {
  errors: Error.t list;
  lookup: Lookup.t option;
  type_coverage: Coverage.t;
}

val check
  :  Configuration.t
  -> (module Environment.Handler)
  -> Source.t
  -> result
