(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Statement

module Error = AnalysisError


module State : sig
  type t
  [@@deriving eq]

  val create
    :  ?configuration: Configuration.Analysis.t
    -> ?bottom: bool
    -> resolution: Resolution.t
    -> define: Statement.Define.t Node.t
    -> unit
    -> t
  val initial
    :  ?configuration: Configuration.Analysis.t
    -> resolution: Resolution.t
    -> Define.t Node.t
    -> t

  val initial_forward
    :  configuration: Configuration.Analysis.t
    -> resolution: Resolution.t
    -> Statement.Define.t Node.t
    -> t
  val initial_backward
    :  ?configuration: Configuration.Analysis.t
    -> Statement.Define.t Node.t
    -> forward: t
    -> t

  include Fixpoint.State with type t := t
end

module Fixpoint : Fixpoint.Fixpoint with type state := State.t

val backward_fixpoint
  :  Cfg.t
  -> initial_forward: State.t
  -> initialize_backward: (forward: State.t -> State.t)
  -> Fixpoint.t

val name: string

val run
  :  configuration: Configuration.Analysis.t
  -> environment: (module Environment.Handler)
  -> source: Source.t
  -> Error.t list
