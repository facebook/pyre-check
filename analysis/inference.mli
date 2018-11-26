(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast

module Error = AnalysisError


module State : sig
  include module type of struct include TypeCheck.State end

  val initial_backward
    :  ?configuration: Configuration.Analysis.t
    -> Statement.Define.t Node.t
    -> forward: t
    -> t

  val update_only_existing_annotations: t -> t -> t
  val check_entry: t -> t
end

module Fixpoint : Fixpoint.Fixpoint with type state := State.t

val backward_fixpoint
  :  Cfg.t
  -> initial_forward: State.t
  -> initialize_backward: (forward: State.t -> State.t)
  -> Fixpoint.t

val infer
  :  configuration: Configuration.Analysis.t
  -> environment: (module Environment.Handler)
  -> source: Source.t
  -> TypeCheck.Result.t
