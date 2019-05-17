(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

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
    -> ?resolution_fixpoint: ResolutionSharedMemory.annotation_map Int.Map.Tree.t
    -> unit
    -> t

  val errors: t -> Error.t list

  val initial
    :  ?configuration: Configuration.Analysis.t
    -> resolution: Resolution.t
    -> Define.t Node.t
    -> t

  val resolve_exports: resolution: Resolution.t -> Reference.t -> Reference.t

  (* Visible for testing. *)
  type resolved = {
    state: t;
    resolved: Type.t;
  }

  val parse_and_check_annotation: ?bind_variables:bool ->  state: t -> Expression.t -> t * Type.t
  val forward_expression: state: t -> expression: Expression.t -> resolved
  val forward_statement: state: t -> statement: Statement.t -> t

  include Fixpoint.State with type t := t

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

  val update_only_existing_annotations: t -> t -> t
  val check_entry: t -> t
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
