(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
module TypeEnvironment = Analysis.TypeEnvironment
module Kind = AnalysisKind

(* See `.ml` for documentation of modules and functions. *)

module type MODEL = sig
  type t [@@deriving show]

  val join : iteration:int -> t -> t -> t

  val widen : iteration:int -> callable:Target.t -> previous:t -> next:t -> t

  val reached_fixpoint : iteration:int -> callable:Target.t -> previous:t -> next:t -> bool

  val strip_for_callsite : t -> t
end

module type RESULT = sig
  type t

  val empty : t
end

type expensive_callable = {
  time_to_analyze_in_ms: int;
  callable: Target.t;
}

module type LOGGER = sig
  val initial_models_stored : timer:Timer.t -> unit

  val reached_maximum_iteration_exception
    :  iteration:int ->
    callables_to_analyze:Target.t list ->
    exn

  val iteration_start
    :  iteration:int ->
    callables_to_analyze:Target.t list ->
    number_of_callables:int ->
    unit

  val iteration_end
    :  iteration:int ->
    expensive_callables:expensive_callable list ->
    number_of_callables:int ->
    timer:Timer.t ->
    unit

  val iteration_progress
    :  iteration:int ->
    callables_processed:int ->
    number_of_callables:int ->
    unit

  val is_expensive_callable : callable:Target.t -> timer:Timer.t -> bool

  val override_analysis_end : callable:Target.t -> timer:Timer.t -> unit

  val on_analyze_define_exception : iteration:int -> callable:Target.t -> exn:exn -> unit

  val on_global_fixpoint_exception : exn:exn -> unit
end

module type ANALYSIS = sig
  type context

  module Model : MODEL

  module Result : RESULT

  module Logger : LOGGER

  val initial_model : Model.t

  val empty_model : Model.t

  val obscure_model : Model.t

  val analyze_define
    :  context:context ->
    qualifier:Reference.t ->
    callable:Target.t ->
    define:Statement.Define.t Node.t ->
    previous_model:Model.t ->
    get_callee_model:(Target.t -> Model.t option) ->
    Result.t * Model.t
end

module Make (Analysis : ANALYSIS) : sig
  module Registry : sig
    type t

    val empty : t

    val set : t -> target:Target.t -> model:Analysis.Model.t -> t

    val add : t -> target:Target.t -> model:Analysis.Model.t -> t

    val get : t -> Target.t -> Analysis.Model.t option

    val merge : t -> t -> t

    val of_alist : (Target.t * Analysis.Model.t) list -> t

    val to_alist : t -> (Target.t * Analysis.Model.t) list

    val iteri : t -> f:(target:Target.t -> model:Analysis.Model.t -> unit) -> unit

    val map : t -> f:(Analysis.Model.t -> Analysis.Model.t) -> t

    val targets : t -> Target.t list

    val object_targets : t -> Target.HashSet.t
  end

  module Epoch : sig
    type t

    val predefined : t

    val initial : t
  end

  type t

  (* TODO(T117715045): Define a proper dependency graph module and type. *)
  type dependency_graph = Target.t list Target.Map.t

  val compute
    :  scheduler:Scheduler.t ->
    type_environment:TypeEnvironment.ReadOnly.t ->
    context:Analysis.context ->
    dependency_graph:dependency_graph ->
    initial_callables:Target.t list ->
    stubs:Target.t list ->
    override_targets:Target.t list ->
    callables_to_analyze:Target.t list ->
    initial_models:Registry.t ->
    max_iterations:int ->
    epoch:Epoch.t ->
    t

  val get_result : t -> Target.t -> Analysis.Result.t

  val get_model : t -> Target.t -> Analysis.Model.t option

  val get_iterations : t -> int

  val cleanup : t -> unit
end

module WithoutLogging : LOGGER

module WithLogging (Config : sig
  val expensive_callable_ms : int
end) : LOGGER
