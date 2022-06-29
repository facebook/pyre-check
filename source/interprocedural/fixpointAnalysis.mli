(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
module TypeEnvironment = Analysis.TypeEnvironment
module Kind = AnalysisKind

(** Represents the set of information that must be propagated from callees to callers during an
    interprocedural analysis, within the global fixpoint. Each iteration should produce a model for
    each callable (function, method). This must have an abstract domain structure (e.g, join, widen,
    less_or_equal, etc.) *)
module type MODEL = sig
  type t [@@deriving show]

  val join : iteration:int -> t -> t -> t

  val widen : iteration:int -> callable:Target.t -> previous:t -> next:t -> t

  val reached_fixpoint : iteration:int -> callable:Target.t -> previous:t -> next:t -> bool

  val strip_for_callsite : t -> t
  (** Remove aspects from the model that are not needed at call sites. Just for optimization. *)
end

(** Represents the result of the analysis.

    Each iteration should produce results for each callable (function, method). Results from the
    previous iterations are discarded. This is usually used for a set of errors. In the taint
    analysis, this represents valid issues. *)
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
  (** This is called at the begining of each iteration. *)

  val iteration_end
    :  iteration:int ->
    expensive_callables:expensive_callable list ->
    number_of_callables:int ->
    timer:Timer.t ->
    unit
  (** This is called at the end of each iteration. *)

  val iteration_progress
    :  iteration:int ->
    callables_processed:int ->
    number_of_callables:int ->
    unit
  (** This is called after a worker made progress on an iteration. *)

  val is_expensive_callable : callable:Target.t -> timer:Timer.t -> bool

  val override_analysis_end : callable:Target.t -> timer:Timer.t -> unit
  (** This is called after analyzing an override target (i.e, joining models of overriding methods). *)

  val on_analyze_define_exception : iteration:int -> callable:Target.t -> exn:exn -> unit

  val on_global_fixpoint_exception : exn:exn -> unit
end

(** Must be implemented to perform a global fixpoint. *)
module type ANALYSIS = sig
  type context
  (** Passed down from the top level call to the `analyze_define` function. This should be cheap to
      marshal, since it will be sent to multiple workers. *)

  module Model : MODEL

  module Result : RESULT

  module Logger : LOGGER

  val initial_model : Model.t

  val empty_model : Model.t

  val obscure_model : Model.t
  (** Model for obscure callables (usually, stubs) *)

  val analyze_define
    :  context:context ->
    qualifier:Reference.t ->
    callable:Target.t ->
    define:Statement.Define.t Node.t ->
    previous_model:Model.t ->
    get_callee_model:(Target.t -> Model.t option) ->
    Result.t * Model.t
  (** Analyze a function or method definition.

      `get_callee_model` can be used to get the model of a callee, as long as it was registered in
      the call graph. *)
end

module Make (Analysis : ANALYSIS) : sig
  (** Represents a mapping from target to models, living in the ocaml heap. *)
  module Registry : sig
    type t

    val empty : t

    val size : t -> int

    val set : t -> target:Target.t -> model:Analysis.Model.t -> t

    val add
      :  join:(Analysis.Model.t -> Analysis.Model.t -> Analysis.Model.t) ->
      t ->
      target:Target.t ->
      model:Analysis.Model.t ->
      t

    val get : t -> Target.t -> Analysis.Model.t option

    val merge : join:(Analysis.Model.t -> Analysis.Model.t -> Analysis.Model.t) -> t -> t -> t

    val of_alist
      :  join:(Analysis.Model.t -> Analysis.Model.t -> Analysis.Model.t) ->
      (Target.t * Analysis.Model.t) list ->
      t

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

  val compute
    :  scheduler:Scheduler.t ->
    type_environment:TypeEnvironment.ReadOnly.t ->
    override_graph:OverrideGraph.SharedMemory.t ->
    dependency_graph:DependencyGraph.t ->
    context:Analysis.context ->
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
  (** Remove the fixpoint state from the shared memory. This must be called before computing another
      fixpoint. *)
end

module WithoutLogging : LOGGER

module WithLogging (Config : sig
  val expensive_callable_ms : int
end) : LOGGER
