(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module PyrePysaEnvironment = Analysis.PyrePysaEnvironment

(** Represents the set of information that must be propagated from callees to callers during an
    interprocedural analysis, within the global fixpoint. Each iteration should produce a model for
    each callable (function, method). This must have an abstract domain structure (e.g, join, widen,
    less_or_equal, etc.) *)
module type MODEL = sig
  type t [@@deriving show]

  val join : iteration:int -> t -> t -> t

  val widen : iteration:int -> callable:Target.t -> previous:t -> next:t -> t

  val less_or_equal : callable:Target.t -> left:t -> right:t -> bool

  (** Transform the model before joining into the override model. *)
  val for_override_model : callable:Target.t -> t -> t

  (** Initial models for the dependencies that are newly discovered during the fixpoint. *)
  val for_new_dependency : get_model:(Target.t -> t option) -> Target.t -> t
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

  val reached_maximum_iteration_exit : iteration:int -> callables_to_analyze:Target.t list -> unit

  (** This is called at the beginning of each iteration. *)
  val iteration_start
    :  iteration:int ->
    callables_to_analyze:Target.t list ->
    number_of_callables:int ->
    unit

  (** This is called at the end of each iteration. *)
  val iteration_end
    :  iteration:int ->
    expensive_callables:expensive_callable list ->
    number_of_callables:int ->
    timer:Timer.t ->
    unit

  (** This is called after a worker made progress on an iteration. *)
  val iteration_progress
    :  iteration:int ->
    callables_processed:int ->
    number_of_callables:int ->
    unit

  val is_expensive_callable : callable:Target.t -> timer:Timer.t -> bool

  (** This is called after analyzing an override target (i.e, joining models of overriding methods). *)
  val override_analysis_end : callable:Target.t -> timer:Timer.t -> unit

  val on_analyze_define_exception : iteration:int -> callable:Target.t -> exn:exn -> unit

  val on_approaching_max_iterations
    :  max_iterations:int ->
    current_iteration:int ->
    ('a, Format.formatter, unit, unit, unit, unit) format6 ->
    'a
end

(** Must be implemented to perform a global fixpoint. *)
module type ANALYSIS = sig
  (** Passed down from the top level call to the `analyze_define` function. This should be cheap to
      marshal, since it will be sent to multiple workers. *)
  type context

  module Model : MODEL

  module Result : RESULT

  module Logger : LOGGER

  val initial_model : Model.t

  val empty_model : Model.t

  (** Model for obscure callables (usually, stubs) *)
  val obscure_model : Model.t

  module AnalyzeDefineResult : sig
    type t = {
      result: Result.t;
      model: Model.t;
      additional_dependencies: Target.t list;
    }
  end

  (** Analyze a function or method definition.

      `get_callee_model` can be used to get the model of a callee, as long as it is registered in
      the call graph. *)
  val analyze_define
    :  context:context ->
    callable:Target.t ->
    previous_model:Model.t ->
    get_callee_model:(Target.t -> Model.t option) ->
    AnalyzeDefineResult.t
end

module Make (Analysis : ANALYSIS) : sig
  (** Represents a mapping from target to models, living in the ocaml heap. *)
  module Registry : sig
    type t

    val empty : t

    val is_empty : t -> bool

    val singleton : target:Target.t -> model:Analysis.Model.t -> t

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

    val object_targets : t -> Target.Set.t

    val fold : init:'a -> f:(target:Target.t -> model:Analysis.Model.t -> 'a -> 'a) -> t -> 'a
  end

  (** Represents a mapping from target to models, living in shared memory. *)
  module SharedModels : sig
    include
      Hack_parallel.Std.SharedMemory.FirstClassWithKeys.S
        with type key = Target.t
         and type value = Analysis.Model.t

    val targets : t -> Target.t list

    val size : t -> int

    val of_alist_parallel : scheduler:Scheduler.t -> (Target.t * Analysis.Model.t) list -> t

    val join_with_registry_sequential
      :  t ->
      model_join:(Analysis.Model.t -> Analysis.Model.t -> Analysis.Model.t) ->
      Registry.t ->
      t

    val fold_sequential
      :  t ->
      init:'a ->
      f:(target:Target.t -> model:Analysis.Model.t -> 'a -> 'a) ->
      'a

    val map_parallel_targets
      :  t ->
      scheduler:Scheduler.t ->
      f:(target:Target.t -> model:Analysis.Model.t -> Analysis.Model.t) ->
      targets:Target.t list ->
      t

    val map_parallel
      :  t ->
      scheduler:Scheduler.t ->
      f:(target:Target.t -> model:Analysis.Model.t -> Analysis.Model.t) ->
      t

    val get_model : ReadOnly.t -> Target.t -> Analysis.Model.t option
  end

  module Epoch : sig
    type t

    val predefined : t

    val initial : t
  end

  module State : sig
    type t

    module ReadOnly : sig
      type t

      val get_model : t -> Target.t -> Analysis.Model.t option

      val get_result : t -> Target.t -> Analysis.Result.t
    end

    val read_only : t -> ReadOnly.t

    (** Remove the fixpoint state from the shared memory. This must be called before computing
        another fixpoint. *)
    val cleanup : keep_models:bool -> t -> unit

    val targets : t -> Target.t list

    val update_models
      :  scheduler:Scheduler.t ->
      update_model:(target:Target.t -> model:Analysis.Model.t -> Analysis.Model.t) ->
      t ->
      t
  end

  type t = {
    fixpoint_reached_iterations: int;
    state: State.t;
  }

  val record_initial_models
    :  scheduler:Scheduler.t ->
    initial_models:SharedModels.t ->
    callables_to_analyze:Target.t list ->
    stubs:Target.t list ->
    override_targets:Target.t list ->
    State.t

  val compute
    :  scheduler:Scheduler.t ->
    scheduler_policy:Scheduler.Policy.t ->
    override_graph:OverrideGraph.SharedMemory.ReadOnly.t ->
    dependency_graph:DependencyGraph.t ->
    skip_analysis_targets:Target.HashSet.t ->
    context:Analysis.context ->
    callables_to_analyze:Target.t list ->
    max_iterations:int ->
    error_on_max_iterations:bool ->
    epoch:Epoch.t ->
    state:State.t ->
    t
end

module WithoutLogging : LOGGER

module WithLogging (_ : sig
  val expensive_callable_ms : int
end) : LOGGER
