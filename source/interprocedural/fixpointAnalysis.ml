(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* FixpointAnalysis: implements a generic global fixpoint analysis, which infers
 * a set of invariants across a source code.
 *
 * This is mainly used by the taint analysis, but could be used for any
 * interprocedural analysis (e.g, type inference, correctness analysis, etc..).
 *
 * A model describes the invariants of a function or method, and must have an
 * abstract domain structure (join, widening, less or equal, etc..).
 *
 * Given a set of initial models, this performs iterations to propagate invariants
 * across functions, until reaching a fixpoint (i.e, invariants are stables).
 *)

open Core
open Pyre
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

  (** This is called after a worker makes progress on an iteration. *)
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

(** Must be implemented to compute a global fixpoint. *)
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

module Make (Analysis : ANALYSIS) = struct
  module Model = Analysis.Model
  module Result = Analysis.Result
  module Logger = Analysis.Logger

  (** Represents a mapping from target to models, living in the ocaml heap. *)
  module Registry = struct
    type t = Model.t Target.Map.t

    let empty = Target.Map.empty

    let is_empty = Target.Map.is_empty

    let singleton ~target ~model = Target.Map.singleton target model

    let size registry = Target.Map.cardinal registry

    let set registry ~target ~model = Target.Map.add target model registry

    let add ~join registry ~target ~model =
      Target.Map.update
        target
        (function
          | None -> Some model
          | Some existing -> Some (join existing model))
        registry


    let get target registry = Target.Map.find_opt registry target

    let merge ~join left right =
      Target.Map.union (fun _ left right -> Some (join left right)) left right


    let of_alist ~join = Target.Map.of_alist ~f:join

    let to_alist registry = Target.Map.to_alist registry

    let iteri registry ~f = Target.Map.iter (fun key data -> f ~target:key ~model:data) registry

    let map registry ~f = Target.Map.map f registry

    let targets registry = Target.Map.keys registry

    let object_targets registry =
      let add target _ so_far =
        if Target.is_object target then
          Target.Set.add target so_far
        else
          so_far
      in
      Target.Map.fold add registry Target.Set.empty


    let fold ~init ~f map =
      Target.Map.fold (fun key data so_far -> f ~target:key ~model:data so_far) map init
  end

  module SharedModels = struct
    module T =
      Hack_parallel.Std.SharedMemory.FirstClassWithKeys.Make
        (Target.SharedMemoryKey)
        (struct
          type t = Model.t

          let prefix = Hack_parallel.Std.Prefix.make ()

          let description = "InterproceduralFixpointModel"
        end)

    include T

    let targets = T.keys

    let size handle = handle |> T.keys |> List.length

    let of_alist_parallel ~scheduler list =
      let policy =
        Scheduler.Policy.fixed_chunk_size
          ~minimum_chunks_per_worker:1
          ~minimum_chunk_size:1
          ~preferred_chunk_size:1000
          ()
      in
      let map_reduce =
        Scheduler.map_reduce scheduler ~policy ~initial:() ~reduce:(fun () () -> ())
      in
      T.of_alist_parallel ~map_reduce list


    let join_with_registry_sequential models ~model_join registry =
      T.merge_with_alist_sequential models ~f:model_join (Registry.to_alist registry)


    let fold_sequential models ~init ~f =
      T.fold_sequential models ~init ~f:(fun ~key ~value accumulator ->
          f ~target:key ~model:value accumulator)


    let map_parallel_targets models ~scheduler ~f ~targets =
      let policy =
        Scheduler.Policy.fixed_chunk_size
          ~minimum_chunks_per_worker:1
          ~minimum_chunk_size:1
          ~preferred_chunk_size:1000
          ()
      in
      let map_reduce = Scheduler.map_reduce scheduler ~policy in
      T.map_parallel_keys
        models
        ~map_reduce
        ~f:(fun ~key ~value -> f ~target:key ~model:value)
        ~keys:targets


    let map_parallel models ~scheduler ~f =
      map_parallel_targets models ~scheduler ~f ~targets:(T.keys models)


    let get_new_model = ReadOnly.get ~cache:true

    let get_old_model = ReadOnly.get_old

    let get_model shared_models callable =
      match get_new_model shared_models callable with
      | Some _ as model -> model
      | None -> get_old_model shared_models callable
  end

  module Epoch = struct
    type t = int [@@deriving show]

    let predefined = 0

    let initial = 1
  end

  type step = {
    epoch: Epoch.t;
    iteration: int;
  }
  [@@deriving show]

  (* The fixpoint state, stored in shared memory. *)
  module State = struct
    module SharedResults =
      Hack_parallel.Std.SharedMemory.FirstClass.WithCache.Make
        (Target.SharedMemoryKey)
        (struct
          type t = Result.t

          let prefix = Hack_parallel.Std.Prefix.make ()

          let description = "InterproceduralFixpointResults"
        end)

    module MetaData = struct
      type t = {
        is_partial: bool;
        step: step;
      }
      [@@deriving show]
    end

    (* Caches the fixpoint state (is_partial) of a call model. *)
    module SharedFixpoint = struct
      include
        Hack_parallel.Std.SharedMemory.FirstClass.WithCache.Make
          (Target.SharedMemoryKey)
          (struct
            type t = MetaData.t

            let prefix = Hack_parallel.Std.Prefix.make ()

            let description = "InterproceduralFixpointMetadata"
          end)

      let get_meta_data shared_fixpoint callable =
        match get shared_fixpoint callable with
        | Some _ as meta_data -> meta_data
        | None -> get_old shared_fixpoint callable
    end

    type t = {
      shared_fixpoint: SharedFixpoint.t;
      shared_models: SharedModels.t;
      shared_results: SharedResults.t;
    }

    let get_new_model shared_models callable =
      SharedModels.PreserveKeyOnly.get shared_models ~cache:true callable


    let get_old_model shared_models callable =
      SharedModels.PreserveKeyOnly.get_old shared_models callable


    let get_model shared_models callable =
      match get_new_model shared_models callable with
      | Some _ as model -> model
      | None -> get_old_model shared_models callable


    module ReadOnly = struct
      type t = {
        shared_models: SharedModels.ReadOnly.t;
        shared_results: SharedResults.t;
      }

      let get_model { shared_models; _ } callable =
        match SharedModels.get_new_model shared_models callable with
        | Some _ as model -> model
        | None -> SharedModels.get_old_model shared_models callable


      let get_result { shared_results; _ } callable =
        SharedResults.get shared_results callable |> Option.value ~default:Result.empty
    end

    let read_only { shared_models; shared_results; _ } =
      { ReadOnly.shared_models = SharedModels.read_only shared_models; shared_results }


    type iteration_callable_result = {
      (* Whether to reanalyze this and its callers. *)
      is_partial: bool;
      (* Model to use at call sites. *)
      model: Model.t;
      (* The result of the analysis. *)
      result: Result.t;
      (* Need to be considered in the next iteration. *)
      additional_dependencies: DependencyGraph.t;
    }

    let set_callable_result ~shared_models ~shared_fixpoint ~shared_results step callable state =
      (* Separate diagnostics from state to speed up lookups, and cache fixpoint state
         separately. *)
      let shared_models =
        SharedModels.PreserveKeyOnly.set_new shared_models ~cache:true callable state.model
      in
      (* Skip result writing unless necessary (e.g. overrides don't have results) *)
      let () =
        if Target.is_function_or_method callable then
          SharedResults.add shared_results callable state.result
      in
      let () =
        SharedFixpoint.add shared_fixpoint callable { is_partial = state.is_partial; step }
      in
      shared_models


    let oldify { shared_models; shared_fixpoint; shared_results } callable_set =
      let callable_set = SharedModels.KeySet.of_list callable_set in
      let shared_models = SharedModels.oldify_batch shared_models callable_set in
      let () = SharedFixpoint.oldify_batch shared_fixpoint callable_set in

      (* Old results are never looked up, so remove them. *)
      let () = SharedResults.remove_batch shared_results callable_set in
      shared_models


    let remove_old { shared_models; shared_fixpoint; _ } callable_set =
      let callable_set = SharedModels.KeySet.of_list callable_set in
      let shared_models = SharedModels.remove_old_batch shared_models callable_set in
      let () = SharedFixpoint.remove_old_batch shared_fixpoint callable_set in
      shared_models


    let targets { shared_models; _ } = SharedModels.keys shared_models

    (** Remove the fixpoint state from the shared memory. This must be called before computing
        another fixpoint. *)
    let cleanup ~keep_models ({ shared_models; shared_fixpoint; shared_results } as state) =
      let targets = state |> targets |> SharedResults.KeySet.of_list in
      let () = if keep_models then () else SharedModels.cleanup ~clean_old:true shared_models in
      let () = SharedResults.remove_batch shared_results targets in
      let () = SharedFixpoint.remove_batch shared_fixpoint targets in
      let () = SharedFixpoint.remove_old_batch shared_fixpoint targets in
      ()


    let update_models ~scheduler ~update_model ({ shared_models; _ } as state) =
      let shared_models = SharedModels.map_parallel shared_models ~scheduler ~f:update_model in
      { state with shared_models }
  end

  let initialize_models_for_new_dependencies ~shared_models = function
    | [] -> shared_models
    | targets ->
        let existing_targets = shared_models |> SharedModels.keys |> Target.Set.of_list in
        targets
        |> List.filter ~f:(fun target -> not (Target.Set.mem target existing_targets))
        |> List.map ~f:(fun target ->
               ( target,
                 Model.for_new_dependency
                   ~get_model:
                     (SharedModels.ReadOnly.get (SharedModels.read_only shared_models) ~cache:false)
                   target ))
        |> List.fold
             ~init:(SharedModels.add_only shared_models)
             ~f:(fun shared_models (callable, model) ->
               SharedModels.AddOnly.add shared_models callable model)
        |> SharedModels.from_add_only


  let initialize_meta_data_for_new_dependencies ~shared_fixpoint ~step =
    List.iter ~f:(fun callable ->
        State.SharedFixpoint.add shared_fixpoint callable { is_partial = false; step })


  (* Save initial models in the shared memory. *)
  let record_initial_models
      ~scheduler
      ~initial_models
      ~callables_to_analyze
      ~stubs
      ~override_targets
    =
    let timer = Timer.start () in
    let shared_fixpoint = State.SharedFixpoint.create () in
    let add_initial_fixpoint_state target =
      State.SharedFixpoint.add
        shared_fixpoint
        target
        { is_partial = false; step = { epoch = Epoch.initial; iteration = 0 } }
    in
    let policy =
      Scheduler.Policy.fixed_chunk_size
        ~minimum_chunks_per_worker:1
        ~minimum_chunk_size:1
        ~preferred_chunk_size:1000
        ()
    in
    (* Add initial fixpoint state for all targets with an initial model *)
    let () =
      let map targets = List.iter targets ~f:add_initial_fixpoint_state in
      Scheduler.map_reduce
        scheduler
        ~policy
        ~initial:()
        ~map
        ~reduce:(fun () () -> ())
        ~inputs:(SharedModels.keys initial_models)
        ()
    in
    let add_models_for_targets ~targets ~model initial_models =
      let initial_models = SharedModels.add_only initial_models in
      let empty_initial_models = SharedModels.AddOnly.create_empty initial_models in
      let map targets =
        List.fold
          ~init:empty_initial_models
          ~f:(fun initial_models target ->
            let () = add_initial_fixpoint_state target in
            SharedModels.AddOnly.add initial_models target model)
          targets
      in
      Scheduler.map_reduce
        scheduler
        ~policy
        ~initial:initial_models
        ~map
        ~reduce:(fun left right ->
          SharedModels.AddOnly.merge_same_handle_disjoint_keys ~smaller:left ~larger:right)
        ~inputs:targets
        ()
      |> SharedModels.from_add_only
    in
    let discard_targets_with_model initial_models targets =
      let targets_with_model = initial_models |> SharedModels.keys |> Target.Set.of_list in
      List.filter ~f:(fun target -> not (Target.Set.mem target targets_with_model)) targets
    in
    let initial_models =
      add_models_for_targets
        ~targets:(discard_targets_with_model initial_models callables_to_analyze)
        ~model:Analysis.initial_model
        initial_models
    in
    let initial_models =
      add_models_for_targets
        ~targets:(discard_targets_with_model initial_models stubs)
        ~model:Analysis.obscure_model
        initial_models
    in
    let initial_models =
      add_models_for_targets
        ~targets:(discard_targets_with_model initial_models override_targets)
        ~model:Analysis.empty_model
        initial_models
    in
    Logger.initial_models_stored ~timer;
    {
      State.shared_models = initial_models;
      shared_fixpoint;
      shared_results = State.SharedResults.create ();
    }


  let widen_if_necessary ~step ~callable ~previous_model ~new_model ~result ~additional_dependencies
    =
    (* Check if we've reached a fixed point *)
    if Model.less_or_equal ~callable ~left:new_model ~right:previous_model then
      State.{ is_partial = false; model = previous_model; result; additional_dependencies }
    else
      let model =
        Model.widen ~iteration:step.iteration ~callable ~previous:previous_model ~next:new_model
      in
      State.{ is_partial = true; model; result; additional_dependencies }


  let analyze_define ~shared_models ~context ~step:({ iteration; _ } as step) ~callable =
    let previous_model =
      match State.get_old_model shared_models callable with
      | Some model -> model
      | None ->
          (* We need to ensure the all models are properly initialized before doing the global
             fixpoint analysis. That is, if the global fixpoint analysis discovers any model is not
             initialized, then it is better to be warned that something is wrong with the model
             initialization. *)
          Format.asprintf "No initial model found for `%a`" Target.pp_pretty callable |> failwith
    in
    let { Analysis.AnalyzeDefineResult.result; model = new_model; additional_dependencies } =
      try
        Analysis.analyze_define
          ~context
          ~callable
          ~previous_model
          ~get_callee_model:(State.get_model shared_models)
      with
      | exn ->
          let wrapped_exn = Exception.wrap exn in
          let () = Logger.on_analyze_define_exception ~iteration ~callable ~exn in
          Exception.reraise wrapped_exn
    in
    let additional_dependencies =
      List.fold
        additional_dependencies
        ~f:(fun so_far callee -> DependencyGraph.add ~callee ~caller:callable so_far)
        ~init:DependencyGraph.empty
    in
    widen_if_necessary ~step ~callable ~previous_model ~new_model ~result ~additional_dependencies


  let analyze_overrides
      ~max_iterations
      ~shared_models
      ~override_graph
      ~step:({ iteration; _ } as step)
      ~callable
    =
    let timer = Timer.start () in
    let overrides =
      override_graph
      |> OverrideGraph.SharedMemory.ReadOnly.get_overriding_types
           ~member:
             (* In the override graph, keys can only be `Target.Regular.Method` and hence not
                `Target.Parameterized`. *)
             (Target.get_corresponding_method_exn ~must_be_regular:false callable)
      |> Option.value ~default:[]
      |> List.map ~f:(fun at_type ->
             callable
             |> Target.as_regular_exn
                (* TODO(T204630385): Handle `Target.Parameterized` with `Override`. *)
             |> Target.Regular.create_derived_override_exn ~at_type
             |> Target.from_regular)
    in
    let new_model =
      let lookup override =
        let () =
          Logger.on_approaching_max_iterations
            ~max_iterations
            ~current_iteration:iteration
            "Finding model of overriding callable %a (whose base is %a)"
            Target.pp_pretty
            override
            Target.pp_pretty
            callable
        in
        match State.get_model shared_models override with
        | None ->
            Format.asprintf
              "During override analysis, can't find model for %a when analyzing %a"
              Target.pp_pretty
              override
              Target.pp_pretty
              callable
            |> failwith
        | Some model -> model
      in
      let direct_model =
        let direct_callable =
          (* TODO(T204630385): We do not expect to run into `Target.Parameterized` with
             `Override`. *)
          Target.get_corresponding_method_exn ~must_be_regular:true callable
        in
        State.get_model shared_models direct_callable
        |> Option.value ~default:Analysis.empty_model
        |> Model.for_override_model ~callable:direct_callable
      in
      overrides
      |> List.map ~f:lookup
      |> Algorithms.fold_balanced ~f:(Model.join ~iteration) ~init:direct_model
    in
    let previous_model =
      match State.get_old_model shared_models callable with
      | Some model -> model
      | None ->
          Format.asprintf "No initial model found for `%a`" Target.pp_pretty callable |> failwith
    in
    let state =
      widen_if_necessary
        ~step
        ~callable
        ~previous_model
        ~new_model
        ~result:Result.empty
        ~additional_dependencies:DependencyGraph.empty
    in
    let () = Logger.override_analysis_end ~callable ~timer in
    state


  let analyze_callable
      ~shared_models
      ~shared_fixpoint
      ~max_iterations
      ~override_graph
      ~context
      ~step
      ~callable
    =
    let () =
      (* Verify invariants *)
      match State.SharedFixpoint.get_meta_data shared_fixpoint callable with
      | None -> ()
      | Some { step = { epoch; _ }; _ } when epoch <> step.epoch ->
          Format.asprintf
            "Fixpoint inconsistency: callable %a analyzed during epoch %a, but stored metadata \
             from epoch %a"
            Target.pp_pretty
            callable
            Epoch.pp
            step.epoch
            Epoch.pp
            epoch
          |> failwith
      | _ -> ()
    in
    if Target.is_function_or_method callable then
      analyze_define ~shared_models ~context ~step ~callable
    else if Target.is_object callable then
      Format.asprintf "Found object `%a` in fixpoint analysis" Target.pp_pretty callable |> failwith
    else if Target.is_override callable then
      Alarm.with_alarm
        ~max_time_in_seconds:60
        ~event_name:"override analysis"
        ~callable:(Target.show_pretty callable)
        (fun () -> analyze_overrides ~shared_models ~max_iterations ~override_graph ~step ~callable)
        ()
    else
      Format.asprintf "Unknown type target `%a` in fixpoint analysis" Target.pp_pretty callable
      |> failwith


  type iteration_result = {
    callables_processed: int;
    expensive_callables: expensive_callable list;
    (* Need to be considered in the next iteration. *)
    additional_dependencies: DependencyGraph.t;
    (* Callables whose result changed and need re-analysis along with their callers. *)
    partial_callables: Target.t list;
  }

  (* Called on a worker with a set of targets to analyze. *)
  let one_analysis_pass
      ~max_iterations
      ~shared_models
      ~shared_fixpoint
      ~shared_results
      ~override_graph
      ~context
      ~step:({ iteration; _ } as step)
      ~callables
    =
    let analyze_target (expensive_callables, additional_dependencies, partial_callables) callable =
      let timer = Timer.start () in
      let result =
        analyze_callable
          ~max_iterations
          ~shared_models
          ~shared_fixpoint
          ~override_graph
          ~context
          ~step
          ~callable
      in
      let () =
        Logger.on_approaching_max_iterations
          ~max_iterations
          ~current_iteration:iteration
          "New model of %a: %a"
          Target.pp_pretty
          callable
          Model.pp
          result.State.model
      in
      let _ =
        State.set_callable_result
          ~shared_models
          ~shared_fixpoint
          ~shared_results
          step
          callable
          result
      in
      let additional_dependencies =
        DependencyGraph.merge result.State.additional_dependencies additional_dependencies
      in
      let partial_callables =
        if result.State.is_partial then
          callable :: partial_callables
        else
          partial_callables
      in
      (* Log outliers. *)
      if Logger.is_expensive_callable ~callable ~timer then
        ( { time_to_analyze_in_ms = Timer.stop_in_ms timer; callable } :: expensive_callables,
          additional_dependencies,
          partial_callables )
      else
        expensive_callables, additional_dependencies, partial_callables
    in
    let expensive_callables, additional_dependencies, partial_callables =
      List.fold callables ~f:analyze_target ~init:([], DependencyGraph.empty, [])
    in
    {
      callables_processed = List.length callables;
      expensive_callables;
      additional_dependencies;
      partial_callables;
    }


  let compute_callables_to_reanalyze
      ~shared_fixpoint
      ~dependency_graph
      ~all_callables
      ~partial_callables
      ~step
    =
    let might_change_if_reanalyzed =
      List.fold partial_callables ~init:Target.Set.empty ~f:(fun accumulator callable ->
          (* callable must be re-analyzed next iteration because its result has changed, and
             therefore its callers must also be reanalyzed. *)
          let callers = DependencyGraph.dependencies dependency_graph callable in
          List.fold
            callers
            ~init:(Target.Set.add callable accumulator)
            ~f:(fun accumulator caller -> Target.Set.add caller accumulator))
    in
    (* Filter the original list in order to preserve topological sort order. *)
    let callables_to_reanalyze =
      List.filter all_callables ~f:(fun callable ->
          Target.Set.mem callable might_change_if_reanalyzed)
    in
    let () =
      if List.length callables_to_reanalyze <> Target.Set.cardinal might_change_if_reanalyzed then
        let missing =
          Target.Set.diff might_change_if_reanalyzed (Target.Set.of_list callables_to_reanalyze)
        in
        let check_missing callable =
          match State.SharedFixpoint.get_meta_data shared_fixpoint callable with
          | None -> () (* okay, caller is in a later epoch *)
          | Some { step = { epoch; _ }; _ } when epoch = Epoch.predefined -> ()
          | Some meta ->
              let message =
                Format.asprintf
                  "Re-analysis in iteration %d determined to analyze %a but it is not part of \
                   epoch %a (meta: %a)"
                  step.iteration
                  Target.pp_pretty
                  callable
                  Epoch.pp
                  step.epoch
                  State.MetaData.pp
                  meta
              in
              Log.error "%s" message;
              failwith message
        in
        Target.Set.iter check_missing missing
    in
    callables_to_reanalyze


  type t = {
    fixpoint_reached_iterations: int;
    state: State.t;
  }

  let filter_skip_analysis_targets ~skip_analysis_targets callables =
    let skip, not_skip =
      List.partition_tf ~f:(Target.should_skip_analysis ~skip_analysis_targets) callables
    in
    (* Print less log by grouping targets based on their regular part. *)
    skip
    |> List.map ~f:Target.strip_parameters
    |> Target.Set.of_list
    |> Target.Set.iter (fun callable ->
           Log.info
             "Skipping global fixpoint analysis of `%a` (and its parameterized variants)"
             Target.pp_pretty
             callable);
    not_skip


  let compute
      ~scheduler
      ~scheduler_policy
      ~override_graph
      ~dependency_graph
      ~skip_analysis_targets
      ~context
      ~callables_to_analyze:initial_callables_to_analyze
      ~max_iterations
      ~error_on_max_iterations
      ~epoch
      ~state
    =
    let rec iterate
        ~iteration
        ~dependency_graph
        ~all_callables
        ~state:({ State.shared_fixpoint; shared_results; _ } as state)
        callables_to_analyze
      =
      let number_of_callables = List.length callables_to_analyze in
      if number_of_callables = 0 then (* Fixpoint. *)
        state, iteration
      else if iteration >= max_iterations then
        if error_on_max_iterations then
          raise (Logger.reached_maximum_iteration_exception ~iteration ~callables_to_analyze)
        else
          let () = Logger.reached_maximum_iteration_exit ~iteration ~callables_to_analyze in
          state, iteration
      else
        let () = Logger.iteration_start ~iteration ~callables_to_analyze ~number_of_callables in
        let timer = Timer.start () in
        let step = { epoch; iteration } in
        let shared_models = State.oldify state callables_to_analyze in
        let shared_models_preserve_keys_handle = SharedModels.preserve_key_only shared_models in
        let map callables =
          one_analysis_pass
            ~max_iterations
            ~shared_models:shared_models_preserve_keys_handle
            ~shared_fixpoint
            ~shared_results
            ~override_graph
            ~context
            ~step
            ~callables
        in
        let reduce left right =
          let callables_processed = left.callables_processed + right.callables_processed in
          let () = Logger.iteration_progress ~iteration ~callables_processed ~number_of_callables in
          {
            callables_processed;
            expensive_callables = List.rev_append left.expensive_callables right.expensive_callables;
            additional_dependencies =
              DependencyGraph.merge left.additional_dependencies right.additional_dependencies;
            partial_callables = List.rev_append left.partial_callables right.partial_callables;
          }
        in
        let { expensive_callables; additional_dependencies; partial_callables; _ } =
          Scheduler.map_reduce
            scheduler
            ~policy:scheduler_policy
            ~initial:
              {
                callables_processed = 0;
                expensive_callables = [];
                additional_dependencies = DependencyGraph.empty;
                partial_callables = [];
              }
            ~map
            ~reduce
            ~inputs:callables_to_analyze
            ()
        in
        let shared_models = State.remove_old state callables_to_analyze in
        let callables_to_analyze =
          compute_callables_to_reanalyze
            ~shared_fixpoint
            ~dependency_graph
            ~all_callables
            ~partial_callables
            ~step
        in
        (* Additional dependencies may imply new callables that need to be analyzed. *)
        let new_callables =
          if not (DependencyGraph.is_empty additional_dependencies) then
            Target.Set.diff
              (additional_dependencies |> DependencyGraph.keys |> Target.Set.of_list)
              (Target.Set.of_list all_callables)
            |> Target.Set.elements
            |> filter_skip_analysis_targets ~skip_analysis_targets
          else
            []
        in
        let shared_models = initialize_models_for_new_dependencies ~shared_models new_callables in
        initialize_meta_data_for_new_dependencies ~shared_fixpoint ~step new_callables;
        let () = Logger.iteration_end ~iteration ~expensive_callables ~number_of_callables ~timer in
        let state = { state with State.shared_models } in
        iterate
          ~iteration:(iteration + 1)
          ~dependency_graph:(DependencyGraph.merge dependency_graph additional_dependencies)
          ~all_callables:(List.rev_append new_callables all_callables)
          ~state
          (List.rev_append new_callables callables_to_analyze)
    in
    let initial_callables_to_analyze =
      filter_skip_analysis_targets ~skip_analysis_targets initial_callables_to_analyze
    in
    let state, iterations =
      iterate
        ~iteration:0
        ~dependency_graph
        ~all_callables:initial_callables_to_analyze
        ~state
        initial_callables_to_analyze
    in
    { fixpoint_reached_iterations = iterations; state }
end

module WithoutLogging = struct
  let initial_models_stored ~timer:_ = ()

  let reached_maximum_iteration_exception ~iteration ~callables_to_analyze:_ =
    Format.asprintf "Failed to reach a fixpoint after %d iterations" iteration |> failwith


  let reached_maximum_iteration_exit ~iteration ~callables_to_analyze:_ =
    Log.info "Failed to reach a fixpoint after %d iterations. Terminate now." iteration


  let iteration_start ~iteration:_ ~callables_to_analyze:_ ~number_of_callables:_ = ()

  let iteration_end ~iteration:_ ~expensive_callables:_ ~number_of_callables:_ ~timer:_ = ()

  let iteration_progress ~iteration:_ ~callables_processed:_ ~number_of_callables:_ = ()

  let is_expensive_callable ~callable:_ ~timer:_ = false

  let override_analysis_end ~callable:_ ~timer:_ = ()

  let on_analyze_define_exception ~iteration:_ ~callable:_ ~exn:_ = ()

  let on_approaching_max_iterations ~max_iterations:_ ~current_iteration:_ =
    Format.ifprintf Format.err_formatter
end

module WithLogging (Config : sig
  val expensive_callable_ms : int
end) =
struct
  let initial_models_stored ~timer =
    Statistics.performance
      ~name:"Recorded initial models"
      ~phase_name:"Recording initial models"
      ~timer
      ()


  let show_callables ~max_to_show callables =
    let bucket = callables |> List.map ~f:Target.show_pretty |> List.sort ~compare:String.compare in
    let bucket_len = List.length bucket in
    Format.sprintf
      "%s%s"
      (String.concat ~sep:", " (List.take bucket max_to_show))
      (if bucket_len > max_to_show then "..." else "")


  let reached_maximum_iteration_exception ~iteration ~callables_to_analyze =
    Format.sprintf
      "Failed to reach a fixpoint after %d iterations (%d callables: %s)"
      iteration
      (List.length callables_to_analyze)
      (show_callables ~max_to_show:15 callables_to_analyze)
    |> failwith


  let reached_maximum_iteration_exit ~iteration ~callables_to_analyze =
    Log.info
      "Failed to reach a fixpoint after %d iterations. Terminate now  (%d callables: %s)"
      iteration
      (List.length callables_to_analyze)
      (show_callables ~max_to_show:15 callables_to_analyze)


  let iteration_start ~iteration ~callables_to_analyze ~number_of_callables =
    let witnesses =
      if number_of_callables <= 6 then
        String.concat ~sep:", " (List.map ~f:Target.show_pretty callables_to_analyze)
      else
        "..."
    in
    Log.info "Iteration #%d. %d callables [%s]" iteration number_of_callables witnesses


  let iteration_end ~iteration ~expensive_callables ~number_of_callables ~timer =
    let () =
      if not (List.is_empty expensive_callables) then
        Log.log
          ~section:`Performance
          "Expensive callables for iteration %d: %s"
          iteration
          (expensive_callables
          |> List.sort ~compare:(fun left right ->
                 Int.compare right.time_to_analyze_in_ms left.time_to_analyze_in_ms)
          |> List.map ~f:(fun { time_to_analyze_in_ms; callable } ->
                 Format.asprintf "`%a`: %d ms" Target.pp_pretty callable time_to_analyze_in_ms)
          |> String.concat ~sep:", ")
    in
    Log.info
      "Iteration #%n, %d callables, heap size %.3fGB took %.2fs"
      iteration
      number_of_callables
      (Int.to_float (Hack_parallel.Std.SharedMemory.heap_size ()) /. 1000000000.0)
      (Timer.stop_in_sec timer)


  let iteration_progress ~iteration:_ ~callables_processed ~number_of_callables =
    Log.log
      ~section:`Progress
      "Processed %d of %d callables"
      callables_processed
      number_of_callables


  let is_expensive_callable ~callable ~timer =
    let time_to_analyze_in_ms = Timer.stop_in_ms timer in
    let () =
      if time_to_analyze_in_ms >= Config.expensive_callable_ms then
        Statistics.performance
          ~name:"static analysis of expensive callable"
          ~timer
          ~section:`Interprocedural
          ~normals:["callable", Target.show_pretty callable]
          ()
    in
    time_to_analyze_in_ms >= Config.expensive_callable_ms


  let override_analysis_end ~callable ~timer =
    Statistics.performance
      ~randomly_log_every:1000
      ~always_log_time_threshold:1.0 (* Seconds *)
      ~name:"Override analysis"
      ~section:`Interprocedural
      ~normals:["callable", Target.show_pretty callable]
      ~timer
      ()


  let on_analyze_define_exception ~iteration ~callable ~exn =
    let message =
      match exn with
      | Stdlib.Sys.Break -> "Hit Ctrl+C"
      | _ -> "Analysis failed"
    in
    let message =
      Format.asprintf
        "%s in iteration %d while analyzing `%a`."
        message
        iteration
        Target.pp_pretty
        callable
    in
    Log.log_exception message exn (Hack_parallel.Std.Worker.exception_backtrace exn)


  let on_approaching_max_iterations ~max_iterations ~current_iteration format =
    if current_iteration >= max_iterations - 5 then
      Log.info format
    else
      Format.ifprintf Format.err_formatter format
end
