(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Pyre
open Statement
module TypeEnvironment = Analysis.TypeEnvironment
module Kind = AnalysisKind

(* See `.mli` for documentation of modules and functions. *)

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
    define:Define.t Node.t ->
    previous_model:Model.t ->
    get_callee_model:(Target.t -> Model.t option) ->
    Result.t * Model.t
end

module Make (Analysis : ANALYSIS) = struct
  module Model = Analysis.Model
  module Result = Analysis.Result
  module Logger = Analysis.Logger

  module Registry = struct
    type t = Model.t Target.Map.t

    let empty = Target.Map.empty

    let size registry = Target.Map.length registry

    let set registry ~target ~model = Target.Map.set ~key:target ~data:model registry

    let add ~join registry ~target ~model =
      Target.Map.update registry target ~f:(function
          | None -> model
          | Some existing -> join existing model)


    let get registry target = Target.Map.find registry target

    let merge ~join left right =
      Target.Map.merge left right ~f:(fun ~key:_ -> function
        | `Both (left, right) -> Some (join left right)
        | `Left model
        | `Right model ->
            Some model)


    let of_alist ~join = Target.Map.of_alist_reduce ~f:join

    let to_alist registry = Target.Map.to_alist ~key_order:`Increasing registry

    let iteri registry ~f =
      Target.Map.iteri registry ~f:(fun ~key ~data -> f ~target:key ~model:data)


    let map registry ~f = Target.Map.map ~f registry

    let targets registry = Target.Map.keys registry

    let object_targets registry =
      let objects = Target.HashSet.create () in
      let add ~key:target ~data:_ =
        match target with
        | Target.Object _ -> Hash_set.add objects target
        | _ -> ()
      in
      let () = Target.Map.iteri ~f:add registry in
      objects
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

  (* The fixpoint state, stored in shared memory. *)
  module State = struct
    module SharedModels =
      Memory.WithCache.Make
        (Target.SharedMemoryKey)
        (struct
          type t = Model.t

          let prefix = Prefix.make ()

          let description = "InterproceduralFixpointModel"
        end)

    module SharedResults =
      Memory.WithCache.Make
        (Target.SharedMemoryKey)
        (struct
          type t = Result.t

          let prefix = Prefix.make ()

          let description = "InterproceduralFixpointResults"
        end)

    type meta_data = {
      is_partial: bool;
      step: step;
    }

    (* Caches the fixpoint state (is_partial) of a call model. *)
    module SharedFixpoint =
      Memory.WithCache.Make
        (Target.SharedMemoryKey)
        (struct
          type t = meta_data

          let prefix = Prefix.make ()

          let description = "InterproceduralFixpointMetadata"
        end)

    module KeySet = SharedModels.KeySet

    (* Store all targets in order to clean-up the shared memory afterward. *)
    module SharedTargets =
      Memory.NoCache.Make
        (Memory.SingletonKey)
        (struct
          type t = KeySet.t

          let prefix = Prefix.make ()

          let description = "InterproceduralFixpointTarget"
        end)

    let get_new_model callable = SharedModels.get callable

    let get_old_model callable = SharedModels.get_old callable

    let get_model callable =
      match get_new_model callable with
      | Some _ as model -> model
      | None -> get_old_model callable


    let get_result callable = SharedResults.get callable |> Option.value ~default:Result.empty

    let get_is_partial callable =
      match SharedFixpoint.get callable with
      | Some { is_partial; _ } -> is_partial
      | None -> (
          match SharedFixpoint.get_old callable with
          | None -> true
          | Some { is_partial; _ } -> is_partial)


    let get_meta_data callable =
      match SharedFixpoint.get callable with
      | Some _ as meta_data -> meta_data
      | None -> SharedFixpoint.get_old callable


    let meta_data_to_string { is_partial; step = { epoch; iteration } } =
      Format.sprintf "{ partial: %b; epoch: %d; iteration: %d }" is_partial epoch iteration


    type t = {
      (* Whether to reanalyze this and its callers. *)
      is_partial: bool;
      (* Model to use at call sites. *)
      model: Model.t;
      (* The result of the analysis. *)
      result: Result.t;
    }

    let add step callable state =
      (* Separate diagnostics from state to speed up lookups, and cache fixpoint state separately. *)
      let () = SharedModels.add callable state.model in
      (* Skip result writing unless necessary (e.g. overrides don't have results) *)
      let () =
        match callable with
        | Target.Function _
        | Target.Method _ ->
            SharedResults.add callable state.result
        | _ -> ()
      in
      SharedFixpoint.add callable { is_partial = state.is_partial; step }


    let add_predefined epoch callable model =
      let () = SharedModels.add callable model in
      let step = { epoch; iteration = 0 } in
      SharedFixpoint.add callable { is_partial = false; step }


    let oldify callable_set =
      SharedModels.oldify_batch callable_set;
      SharedFixpoint.oldify_batch callable_set;

      (* Old results are never looked up, so remove them. *)
      SharedResults.remove_batch callable_set


    let remove_old callable_set =
      SharedModels.remove_old_batch callable_set;
      SharedFixpoint.remove_old_batch callable_set


    let set_targets targets =
      let () =
        SharedTargets.remove_batch (SharedTargets.KeySet.singleton Memory.SingletonKey.key)
      in
      SharedTargets.add Memory.SingletonKey.key targets


    let cleanup () =
      let targets =
        SharedTargets.get Memory.SingletonKey.key |> Option.value ~default:KeySet.empty
      in
      let () = SharedModels.remove_batch targets in
      let () = SharedModels.remove_old_batch targets in
      let () = SharedResults.remove_batch targets in
      let () = SharedFixpoint.remove_batch targets in
      let () = SharedFixpoint.remove_old_batch targets in
      ()
  end

  (* Save initial models in the shared memory. *)
  let record_initial_models ~models ~callables ~stubs ~override_targets =
    let timer = Timer.start () in
    (* TODO(T117715045): Use a map reduce to make this faster. *)
    let record_models models =
      let add_model_to_memory ~target ~model = State.add_predefined Epoch.initial target model in
      Registry.iteri models ~f:add_model_to_memory;
      State.set_targets (models |> Registry.targets |> State.KeySet.of_list)
    in
    (* Augment models with initial inferred and obscure models *)
    let add_missing_initial_models models =
      callables
      |> List.filter ~f:(fun target -> not (Target.Map.mem models target))
      |> List.fold ~init:models ~f:(fun models target ->
             Registry.set models ~target ~model:Analysis.initial_model)
    in
    let add_missing_obscure_models models =
      stubs
      |> List.filter ~f:(fun target -> not (Target.Map.mem models target))
      |> List.fold ~init:models ~f:(fun models target ->
             Registry.set models ~target ~model:Analysis.obscure_model)
    in
    let add_override_models models =
      List.fold
        ~init:models
        ~f:(fun models target -> Registry.set models ~target ~model:Analysis.empty_model)
        override_targets
    in
    let () =
      models
      |> add_missing_initial_models
      |> add_missing_obscure_models
      |> add_override_models
      |> record_models
    in
    Logger.initial_models_stored ~timer


  let widen_if_necessary ~step ~callable ~previous_model ~new_model ~result =
    (* Check if we've reached a fixed point *)
    if
      Model.reached_fixpoint
        ~iteration:step.iteration
        ~callable
        ~previous:previous_model
        ~next:new_model
    then
      State.{ is_partial = false; model = previous_model; result }
    else
      let model =
        Model.widen ~iteration:step.iteration ~callable ~previous:previous_model ~next:new_model
      in
      State.{ is_partial = true; model; result }


  let analyze_define ~context ~step:({ iteration; _ } as step) ~callable ~qualifier ~define =
    let previous_model =
      match State.get_old_model callable with
      | Some model -> model
      | None ->
          Format.asprintf "No initial model found for `%a`" Target.pp_pretty callable |> failwith
    in
    let result, new_model =
      try
        Analysis.analyze_define
          ~context
          ~qualifier
          ~callable
          ~define
          ~previous_model
          ~get_callee_model:State.get_model
      with
      | exn ->
          let () = Logger.on_analyze_define_exception ~iteration ~callable ~exn in
          raise exn
    in
    widen_if_necessary ~step ~callable ~previous_model ~new_model ~result


  let analyze_overrides ~override_graph ~step:({ iteration; _ } as step) ~callable =
    let timer = Timer.start () in
    let overrides =
      OverrideGraph.SharedMemory.get_overriding_types
        override_graph
        ~member:(Target.get_corresponding_method callable)
      |> Option.value ~default:[]
      |> List.map ~f:(fun at_type -> Target.create_derived_override callable ~at_type)
    in
    let new_model =
      let lookup_and_join accumulator override =
        match State.get_model override with
        | None ->
            Format.asprintf
              "During override analysis, can't find model for %a"
              Target.pp_pretty
              override
            |> failwith
        | Some model -> model |> Model.strip_for_callsite |> Model.join ~iteration accumulator
      in
      let direct_model =
        State.get_model (Target.get_corresponding_method callable)
        |> Option.value ~default:Analysis.empty_model
        |> Model.strip_for_callsite
      in
      List.fold overrides ~f:lookup_and_join ~init:direct_model
    in
    let previous_model =
      match State.get_old_model callable with
      | Some model -> model
      | None ->
          Format.asprintf "No initial model found for `%a`" Target.pp_pretty callable |> failwith
    in
    let state =
      widen_if_necessary ~step ~callable ~previous_model ~new_model ~result:Result.empty
    in
    let () = Logger.override_analysis_end ~callable ~timer in
    state


  let analyze_callable ~type_environment ~override_graph ~context ~step ~callable =
    let resolution = TypeEnvironment.ReadOnly.global_resolution type_environment in
    let () =
      (* Verify invariants *)
      match State.get_meta_data callable with
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
    match callable with
    | (Target.Function _ | Target.Method _) as callable -> (
        match Target.get_module_and_definition callable ~resolution with
        | None ->
            Format.asprintf "Found no definition for `%a`" Target.pp_pretty callable |> failwith
        | Some (qualifier, define) -> analyze_define ~context ~step ~callable ~qualifier ~define)
    | Target.Override _ as callable -> analyze_overrides ~override_graph ~step ~callable
    | Target.Object _ as target ->
        Format.asprintf "Found object `%a` in fixpoint analysis" Target.pp_pretty target |> failwith


  type iteration_result = {
    callables_processed: int;
    expensive_callables: expensive_callable list;
  }

  (* Called on a worker with a set of targets to analyze. *)
  let one_analysis_pass ~type_environment ~override_graph ~context ~step ~callables =
    let analyze_target expensive_callables callable =
      let timer = Timer.start () in
      let result = analyze_callable ~type_environment ~override_graph ~context ~step ~callable in
      State.add step callable result;
      (* Log outliers. *)
      if Logger.is_expensive_callable ~callable ~timer then
        { time_to_analyze_in_ms = Timer.stop_in_ms timer; callable } :: expensive_callables
      else
        expensive_callables
    in
    let expensive_callables = List.fold callables ~f:analyze_target ~init:[] in
    { callables_processed = List.length callables; expensive_callables }


  let compute_callables_to_reanalyze ~dependency_graph ~all_callables ~previous_callables ~step =
    let might_change_if_reanalyzed =
      List.fold previous_callables ~init:Target.Set.empty ~f:(fun accumulator callable ->
          if not (State.get_is_partial callable) then
            accumulator
          else
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
          match State.get_meta_data callable with
          | None -> () (* okay, caller is in a later epoch *)
          | Some { step = { epoch; _ }; _ } when epoch = Epoch.predefined -> ()
          | Some meta ->
              let message =
                Format.asprintf
                  "Re-analysis in iteration %d determined to analyze %a but it is not part of \
                   epoch %a (meta: %s)"
                  step.iteration
                  Target.pp_pretty
                  callable
                  Epoch.pp
                  step.epoch
                  (State.meta_data_to_string meta)
              in
              Log.error "%s" message;
              failwith message
        in
        Target.Set.iter check_missing missing
    in
    callables_to_reanalyze


  type t = FixpointReached of { iterations: int }

  let compute
      ~scheduler
      ~type_environment
      ~override_graph
      ~dependency_graph
      ~context
      ~initial_callables
      ~stubs
      ~override_targets
      ~callables_to_analyze:initial_callables_to_analyze
      ~initial_models
      ~max_iterations
      ~epoch
    =
    let () =
      record_initial_models
        ~models:initial_models
        ~callables:initial_callables
        ~stubs
        ~override_targets
    in

    let rec iterate ~iteration callables_to_analyze =
      let number_of_callables = List.length callables_to_analyze in
      if number_of_callables = 0 then (* Fixpoint. *)
        iteration
      else if iteration >= max_iterations then
        raise (Logger.reached_maximum_iteration_exception ~iteration ~callables_to_analyze)
      else
        let () = Logger.iteration_start ~iteration ~callables_to_analyze ~number_of_callables in
        let timer = Timer.start () in
        let step = { epoch; iteration } in
        let old_batch = State.KeySet.of_list callables_to_analyze in
        let () = State.oldify old_batch in
        let map _ callables =
          one_analysis_pass ~type_environment ~override_graph ~context ~step ~callables
        in
        let reduce left right =
          let callables_processed = left.callables_processed + right.callables_processed in
          let () = Logger.iteration_progress ~iteration ~callables_processed ~number_of_callables in
          {
            callables_processed;
            expensive_callables = List.rev_append left.expensive_callables right.expensive_callables;
          }
        in
        let { expensive_callables; _ } =
          Scheduler.map_reduce
            scheduler
            ~policy:(Scheduler.Policy.legacy_fixed_chunk_size 1000)
            ~initial:{ callables_processed = 0; expensive_callables = [] }
            ~map
            ~reduce
            ~inputs:callables_to_analyze
            ()
        in
        let () = State.remove_old old_batch in
        let callables_to_analyze =
          compute_callables_to_reanalyze
            ~dependency_graph
            ~all_callables:initial_callables_to_analyze
            ~previous_callables:callables_to_analyze
            ~step
        in
        let () = Logger.iteration_end ~iteration ~expensive_callables ~number_of_callables ~timer in
        iterate ~iteration:(iteration + 1) callables_to_analyze
    in
    try
      let iterations = iterate ~iteration:0 initial_callables_to_analyze in
      FixpointReached { iterations }
    with
    | exn ->
        let () = Logger.on_global_fixpoint_exception ~exn in
        raise exn


  let get_model (FixpointReached _) target = State.get_model target

  let get_result (FixpointReached _) target = State.get_result target

  let get_iterations (FixpointReached { iterations }) = iterations

  let cleanup (FixpointReached _) = State.cleanup ()
end

module WithoutLogging = struct
  let initial_models_stored ~timer:_ = ()

  let reached_maximum_iteration_exception ~iteration ~callables_to_analyze:_ =
    Format.asprintf "Failed to reach a fixpoint after %d iterations" iteration |> failwith


  let iteration_start ~iteration:_ ~callables_to_analyze:_ ~number_of_callables:_ = ()

  let iteration_end ~iteration:_ ~expensive_callables:_ ~number_of_callables:_ ~timer:_ = ()

  let iteration_progress ~iteration:_ ~callables_processed:_ ~number_of_callables:_ = ()

  let is_expensive_callable ~callable:_ ~timer:_ = false

  let override_analysis_end ~callable:_ ~timer:_ = ()

  let on_analyze_define_exception ~iteration:_ ~callable:_ ~exn:_ = ()

  let on_global_fixpoint_exception ~exn:_ = ()
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


  let reached_maximum_iteration_exception ~iteration ~callables_to_analyze =
    let max_to_show = 15 in
    let bucket =
      callables_to_analyze |> List.map ~f:Target.show_pretty |> List.sort ~compare:String.compare
    in
    let bucket_len = List.length bucket in
    Format.sprintf
      "Failed to reach a fixpoint after %d iterations (%d callables: %s%s)"
      iteration
      (List.length callables_to_analyze)
      (String.concat ~sep:", " (List.take bucket max_to_show))
      (if bucket_len > max_to_show then "..." else "")
    |> failwith


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
      (Int.to_float (SharedMemory.heap_size ()) /. 1000000000.0)
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
      | Sys.Break -> "Hit Ctrl+C"
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
    Log.log_exception message exn (Worker.exception_backtrace exn)


  let on_global_fixpoint_exception ~exn =
    Log.log_exception "Fixpoint iteration failed." exn (Worker.exception_backtrace exn)
end
