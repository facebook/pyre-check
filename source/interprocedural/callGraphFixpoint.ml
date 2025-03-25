(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

module CallGraphAnalysis = struct
  module Context = struct
    type t = {
      pyre_api: Analysis.PyrePysaEnvironment.ReadOnly.t;
      define_call_graphs: CallGraph.SharedMemory.ReadOnly.t;
      method_kinds: CallGraph.MethodKind.SharedMemory.ReadOnly.t;
      callables_to_definitions_map: Target.DefinesSharedMemory.ReadOnly.t;
      maximum_target_depth: int;
    }
  end

  type context = Context.t

  module Model = struct
    type t = CallGraph.HigherOrderCallGraph.t [@@deriving show]

    let join ~iteration:_ = CallGraph.HigherOrderCallGraph.merge

    (* TODO(T218941022): The current widening is not sound, since we only return the right hand
       side. This can lead to unsoundness (missing call edges), although we haven't seen concrete
       examples yet. It will also lead to nondeterminism. *)
    let widen ~iteration:_ ~callable:_ ~previous:_ ~next = next

    (* Since this is only used to determine if we have reached a fixpoint, it is fine to only check
       the equality. *)
    let less_or_equal ~callable:_ ~left ~right = CallGraph.HigherOrderCallGraph.equal left right

    (* For an `Override` target, it make senses to let it return its overriding targets' returned
       callables, but it does not make sense to let its call graph be the union. *)
    let for_override_model ~callable:_ model =
      { model with CallGraph.HigherOrderCallGraph.call_graph = CallGraph.DefineCallGraph.empty }


    let for_new_dependency ~get_model callable =
      callable
      |> Target.strip_parameters
      |> get_model
      |> Option.value ~default:CallGraph.HigherOrderCallGraph.empty
  end

  module Result = struct
    type t = unit

    let empty = ()
  end

  let empty_model =
    {
      CallGraph.HigherOrderCallGraph.returned_callables = CallGraph.CallTarget.Set.bottom;
      call_graph = CallGraph.DefineCallGraph.empty;
    }


  let initial_model = empty_model

  let obscure_model = empty_model

  module Logger = struct
    include FixpointAnalysis.WithLogging (struct
      let expensive_callable_ms = 500
    end)

    let iteration_end ~iteration ~expensive_callables ~number_of_callables ~timer =
      (* Explicitly collect the shared memory to reduce heap size. *)
      let () = Memory.SharedMemory.collect `aggressive in
      iteration_end ~iteration ~expensive_callables ~number_of_callables ~timer
  end

  module AnalyzeDefineResult = struct
    type t = {
      result: Result.t;
      model: Model.t;
      additional_dependencies: Target.t list;
    }
  end

  let analyze_define
      ~context:
        {
          Context.pyre_api;
          define_call_graphs;
          method_kinds;
          callables_to_definitions_map;
          maximum_target_depth;
        }
      ~callable
      ~previous_model:{ CallGraph.HigherOrderCallGraph.call_graph = previous_call_graph; _ }
      ~get_callee_model
    =
    let get_definition callable =
      callable
      |> Target.strip_parameters
      |> Target.DefinesSharedMemory.ReadOnly.get callables_to_definitions_map
    in
    let { Target.DefinesSharedMemory.Define.qualifier; define = { Ast.Node.value = define; _ } } =
      callable
      |> get_definition
      |> Option.value_exn
           ~message:(Format.asprintf "Found no definition for `%a`" Target.pp_pretty callable)
    in
    if Ast.Statement.Define.is_stub define then
      (* Skip analyzing stubs, which do not have initial call graphs. Otherwise we would fail to get
         their initial call graphs below, when analyzing them in the next iteration. *)
      { AnalyzeDefineResult.result = (); model = empty_model; additional_dependencies = [] }
    else
      let define_call_graph =
        define_call_graphs
        |> CallGraph.SharedMemory.ReadOnly.get
             ~cache:false
             ~callable:(Target.strip_parameters callable)
        |> Option.value_exn
             ~message:(Format.asprintf "Missing call graph for `%a`" Target.pp callable)
      in
      let profiler =
        if Ast.Statement.Define.dump_perf_higher_order_call_graph define then
          CallGraphProfiler.start ~enable_perf:false ~callable ()
        else
          CallGraphProfiler.disabled
      in
      let ({ CallGraph.HigherOrderCallGraph.call_graph; _ } as model) =
        Alarm.with_alarm
          ~max_time_in_seconds:60
          ~event_name:"Building higher order call graph"
          ~callable:(Target.show_pretty callable)
          (fun () ->
            CallGraph.higher_order_call_graph_of_define
              ~define_call_graph
              ~pyre_api
              ~callables_to_definitions_map
              ~callable:(Some callable)
              ~qualifier
              ~define
              ~initial_state:
                (CallGraph.HigherOrderCallGraph.State.initialize_from_callable
                   ~method_kinds
                   callable)
              ~get_callee_model
              ~profiler
              ~maximum_target_depth)
          ()
      in
      CallGraphProfiler.stop ~max_number_expressions:50 ~max_number_apply_call_steps:50 profiler;
      let dependencies call_graph =
        call_graph
        |> CallGraph.DefineCallGraph.all_targets
             ~use_case:CallGraph.AllTargetsUseCase.CallGraphDependency
        |> Target.Set.of_list
      in
      let additional_dependencies =
        Target.Set.diff (dependencies call_graph) (dependencies previous_call_graph)
        (* It is possible to see additional dependencies that have no definition. If not skipping
           them, in the next iteration we would fail due to not being able to find their
           definitions. *)
        |> Target.Set.filter (fun callable -> callable |> get_definition |> Option.is_some)
      in
      if CallGraph.debug_higher_order_call_graph define then (
        Log.dump
          "Returned callables for `%a`: `%a`"
          Target.pp_pretty_with_kind
          callable
          CallGraph.CallTarget.Set.pp
          model.CallGraph.HigherOrderCallGraph.returned_callables;
        Log.dump
          "Additional dependencies for `%a`: `%a`"
          Ast.Reference.pp
          (Analysis.PyrePysaLogic.qualified_name_of_define ~module_name:qualifier define)
          Target.Set.pp_pretty_with_kind
          additional_dependencies);
      {
        AnalyzeDefineResult.result = ();
        model;
        additional_dependencies = Target.Set.elements additional_dependencies;
      }
end

module Fixpoint = FixpointAnalysis.Make (CallGraphAnalysis)

type fixpoint = Fixpoint.t

type t = {
  fixpoint: fixpoint;
  whole_program_call_graph: CallGraph.WholeProgramCallGraph.t;
  get_define_call_graph: Target.t -> CallGraph.DefineCallGraph.t option;
}

let get_model_from_readonly_state ~readonly_state callable =
  Fixpoint.State.ReadOnly.get_model readonly_state callable


let get_model { fixpoint = { state; _ }; _ } =
  get_model_from_readonly_state ~readonly_state:(Fixpoint.State.read_only state)


let cleanup ~keep_models { Fixpoint.state; _ } = Fixpoint.State.cleanup ~keep_models state

let get_define_call_graph ~readonly_state callable =
  let open Option in
  callable
  |> get_model_from_readonly_state ~readonly_state
  >>| fun { CallGraph.HigherOrderCallGraph.call_graph; _ } -> call_graph


let build_whole_program_call_graph ~scheduler ~scheduler_policy state =
  let build_whole_program_call_graph ~readonly_state =
    List.fold ~init:CallGraph.WholeProgramCallGraph.empty ~f:(fun so_far callable ->
        match get_define_call_graph ~readonly_state callable with
        | None -> so_far
        | Some call_graph ->
            CallGraph.WholeProgramCallGraph.add_or_exn
              ~callable
              ~callees:
                (CallGraph.DefineCallGraph.all_targets
                   ~use_case:CallGraph.AllTargetsUseCase.TaintAnalysisDependency
                   call_graph)
              so_far)
  in
  let readonly_state = Fixpoint.State.read_only state in
  Scheduler.map_reduce
    scheduler
    ~policy:scheduler_policy
    ~initial:CallGraph.WholeProgramCallGraph.empty
    ~map:(build_whole_program_call_graph ~readonly_state)
    ~reduce:CallGraph.WholeProgramCallGraph.merge_disjoint
    ~inputs:(Fixpoint.State.targets state)
    ()


let analyzed_callables { Fixpoint.state; _ } = Fixpoint.State.targets state

let log_decorated_targets_if_no_returned_callables ~scheduler ~scheduler_policy ~decorators state =
  let log_if_no_returned_callables ~readonly_state callable =
    let open Option in
    callable
    |> get_model_from_readonly_state ~readonly_state
    >>| (fun { CallGraph.HigherOrderCallGraph.returned_callables; _ } ->
          if CallGraph.CallTarget.Set.is_bottom returned_callables then
            Log.info
              "Failed to apply decorators for `%a`, which may lead to false negatives. Consider \
               skipping one of these decorators: [%s]"
              Target.pp_pretty
              callable
              (Target.set_kind Target.Normal callable
              |> CallGraph.CallableToDecoratorsMap.SharedMemory.ReadOnly.get_decorators decorators
              |> Option.value ~default:[]
              |> List.map ~f:Ast.Expression.show
              |> String.concat ~sep:"; "))
    |> Option.value ~default:()
  in
  let readonly_state = Fixpoint.State.read_only state in
  Scheduler.map_reduce
    scheduler
    ~policy:scheduler_policy
    ~initial:()
    ~map:(List.iter ~f:(log_if_no_returned_callables ~readonly_state))
    ~reduce:(fun () () -> ())
    ~inputs:(state |> Fixpoint.State.targets |> List.filter ~f:Target.is_decorated)
    ()


let compute
    ~scheduler
    ~scheduler_policy
    ~static_analysis_configuration
    ~resolve_module_path
    ~pyre_api
    ~call_graph:{ CallGraph.SharedMemory.define_call_graphs; _ }
    ~dependency_graph:{ DependencyGraph.dependency_graph; override_targets; _ }
    ~override_graph_shared_memory
    ~skip_analysis_targets
    ~decorator_resolution
    ~decorators
    ~method_kinds
    ~callables_to_definitions_map
    ~max_iterations
    ~maximum_target_depth
  =
  let callables_to_definitions_map =
    CallGraph.DecoratorResolution.Results.register_decorator_defines
      ~decorator_resolution
      callables_to_definitions_map
  in
  let callables_with_call_graphs = CallGraph.SharedMemory.callables define_call_graphs in
  let initial_models_for_callables_with_call_graphs =
    let policy =
      Scheduler.Policy.fixed_chunk_size
        ~minimum_chunks_per_worker:1
        ~minimum_chunk_size:1
        ~preferred_chunk_size:1000
        ()
    in
    let initial_models = Fixpoint.SharedModels.create () |> Fixpoint.SharedModels.add_only in
    let empty_initial_models = Fixpoint.SharedModels.AddOnly.create_empty initial_models in
    let initial_call_graph ~define_call_graphs callable =
      define_call_graphs
      |> CallGraph.SharedMemory.ReadOnly.get ~cache:false ~callable
      |> Option.value ~default:CallGraph.DefineCallGraph.empty
    in
    let map ~define_call_graphs =
      List.fold ~init:empty_initial_models ~f:(fun initial_models callable ->
          Fixpoint.SharedModels.AddOnly.add
            initial_models
            callable
            {
              CallGraph.HigherOrderCallGraph.empty with
              call_graph = initial_call_graph ~define_call_graphs callable;
            })
    in
    Scheduler.map_reduce
      scheduler
      ~policy
      ~initial:initial_models
      ~map:(map ~define_call_graphs:(CallGraph.SharedMemory.read_only define_call_graphs))
      ~reduce:(fun left right ->
        Fixpoint.SharedModels.AddOnly.merge_same_handle_disjoint_keys ~smaller:left ~larger:right)
      ~inputs:callables_with_call_graphs
      ()
    |> Fixpoint.SharedModels.from_add_only
  in
  let state =
    Fixpoint.record_initial_models
      ~scheduler
      ~callables_to_analyze:[]
      ~stubs:[]
        (* No need to initialize models for stubs, since we cannot build call graphs for them
           anyway. *)
      ~override_targets
      ~initial_models:initial_models_for_callables_with_call_graphs
  in
  let ({ Fixpoint.state; _ } as fixpoint) =
    Fixpoint.compute
      ~scheduler
      ~scheduler_policy
      ~override_graph:(OverrideGraph.SharedMemory.read_only override_graph_shared_memory)
      ~dependency_graph
      ~skip_analysis_targets
      ~context:
        {
          CallGraphAnalysis.Context.pyre_api;
          define_call_graphs = CallGraph.SharedMemory.read_only define_call_graphs;
          method_kinds;
          callables_to_definitions_map =
            Target.DefinesSharedMemory.read_only callables_to_definitions_map;
          maximum_target_depth;
        }
      ~callables_to_analyze:(List.rev_append override_targets callables_with_call_graphs)
        (* Build higher order call graphs only for targets that have call graphs. *)
      ~max_iterations
      ~error_on_max_iterations:false
      ~epoch:Fixpoint.Epoch.initial
      ~state
  in
  let drop_decorated_targets
      ~target:_
      ~model:({ CallGraph.HigherOrderCallGraph.call_graph; _ } as model)
    =
    {
      model with
      CallGraph.HigherOrderCallGraph.call_graph =
        CallGraph.DefineCallGraph.drop_decorated_targets call_graph;
    }
  in
  let timer = Timer.start () in
  let state = Fixpoint.State.update_models state ~scheduler ~update_model:drop_decorated_targets in
  Log.info "Dropping decorated targets in models took %.2fs" (Timer.stop_in_sec timer);
  let fixpoint = { fixpoint with state } in
  let readonly_state = Fixpoint.State.read_only state in
  let () =
    CallGraph.HigherOrderCallGraph.save_to_directory
      ~scheduler
      ~static_analysis_configuration
      ~callables_to_definitions_map:
        (Target.DefinesSharedMemory.read_only callables_to_definitions_map)
      ~resolve_module_path
      ~get_call_graph:(get_model_from_readonly_state ~readonly_state)
      ~json_kind:NewlineDelimitedJson.Kind.HigherOrderCallGraph
      ~filename_prefix:"higher-order-call-graph"
      ~callables:callables_with_call_graphs
  in
  log_decorated_targets_if_no_returned_callables ~scheduler ~scheduler_policy ~decorators state;
  {
    fixpoint;
    whole_program_call_graph = build_whole_program_call_graph ~scheduler ~scheduler_policy state;
    get_define_call_graph =
      (* Use a lightweight handle, to avoid copying a large handle for each worker, when used in map
         reduce. *)
      get_define_call_graph ~readonly_state;
  }
