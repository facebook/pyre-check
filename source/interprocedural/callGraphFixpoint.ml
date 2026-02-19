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
      pyre_api: PyrePysaApi.ReadOnly.t;
      define_call_graphs: CallGraph.SharedMemory.ReadOnly.t;
      callables_to_definitions_map: CallablesSharedMemory.ReadOnly.t;
      type_of_expression_shared_memory: TypeOfExpressionSharedMemory.t;
      skip_analysis_targets: Target.HashSet.t;
      called_when_parameter: Target.HashSet.t;
      maximum_target_depth: int;
      maximum_parameterized_targets_at_call_site: int option;
    }
  end

  type context = Context.t

  module Model = struct
    type t = CallGraphBuilder.HigherOrderCallGraph.t [@@deriving show]

    let join ~iteration:_ = CallGraphBuilder.HigherOrderCallGraph.merge

    (* TODO(T218941022): The current widening is not sound, since we only return the right hand
       side. This can lead to unsoundness (missing call edges), although we haven't seen concrete
       examples yet. It will also lead to nondeterminism. *)
    let widen ~iteration:_ ~callable:_ ~previous:_ ~next = next

    (* Since this is only used to determine if we have reached a fixpoint, it is fine to only check
       the equality. *)
    let less_or_equal ~callable:_ ~left ~right =
      CallGraphBuilder.HigherOrderCallGraph.equal left right


    (* For an `Override` target, it make senses to let it return its overriding targets' returned
       callables, but it does not make sense to let its call graph be the union. *)
    let for_override_model ~callable:_ model =
      {
        model with
        CallGraphBuilder.HigherOrderCallGraph.call_graph = CallGraph.DefineCallGraph.empty;
      }


    let for_new_dependency ~get_model callable =
      callable
      |> Target.strip_parameters
      |> get_model
      |> Option.value ~default:CallGraphBuilder.HigherOrderCallGraph.empty
  end

  module Result = struct
    type t = unit

    let empty = ()
  end

  let empty_model =
    {
      CallGraphBuilder.HigherOrderCallGraph.returned_callables = CallGraph.CallTarget.Set.bottom;
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
          callables_to_definitions_map;
          type_of_expression_shared_memory;
          skip_analysis_targets;
          called_when_parameter;
          maximum_target_depth;
          maximum_parameterized_targets_at_call_site;
        }
      ~callable
      ~previous_model:_
      ~get_callee_model
    =
    if
      callable
      |> Target.strip_parameters
      |> CallablesSharedMemory.ReadOnly.is_stub_like callables_to_definitions_map
      |> Option.value_exn
           ~message:(Format.asprintf "Found no definition for `%a`" Target.pp_pretty callable)
    then
      (* Skip analyzing stubs, which do not have initial call graphs. Otherwise we would fail to get
         their initial call graphs below, when analyzing them in the next iteration. *)
      { AnalyzeDefineResult.result = (); model = empty_model; additional_dependencies = [] }
    else
      let { CallablesSharedMemory.DefineAndQualifier.qualifier; define } =
        callable
        |> Target.strip_parameters
        |> CallablesSharedMemory.ReadOnly.get_define callables_to_definitions_map
        |> PyrePysaApi.AstResult.value_exn
             ~message:(Format.asprintf "Found no definition for `%a`" Target.pp_pretty callable)
      in
      let define_call_graph =
        define_call_graphs
        |> CallGraph.SharedMemory.ReadOnly.get
             ~cache:false
             ~callable:(Target.strip_parameters callable)
        |> Option.value_exn
             ~message:(Format.asprintf "Missing call graph for `%a`" Target.pp callable)
      in
      let profiler =
        if Ast.Statement.Define.dump_perf_higher_order_call_graph (Ast.Node.value define) then
          CallGraphProfiler.start ~enable_perf:false ~callable ()
        else
          CallGraphProfiler.disabled
      in
      let ({ CallGraphBuilder.HigherOrderCallGraph.call_graph; _ } as model) =
        Alarm.with_alarm
          ~max_time_in_seconds:60
          ~event_name:"Building higher order call graph"
          ~callable:(Target.show_pretty callable)
          (fun () ->
            CallGraphBuilder.higher_order_call_graph_of_define
              ~define_call_graph
              ~pyre_api
              ~callables_to_definitions_map
              ~type_of_expression_shared_memory
              ~skip_analysis_targets
              ~called_when_parameter
              ~qualifier
              ~callable
              ~define
              ~initial_state:
                (CallGraphBuilder.HigherOrderCallGraph.State.initialize_from_callable
                   ~pyre_api
                   ~callables_to_definitions_map
                   callable)
              ~get_callee_model
              ~profiler
              ~maximum_target_depth
              ~maximum_parameterized_targets_at_call_site)
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
        (* Compare with the original call graph, instead of the higher order call graph from the
           previous iteration, since these are used to merge with the original dependency graph. *)
        Target.Set.diff (dependencies call_graph) (dependencies define_call_graph)
        (* It is possible to see additional dependencies that have no definition. If not skipping
           them, in the next iteration we would fail due to not being able to find their
           definitions. *)
        |> Target.Set.filter (fun callable ->
               callable
               |> Target.strip_parameters
               |> CallablesSharedMemory.ReadOnly.is_stub_like callables_to_definitions_map
               |> Option.value ~default:true
               |> not)
      in
      if CallGraphBuilder.debug_higher_order_call_graph (Ast.Node.value define) then (
        Log.dump
          "Returned callables for `%a`: `%a`"
          Target.pp_pretty_with_kind
          callable
          CallGraph.CallTarget.Set.pp
          model.CallGraphBuilder.HigherOrderCallGraph.returned_callables;
        Log.dump
          "Additional dependencies for `%a`: `%a`"
          Target.pp_pretty_with_kind
          callable
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
  >>| fun { CallGraphBuilder.HigherOrderCallGraph.call_graph; _ } -> call_graph


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

let log_decorated_targets_if_no_returned_callables
    ~scheduler
    ~scheduler_policy
    ~callables_to_decorators_map
    state
  =
  let open Data_structures in
  let merge_decorator_counts _ left_count right_count = Some (left_count + right_count) in
  let log_and_count_if_no_returned_callables ~readonly_state so_far callable =
    let open Option in
    callable
    |> get_model_from_readonly_state ~readonly_state
    >>| (fun { CallGraphBuilder.HigherOrderCallGraph.returned_callables; _ } ->
          if CallGraph.CallTarget.Set.is_bottom returned_callables then (
            let decorator_expressions =
              Target.set_kind Target.Normal callable
              |> CallableToDecoratorsMap.SharedMemory.ReadOnly.get_decorators
                   callables_to_decorators_map
              |> Option.value ~default:[]
              |> List.map ~f:Ast.Expression.show
            in
            Log.info
              "Failed to apply decorators for `%a`, which may lead to false negatives. Consider \
               skipping one of these decorators: [%s]"
              Target.pp_pretty
              callable
              (String.concat ~sep:"; " decorator_expressions);
            decorator_expressions
            |> List.sort_and_group ~compare:String.compare
            |> List.filter_map ~f:(function
                   | [] -> None
                   | decorator :: _ as decorators -> Some (decorator, List.length decorators))
            |> SerializableStringMap.of_alist_exn
            |> SerializableStringMap.union merge_decorator_counts so_far)
          else
            so_far)
    |> Option.value ~default:so_far
  in
  let readonly_state = Fixpoint.State.read_only state in
  let decorator_counts =
    Scheduler.map_reduce
      scheduler
      ~policy:scheduler_policy
      ~initial:SerializableStringMap.empty
      ~map:
        (List.fold
           ~init:SerializableStringMap.empty
           ~f:(log_and_count_if_no_returned_callables ~readonly_state))
      ~reduce:(SerializableStringMap.union merge_decorator_counts)
      ~inputs:(state |> Fixpoint.State.targets |> List.filter ~f:Target.is_decorated)
      ()
  in
  if not (SerializableStringMap.is_empty decorator_counts) then (
    Log.info "Top frequent decorators that potentially lead to failing to apply decorators:";
    let sorted_decorator_counts =
      decorator_counts
      |> SerializableStringMap.to_alist
      |> List.sort ~compare:(fun (_, left_count) (_, right_count) ->
             Int.compare right_count left_count)
    in
    List.take sorted_decorator_counts 10
    |> List.iter ~f:(fun (decorator, count) -> Log.info "Decorator %s: %d times" decorator count))


let compute
    ~scheduler
    ~scheduler_policy
    ~static_analysis_configuration:
      ({
         Configuration.StaticAnalysis.maximum_target_depth;
         maximum_parameterized_targets_at_call_site;
         higher_order_call_graph_max_iterations;
         _;
       } as static_analysis_configuration)
    ~resolve_module_path
    ~pyre_api
    ~callables_to_definitions_map
    ~callables_to_decorators_map
    ~type_of_expression_shared_memory
    ~call_graph:{ CallGraph.SharedMemory.define_call_graphs; _ }
    ~dependency_graph:
      {
        DependencyGraph.dependency_graph;
        override_targets;
        callables_to_analyze = original_callables_to_analyze;
        _;
      }
    ~override_graph_shared_memory
    ~skip_analysis_targets
    ~called_when_parameter
  =
  let callables_to_definitions_map =
    CallableToDecoratorsMap.SharedMemory.register_decorator_defines
      ~pyre_api
      callables_to_decorators_map
      callables_to_definitions_map
  in
  let callables_to_definitions_map =
    CallablesSharedMemory.ReadOnly.read_only callables_to_definitions_map
  in
  (* Build higher order call graphs only for targets that are reachable, and decorated targets,
     which are not included in `callables_to_analyze`. *)
  let callables_to_analyze =
    List.rev_append
      (CallableToDecoratorsMap.SharedMemory.decorated_targets callables_to_decorators_map)
      original_callables_to_analyze
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
              CallGraphBuilder.HigherOrderCallGraph.empty with
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
          callables_to_definitions_map;
          type_of_expression_shared_memory;
          skip_analysis_targets;
          called_when_parameter;
          maximum_target_depth;
          maximum_parameterized_targets_at_call_site;
        }
      ~callables_to_analyze
      ~max_iterations:higher_order_call_graph_max_iterations
      ~error_on_max_iterations:false
      ~epoch:Fixpoint.Epoch.initial
      ~state
  in
  let drop_decorated_targets
      ~target:_
      ~model:({ CallGraphBuilder.HigherOrderCallGraph.call_graph; _ } as model)
    =
    {
      model with
      CallGraphBuilder.HigherOrderCallGraph.call_graph =
        CallGraph.DefineCallGraph.drop_decorated_targets call_graph;
    }
  in
  let timer = Timer.start () in
  let state = Fixpoint.State.update_models state ~scheduler ~update_model:drop_decorated_targets in
  Log.info "Dropping decorated targets in models took %.2fs" (Timer.stop_in_sec timer);
  let fixpoint = { fixpoint with state } in
  let readonly_state = Fixpoint.State.read_only state in
  let () =
    CallGraphBuilder.HigherOrderCallGraph.save_to_directory
      ~scheduler
      ~static_analysis_configuration
      ~resolve_qualifier:(CallablesSharedMemory.ReadOnly.get_qualifier callables_to_definitions_map)
      ~resolve_module_path
      ~get_call_graph:(get_model_from_readonly_state ~readonly_state)
      ~json_kind:NewlineDelimitedJson.Kind.HigherOrderCallGraph
      ~filename_prefix:"higher-order-call-graph"
      ~callables:(analyzed_callables fixpoint)
  in
  log_decorated_targets_if_no_returned_callables
    ~scheduler
    ~scheduler_policy
    ~callables_to_decorators_map:
      (CallableToDecoratorsMap.SharedMemory.read_only callables_to_decorators_map)
    state;
  {
    fixpoint;
    whole_program_call_graph = build_whole_program_call_graph ~scheduler ~scheduler_policy state;
    get_define_call_graph =
      (* Use a lightweight handle, to avoid copying a large handle for each worker, when used in map
         reduce. *)
      get_define_call_graph ~readonly_state;
  }
