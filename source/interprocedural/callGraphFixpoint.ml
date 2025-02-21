(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

let not_skip_analysis_target ~skip_analysis_targets callable =
  (* We assume `skip_analysis_targets` only contains regular callables. *)
  not (Hash_set.mem skip_analysis_targets (Target.strip_parameters callable))


module CallGraphAnalysis = struct
  module Context = struct
    type t = {
      pyre_api: Analysis.PyrePysaEnvironment.ReadOnly.t;
      define_call_graphs: CallGraph.SharedMemory.ReadOnly.t;
      decorator_resolution: CallGraph.DecoratorResolution.Results.t;
      method_kinds: CallGraph.MethodKind.SharedMemory.ReadOnly.t;
      skip_analysis_targets: Target.HashSet.t;
    }
  end

  type context = Context.t

  module Model = struct
    type t = CallGraph.HigherOrderCallGraph.t [@@deriving show]

    let join ~iteration:_ = CallGraph.HigherOrderCallGraph.merge

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
          decorator_resolution;
          method_kinds;
          skip_analysis_targets;
        }
      ~callable
      ~previous_model:{ CallGraph.HigherOrderCallGraph.call_graph = previous_call_graph; _ }
      ~get_callee_model
    =
    let qualifier, { Ast.Node.value = define; _ } =
      callable
      |> Target.strip_parameters
      |> CallGraph.get_module_and_definition_exn ~pyre_api ~decorator_resolution
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
      let ({ CallGraph.HigherOrderCallGraph.call_graph; _ } as model) =
        CallGraph.higher_order_call_graph_of_define
          ~define_call_graph
          ~pyre_api
          ~qualifier
          ~define
          ~initial_state:
            (CallGraph.HigherOrderCallGraph.State.initialize_from_callable ~method_kinds callable)
          ~get_callee_model
      in
      let dependencies call_graph =
        call_graph
        |> CallGraph.DefineCallGraph.all_targets
             ~use_case:CallGraph.AllTargetsUseCase.CallGraphDependency
        |> Target.Set.of_list
      in
      let additional_dependencies =
        Target.Set.diff (dependencies call_graph) (dependencies previous_call_graph)
        |> Target.Set.filter (not_skip_analysis_target ~skip_analysis_targets)
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

let get_model { fixpoint = { state; _ }; _ } =
  Fixpoint.State.ReadOnly.get_model (Fixpoint.State.read_only state)


let cleanup { fixpoint = { state; _ }; _ } = Fixpoint.State.cleanup state

let get_define_call_graph ~state callable =
  let open Option in
  callable
  |> Fixpoint.State.ReadOnly.get_model state
  >>| fun { CallGraph.HigherOrderCallGraph.call_graph; _ } -> call_graph


let build_whole_program_call_graph ~scheduler ~scheduler_policy state =
  let build_whole_program_call_graph ~state =
    List.fold ~init:CallGraph.WholeProgramCallGraph.empty ~f:(fun so_far callable ->
        match get_define_call_graph ~state callable with
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
  Scheduler.map_reduce
    scheduler
    ~policy:scheduler_policy
    ~initial:CallGraph.WholeProgramCallGraph.empty
    ~map:(build_whole_program_call_graph ~state:(Fixpoint.State.read_only state))
    ~reduce:CallGraph.WholeProgramCallGraph.merge_disjoint
    ~inputs:(Fixpoint.State.targets state)
    ()


let analyzed_callables { Fixpoint.state; _ } = Fixpoint.State.targets state

let compute
    ~scheduler
    ~scheduler_policy
    ~pyre_api
    ~call_graph:{ CallGraph.SharedMemory.define_call_graphs; _ }
    ~dependency_graph:
      { DependencyGraph.dependency_graph; callables_to_analyze; override_targets; _ }
    ~override_graph_shared_memory
    ~initial_callables
    ~skip_analysis_targets
    ~decorator_resolution
    ~method_kinds
    ~max_iterations
  =
  let decorated_callables =
    CallGraph.DecoratorResolution.Results.decorated_callables decorator_resolution
  in
  let skip_analysis_targets =
    skip_analysis_targets |> Target.Set.elements |> Target.HashSet.of_list
  in
  let definitions =
    initial_callables
    |> FetchCallables.get_definitions
    |> List.filter ~f:(not_skip_analysis_target ~skip_analysis_targets)
    |> List.rev_append decorated_callables
  in
  let initial_models =
    let policy =
      Scheduler.Policy.fixed_chunk_size
        ~minimum_chunks_per_worker:1
        ~minimum_chunk_size:1
        ~preferred_chunk_size:1000
        ()
    in
    let initial_models = Fixpoint.SharedModels.create () |> Fixpoint.SharedModels.add_only in
    let empty_initial_models = Fixpoint.SharedModels.AddOnly.create_empty initial_models in
    let initial_call_graph callable =
      define_call_graphs
      |> CallGraph.SharedMemory.read_only
      |> CallGraph.SharedMemory.ReadOnly.get ~cache:false ~callable
      |> Option.value ~default:CallGraph.DefineCallGraph.empty
    in
    let map =
      List.fold ~init:empty_initial_models ~f:(fun initial_models callable ->
          Fixpoint.SharedModels.AddOnly.add
            initial_models
            callable
            { CallGraph.HigherOrderCallGraph.empty with call_graph = initial_call_graph callable })
    in
    Scheduler.map_reduce
      scheduler
      ~policy
      ~initial:initial_models
      ~map
      ~reduce:(fun left right ->
        Fixpoint.SharedModels.AddOnly.merge_same_handle_disjoint_keys ~smaller:left ~larger:right)
      ~inputs:definitions
      ()
    |> Fixpoint.SharedModels.from_add_only
  in
  let state =
    Fixpoint.record_initial_models
      ~scheduler
      ~callables_to_analyze:definitions
      ~stubs:[]
        (* No need to initialize models for stubs, since we cannot build call graphs for them
           anyway. *)
      ~override_targets
      ~initial_models
  in
  let callables_to_analyze =
    List.filter ~f:(not_skip_analysis_target ~skip_analysis_targets) callables_to_analyze
  in
  let ({ Fixpoint.state; _ } as fixpoint) =
    Fixpoint.compute
      ~scheduler
      ~scheduler_policy
      ~override_graph:(OverrideGraph.SharedMemory.read_only override_graph_shared_memory)
      ~dependency_graph
      ~context:
        {
          CallGraphAnalysis.Context.pyre_api;
          define_call_graphs = CallGraph.SharedMemory.read_only define_call_graphs;
          decorator_resolution;
          method_kinds;
          skip_analysis_targets;
        }
      ~callables_to_analyze:(List.rev_append callables_to_analyze decorated_callables)
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
  {
    fixpoint;
    whole_program_call_graph = build_whole_program_call_graph ~scheduler ~scheduler_policy state;
    get_define_call_graph =
      (* Use a lightweight handle, to avoid copying a large handle for each worker, when used in map
         reduce. *)
      get_define_call_graph ~state:(Fixpoint.State.read_only state);
  }
