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
      decorator_resolution: CallGraph.DecoratorResolution.Results.t;
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

  module Logger = FixpointAnalysis.WithoutLogging

  module AnalyzeDefineResult = struct
    type t = {
      result: Result.t;
      model: Model.t;
      additional_dependencies: Target.t list;
    }
  end

  let analyze_define
      ~context:{ Context.pyre_api; define_call_graphs; decorator_resolution; _ }
      ~callable
      ~previous_model:{ CallGraph.HigherOrderCallGraph.call_graph = previous_call_graph; _ }
      ~get_callee_model
    =
    let define_call_graph =
      define_call_graphs
      |> CallGraph.SharedMemory.ReadOnly.get ~callable:(Target.strip_parameters callable)
      |> Option.value_exn
           ~message:(Format.asprintf "Missing call graph for `%a`" Target.pp callable)
    in
    let qualifier, { Ast.Node.value = define; _ } =
      callable
      |> Target.strip_parameters
      |> CallGraph.get_module_and_definition_exn ~pyre_api ~decorator_resolution
    in
    let ({ CallGraph.HigherOrderCallGraph.call_graph; _ } as model) =
      CallGraph.higher_order_call_graph_of_define
        ~define_call_graph
        ~pyre_api
        ~qualifier
        ~define
        ~initial_state:(CallGraph.HigherOrderCallGraph.State.initialize_from_callable callable)
        ~get_callee_model
    in
    let dependencies call_graph =
      call_graph
      |> CallGraph.DefineCallGraph.all_targets ~exclude_reference_only:true
      |> Target.Set.of_list
    in
    let additional_dependencies =
      Target.Set.diff (dependencies call_graph) (dependencies previous_call_graph)
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

type t = Fixpoint.t

let compute
    ~scheduler
    ~scheduler_policy
    ~pyre_api
    ~call_graph:{ CallGraph.SharedMemory.define_call_graphs; _ }
    ~dependency_graph:
      { DependencyGraph.dependency_graph; callables_to_analyze; override_targets; _ }
    ~override_graph_shared_memory
    ~initial_callables
    ~decorator_resolution
    ~max_iterations
  =
  let decorated_callables =
    CallGraph.DecoratorResolution.Results.decorated_callables decorator_resolution
  in
  let definitions =
    initial_callables |> FetchCallables.get_definitions |> List.rev_append decorated_callables
  in
  let initial_call_graph callable =
    define_call_graphs
    |> CallGraph.SharedMemory.read_only
    |> CallGraph.SharedMemory.ReadOnly.get ~callable
    |> Option.value ~default:CallGraph.DefineCallGraph.empty
  in
  let initial_models =
    definitions
    |> List.map ~f:(fun callable ->
           ( callable,
             { CallGraph.HigherOrderCallGraph.empty with call_graph = initial_call_graph callable }
           ))
    |> Fixpoint.Registry.of_alist ~join:CallGraph.HigherOrderCallGraph.merge
  in
  let shared_models =
    Fixpoint.record_initial_models
      ~scheduler
      ~initial_callables:definitions
      ~stubs:(FetchCallables.get_stubs initial_callables)
      ~override_targets
      ~initial_models
  in
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
      }
    ~callables_to_analyze:(List.rev_append decorated_callables callables_to_analyze)
    ~max_iterations
    ~error_on_max_iterations:false
    ~epoch:Fixpoint.Epoch.initial
    ~shared_models


let get_model = Fixpoint.get_model

let cleanup = Fixpoint.cleanup
