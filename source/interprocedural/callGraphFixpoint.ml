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
      define_call_graphs: CallGraph.MutableDefineCallGraphSharedMemory.ReadOnly.t;
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
      {
        model with
        CallGraph.HigherOrderCallGraph.call_graph = CallGraph.MutableDefineCallGraph.empty;
      }


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
      call_graph = CallGraph.MutableDefineCallGraph.empty;
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
      ~context:{ Context.pyre_api; define_call_graphs; _ }
      ~qualifier
      ~callable
      ~define:{ Ast.Node.value = define; _ }
      ~previous_model:{ CallGraph.HigherOrderCallGraph.call_graph = previous_call_graph; _ }
      ~get_callee_model
    =
    let define_call_graph =
      define_call_graphs
      |> CallGraph.MutableDefineCallGraphSharedMemory.ReadOnly.get
           ~callable:(Target.strip_parameters callable)
      |> Option.value_exn
           ~message:(Format.asprintf "Missing call graph for `%a`" Target.pp callable)
    in
    let ({ CallGraph.HigherOrderCallGraph.call_graph; _ } as model) =
      CallGraph.higher_order_call_graph_of_callable
        ~pyre_api
        ~define_call_graph
        ~callable
        ~get_callee_model
    in
    let dependencies call_graph =
      call_graph
      |> CallGraph.MutableDefineCallGraph.all_targets ~exclude_reference_only:true
      |> Target.Set.of_list
    in
    let additional_dependencies =
      Target.Set.diff (dependencies call_graph) (dependencies previous_call_graph)
      |> Target.Set.elements
    in
    (if CallGraph.debug_higher_order_call_graph define then
       let pp_targets formatter =
         List.iter ~f:(fun target -> Format.fprintf formatter "%a" Target.pp target)
       in
       Log.dump
         "Additional dependencies for `%a`: `%a`"
         Ast.Reference.pp
         (Analysis.PyrePysaLogic.qualified_name_of_define ~module_name:qualifier define)
         pp_targets
         additional_dependencies);
    { AnalyzeDefineResult.result = (); model; additional_dependencies }
end

module Fixpoint = FixpointAnalysis.Make (CallGraphAnalysis)

type t = Fixpoint.t

let compute
    ~scheduler
    ~scheduler_policy
    ~pyre_api
    ~call_graph:{ CallGraph.MutableDefineCallGraphSharedMemory.define_call_graphs; _ }
    ~dependency_graph:
      { DependencyGraph.dependency_graph; callables_to_analyze; override_targets; _ }
    ~override_graph_shared_memory
    ~initial_callables
    ~max_iterations
  =
  let definitions = FetchCallables.get_definitions initial_callables in
  let initial_call_graph callable =
    define_call_graphs
    |> CallGraph.MutableDefineCallGraphSharedMemory.read_only
    |> CallGraph.MutableDefineCallGraphSharedMemory.ReadOnly.get ~callable
    |> Option.value ~default:CallGraph.MutableDefineCallGraph.empty
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
    ~pyre_api
    ~override_graph:(OverrideGraph.SharedMemory.read_only override_graph_shared_memory)
    ~dependency_graph
    ~context:
      {
        CallGraphAnalysis.Context.pyre_api;
        define_call_graphs =
          CallGraph.MutableDefineCallGraphSharedMemory.read_only define_call_graphs;
      }
    ~callables_to_analyze
    ~max_iterations
    ~epoch:Fixpoint.Epoch.initial
    ~shared_models


let get_model = Fixpoint.get_model

let cleanup = Fixpoint.cleanup
