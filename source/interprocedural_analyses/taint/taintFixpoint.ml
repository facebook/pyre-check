(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Fixpoint: implements the global taint analysis fixpoint.
 *
 * Given a set of initial user models, this performs iterations to propagate
 * sources and sinks, until reaching a fixpoint (i.e, nothing else can be
 * propagated).
 *
 * Each iteration performs a forward and backward analysis on the set of
 * callables that might have new sources or sinks.
 *)

open Core
open Pyre
module PyrePysaEnvironment = Analysis.PyrePysaEnvironment
module PyrePysaLogic = Analysis.PyrePysaLogic

module Context = struct
  type t = {
    taint_configuration: TaintConfiguration.SharedMemory.t;
    pyre_api: PyrePysaEnvironment.ReadOnly.t;
    class_interval_graph: Interprocedural.ClassIntervalSetGraph.SharedMemory.t;
    (* Avoid copying a large-sized closure for each worker, to reduce the memory usage. *)
    get_define_call_graph:
      Interprocedural.Target.t -> Interprocedural.CallGraph.DefineCallGraph.t option;
    global_constants: Interprocedural.GlobalConstants.SharedMemory.ReadOnly.t;
    (* Whether decorators are inlined during pre-processing. *)
    decorator_inlined: bool;
    callables_to_definitions_map: Interprocedural.Target.CallablesSharedMemory.ReadOnly.t;
  }
end

module Analysis = struct
  type context = Context.t

  module Model = struct
    include Model

    let join ~iteration:_ left right = Model.join left right

    let widen ~iteration ~callable ~previous ~next =
      let result = Model.widen ~iteration ~previous ~next in
      let () =
        Log.log
          ~section:`Interprocedural
          "Widened fixpoint for `%a`\nold: %anew: %a\nwidened: %a"
          Interprocedural.Target.pp_pretty
          callable
          Model.pp
          previous
          Model.pp
          next
          Model.pp
          result
      in
      result


    let less_or_equal ~callable ~left ~right =
      let result = Model.less_or_equal ~left ~right in
      let () =
        if result then
          Log.log
            ~section:`Interprocedural
            "Reached fixpoint for `%a`\n%a"
            Interprocedural.Target.pp_pretty
            callable
            Model.pp
            right
      in
      result


    let for_new_dependency ~get_model:_ _ = failwith "Not expecting new dependencies"
  end

  module Result = struct
    type t = Issue.t IssueHandle.SerializableMap.t

    let empty = IssueHandle.SerializableMap.empty
  end

  let initial_model = Model.empty_model

  let empty_model = Model.empty_model

  let obscure_model = Model.obscure_model

  module Logger = struct
    include Interprocedural.FixpointAnalysis.WithLogging (struct
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
      additional_dependencies: Interprocedural.Target.t list;
    }
  end

  let analyze_define_with_sanitizers_and_modes
      ~taint_configuration
      ~string_combine_partial_sink_tree
      ~pyre_api
      ~class_interval_graph
      ~global_constants
      ~get_define_call_graph
      ~qualifier
      ~callable
      ~define
      ~sanitizers
      ~modes
      ~previous_model
      ~get_callee_model
      ~decorator_inlined
    =
    let taint_configuration = TaintConfiguration.SharedMemory.get taint_configuration in
    let profiler =
      if Ast.Statement.Define.dump_perf (Ast.Node.value define) then
        TaintProfiler.start ~enable_perf:true ~callable ()
      else
        TaintProfiler.disabled
    in
    let call_graph_of_define =
      match get_define_call_graph callable with
      | Some call_graph -> call_graph
      | None ->
          Format.asprintf "Missing call graph for `%a`" Interprocedural.Target.pp callable
          |> failwith
    in
    let cfg =
      TaintProfiler.track_duration ~profiler ~name:"Control flow graph" ~f:(fun () ->
          PyrePysaLogic.Cfg.create define.value)
    in
    let forward, result, triggered_sinks =
      TaintProfiler.track_duration ~profiler ~name:"Forward analysis" ~f:(fun () ->
          ForwardAnalysis.run
            ~profiler
            ~taint_configuration
            ~string_combine_partial_sink_tree
            ~pyre_api
            ~class_interval_graph
            ~global_constants
            ~qualifier
            ~callable
            ~define
            ~cfg
            ~call_graph_of_define
            ~get_callee_model
            ~existing_model:previous_model
            ())
    in
    let backward =
      TaintProfiler.track_duration ~profiler ~name:"Backward analysis" ~f:(fun () ->
          BackwardAnalysis.run
            ~profiler
            ~taint_configuration
            ~string_combine_partial_sink_tree
            ~pyre_api
            ~class_interval_graph
            ~global_constants
            ~qualifier
            ~callable
            ~define
            ~cfg
            ~call_graph_of_define
            ~get_callee_model
            ~existing_model:previous_model
            ~triggered_sinks
            ~decorator_inlined
            ())
    in
    let forward, backward =
      if Model.ModeSet.contains Model.Mode.SkipAnalysis modes then
        empty_model.forward, empty_model.backward
      else
        forward, backward
    in
    let model =
      {
        Model.forward;
        backward;
        parameter_sources = previous_model.parameter_sources;
        sanitizers;
        model_generators = previous_model.model_generators;
        modes;
      }
    in
    let model =
      TaintProfiler.track_duration ~profiler ~name:"Sanitize" ~f:(fun () ->
          Model.apply_sanitizers ~taint_configuration model)
    in
    TaintProfiler.stop ~max_number_expressions:50 ~max_number_apply_call_steps:50 profiler;
    { AnalyzeDefineResult.result; model; additional_dependencies = [] }


  let analyze_define
      ~context:
        {
          Context.taint_configuration;
          pyre_api;
          class_interval_graph;
          get_define_call_graph;
          global_constants;
          decorator_inlined;
          callables_to_definitions_map;
        }
      ~callable
      ~previous_model:({ Model.modes; sanitizers; _ } as previous_model)
      ~get_callee_model
    =
    let () =
      Log.log ~section:`Interprocedural "Analyzing %a" Interprocedural.Target.pp_pretty callable
    in
    let {
      Interprocedural.Target.CallablesSharedMemory.DefineAndQualifier.qualifier;
      define = { Ast.Node.value = { Ast.Statement.Define.signature = { name; _ }; _ }; _ } as define;
    }
      =
      callable
      |> Interprocedural.Target.strip_parameters
      |> Interprocedural.Target.CallablesSharedMemory.ReadOnly.get_define
           callables_to_definitions_map
      |> Option.value_exn
    in
    let define_qualifier = Ast.Reference.delocalize name in
    let open Ast in
    let module_reference =
      (* Pysa inlines decorators when a function is decorated. However, we want issues and models to
         point to the lines in the module where the decorator was defined, not the module where it
         was inlined. So, look up the originating module, if any, and use that as the module
         qualifier. *)
      PyrePysaLogic.DecoratorPreprocessing.original_name_from_inlined_name define_qualifier
      >>= PyrePysaEnvironment.ReadOnly.location_of_global pyre_api
      >>| fun { Location.WithModule.module_reference; _ } -> module_reference
    in
    let qualifier = Option.value ~default:qualifier module_reference in
    let string_combine_partial_sink_tree =
      taint_configuration
      |> TaintConfiguration.SharedMemory.get
      |> CallModel.StringFormatCall.declared_partial_sink_tree
    in
    if Model.ModeSet.contains Model.Mode.SkipAnalysis modes then
      failwithf
        "Expect the global fixpoint to skip analyzing %s"
        (Interprocedural.Target.show_pretty callable)
        ()
    else
      analyze_define_with_sanitizers_and_modes
        ~taint_configuration
        ~string_combine_partial_sink_tree
        ~pyre_api
        ~class_interval_graph
        ~global_constants
        ~get_define_call_graph
        ~qualifier
        ~callable
        ~define
        ~sanitizers
        ~modes
        ~previous_model
        ~get_callee_model
        ~decorator_inlined


  let skip_additional_dependency _ = false
end

let get_scheduler_policy policies =
  Scheduler.Policy.from_configuration_or_default
    policies
    Configuration.ScheduleIdentifier.TaintFixpoint
    ~default:
      (Scheduler.Policy.fixed_chunk_size
         ~minimum_chunks_per_worker:1
         ~minimum_chunk_size:1
         ~preferred_chunk_size:5500
         ())


include Interprocedural.FixpointAnalysis.Make (Analysis)
