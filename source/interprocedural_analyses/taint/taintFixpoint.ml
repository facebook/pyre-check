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
module TypeEnvironment = Analysis.TypeEnvironment

module Context = struct
  type t = {
    taint_configuration: TaintConfiguration.SharedMemory.t;
    type_environment: TypeEnvironment.ReadOnly.t;
    class_interval_graph: Interprocedural.ClassIntervalSetGraph.SharedMemory.t;
    define_call_graphs: Interprocedural.CallGraph.DefineCallGraphSharedMemory.t;
    global_constants: Interprocedural.GlobalConstants.SharedMemory.t;
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

  let analyze_define_with_sanitizers_and_modes
      ~taint_configuration
      ~type_environment
      ~class_interval_graph
      ~global_constants
      ~define_call_graphs
      ~qualifier
      ~callable
      ~define
      ~sanitizers
      ~modes
      ~previous_model
      ~get_callee_model
    =
    let taint_configuration = TaintConfiguration.SharedMemory.get taint_configuration in
    let profiler =
      if Ast.Statement.Define.dump_perf (Ast.Node.value define) then
        TaintProfiler.create ()
      else
        TaintProfiler.none
    in
    let call_graph_of_define =
      match
        Interprocedural.CallGraph.DefineCallGraphSharedMemory.get define_call_graphs ~callable
      with
      | Some call_graph -> call_graph
      | None ->
          Format.asprintf "Missing call graph for `%a`" Interprocedural.Target.pp callable
          |> failwith
    in
    let cfg =
      TaintProfiler.track_duration ~profiler ~name:"Control flow graph" ~f:(fun () ->
          Analysis.Cfg.create define.value)
    in
    let forward, result, triggered_sinks =
      TaintProfiler.track_duration ~profiler ~name:"Forward analysis" ~f:(fun () ->
          ForwardAnalysis.run
            ~profiler
            ~taint_configuration
            ~string_combine_partial_sink_tree:
              (CallModel.string_combine_partial_sink_tree taint_configuration)
            ~environment:type_environment
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
            ~environment:type_environment
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
            ())
    in
    let forward, backward =
      if Model.ModeSet.contains Model.Mode.SkipAnalysis modes then
        empty_model.forward, empty_model.backward
      else
        forward, backward
    in
    let model = { Model.forward; backward; sanitizers; modes } in
    let model =
      TaintProfiler.track_duration ~profiler ~name:"Sanitize" ~f:(fun () ->
          Model.apply_sanitizers ~taint_configuration model)
    in
    TaintProfiler.dump profiler;
    result, model


  let analyze_define
      ~context:
        {
          Context.taint_configuration;
          type_environment;
          class_interval_graph;
          define_call_graphs;
          global_constants;
        }
      ~qualifier
      ~callable
      ~define:
        ({ Ast.Node.value = { Ast.Statement.Define.signature = { name; _ }; _ }; _ } as define)
      ~previous_model:({ Model.modes; sanitizers; _ } as previous_model)
      ~get_callee_model
    =
    let () =
      Log.log ~section:`Interprocedural "Analyzing %a" Interprocedural.Target.pp_pretty callable
    in
    let define_qualifier = Ast.Reference.delocalize name in
    let open Analysis in
    let open Ast in
    let module_reference =
      let global_resolution = TypeEnvironment.ReadOnly.global_resolution type_environment in
      let annotated_global_environment =
        GlobalResolution.annotated_global_environment global_resolution
      in
      (* Pysa inlines decorators when a function is decorated. However, we want issues and models to
         point to the lines in the module where the decorator was defined, not the module where it
         was inlined. So, look up the originating module, if any, and use that as the module
         qualifier. *)
      DecoratorPreprocessing.original_name_from_inlined_name define_qualifier
      >>= AnnotatedGlobalEnvironment.ReadOnly.get_global_location annotated_global_environment
      >>| fun { Location.WithModule.module_reference; _ } -> module_reference
    in
    let qualifier = Option.value ~default:qualifier module_reference in
    if Model.ModeSet.contains Model.Mode.SkipAnalysis modes then
      let () = Log.info "Skipping taint analysis of %a" Interprocedural.Target.pp_pretty callable in
      Result.empty, previous_model
    else
      analyze_define_with_sanitizers_and_modes
        ~taint_configuration
        ~type_environment
        ~class_interval_graph
        ~global_constants
        ~define_call_graphs
        ~qualifier
        ~callable
        ~define
        ~sanitizers
        ~modes
        ~previous_model
        ~get_callee_model
end

include Interprocedural.FixpointAnalysis.Make (Analysis)
