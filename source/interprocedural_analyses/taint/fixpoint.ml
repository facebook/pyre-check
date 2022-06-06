(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre
module TypeEnvironment = Analysis.TypeEnvironment

module Context = struct
  type t = {
    type_environment: TypeEnvironment.ReadOnly.t;
    class_interval_graph: Interprocedural.ClassIntervalSetGraph.SharedMemory.t;
    define_call_graphs: Interprocedural.CallGraph.DefineCallGraphSharedMemory.t;
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


    let reached_fixpoint ~iteration ~callable ~previous ~next =
      let result = Model.reached_fixpoint ~iteration ~previous ~next in
      let () =
        if result then
          Log.log
            ~section:`Interprocedural
            "Reached fixpoint for `%a`\n%a"
            Interprocedural.Target.pp_pretty
            callable
            Model.pp
            previous
      in
      result
  end

  module Result = struct
    type t = Issue.t list

    let empty = []
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
      ~type_environment
      ~class_interval_graph
      ~define_call_graphs
      ~qualifier
      ~callable
      ~define
      ~sanitizers
      ~modes
      ~previous_model
      ~get_callee_model
    =
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
    let forward, result, triggered_sinks =
      TaintProfiler.track_duration ~profiler ~name:"Forward analysis" ~f:(fun () ->
          ForwardAnalysis.run
            ~profiler
            ~environment:type_environment
            ~class_interval_graph
            ~qualifier
            ~callable
            ~define
            ~call_graph_of_define
            ~get_callee_model
            ~existing_model:previous_model)
    in
    let backward =
      TaintProfiler.track_duration ~profiler ~name:"Backward analysis" ~f:(fun () ->
          BackwardAnalysis.run
            ~profiler
            ~environment:type_environment
            ~class_interval_graph
            ~qualifier
            ~callable
            ~define
            ~call_graph_of_define
            ~get_callee_model
            ~existing_model:previous_model
            ~triggered_sinks)
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
          Model.apply_sanitizers model)
    in
    TaintProfiler.dump profiler;
    result, model


  let analyze_define
      ~context:{ Context.type_environment; class_interval_graph; define_call_graphs }
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
      InlineDecorator.InlinedNameToOriginalName.get define_qualifier
      >>= AnnotatedGlobalEnvironment.ReadOnly.get_global_location annotated_global_environment
      >>| fun { Location.WithModule.module_reference; _ } -> module_reference
    in
    let qualifier = Option.value ~default:qualifier module_reference in
    if Model.ModeSet.contains Model.Mode.SkipAnalysis modes then
      let () = Log.info "Skipping taint analysis of %a" Interprocedural.Target.pp_pretty callable in
      [], previous_model
    else
      analyze_define_with_sanitizers_and_modes
        ~type_environment
        ~class_interval_graph
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
