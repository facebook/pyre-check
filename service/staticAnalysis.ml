(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Analysis
open Ast
open Interprocedural
open Statement
open Pyre

let record_and_merge_call_graph ~environment ~call_graph ~source =
  let record_and_merge_call_graph map call_graph =
    Map.merge_skewed map call_graph ~combine:(fun ~key:_ left _ -> left)
  in
  DependencyGraph.create_callgraph ~environment ~source |> record_and_merge_call_graph call_graph


let record_overrides overrides =
  let record_override_edge ~key:member ~data:subtypes =
    DependencyGraphSharedMemory.add_overriding_types ~member ~subtypes
  in
  Reference.Map.iteri overrides ~f:record_override_edge


let callables ~resolution ~source =
  if GlobalResolution.source_is_unit_test resolution ~source then
    []
  else
    let defines = Preprocessing.defines ~include_stubs:true ~include_toplevels:true source in
    let record_toplevel_definition definition =
      let name = definition.Node.value.Define.signature.name in
      match definition.Node.value.Define.signature.parent with
      | None ->
          (* Only record top-level definitions. *)
          Some (Callable.create_function name, definition)
      | Some class_name ->
          let class_annotation = Type.Primitive (Reference.show class_name) in
          let class_exists =
            GlobalResolution.class_definition resolution class_annotation |> Option.is_some
          in
          if not class_exists then
            Log.warning
              "Class %a for method %a is not part of the type environment"
              Reference.pp
              class_name
              Reference.pp
              name;
          let is_test_function =
            GlobalResolution.less_or_equal
              resolution
              ~left:(GlobalResolution.parse_reference resolution class_name)
              ~right:(Type.Primitive "unittest.case.TestCase")
          in
          if is_test_function then
            None
          else
            Some (Callable.create_method name, definition)
    in
    List.filter_map ~f:record_toplevel_definition defines


let analyze
    ~scheduler
    ~configuration:( { Configuration.StaticAnalysis.configuration; dump_call_graph; _ } as
                   analysis_configuration )
    ~environment
    ~qualifiers
    ()
  =
  let global_resolution = Environment.resolution environment () in
  let resolution = TypeCheck.resolution global_resolution () in
  Log.info "Recording overrides...";
  let timer = Timer.start () in
  let overrides =
    let combine ~key:_ left right = List.rev_append left right in
    let build_overrides overrides qualifier =
      try
        match Ast.SharedMemory.Sources.get qualifier with
        | None -> overrides
        | Some source ->
            let new_overrides = DependencyGraph.create_overrides ~environment ~source in
            record_overrides new_overrides;
            Map.merge_skewed overrides new_overrides ~combine
      with
      | ClassHierarchy.Untracked untracked_type ->
          Log.warning
            "Error building overrides in path %a for untracked type %a"
            Reference.pp
            qualifier
            Type.pp
            untracked_type;
          overrides
    in
    Scheduler.map_reduce
      scheduler
      ~configuration
      ~initial:DependencyGraph.empty_overrides
      ~map:(fun _ qualifiers ->
        List.fold qualifiers ~init:DependencyGraph.empty_overrides ~f:build_overrides)
      ~reduce:(Map.merge_skewed ~combine)
      ~inputs:qualifiers
      ()
  in
  Statistics.performance ~name:"Overrides recorded" ~timer ();
  Log.info "Building call graph...";
  let timer = Timer.start () in
  let callgraph =
    let build_call_graph call_graph qualifier =
      try
        Ast.SharedMemory.Sources.get qualifier
        >>| (fun source -> record_and_merge_call_graph ~environment ~call_graph ~source)
        |> Option.value ~default:call_graph
      with
      | ClassHierarchy.Untracked untracked_type ->
          Log.info
            "Error building call graph in path %a for untracked type %a"
            Reference.pp
            qualifier
            Type.pp
            untracked_type;
          call_graph
    in
    Scheduler.map_reduce
      scheduler
      ~configuration
      ~initial:Callable.RealMap.empty
      ~map:(fun _ qualifiers ->
        List.fold qualifiers ~init:Callable.RealMap.empty ~f:build_call_graph)
      ~reduce:(Map.merge_skewed ~combine:(fun ~key:_ left _ -> left))
      ~inputs:qualifiers
      ()
  in
  Statistics.performance ~name:"Call graph built" ~timer ();
  Log.info "Call graph edges: %d" (Callable.RealMap.length callgraph);
  if dump_call_graph then
    DependencyGraph.from_callgraph callgraph |> DependencyGraph.dump ~configuration;
  let callables, stubs =
    let classify_source (callables, stubs) (callable, { Node.value = define; _ }) =
      if Define.is_stub define then
        callables, callable :: stubs
      else
        callable :: callables, stubs
    in
    let make_callables result qualifier =
      Ast.SharedMemory.Sources.get qualifier
      >>| (fun source ->
            callables ~resolution:(Resolution.global_resolution resolution) ~source
            |> List.fold ~f:classify_source ~init:result)
      |> Option.value ~default:result
    in
    List.fold qualifiers ~f:make_callables ~init:([], [])
  in
  (* TODO(T41380664): generalize this to handle more than taint analysis. The
     analysis_configuration here should be picked from the command line somehow and it would
     indicate which analysis to run (taint vs others), and also contain particular analysis options
     passed the the analysis initialization code. *)
  let configuration_json =
    let taint_models_directories =
      configuration.Configuration.Analysis.taint_models_directories
      |> List.map ~f:Path.absolute
      |> List.map ~f:(fun directory -> `String directory)
    in
    `Assoc ["taint", `Assoc ["model_directories", `List taint_models_directories]]
  in
  let analyses = [Taint.Analysis.abstract_kind] in
  (* Initialize and add initial models of analyses to shared mem. *)
  let () =
    Analysis.initialize
      analyses
      ~configuration:configuration_json
      ~environment
      ~functions:callables
    |> Analysis.record_initial_models ~functions:callables ~stubs
  in
  let override_dependencies = DependencyGraph.from_overrides overrides in
  let dependencies =
    DependencyGraph.from_callgraph callgraph
    |> DependencyGraph.union override_dependencies
    |> DependencyGraph.reverse
  in
  let override_targets = (Callable.Map.keys override_dependencies :> Callable.t list) in
  let () =
    let add_predefined callable =
      Fixpoint.add_predefined Fixpoint.Epoch.initial callable Result.empty_model
    in
    List.iter override_targets ~f:add_predefined
  in
  let all_callables = List.rev_append override_targets callables in
  Log.info
    "Analysis fixpoint started for %d overrides %d functions..."
    (List.length override_targets)
    (List.length callables);
  let timer = Timer.start () in
  let () =
    try
      let iterations =
        Interprocedural.Analysis.compute_fixpoint
          ~configuration
          ~scheduler
          ~environment
          ~analyses
          ~dependencies
          ~all_callables
          Interprocedural.Fixpoint.Epoch.initial
      in
      Log.info "Fixpoint iterations: %d" iterations
    with
    | exn ->
        Interprocedural.Analysis.save_results
          ~configuration:analysis_configuration
          ~analyses
          all_callables;
        raise exn
  in
  let () =
    Interprocedural.Analysis.save_results
      ~configuration:analysis_configuration
      ~analyses
      all_callables
  in
  let errors = Interprocedural.Analysis.extract_errors scheduler ~configuration all_callables in
  Statistics.performance ~name:"Analysis fixpoint complete" ~timer ();

  (* If saving to a file, don't return errors. Thousands of errors on output is inconvenient *)
  if Option.is_some analysis_configuration.result_json_path then
    []
  else
    errors
