(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Analysis
open Ast
open Interprocedural
open Statement
open Pyre


let record_and_merge_call_graph ~environment ~call_graph ~path:_ ~source =
  let record_and_merge_call_graph map call_graph =
    Map.merge_skewed map call_graph ~combine:(fun ~key:_ left _ -> left)
  in
  DependencyGraph.create_callgraph ~environment ~source
  |> record_and_merge_call_graph call_graph


let record_overrides overrides =
  let record_override_edge ~key:member ~data:subtypes =
    DependencyGraphSharedMemory.add_overriding_types ~member ~subtypes
  in
  Reference.Map.iteri overrides ~f:record_override_edge


let record_path_of_definitions ~path ~source =
  let defines = Preprocessing.defines ~include_stubs:true source in
  let record_classes { Node.value = class_node; _ } =
    (*
    Log.info "Recording class %a -> %a"
      Access.pp class_node.Class.name
      File.Handle.pp path;
    *)
    Callable.add_class_definition class_node.Class.name path
  in
  let record_toplevel_definition definition =
    let name = definition.Node.value.Define.name in
    match definition.Node.value.Define.parent with
    | None ->
        (*
        Log.info "Recording function %a -> %a"
          Access.pp name
          File.Handle.pp path;
        *)
        (* Only record top-level definitions. *)
        let () = Callable.add_function_definition name path in
        Callable.create_function name, definition
    | Some class_name ->
        if not (Callable.class_exists class_name) then
          begin
            Log.error "Method's class non-existing";
            Format.asprintf
              "Class %a for method %a not found"
              Reference.pp class_name
              Reference.pp name
            |> failwith
          end
        else
          Callable.create_method name, definition
  in
  List.iter ~f:record_classes (Preprocessing.classes source);
  List.map ~f:record_toplevel_definition defines


let analyze
    ?(taint_models_directory = "")
    ~scheduler
    ~configuration:({
        Configuration.StaticAnalysis.configuration;
        dump_call_graph;
        _;
      } as analysis_configuration)
    ~environment
    ~handles:paths
    () =
  Log.info "Recording overrides...";
  let timer = Timer.start () in
  let overrides =
    let combine ~key:_ left right =
      List.rev_append left right
    in
    let build_overrides overrides path =
      try
        match Ast.SharedMemory.Sources.get path with
        | None -> overrides
        | Some source ->
            let new_overrides = DependencyGraph.create_overrides ~environment ~source in
            record_overrides new_overrides;
            Map.merge_skewed overrides new_overrides ~combine
      with TypeOrder.Untracked untracked_type ->
        Log.info
          "Error building overrides in path %a for untracked type %a"
          File.Handle.pp path
          Type.pp untracked_type;
        overrides
    in
    Scheduler.map_reduce
      scheduler
      ~configuration
      ~initial:DependencyGraph.empty_overrides
      ~map:(fun _ paths -> List.fold paths ~init:DependencyGraph.empty_overrides ~f:build_overrides)
      ~reduce:(Map.merge_skewed ~combine)
      ~inputs:paths
      ()
  in
  Statistics.performance ~name:"Overrides recorded" ~timer ();

  Log.info "Building call graph...";
  let timer = Timer.start () in
  let callgraph =
    let build_call_graph call_graph path =
      try
        Ast.SharedMemory.Sources.get path
        >>| (fun source -> record_and_merge_call_graph ~environment ~call_graph ~path ~source)
        |> Option.value ~default:call_graph
      with TypeOrder.Untracked untracked_type ->
        Log.info "Error building call graph in path %a for untracked type %a"
          File.Handle.pp path
          Type.pp untracked_type;
        call_graph
    in
    Scheduler.map_reduce
      scheduler
      ~configuration
      ~initial:Callable.RealMap.empty
      ~map:(fun _ paths -> List.fold paths ~init:Callable.RealMap.empty ~f:build_call_graph)
      ~reduce:(Map.merge_skewed ~combine:(fun ~key:_ left _ -> left))
      ~inputs:paths
      ()
  in
  Statistics.performance ~name:"Call graph built" ~timer ();

  Log.info "Call graph edges: %d" (Callable.RealMap.length callgraph);
  if dump_call_graph then
    DependencyGraph.from_callgraph callgraph
    |> DependencyGraph.dump ~configuration;

  let callables, stubs =
    let classify_source (callables, stubs) (callable, define) =
      if Define.is_stub define.Node.value then
        callables, callable :: stubs
      else
        callable :: callables, stubs
    in
    let make_callables result path =
      Ast.SharedMemory.Sources.get path
      >>|
      (fun source ->
         record_path_of_definitions ~path ~source
         |> List.fold ~f:classify_source ~init:result)
      |> Option.value ~default:result
    in
    List.fold paths ~f:make_callables ~init:([], [])
  in
  (* TODO(T41380664): generalize this to handle more than taint analysis.
     The analysis_configuration here should be picked from the command line somehow and it
     would indicate which analysis to run (taint vs others), and also contain particular
     analysis options passed the the analysis initialization code.
  *)
  let configuration_json = `Assoc [
      "taint", `Assoc [
        "model_directory", `String taint_models_directory;
      ];
    ]
  in
  let analyses = [Taint.Analysis.abstract_kind] in
  (* Initialize and add initial models of analyses to shared mem. *)
  let () =
    Analysis.initialize
      analyses
      ~configuration:configuration_json
      ~environment
      ~functions:callables
    |> Analysis.record_initial_models
      ~functions:callables
      ~stubs
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
      Log.info "Fixpoint iterations: %d" iterations;
    with
      exn ->
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
