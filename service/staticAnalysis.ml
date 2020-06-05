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


let unfiltered_callables ~resolution ~source:{ Source.source_path = { SourcePath.qualifier; _ }; _ }
  =
  let defines =
    GlobalResolution.unannotated_global_environment resolution
    |> (fun environment ->
         UnannotatedGlobalEnvironment.ReadOnly.all_defines_in_module environment qualifier)
    |> List.filter_map ~f:(GlobalResolution.function_definitions resolution)
    |> List.concat
    |> List.filter ~f:(fun { Node.value = define; _ } -> not (Define.is_overloaded_function define))
  in
  let record_toplevel_definition definition =
    let name = Node.value definition.Node.value.Define.signature.name in
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
        if Define.is_property_setter (Node.value definition) then
          Some (Callable.create_property_setter name, definition)
        else
          Some (Callable.create_method name, definition)
  in
  List.filter_map ~f:record_toplevel_definition defines


let regular_and_filtered_callables ~resolution ~source =
  let callables = unfiltered_callables ~resolution ~source in
  if GlobalResolution.source_is_unit_test resolution ~source then
    [], List.map callables ~f:fst
  else if Ast.SourcePath.is_stub source.source_path then
    ( List.filter callables ~f:(fun (_, { Node.value = define; _ }) ->
          not (Define.is_toplevel define || Define.is_class_toplevel define)),
      [] )
  else
    callables, []


let analyze
    ~scheduler
    ~analysis_kind
    ~configuration:
      ( {
          Configuration.StaticAnalysis.configuration;
          dump_call_graph;
          verify_models;
          rule_filter;
          _;
        } as analysis_configuration )
    ~filename_lookup
    ~environment
    ~qualifiers
    ()
  =
  let global_resolution = TypeEnvironment.ReadOnly.global_resolution environment in
  let get_source qualifier =
    let ast_environment = TypeEnvironment.ReadOnly.ast_environment environment in
    AstEnvironment.ReadOnly.get_processed_source ast_environment qualifier
  in

  Log.info "Recording overrides...";
  let timer = Timer.start () in
  let overrides =
    let combine ~key:_ left right = List.rev_append left right in
    let build_overrides overrides qualifier =
      try
        match get_source qualifier with
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
      ~policy:(Scheduler.Policy.legacy_fixed_chunk_count ())
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
        get_source qualifier
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
      ~policy:(Scheduler.Policy.legacy_fixed_chunk_count ())
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
  let timer = Timer.start () in
  Log.info "Fetching initial callables to analyze...";
  let callables, stubs, filtered_callables =
    let classify_source (callables, stubs) (callable, { Node.value = define; _ }) =
      if Define.is_stub define then
        callables, callable :: stubs
      else
        callable :: callables, stubs
    in
    let map result qualifiers =
      let make_callables
          ((existing_callables, existing_stubs, filtered_callables) as result)
          qualifier
        =
        get_source qualifier
        >>| (fun source ->
              let callables, new_filtered_callables =
                regular_and_filtered_callables ~resolution:global_resolution ~source
              in
              let callables, stubs =
                List.fold callables ~f:classify_source ~init:(existing_callables, existing_stubs)
              in
              let updated_filtered_callables =
                List.fold
                  new_filtered_callables
                  ~init:filtered_callables
                  ~f:(Fn.flip Callable.Set.add)
              in
              callables, stubs, updated_filtered_callables)
        |> Option.value ~default:result
      in
      List.fold qualifiers ~f:make_callables ~init:result
    in
    let reduce
        (new_callables, new_stubs, new_filtered_callables)
        (callables, stubs, filtered_callables)
      =
      ( List.rev_append new_callables callables,
        List.rev_append new_stubs stubs,
        Callable.Set.union new_filtered_callables filtered_callables )
    in
    Scheduler.map_reduce
      scheduler
      ~configuration
      ~policy:
        (Scheduler.Policy.fixed_chunk_count
           ~minimum_chunk_size:50
           ~preferred_chunks_per_worker:1
           ())
      ~map
      ~reduce
      ~initial:([], [], Callable.Set.empty)
      ~inputs:qualifiers
      ()
  in
  let callables = List.sort callables ~compare:Interprocedural.Callable.compare in
  Statistics.performance ~name:"Fetched initial callables to analyze" ~timer ();
  let analyses = [analysis_kind] in
  let timer = Timer.start () in
  Log.info "Initializing analysis...";
  (* Initialize and add initial models of analyses to shared mem. *)
  let () =
    let configuration_json =
      let taint_model_paths =
        configuration.Configuration.Analysis.taint_model_paths
        |> List.map ~f:Path.absolute
        |> List.map ~f:(fun directory -> `String directory)
      in
      let rule_settings =
        match rule_filter with
        | Some rule_filter ->
            ["rule_filter", `List (List.map rule_filter ~f:(fun rule -> `Int rule))]
        | None -> []
      in
      `Assoc
        [
          ( "taint",
            `Assoc
              ( ["model_paths", `List taint_model_paths; "verify_models", `Bool verify_models]
              @ rule_settings ) );
        ]
    in

    Analysis.initialize analyses ~configuration:configuration_json ~environment ~functions:callables
    |> Analysis.record_initial_models ~functions:callables ~stubs
  in
  Statistics.performance ~name:"Computed initial analysis state" ~timer ();
  let timer = Timer.start () in
  Log.info "Computing overrides...";
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
  Statistics.performance ~name:"Computed overrides" ~timer ();
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
          ~filtered_callables
          ~all_callables
          Interprocedural.Fixpoint.Epoch.initial
      in
      Log.info "Fixpoint iterations: %d" iterations
    with
    | exn ->
        Interprocedural.Analysis.save_results
          ~configuration:analysis_configuration
          ~filename_lookup
          ~analyses
          all_callables;
        raise exn
  in
  let () =
    Interprocedural.Analysis.save_results
      ~configuration:analysis_configuration
      ~filename_lookup
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
