(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Analysis
open Ast
open Interprocedural
open Statement
open Pyre

type initial_callables = {
  callables_with_dependency_information: (Callable.target_with_result * bool) list;
  stubs: Callable.target_with_result list;
  filtered_callables: Callable.Set.t;
}

module InitialCallablesSharedMemory = Memory.Serializer (struct
  type t = initial_callables

  module Serialized = struct
    type t = initial_callables

    let prefix = Prefix.make ()

    let description = "Initial callables to analyze"

    let unmarshall value = Marshal.from_string value 0
  end

  let serialize = Fn.id

  let deserialize = Fn.id
end)

module Cache : sig
  val load_environment : configuration:Configuration.Analysis.t -> TypeEnvironment.t option

  val save_environment
    :  configuration:Configuration.Analysis.t ->
    environment:TypeEnvironment.t ->
    unit

  val load_call_graph : configuration:Configuration.Analysis.t -> DependencyGraph.callgraph option

  val save_call_graph
    :  configuration:Configuration.Analysis.t ->
    callgraph:DependencyGraph.callgraph ->
    unit

  val load_initial_callables
    :  configuration:Configuration.Analysis.t ->
    ((Callable.target_with_result * bool) list * Callable.target_with_result list * Callable.Set.t)
    option

  val save_initial_callables
    :  configuration:Configuration.Analysis.t ->
    callables_with_dependency_information:(Callable.target_with_result * bool) list ->
    stubs:Callable.target_with_result list ->
    filtered_callables:Callable.Set.t ->
    unit

  val load_overrides : configuration:Configuration.Analysis.t -> DependencyGraph.overrides option

  val save_overrides
    :  configuration:Configuration.Analysis.t ->
    overrides:DependencyGraph.overrides ->
    unit

  val save_cache : configuration:Configuration.Analysis.t -> unit
end = struct
  let is_initialized = ref false

  let get_cache_path ~configuration =
    Path.create_relative
      ~root:(Configuration.Analysis.log_directory configuration)
      ~relative:"pysa.cache"


  let init_shared_memory ~configuration =
    if not !is_initialized then (
      let path = get_cache_path ~configuration in
      try
        let _ = Memory.get_heap_handle configuration in
        Memory.load_shared_memory ~path:(Path.absolute path) ~configuration;
        is_initialized := true;
        Log.warning
          "Using cached state from file: %s. Please try deleting this file and running Pysa again \
           if unexpected results occur."
          (Path.absolute path)
      with
      | error ->
          is_initialized := false;
          raise error )


  let load_environment ~configuration =
    let path = get_cache_path ~configuration in
    try
      init_shared_memory ~configuration;
      let environment =
        ModuleTracker.SharedMemory.load ()
        |> AstEnvironment.load
        |> AnnotatedGlobalEnvironment.create
        |> TypeEnvironment.create
      in
      SharedMemoryKeys.DependencyKey.Registry.load ();
      Log.info "Loaded type environment from cache shared memory.";
      Some environment
    with
    | error when Path.file_exists path ->
        Log.error
          "Error loading cached type environment from shared memory: %s"
          (Exn.to_string error);
        None
    | _ -> None


  let save_environment ~configuration ~environment =
    let path = get_cache_path ~configuration in
    try
      Memory.SharedMemory.collect `aggressive;
      TypeEnvironment.module_tracker environment |> ModuleTracker.SharedMemory.store;
      TypeEnvironment.ast_environment environment |> AstEnvironment.store;
      SharedMemoryKeys.DependencyKey.Registry.store ();
      Log.info "Saved type environment to cache shared memory."
    with
    | error when not (Path.file_exists path) ->
        Log.error "Error saving type environment to cache shared memory: %s" (Exn.to_string error)
    | _ -> ()


  let load_call_graph ~configuration =
    let path = get_cache_path ~configuration in
    try
      let callgraph = DependencyGraph.CallGraphSharedMemory.load () |> Callable.RealMap.of_tree in
      Log.info "Loaded call graph from cache shared memory.";
      Some callgraph
    with
    | error when Path.file_exists path ->
        Log.error "Error loading cached call graph from shared memory: %s" (Exn.to_string error);
        None
    | _ -> None


  let save_call_graph ~configuration ~callgraph =
    let path = get_cache_path ~configuration in
    try
      Memory.SharedMemory.collect `aggressive;
      Callable.RealMap.to_tree callgraph |> DependencyGraph.CallGraphSharedMemory.store;
      Log.info "Saved call graph to cache shared memory."
    with
    | error when not (Path.file_exists path) ->
        Log.error "Error saving call graph to cache shared memory: %s" (Exn.to_string error)
    | _ -> ()


  let load_initial_callables ~configuration =
    let path = get_cache_path ~configuration in
    try
      let { callables_with_dependency_information; stubs; filtered_callables } =
        InitialCallablesSharedMemory.load ()
      in
      Log.info "Loaded initial callables from cache shared memory.";
      Some (callables_with_dependency_information, stubs, filtered_callables)
    with
    | error when Path.file_exists path ->
        Log.error
          "Error loading cached initial callables from shared memory: %s"
          (Exn.to_string error);
        None
    | _ -> None


  let save_initial_callables
      ~configuration
      ~callables_with_dependency_information
      ~stubs
      ~filtered_callables
    =
    let path = get_cache_path ~configuration in
    try
      Memory.SharedMemory.collect `aggressive;
      InitialCallablesSharedMemory.store
        { callables_with_dependency_information; stubs; filtered_callables };
      Log.info "Saved initial callables to cache shared memory."
    with
    | error when not (Path.file_exists path) ->
        Log.error "Error saving initial callables to cache shared memory: %s" (Exn.to_string error)
    | _ -> ()


  let load_overrides ~configuration =
    let path = get_cache_path ~configuration in
    try
      let overrides = DependencyGraph.OverridesSharedMemory.load () |> Reference.Map.of_tree in
      Log.info "Loaded overrides from cache shared memory.";
      Some overrides
    with
    | error when Path.file_exists path ->
        Log.error "Error loading cached overrides from shared memory: %s" (Exn.to_string error);
        None
    | _ -> None


  let save_overrides ~configuration ~overrides =
    let path = get_cache_path ~configuration in
    try
      Memory.SharedMemory.collect `aggressive;
      Reference.Map.to_tree overrides |> DependencyGraph.OverridesSharedMemory.store;
      Log.info "Saved overrides to cache shared memory."
    with
    | error when not (Path.file_exists path) ->
        Log.error "Error saving overrides to cache shared memory: %s" (Exn.to_string error)
    | _ -> ()


  let save_cache ~configuration =
    let path = get_cache_path ~configuration in
    try
      Log.info "Saving state to cache file...";
      Memory.save_shared_memory ~path:(Path.absolute path) ~configuration;
      Log.info "Saved state to cache file: %s" (Path.absolute path)
    with
    | error when not (Path.file_exists path) ->
        Log.error "Error saving cached state to file: %s" (Exn.to_string error)
    | _ -> ()
end

let record_and_merge_call_graph ~environment ~call_graph ~source =
  let record_and_merge_call_graph map call_graph =
    Map.merge_skewed map call_graph ~combine:(fun ~key:_ left _ -> left)
  in
  CallGraph.create_callgraph ~use_shared_memory:true ~environment ~source
  |> record_and_merge_call_graph call_graph


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
  let record_toplevel_definition
      ( { Node.value = { Define.signature = { parent; name = { Node.value = name; _ }; _ }; _ }; _ }
      as define )
    =
    (* Warn if we're trying to add a method for a class that doesn't exist. *)
    begin
      match parent with
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
              name
      | None -> ()
    end;
    Callable.create define, define
  in
  List.map ~f:record_toplevel_definition defines


type found_callable = {
  callable: Callable.real_target;
  define: Define.t Node.t;
  is_internal: bool;
}

let regular_and_filtered_callables ~configuration ~resolution ~source =
  let callables = unfiltered_callables ~resolution ~source in
  let included, filtered =
    if GlobalResolution.source_is_unit_test resolution ~source then
      [], List.map callables ~f:fst
    else if Ast.SourcePath.is_stub source.source_path then
      ( List.filter callables ~f:(fun (_, { Node.value = define; _ }) ->
            not (Define.is_toplevel define || Define.is_class_toplevel define)),
        [] )
    else
      callables, []
  in
  let is_internal_source =
    Ast.SourcePath.is_internal_path
      ~configuration
      (Ast.SourcePath.full_path ~configuration source.source_path)
  in
  ( List.map included ~f:(fun (callable, define) ->
        { callable; define; is_internal = is_internal_source }),
    filtered )


let get_source ~environment qualifier =
  let ast_environment = TypeEnvironment.ReadOnly.ast_environment environment in
  AstEnvironment.ReadOnly.get_processed_source ast_environment qualifier


let fetch_callables_to_analyze ~scheduler ~environment ~configuration ~qualifiers =
  let global_resolution = TypeEnvironment.ReadOnly.global_resolution environment in
  let classify_source
      (callables, stubs)
      { callable; define = { Node.value = define; _ }; is_internal }
    =
    if Define.is_stub define then
      callables, callable :: stubs
    else
      (callable, is_internal) :: callables, stubs
  in
  let map result qualifiers =
    let make_callables
        ((existing_callables, existing_stubs, filtered_callables) as result)
        qualifier
      =
      get_source ~environment qualifier
      >>| (fun source ->
            let callables, new_filtered_callables =
              regular_and_filtered_callables ~configuration ~resolution:global_resolution ~source
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
    ~policy:
      (Scheduler.Policy.fixed_chunk_count ~minimum_chunk_size:50 ~preferred_chunks_per_worker:1 ())
    ~map
    ~reduce
    ~initial:([], [], Callable.Set.empty)
    ~inputs:qualifiers
    ()


let record_overrides_for_qualifiers ~scheduler ~environment ~skip_overrides ~qualifiers =
  let overrides =
    let combine ~key:_ left right = List.rev_append left right in
    let build_overrides overrides qualifier =
      try
        match get_source ~environment qualifier with
        | None -> overrides
        | Some source ->
            let new_overrides =
              DependencyGraph.create_overrides ~environment ~source
              |> Reference.Map.filter_keys ~f:(fun override ->
                     not (Reference.Set.mem skip_overrides override))
            in
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
      ~initial:DependencyGraph.empty_overrides
      ~map:(fun _ qualifiers ->
        List.fold qualifiers ~init:DependencyGraph.empty_overrides ~f:build_overrides)
      ~reduce:(Map.merge_skewed ~combine)
      ~inputs:qualifiers
      ()
  in
  let {
    Taint.TaintConfiguration.analysis_model_constraints = { maximum_overrides_to_analyze; _ };
    _;
  }
    =
    Taint.TaintConfiguration.get ()
  in
  DependencyGraphSharedMemory.record_overrides ?maximum_overrides_to_analyze overrides;
  overrides


let analyze
    ~scheduler
    ~analysis_kind
    ~configuration:
      ( {
          Configuration.StaticAnalysis.configuration;
          dump_call_graph;
          verify_models;
          rule_filter;
          find_missing_flows;
          dump_model_query_results;
          use_cache;
          _;
        } as analysis_configuration )
    ~filename_lookup
    ~environment
    ~qualifiers
    ()
  =
  let pre_fixpoint_timer = Timer.start () in
  let get_source = get_source ~environment in

  let cached_initial_callables =
    if use_cache then Cache.load_initial_callables ~configuration else None
  in
  let callables_with_dependency_information, stubs, filtered_callables =
    match cached_initial_callables with
    | Some (cached_callables, cached_stubs, cached_filtered_callables) ->
        Log.warning "Using cached results for initial callables to analyze.";
        cached_callables, cached_stubs, cached_filtered_callables
    | _ ->
        let timer = Timer.start () in
        Log.info "No cached initial callables loaded, fetching initial callables to analyze...";
        let new_callables, new_stubs, new_filtered_callables =
          fetch_callables_to_analyze ~scheduler ~environment ~configuration ~qualifiers
        in
        if use_cache then
          Cache.save_initial_callables
            ~configuration
            ~callables_with_dependency_information:new_callables
            ~stubs:new_stubs
            ~filtered_callables:new_filtered_callables;
        Statistics.performance ~name:"Fetched initial callables to analyze" ~timer ();
        new_callables, new_stubs, new_filtered_callables
  in
  let stubs = (stubs :> Callable.t list) in
  let analyses = [analysis_kind] in
  let timer = Timer.start () in
  Log.info "Initializing analysis...";
  (* Initialize and add initial models of analyses to shared mem. *)
  let skip_overrides =
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
      let find_missing_flows_settings =
        match find_missing_flows with
        | Some missing_flow -> ["find_missing_flows", `String missing_flow]
        | None -> []
      in
      `Assoc
        [
          ( "taint",
            `Assoc
              ( [
                  "model_paths", `List taint_model_paths;
                  "verify_models", `Bool verify_models;
                  "dump_model_query_results", `Bool dump_model_query_results;
                ]
              @ rule_settings
              @ find_missing_flows_settings ) );
        ]
    in
    let functions = (List.map callables_with_dependency_information ~f:fst :> Callable.t list) in
    let { Interprocedural.Analysis.initial_models = models; skip_overrides } =
      Analysis.initialize
        analyses
        ~configuration:configuration_json
        ~scheduler
        ~environment
        ~functions
        ~stubs
    in
    Analysis.record_initial_models ~functions ~stubs models;
    skip_overrides
  in
  Statistics.performance ~name:"Computed initial analysis state" ~timer ();
  let cached_overrides = if use_cache then Cache.load_overrides ~configuration else None in
  let override_dependencies =
    match cached_overrides with
    | Some overrides ->
        Log.warning "Using cached overrides.";
        overrides |> DependencyGraph.from_overrides
    | _ ->
        Log.info "No cached overrides loaded, recording overrides...";
        let timer = Timer.start () in
        let new_overrides =
          record_overrides_for_qualifiers ~scheduler ~environment ~skip_overrides ~qualifiers
        in
        if use_cache then Cache.save_overrides ~configuration ~overrides:new_overrides;
        let new_override_dependencies = DependencyGraph.from_overrides new_overrides in
        Statistics.performance ~name:"Overrides recorded" ~timer ();
        new_override_dependencies
  in
  (* It's imperative that the call graph is built after the overrides are, due to a hidden global
     state dependency. We rely on shared memory to tell us which methods are overridden to
     accurately model the call graph's overrides. Without it, we'll underanalyze and have an
     inconsistent fixpoint. *)
  let cached_callgraph = if use_cache then Cache.load_call_graph ~configuration else None in
  let callgraph =
    match cached_callgraph with
    | Some cached_callgraph ->
        Log.warning "Using cached call graph.";
        cached_callgraph
    | _ ->
        Log.info "No cached call graph loaded, building call graph...";
        let timer = Timer.start () in
        let new_callgraph =
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
            ~initial:Callable.RealMap.empty
            ~map:(fun _ qualifiers ->
              List.fold qualifiers ~init:Callable.RealMap.empty ~f:build_call_graph)
            ~reduce:(Map.merge_skewed ~combine:(fun ~key:_ left _ -> left))
            ~inputs:qualifiers
            ()
        in
        Statistics.performance ~name:"Call graph built" ~timer ();
        Log.info "Call graph edges: %d" (Callable.RealMap.length new_callgraph);
        if use_cache then Cache.save_call_graph ~configuration ~callgraph:new_callgraph;
        if dump_call_graph then
          DependencyGraph.from_callgraph new_callgraph |> DependencyGraph.dump ~configuration;
        new_callgraph
  in
  let timer = Timer.start () in
  Log.info "Computing overrides...";
  let override_targets = (Callable.Map.keys override_dependencies :> Callable.t list) in
  let dependencies, callables =
    let dependencies =
      DependencyGraph.from_callgraph callgraph |> DependencyGraph.union override_dependencies
    in
    let { DependencyGraph.dependencies; pruned_callables } =
      DependencyGraph.prune
        dependencies
        ~callables_with_dependency_information:
          (callables_with_dependency_information :> (Callable.t * bool) list)
    in
    DependencyGraph.reverse dependencies, pruned_callables
  in
  let () =
    let add_predefined callable =
      Fixpoint.add_predefined Fixpoint.Epoch.initial callable Result.empty_model
    in
    List.iter override_targets ~f:add_predefined
  in
  Statistics.performance ~name:"Computed overrides" ~timer ();
  Statistics.performance
    ~name:"Pre-fixpoint computation for static analysis"
    ~phase_name:"Pre-fixpoint computation for static analysis"
    ~timer:pre_fixpoint_timer
    ();
  let all_callables = List.rev_append override_targets callables in
  Log.info
    "Analysis fixpoint started for %d overrides %d functions..."
    (List.length override_targets)
    (List.length callables);
  let timer = Timer.start () in
  let save_results () =
    Interprocedural.Analysis.save_results
      ~configuration:analysis_configuration
      ~filename_lookup
      ~analyses
      all_callables
  in
  let () =
    try
      let iterations =
        Interprocedural.Analysis.compute_fixpoint
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
        save_results ();
        raise exn
  in
  save_results ();
  let errors = Interprocedural.Analysis.extract_errors scheduler all_callables in
  Statistics.performance
    ~name:"Analysis fixpoint complete"
    ~phase_name:"Static analysis fixpoint"
    ~timer
    ();

  (* If saving to a file, don't return errors. Thousands of errors on output is inconvenient *)
  if Option.is_some analysis_configuration.result_json_path then
    []
  else
    errors
