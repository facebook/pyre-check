(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Analysis
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

  val load_call_graph : configuration:Configuration.Analysis.t -> DependencyGraph.callgraph option

  val save_call_graph
    :  configuration:Configuration.Analysis.t ->
    callgraph:DependencyGraph.callgraph ->
    unit
end = struct
  let is_initialized = ref false

  let get_save_directory ~configuration =
    Path.create_relative
      ~root:(Configuration.Analysis.log_directory configuration)
      ~relative:".pysa_cache"


  let get_shared_memory_save_path ~configuration =
    Path.append (get_save_directory ~configuration) ~element:"sharedmem"


  let get_overrides_save_path ~configuration =
    Path.append (get_save_directory ~configuration) ~element:"overrides"


  let get_callgraph_save_path ~configuration =
    Path.append (get_save_directory ~configuration) ~element:"callgraph"


  let invalidate_cache ~configuration =
    let directory = get_save_directory ~configuration in
    let remove_if_exists path =
      match Sys.file_exists path with
      | `Yes -> Core.Unix.remove path
      | `No
      | `Unknown ->
          ()
    in
    Memory.reset_shared_memory ();
    List.iter ~f:Path.remove (Path.list ~root:directory ());
    remove_if_exists (Path.absolute directory)


  let is_pysa_model path = String.is_suffix ~suffix:".pysa" (Path.get_suffix_path path)

  let is_taint_config path = String.is_suffix ~suffix:"taint.config" (Path.absolute path)

  let ensure_save_directory_exists ~configuration =
    let directory = Path.absolute (get_save_directory ~configuration) in
    try Core.Unix.mkdir directory with
    (* [mkdir] on MacOSX returns [EISDIR] instead of [EEXIST] if the directory already exists. *)
    | Core.Unix.Unix_error ((EEXIST | EISDIR), _, _) -> ()
    | e -> raise e


  let init_shared_memory ~configuration =
    if not !is_initialized then (
      let path = get_shared_memory_save_path ~configuration in
      try
        let _ = Memory.get_heap_handle configuration in
        Memory.load_shared_memory ~path:(Path.absolute path) ~configuration;
        is_initialized := true;
        Log.warning
          "Loaded cached state. Please try deleting the cache folder at %s and running Pysa again \
           if unexpected results occur."
          (Path.absolute (get_save_directory ~configuration))
      with
      | error ->
          is_initialized := false;
          raise error )


  let save_shared_memory ~configuration =
    let path = get_shared_memory_save_path ~configuration in
    try
      Log.info "Saving shared memory state to cache file...";
      ensure_save_directory_exists ~configuration;
      Memory.save_shared_memory ~path:(Path.absolute path) ~configuration;
      Log.info "Saved shared memory state to cache file: %s" (Path.absolute path)
    with
    | error when not (Path.file_exists path) ->
        Log.error "Error saving cached state to file: %s" (Exn.to_string error)
    | _ -> ()


  let load_environment ~configuration =
    let path = get_shared_memory_save_path ~configuration in
    try
      init_shared_memory ~configuration;
      let module_tracker = ModuleTracker.SharedMemory.load () in
      let ast_environment = AstEnvironment.load module_tracker in
      let environment =
        AnnotatedGlobalEnvironment.create ast_environment |> TypeEnvironment.create
      in
      let scheduler = Scheduler.create ~configuration () in
      let changed_paths =
        Log.info "Determining if source files have changed since cache was created.";
        ChangedPaths.compute_locally_changed_paths
          ~scheduler
          ~configuration
          ~module_tracker
          ~ast_environment:(AstEnvironment.read_only ast_environment)
        |> List.filter ~f:(fun path -> not (is_pysa_model path || is_taint_config path))
      in
      match changed_paths with
      | [] ->
          SharedMemoryKeys.DependencyKey.Registry.load ();
          Log.info "Loaded type environment from cache shared memory.";
          Some environment
      | _ ->
          Log.info "Changes to source files detected, existing cache has been invalidated.";
          invalidate_cache ~configuration;
          None
    with
    | error when Path.file_exists path ->
        Log.error
          "Error loading cached type environment from shared memory: %s"
          (Exn.to_string error);
        (* Special case to deal with instances where the type environment doesn't load successfully
           but the cached callables and overrides do, leading to odd behavior. If the type
           environment doesn't load, we should invalidate the entire cache. *)
        invalidate_cache ~configuration;
        None
    | _ -> None


  let save_environment ~configuration ~environment =
    let path = get_shared_memory_save_path ~configuration in
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


  let load_initial_callables ~configuration =
    let path = get_shared_memory_save_path ~configuration in
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
    let path = get_shared_memory_save_path ~configuration in
    try
      Memory.SharedMemory.collect `aggressive;
      InitialCallablesSharedMemory.store
        { callables_with_dependency_information; stubs; filtered_callables };
      Log.info "Saved initial callables to cache shared memory.";
      (* Shared memory is saved to file after caching the callables to shared memory. The remaining
         overrides and callgraph to be cached don't use shared memory and are saved as serialized
         sexps to separate files. *)
      save_shared_memory ~configuration
    with
    | error when not (Path.file_exists path) ->
        Log.error "Error saving initial callables to cache shared memory: %s" (Exn.to_string error)
    | _ -> ()


  let load_overrides ~configuration =
    let path = get_overrides_save_path ~configuration in
    try
      let sexp = Sexplib.Sexp.load_sexp (Path.absolute path) in
      let overrides = Reference.Map.t_of_sexp (Core.List.t_of_sexp Reference.t_of_sexp) sexp in
      Log.info "Loaded overrides from cache.";
      Some overrides
    with
    | error when Path.file_exists path ->
        Log.error "Error loading overrides from cache: %s" (Exn.to_string error);
        None
    | _ -> None


  let save_overrides ~configuration ~overrides =
    let path = get_overrides_save_path ~configuration in
    try
      let data = Reference.Map.sexp_of_t (Core.List.sexp_of_t Reference.sexp_of_t) overrides in
      ensure_save_directory_exists ~configuration;
      Sexplib.Sexp.save (Path.absolute path) data;
      Log.info "Saved overrides to cache file: %s" (Path.absolute path)
    with
    | error when not (Path.file_exists path) ->
        Log.error "Error saving overrides to cache: %s" (Exn.to_string error)
    | _ -> ()


  let load_call_graph ~configuration =
    let path = get_callgraph_save_path ~configuration in
    try
      let sexp = Sexplib.Sexp.load_sexp (Path.absolute path) in
      let callgraph = Callable.RealMap.t_of_sexp (Core.List.t_of_sexp Callable.t_of_sexp) sexp in
      Log.info "Loaded call graph from cache.";
      Some callgraph
    with
    | error when Path.file_exists path ->
        Log.error "Error loading call graph from cache: %s" (Exn.to_string error);
        None
    | _ -> None


  let save_call_graph ~configuration ~callgraph =
    let path = get_callgraph_save_path ~configuration in
    try
      let data = Callable.RealMap.sexp_of_t (Core.List.sexp_of_t Callable.sexp_of_t) callgraph in
      ensure_save_directory_exists ~configuration;
      Sexplib.Sexp.save (Path.absolute path) data;
      Log.info "Saved call graph to cache file: %s" (Path.absolute path)
    with
    | error when not (Path.file_exists path) ->
        Log.error "Error saving call graph to cache: %s" (Exn.to_string error)
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
  List.map ~f:(fun define -> Callable.create define, define) defines


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


let record_overrides_for_qualifiers
    ~configuration
    ~use_cache
    ~scheduler
    ~environment
    ~skip_overrides
    ~qualifiers
  =
  let overrides =
    let cached_overrides = if use_cache then Cache.load_overrides ~configuration else None in
    match cached_overrides with
    | Some overrides ->
        Log.warning "Using cached overrides.";
        overrides
    | _ ->
        let () = Log.warning "No cached overrides loaded, computing overrides..." in
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
                "Error building overrides in path %a for untracked type %s"
                Reference.pp
                qualifier
                untracked_type;
              overrides
        in
        let new_overrides =
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
        if use_cache then Cache.save_overrides ~configuration ~overrides:new_overrides;
        new_overrides
  in
  let {
    Taint.TaintConfiguration.analysis_model_constraints = { maximum_overrides_to_analyze; _ };
    _;
  }
    =
    Taint.TaintConfiguration.get ()
  in
  let ({ DependencyGraphSharedMemory.overrides; _ } as cap_override_result) =
    DependencyGraphSharedMemory.cap_overrides ?maximum_overrides_to_analyze overrides
  in
  DependencyGraphSharedMemory.record_overrides overrides;
  cap_override_result


let analyze
    ~scheduler
    ~analysis_kind
    ~configuration:
      ({ Configuration.StaticAnalysis.configuration; use_cache; _ } as static_analysis_configuration)
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
        Statistics.performance
          ~name:"Fetched initial callables to analyze"
          ~phase_name:"Fetching initial callables to analyze"
          ~timer
          ();
        new_callables, new_stubs, new_filtered_callables
  in
  let stubs = (stubs :> Callable.t list) in
  let analyses = [analysis_kind] in
  let timer = Timer.start () in
  (* Initialize and add initial models of analyses to shared mem. We do this prior to computing
     overrides because models can optionally specify overrides to skip *)
  Log.info "Initializing analysis...";
  let skip_overrides, initial_models_callables =
    let functions = (List.map callables_with_dependency_information ~f:fst :> Callable.t list) in
    let { Interprocedural.Analysis.initial_models = models; skip_overrides } =
      Analysis.initialize
        analyses
        ~configuration:static_analysis_configuration
        ~scheduler
        ~environment
        ~functions
        ~stubs
    in
    Analysis.record_initial_models ~functions ~stubs models;
    skip_overrides, Callable.Map.keys models
  in
  Statistics.performance
    ~name:"Computed initial analysis state"
    ~phase_name:"Computing initial analysis state"
    ~timer
    ();
  (* Compute the override graph, which maps overide_targets (parent methods which are overridden) to
     all concrete methods overriding them. We also save data used for callgraph construction into
     shared memory. *)
  Log.info "Computing overrides...";
  let timer = Timer.start () in
  let { DependencyGraphSharedMemory.overrides; skipped_overrides } =
    record_overrides_for_qualifiers
      ~configuration
      ~use_cache
      ~scheduler
      ~environment
      ~skip_overrides
      ~qualifiers
  in
  let override_dependencies = DependencyGraph.from_overrides overrides in
  Statistics.performance ~name:"Overrides computed" ~phase_name:"Computing overrides" ~timer ();
  (* Build the callgraph, a map from caller to callees. The overrides must be computed first because
     we depend on a global shared memory graph to include overrides in the call graph. Without it,
     we'll underanalyze and have an inconsistent fixpoint. *)
  let callgraph =
    let cached_callgraph = if use_cache then Cache.load_call_graph ~configuration else None in
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
                  "Error building call graph in path %a for untracked type %s"
                  Reference.pp
                  qualifier
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
        Statistics.performance ~name:"Call graph built" ~phase_name:"Building call graph" ~timer ();
        Log.info "Call graph edges: %d" (Callable.RealMap.length new_callgraph);
        if use_cache then
          Cache.save_call_graph ~configuration ~callgraph:new_callgraph;
        if static_analysis_configuration.dump_call_graph then
          DependencyGraph.from_callgraph new_callgraph |> DependencyGraph.dump ~configuration;
        new_callgraph
  in
  (* Merge overrides and callgraph into a combined dependency graph, and prune anything not linked
     to the callables we are actually analyzing. Then reverse the graph, which maps dependers to
     dependees (i.e. override targets to overrides + callers to callees) into a scheduling graph
     that maps dependees to dependers. *)
  Log.info "Computing dependencies...";
  let timer = Timer.start () in
  let override_targets = (Callable.Map.keys override_dependencies :> Callable.t list) in
  let dependencies, callables_to_analyze =
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
  (* Create an empty callable for each override target (on each iteration, the framework will update
     these by joining models for all overrides *)
  let () =
    let add_predefined callable =
      Fixpoint.add_predefined Fixpoint.Epoch.initial callable Result.empty_model
    in
    List.iter override_targets ~f:add_predefined
  in
  Statistics.performance
    ~name:"Computed dependencies"
    ~phase_name:"Computing dependencies"
    ~timer
    ();
  Statistics.performance
    ~name:"Pre-fixpoint computation for static analysis"
    ~phase_name:"Pre-fixpoint computation for static analysis"
    ~timer:pre_fixpoint_timer
    ();
  (* Run the analysis and save results *)
  Log.info
    "Analysis fixpoint started for %d overrides and %d functions..."
    (List.length override_targets)
    (List.length callables_to_analyze);
  let callables_to_analyze = List.rev_append override_targets callables_to_analyze in
  let timer = Timer.start () in
  let compute_fixpoint () =
    Interprocedural.Analysis.compute_fixpoint
      ~scheduler
      ~environment
      ~analyses
      ~dependencies
      ~filtered_callables
      ~all_callables:callables_to_analyze
      Interprocedural.Fixpoint.Epoch.initial
  in
  let report_results iterations =
    Interprocedural.Analysis.report_results
      ~scheduler
      ~static_analysis_configuration
      ~filename_lookup
      ~analyses
      ~callables_to_analyze
      ~initial_models_callables
      ~skipped_overrides
      ~iterations
  in
  try
    let iterations = compute_fixpoint () in
    Log.info "Fixpoint iterations: %d" iterations;
    let analysis_stats_integers = report_results (Some iterations) in
    Statistics.performance
      ~name:"Analysis fixpoint complete"
      ~phase_name:"Static analysis fixpoint"
      ~timer
      ~integers:analysis_stats_integers
      ()
  with
  | exn ->
      let _ = report_results None in
      raise exn
