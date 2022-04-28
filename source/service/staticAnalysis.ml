(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Statement
open Pyre
module Target = Interprocedural.Target
module TypeEnvironment = Analysis.TypeEnvironment
module AstEnvironment = Analysis.AstEnvironment
module GlobalResolution = Analysis.GlobalResolution
module DependencyGraph = Interprocedural.DependencyGraph
module DependencyGraphSharedMemory = Interprocedural.DependencyGraphSharedMemory
module ClassHierarchyGraph = Interprocedural.ClassHierarchyGraph

(* The boolean indicated whether the callable is internal or not. *)
type callable_with_dependency_information = Target.t * bool

type initial_callables = {
  callables_with_dependency_information: callable_with_dependency_information list;
  stubs: Target.t list;
  filtered_callables: Target.Set.t;
}

module InitialCallablesSharedMemory = Memory.Serializer (struct
  type t = initial_callables

  module Serialized = struct
    type t = initial_callables

    let prefix = Prefix.make ()

    let description = "Initial callables to analyze"
  end

  let serialize = Fn.id

  let deserialize = Fn.id
end)

module ClassHierarchyGraphSharedMemory = Memory.Serializer (struct
  type t = ClassHierarchyGraph.t

  module Serialized = struct
    type t = ClassHierarchyGraph.t

    let prefix = Prefix.make ()

    let description = "Class hierarchy graph"
  end

  let serialize = Fn.id

  let deserialize = Fn.id
end)

module Cache = struct
  type cached = { module_tracker: Analysis.ModuleTracker.t }

  type error =
    | InvalidByCodeChange
    | LoadError
    | NotFound
    | Disabled

  type t = {
    cache: (cached, error) Result.t;
    save_cache: bool;
    scheduler: Scheduler.t;
    configuration: Configuration.Analysis.t;
  }

  let get_save_directory ~configuration =
    PyrePath.create_relative
      ~root:(Configuration.Analysis.log_directory configuration)
      ~relative:".pysa_cache"


  let get_shared_memory_save_path ~configuration =
    PyrePath.append (get_save_directory ~configuration) ~element:"sharedmem"


  let get_overrides_save_path ~configuration =
    PyrePath.append (get_save_directory ~configuration) ~element:"overrides"


  let get_callgraph_save_path ~configuration =
    PyrePath.append (get_save_directory ~configuration) ~element:"callgraph"


  let exception_to_error ~error ~message ~f =
    try f () with
    | exception_ ->
        Log.error "Error %s:\n%s" message (Exn.to_string exception_);
        Error error


  let ignore_result (_ : ('a, 'b) result) = ()

  let initialize_shared_memory ~configuration =
    let path = get_shared_memory_save_path ~configuration in
    if not (PyrePath.file_exists path) then (
      Log.warning "Could not find a cached state.";
      Error NotFound)
    else
      exception_to_error ~error:LoadError ~message:"loading cached state" ~f:(fun () ->
          let _ = Memory.get_heap_handle configuration in
          Memory.load_shared_memory ~path:(PyrePath.absolute path) ~configuration;
          Log.warning
            "Cached state successfully loaded from `%s`."
            (PyrePath.absolute (get_save_directory ~configuration));
          Ok ())


  let load_module_tracker ~scheduler ~configuration =
    let open Result in
    Log.info "Determining if source files have changed since cache was created.";
    exception_to_error ~error:LoadError ~message:"loading module tracker from cache" ~f:(fun () ->
        Ok (Analysis.ModuleTracker.Serializer.from_stored_layouts ~configuration ()))
    >>= fun old_module_tracker ->
    let new_module_tracker = Analysis.ModuleTracker.create configuration in
    let changed_paths =
      let is_pysa_model path = String.is_suffix ~suffix:".pysa" (PyrePath.get_suffix_path path) in
      let is_taint_config path = String.is_suffix ~suffix:"taint.config" (PyrePath.absolute path) in
      ChangedPaths.compute_locally_changed_paths
        ~scheduler
        ~configuration
        ~old_module_tracker
        ~new_module_tracker
      |> List.filter ~f:(fun path -> not (is_pysa_model path || is_taint_config path))
    in
    match changed_paths with
    | [] -> Ok new_module_tracker
    | _ ->
        Log.warning "Changes to source files detected, ignoring existing cache.";
        Error InvalidByCodeChange


  let load ~scheduler ~configuration ~enabled =
    if not enabled then
      { cache = Error Disabled; save_cache = false; scheduler; configuration }
    else
      let open Result in
      let module_tracker =
        initialize_shared_memory ~configuration
        >>= fun () -> load_module_tracker ~scheduler ~configuration
      in
      let cache =
        match module_tracker with
        | Ok module_tracker -> Ok { module_tracker }
        | Error error ->
            Memory.reset_shared_memory ();
            Error error
      in
      { cache; save_cache = true; scheduler; configuration }


  let load_type_environment ~module_tracker =
    exception_to_error ~error:LoadError ~message:"loading type environment from cache" ~f:(fun () ->
        let ast_environment = AstEnvironment.load module_tracker in
        let environment =
          Analysis.AnnotatedGlobalEnvironment.create ast_environment |> TypeEnvironment.create
        in
        Analysis.SharedMemoryKeys.DependencyKey.Registry.load ();
        Log.info "Loaded cached type environment.";
        Ok environment)


  let save_type_environment ~scheduler ~configuration ~environment =
    exception_to_error ~error:() ~message:"saving type environment to cache" ~f:(fun () ->
        Memory.SharedMemory.collect `aggressive;
        let module_tracker = TypeEnvironment.module_tracker environment in
        let ast_environment = TypeEnvironment.ast_environment environment in
        ChangedPaths.save_current_paths ~scheduler ~configuration ~module_tracker;
        Analysis.ModuleTracker.Serializer.store_layouts module_tracker;
        AstEnvironment.store ast_environment;
        Analysis.SharedMemoryKeys.DependencyKey.Registry.store ();
        Log.info "Saved type environment to cache shared memory.";
        Ok ())


  let type_environment { cache; save_cache; scheduler; configuration } f =
    let type_environment =
      match cache with
      | Ok { module_tracker } -> load_type_environment ~module_tracker |> Result.ok
      | _ -> None
    in
    match type_environment with
    | Some type_environment -> type_environment
    | None ->
        let environment = f () in
        if save_cache then
          save_type_environment ~scheduler ~configuration ~environment |> ignore_result;
        environment


  let load_initial_callables () =
    exception_to_error
      ~error:LoadError
      ~message:"loading initial callables from cache"
      ~f:(fun () ->
        let initial_callables = InitialCallablesSharedMemory.load () in
        Log.info "Loaded cached initial callables.";
        Ok initial_callables)


  let ensure_save_directory_exists ~configuration =
    let directory = PyrePath.absolute (get_save_directory ~configuration) in
    try Core.Unix.mkdir directory with
    (* [mkdir] on MacOSX returns [EISDIR] instead of [EEXIST] if the directory already exists. *)
    | Core.Unix.Unix_error ((EEXIST | EISDIR), _, _) -> ()
    | e -> raise e


  let save_shared_memory ~configuration =
    exception_to_error ~error:() ~message:"saving cached state to file" ~f:(fun () ->
        let path = get_shared_memory_save_path ~configuration in
        Log.info "Saving shared memory state to cache file...";
        ensure_save_directory_exists ~configuration;
        Memory.save_shared_memory ~path:(PyrePath.absolute path) ~configuration;
        Log.info "Saved shared memory state to cache file: `%s`" (PyrePath.absolute path);
        Ok ())


  let save_initial_callables ~configuration ~initial_callables =
    exception_to_error ~error:() ~message:"saving initial callables to cache" ~f:(fun () ->
        Memory.SharedMemory.collect `aggressive;
        InitialCallablesSharedMemory.store initial_callables;
        Log.info "Saved initial callables to cache shared memory.";
        (* Shared memory is saved to file after caching the callables to shared memory. The
           remaining overrides and callgraph to be cached don't use shared memory and are saved as
           serialized sexps to separate files. *)
        save_shared_memory ~configuration)


  let initial_callables { cache; save_cache; configuration; _ } f =
    let initial_callables =
      match cache with
      | Ok _ -> load_initial_callables () |> Result.ok
      | _ -> None
    in
    match initial_callables with
    | Some initial_callables -> initial_callables
    | None ->
        let callables = f () in
        if save_cache then
          save_initial_callables ~configuration ~initial_callables:callables |> ignore_result;
        callables


  let load_overrides ~configuration =
    exception_to_error ~error:LoadError ~message:"loading overrides from cache" ~f:(fun () ->
        let path = get_overrides_save_path ~configuration in
        let sexp = Sexplib.Sexp.load_sexp (PyrePath.absolute path) in
        let overrides = Reference.Map.t_of_sexp (Core.List.t_of_sexp Reference.t_of_sexp) sexp in
        Log.info "Loaded overrides from cache.";
        Ok overrides)


  let save_overrides ~configuration ~overrides =
    exception_to_error ~error:() ~message:"saving overrides to cache" ~f:(fun () ->
        let path = get_overrides_save_path ~configuration in
        let data = Reference.Map.sexp_of_t (Core.List.sexp_of_t Reference.sexp_of_t) overrides in
        ensure_save_directory_exists ~configuration;
        Sexplib.Sexp.save (PyrePath.absolute path) data;
        Log.info "Saved overrides to cache file: `%s`" (PyrePath.absolute path);
        Ok ())


  let overrides { cache; save_cache; configuration; _ } f =
    let overrides =
      match cache with
      | Ok _ -> load_overrides ~configuration |> Result.ok
      | _ -> None
    in
    match overrides with
    | Some overrides -> overrides
    | None ->
        let overrides = f () in
        if save_cache then save_overrides ~configuration ~overrides |> ignore_result;
        overrides


  let load_call_graph ~configuration =
    exception_to_error ~error:LoadError ~message:"loading call graph from cache" ~f:(fun () ->
        let path = get_callgraph_save_path ~configuration in
        let sexp = Sexplib.Sexp.load_sexp (PyrePath.absolute path) in
        let callgraph = Target.Map.t_of_sexp (Core.List.t_of_sexp Target.t_of_sexp) sexp in
        Log.info "Loaded call graph from cache.";
        Ok callgraph)


  let save_call_graph ~configuration ~call_graph =
    exception_to_error ~error:() ~message:"saving call graph to cache" ~f:(fun () ->
        let path = get_callgraph_save_path ~configuration in
        let data = Target.Map.sexp_of_t (Core.List.sexp_of_t Target.sexp_of_t) call_graph in
        ensure_save_directory_exists ~configuration;
        Sexplib.Sexp.save (PyrePath.absolute path) data;
        Log.info "Saved call graph to cache file: `%s`" (PyrePath.absolute path);
        Ok ())


  let call_graph { cache; save_cache; configuration; _ } f =
    let call_graph =
      match cache with
      | Ok _ -> load_call_graph ~configuration |> Result.ok
      | _ -> None
    in
    match call_graph with
    | Some call_graph -> call_graph
    | None ->
        let call_graph = f () in
        if save_cache then save_call_graph ~configuration ~call_graph |> ignore_result;
        call_graph


  let load_class_hierarchy_graph () =
    exception_to_error
      ~error:LoadError
      ~message:"loading class hierarchy graph from cache"
      ~f:(fun () ->
        let class_hierarchy_graph = ClassHierarchyGraphSharedMemory.load () in
        Log.info "Loaded class hierarchy graph.";
        Ok class_hierarchy_graph)


  let save_class_hierarchy_graph ~class_hierarchy_graph =
    exception_to_error ~error:() ~message:"saving class hierarchy graph to cache" ~f:(fun () ->
        Memory.SharedMemory.collect `aggressive;
        ClassHierarchyGraphSharedMemory.store class_hierarchy_graph;
        Log.info "Saved class hierarchy graph to cache shared memory.";
        Ok ())


  let class_hierarchy_graph { cache; save_cache; _ } f =
    let class_hierarchy_graph =
      match cache with
      | Ok _ -> load_class_hierarchy_graph () |> Result.ok
      | _ -> None
    in
    match class_hierarchy_graph with
    | Some class_hierarchy_graph -> class_hierarchy_graph
    | None ->
        let class_hierarchy_graph = f () in
        if save_cache then
          save_class_hierarchy_graph ~class_hierarchy_graph |> ignore_result;
        class_hierarchy_graph
end

(* Perform a full type check and build a type environment. *)
let type_check ~scheduler ~configuration ~cache =
  Cache.type_environment cache (fun () ->
      let configuration =
        (* In order to get an accurate call graph and type information, we need to ensure that we
           schedule a type check for external files. *)
        { configuration with Configuration.Analysis.analyze_external_sources = true }
      in
      Check.check
        ~scheduler
        ~configuration
        ~call_graph_builder:(module Analysis.Callgraph.NullBuilder)
      |> fun { environment; _ } -> environment)


let parse_and_save_decorators_to_skip
    ~inline_decorators
    { Configuration.Analysis.taint_model_paths; _ }
  =
  Analysis.InlineDecorator.set_should_inline_decorators inline_decorators;
  if inline_decorators then (
    let timer = Timer.start () in
    Log.info "Getting decorators to skip when inlining...";
    let model_sources = Taint.ModelParser.get_model_sources ~paths:taint_model_paths in
    let decorators_to_skip =
      List.concat_map model_sources ~f:(fun (path, source) ->
          Analysis.InlineDecorator.decorators_to_skip ~path source)
    in
    List.iter decorators_to_skip ~f:(fun decorator ->
        Analysis.InlineDecorator.DecoratorsToSkip.add decorator decorator);
    Statistics.performance
      ~name:"Getting decorators to skip when inlining"
      ~phase_name:"Getting decorators to skip when inlining"
      ~timer
      ())


let record_and_merge_call_graph ~static_analysis_configuration ~environment ~call_graph ~source =
  let record_and_merge_call_graph map call_graph =
    Map.merge_skewed map call_graph ~combine:(fun ~key:_ left _ -> left)
  in
  Interprocedural.CallGraph.create_callgraph
    ~static_analysis_configuration
    ~store_shared_memory:true
    ~environment
    ~source
  |> record_and_merge_call_graph call_graph


let unfiltered_callables ~resolution ~source:{ Source.source_path = { SourcePath.qualifier; _ }; _ }
  =
  let defines =
    GlobalResolution.unannotated_global_environment resolution
    |> (fun environment ->
         Analysis.UnannotatedGlobalEnvironment.ReadOnly.all_defines_in_module environment qualifier)
    |> List.filter_map ~f:(GlobalResolution.function_definitions resolution)
    |> List.concat
    |> List.filter ~f:(fun { Node.value = define; _ } -> not (Define.is_overloaded_function define))
  in
  List.map ~f:(fun define -> Target.create define, define) defines


type found_callable = {
  callable: Target.t;
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
        ({
           callables_with_dependency_information = existing_callables;
           stubs = existing_stubs;
           filtered_callables = existing_filtered_callables;
         } as result)
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
            let filtered_callables =
              List.fold
                new_filtered_callables
                ~init:existing_filtered_callables
                ~f:(Fn.flip Target.Set.add)
            in
            { callables_with_dependency_information = callables; stubs; filtered_callables })
      |> Option.value ~default:result
    in
    List.fold qualifiers ~f:make_callables ~init:result
  in
  let reduce
      {
        callables_with_dependency_information = new_callables;
        stubs = new_stubs;
        filtered_callables = new_filtered_callables;
      }
      { callables_with_dependency_information = callables; stubs; filtered_callables }
    =
    {
      callables_with_dependency_information = List.rev_append new_callables callables;
      stubs = List.rev_append new_stubs stubs;
      filtered_callables = Target.Set.union new_filtered_callables filtered_callables;
    }
  in
  Scheduler.map_reduce
    scheduler
    ~policy:
      (Scheduler.Policy.fixed_chunk_count ~minimum_chunk_size:50 ~preferred_chunks_per_worker:1 ())
    ~map
    ~reduce
    ~initial:
      {
        callables_with_dependency_information = [];
        stubs = [];
        filtered_callables = Target.Set.empty;
      }
    ~inputs:qualifiers
    ()


(* Traverse the AST to find all callables (functions and methods), filtering out callables from test
   files. *)
let fetch_initial_callables ~scheduler ~configuration ~cache ~environment ~qualifiers =
  Cache.initial_callables cache (fun () ->
      let timer = Timer.start () in
      let initial_callables =
        fetch_callables_to_analyze ~scheduler ~environment ~configuration ~qualifiers
      in
      Statistics.performance
        ~name:"Fetched initial callables to analyze"
        ~phase_name:"Fetching initial callables to analyze"
        ~timer
        ();
      initial_callables)


let build_class_hierarchy_graph ~scheduler ~cache ~environment ~qualifiers =
  Cache.class_hierarchy_graph cache (fun () ->
      let timer = Timer.start () in
      let build_class_hierarchy_graph _ qualifiers =
        List.fold qualifiers ~init:ClassHierarchyGraph.empty ~f:(fun accumulator qualifier ->
            match get_source ~environment qualifier with
            | Some source ->
                let graph = ClassHierarchyGraph.from_source ~environment ~source in
                ClassHierarchyGraph.join accumulator graph
            | None -> accumulator)
      in
      let class_hierarchy_graph =
        Scheduler.map_reduce
          scheduler
          ~policy:(Scheduler.Policy.legacy_fixed_chunk_count ())
          ~initial:ClassHierarchyGraph.empty
          ~map:build_class_hierarchy_graph
          ~reduce:ClassHierarchyGraph.join
          ~inputs:qualifiers
          ()
      in
      Statistics.performance
        ~name:"Computed class hierarchy graph"
        ~phase_name:"Computing class hierarchy graph"
        ~timer
        ();
      class_hierarchy_graph)


let build_class_intervals class_hierarchy_graph =
  let timer = Timer.start () in
  Interprocedural.ClassInterval.compute_intervals class_hierarchy_graph
  |> Interprocedural.ClassInterval.SharedMemory.store;
  Statistics.performance
    ~name:"Computed class intervals"
    ~phase_name:"Computing class intervals"
    ~timer
    ()


(* Compute the override graph, which maps overide_targets (parent methods which are overridden) to
   all concrete methods overriding them, and save it to shared memory. *)
let record_overrides_for_qualifiers ~scheduler ~cache ~environment ~skip_overrides ~qualifiers =
  let overrides =
    Cache.overrides cache (fun () ->
        let combine ~key:_ left right = List.rev_append left right in
        let build_overrides overrides qualifier =
          match get_source ~environment qualifier with
          | None -> overrides
          | Some source ->
              let new_overrides =
                DependencyGraph.create_overrides ~environment ~source
                |> Reference.Map.filter_keys ~f:(fun override ->
                       not (Reference.Set.mem skip_overrides override))
              in
              Map.merge_skewed overrides new_overrides ~combine
        in
        Scheduler.map_reduce
          scheduler
          ~policy:(Scheduler.Policy.legacy_fixed_chunk_count ())
          ~initial:DependencyGraph.empty_overrides
          ~map:(fun _ qualifiers ->
            List.fold qualifiers ~init:DependencyGraph.empty_overrides ~f:build_overrides)
          ~reduce:(Map.merge_skewed ~combine)
          ~inputs:qualifiers
          ())
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


(* Build the callgraph, a map from caller to callees. The overrides must be computed first because
   we depend on a global shared memory graph to include overrides in the call graph. Without it,
   we'll underanalyze and have an inconsistent fixpoint. *)
let build_call_graph ~scheduler ~static_analysis_configuration ~cache ~environment ~qualifiers =
  let call_graph =
    Cache.call_graph cache (fun () ->
        let build_call_graph call_graph qualifier =
          get_source ~environment qualifier
          >>| (fun source ->
                record_and_merge_call_graph
                  ~static_analysis_configuration
                  ~environment
                  ~call_graph
                  ~source)
          |> Option.value ~default:call_graph
        in
        Scheduler.map_reduce
          scheduler
          ~policy:(Scheduler.Policy.legacy_fixed_chunk_count ())
          ~initial:Target.Map.empty
          ~map:(fun _ qualifiers -> List.fold qualifiers ~init:Target.Map.empty ~f:build_call_graph)
          ~reduce:(Map.merge_skewed ~combine:(fun ~key:_ left _ -> left))
          ~inputs:qualifiers
          ())
  in
  let () =
    match static_analysis_configuration.Configuration.StaticAnalysis.dump_call_graph with
    | Some path -> DependencyGraph.from_callgraph call_graph |> DependencyGraph.dump ~path
    | None -> ()
  in
  call_graph


(* Merge overrides and callgraph into a combined dependency graph, and prune anything not linked to
   the callables we are actually analyzing. Then reverse the graph, which maps dependers to
   dependees (i.e. override targets to overrides + callers to callees) into a scheduling graph that
   maps dependees to dependers. *)
let build_dependency_graph ~callables_with_dependency_information ~callgraph ~override_dependencies =
  let override_targets = Target.Map.keys override_dependencies in
  let dependencies, callables_to_analyze =
    let dependencies =
      DependencyGraph.from_callgraph callgraph |> DependencyGraph.union override_dependencies
    in
    let { DependencyGraph.dependencies; pruned_callables } =
      DependencyGraph.prune dependencies ~callables_with_dependency_information
    in
    DependencyGraph.reverse dependencies, pruned_callables
  in

  (* Create an empty callable for each override target (on each iteration, the framework will update
     these by joining models for all overrides *)
  let () =
    let add_predefined callable =
      Interprocedural.FixpointState.add_predefined
        Interprocedural.FixpointState.Epoch.initial
        callable
        Interprocedural.AnalysisResult.empty_model
    in
    List.iter override_targets ~f:add_predefined
  in
  dependencies, callables_to_analyze, override_targets


let purge_shared_memory ~environment ~qualifiers =
  (* Aggressively remove things we do not need anymore from the shared memory. *)
  let ast_environment = TypeEnvironment.ast_environment environment in
  AstEnvironment.remove_sources ast_environment qualifiers;
  Memory.SharedMemory.collect `aggressive;
  ()
