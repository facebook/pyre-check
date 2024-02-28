(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* PyrePysaApi provides the Pyre environment interfaces used by the Pysa OCaml codebase, including
   both taint analysis and model queries. *)

open Core

(* Private helper module for creating a TypeEnvironment the way Pysa wants to. *)
module PysaTypeEnvironment = struct
  let create ~configuration ~decorator_configuration =
    let configuration =
      (* In order to get an accurate call graph and type information, we need to ensure that we
         schedule a type check for external files. *)
      (* TODO(T180476103) Remove the need for this by using explicit_qualifiers, and delete this
         flag from the configuration + environment logic. *)
      { configuration with Configuration.Analysis.analyze_external_sources = true }
    in
    let () = DecoratorPreprocessing.setup_preprocessing decorator_configuration in
    let errors_environment =
      EnvironmentControls.create ~populate_call_graph:false configuration
      |> ErrorsEnvironment.create_with_ast_environment
    in
    ErrorsEnvironment.AssumeDownstreamNeverNeedsUpdates.type_environment errors_environment


  let qualifiers_and_definitions ~scheduler type_environment =
    Log.info "Starting type checking...";
    PyreProfiling.track_shared_memory_usage ~name:"Before legacy type check" ();
    let qualifiers =
      TypeEnvironment.AssumeGlobalModuleListing.global_module_paths_api type_environment
      |> GlobalModulePathsApi.type_check_qualifiers
    in
    Log.info "Found %d modules" (List.length qualifiers);
    let definitions = TypeEnvironment.collect_definitions ~scheduler type_environment qualifiers in
    Log.info "Found %d functions" (List.length definitions);
    qualifiers, definitions


  let populate ~scheduler type_environment definitions =
    let () = TypeEnvironment.populate_for_definitions ~scheduler type_environment definitions in
    Statistics.event
      ~section:`Memory
      ~name:"shared memory size post-typecheck"
      ~integers:["size", Memory.heap_size ()]
      ();
    PyreProfiling.track_shared_memory_usage ~name:"After legacy type check" ();
    ()


  let absolute_source_path_of_qualifier ~lookup_source type_environment =
    let source_code_api =
      TypeEnvironment.read_only type_environment
      |> TypeEnvironment.ReadOnly.get_untracked_source_code_api
    in
    SourcePaths.absolute_source_path_of_qualifier ~lookup_source ~source_code_api
end

(* Api used in the top-level code of `pyre analyze`, where we need read-write access in order to
   perform operations like loading / saving the Pysa cache cache and scheduling type analysis in
   parallel. *)
module ReadWrite = struct
  type t = { type_environment: TypeEnvironment.t }

  (* Constructors *)

  (* Note: this function assumes that Cache.ml logic has initialized shared memory (which is global,
     and loading is purely side-effect based) from a cache dump already. *)
  let load_from_cache ~configuration =
    let type_environment =
      let controls = EnvironmentControls.create configuration in
      TypeEnvironment.AssumeAstEnvironment.load_without_dependency_keys controls
    in
    { type_environment }


  let create_with_cold_start
      ~scheduler
      ~configuration
      ~decorator_configuration
      ~callback_with_qualifiers_and_definitions
    =
    let type_environment = PysaTypeEnvironment.create ~configuration ~decorator_configuration in
    let qualifiers, definitions =
      PysaTypeEnvironment.qualifiers_and_definitions ~scheduler type_environment
    in
    let () =
      callback_with_qualifiers_and_definitions
        (PysaTypeEnvironment.absolute_source_path_of_qualifier type_environment)
        qualifiers
        definitions
    in
    PysaTypeEnvironment.populate ~scheduler type_environment definitions;
    { type_environment }


  (* Helpers to access underlying environment and configuration *)

  let read_write_type_environment { type_environment } = type_environment

  let read_write_module_tracker api =
    read_write_type_environment api
    |> TypeEnvironment.unannotated_global_environment
    |> UnannotatedGlobalEnvironment.AssumeAstEnvironment.ast_environment
    |> AstEnvironment.module_tracker


  let type_environment { type_environment } = TypeEnvironment.read_only type_environment

  let configuration api =
    type_environment api |> TypeEnvironment.ReadOnly.controls |> EnvironmentControls.configuration


  (* Interface used in cache management *)

  let module_paths api = read_write_module_tracker api |> ModuleTracker.module_paths

  (* Reload module paths from disk, ignoring any caches. Used by Pysa's cache invalidation logic. *)
  let module_paths_from_disk api =
    configuration api
    |> EnvironmentControls.create
    |> ModuleTracker.create
    |> ModuleTracker.module_paths


  let all_module_paths api = read_write_module_tracker api |> ModuleTracker.all_module_paths

  let artifact_path_of_module_path api module_path =
    let configuration = configuration api in
    ArtifactPaths.artifact_path_of_module_path ~configuration module_path


  let save api =
    read_write_type_environment api
    |> TypeEnvironment.AssumeAstEnvironment.store_without_dependency_keys


  (* Interface to get source paths; used when dumping stats from Pysa *)

  let absolute_source_path_of_qualifier ~lookup_source api =
    read_write_type_environment api
    |> PysaTypeEnvironment.absolute_source_path_of_qualifier ~lookup_source
end
