(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* AstEnvironment: layer of the environment stack
 * - upstream: ModuleTracker
 * - downstream: UnannotatedGlobalEnvironment
 * - lookup key: qualifier (module name as a Reference.t)
 * - value: Ast data; depending on the lookup function either raw
 *   or preprocessed.
 *
 * It is responsible for two things:
 * - Parsing raw sources, and maintining a cache. Raw sources here mean
 *   Ast data as they come directly from the parsing logic.
 * - Producing (but *not* caching!) processed sources. Processed sources
 *   here mean that raw Ast data has been transformed in two ways:
 *   - wildcard import expansion (which can introduce dependencies on other
 *     modules)
 *   - Preprocessing logic, which includes various transforms such as
 *     name-mangling private attributes, expending named tuples, etc.
 *)

open Ast
open Core

module ReadOnly = struct
  type t = {
    module_tracker: ModuleTracker.ReadOnly.t;
    raw_source_of_qualifier:
      ?dependency:SharedMemoryKeys.DependencyKey.registered ->
      Reference.t ->
      Parsing.ParseResult.t option;
  }

  let module_tracker { module_tracker; _ } = module_tracker

  let controls environment = module_tracker environment |> ModuleTracker.ReadOnly.controls

  let raw_source_of_qualifier { raw_source_of_qualifier; _ } = raw_source_of_qualifier

  let processed_source_of_qualifier environment ?dependency qualifier =
    (* The fact that preprocessing a module depends on the module itself is implicitly assumed in
       `update`. No need to explicitly record the dependency. But we do need to record all other
       modules used *)
    let raw_source_of_qualifier_and_maybe_track qualifier_to_load =
      let maybe_dependency =
        if Reference.equal qualifier_to_load qualifier then None else dependency
      in
      raw_source_of_qualifier environment ?dependency:maybe_dependency qualifier_to_load
    in
    AstProcessing.processed_source_of_qualifier
      ~raw_source_of_qualifier:raw_source_of_qualifier_and_maybe_track
      qualifier


  let source_code_api_impl environment ~dependency =
    SourceCodeApi.create
      ~controls:(controls environment)
      ~module_path_of_qualifier:
        (module_tracker environment |> ModuleTracker.ReadOnly.module_path_of_qualifier)
      ~raw_source_of_qualifier:(raw_source_of_qualifier environment ?dependency)


  let get_tracked_source_code_api environment ~dependency =
    source_code_api_impl environment ~dependency:(Some dependency)


  let get_untracked_source_code_api environment = source_code_api_impl environment ~dependency:None
end

module UpdateResult = struct
  type t = {
    triggered_dependencies: SharedMemoryKeys.DependencyKey.RegisteredSet.t;
    invalidated_modules: Reference.t list;
    module_updates: ModuleTracker.IncrementalUpdate.t list;
  }

  let triggered_dependencies { triggered_dependencies; _ } = triggered_dependencies

  let invalidated_modules { invalidated_modules; _ } = invalidated_modules

  let module_updates { module_updates; _ } = module_updates
end

module FromReadOnlyUpstream = struct
  module RawSourceValue = struct
    type t = Parsing.ParseResult.t option

    let prefix = Hack_parallel.Std.Prefix.make ()

    let description = "Unprocessed source"

    let equal = Memory.equal_from_compare (Option.compare Parsing.ParseResult.compare)
  end

  module RawSources = struct
    include
      DependencyTrackedMemory.DependencyTrackedTableNoCache
        (SharedMemoryKeys.ReferenceKey)
        (SharedMemoryKeys.DependencyKey)
        (RawSourceValue)

    let add_parsed_source table ({ Source.module_path = { ModulePath.qualifier; _ }; _ } as source) =
      add table qualifier (Some (Result.Ok source))


    let add_unparsed_source
        table
        ({ Parsing.ParseResult.Error.module_path = { ModulePath.qualifier; _ }; _ } as error)
      =
      add table qualifier (Some (Result.Error error))


    let update_and_compute_dependencies table ~update ~scheduler qualifiers =
      let keys = KeySet.of_list qualifiers in
      SharedMemoryKeys.DependencyKey.Transaction.empty ~scheduler
      |> add_to_transaction table ~keys
      |> SharedMemoryKeys.DependencyKey.Transaction.execute ~update


    let remove_sources table qualifiers = KeySet.of_list qualifiers |> remove_batch table
  end

  type t = {
    module_tracker: ModuleTracker.ReadOnly.t;
    raw_sources: RawSources.t;
  }

  let create module_tracker = { module_tracker; raw_sources = RawSources.create () }

  let source_of_module_path ~ast_environment:{ raw_sources; module_tracker; _ } module_path =
    match
      let controls = ModuleTracker.ReadOnly.controls module_tracker in
      ModuleTracker.ReadOnly.code_of_module_path module_tracker module_path
      |> Parsing.parse_result_of_load_result ~controls module_path
    with
    | Ok source -> RawSources.add_parsed_source raw_sources source
    | Error parser_error -> RawSources.add_unparsed_source raw_sources parser_error


  let source_of_module_paths ~scheduler ~ast_environment module_paths =
    (* Note: We don't need SharedMemoryKeys.DependencyKey.Registry.collected_iter here; the
       collection handles *registering* dependencies but not detecting triggered dependencies, and
       this is the upstream-most part of the dependency DAG *)
    Scheduler.iter
      scheduler
      ~policy:
        (Scheduler.Policy.fixed_chunk_count
           ~minimum_chunks_per_worker:1
           ~minimum_chunk_size:100
           ~preferred_chunks_per_worker:5
           ())
      ~f:(List.iter ~f:(source_of_module_path ~ast_environment))
      ~inputs:module_paths


  module LazyRawSources = struct
    let load ~ast_environment:({ module_tracker; raw_sources; _ } as ast_environment) qualifier =
      match ModuleTracker.ReadOnly.module_path_of_qualifier module_tracker qualifier with
      | Some module_path -> source_of_module_path ~ast_environment module_path
      | None -> RawSources.add raw_sources qualifier None


    let get ~ast_environment:({ raw_sources; _ } as ast_environment) ?dependency qualifier =
      match RawSources.get raw_sources ?dependency qualifier with
      | Some result -> result
      | None -> (
          load ~ast_environment qualifier;
          match RawSources.get raw_sources ?dependency qualifier with
          | Some result -> result
          | None -> failwith "impossible - all code paths of `load` add some value")
  end

  (* This code is factored out so that in tests we can use it as a hack to free up memory *)
  let process_module_updates ~scheduler ({ raw_sources; _ } as ast_environment) module_updates =
    let changed_module_paths, removed_modules, new_implicits =
      let categorize = function
        | ModuleTracker.IncrementalUpdate.NewExplicit module_path -> `Fst module_path
        | ModuleTracker.IncrementalUpdate.Delete qualifier -> `Snd qualifier
        | ModuleTracker.IncrementalUpdate.NewImplicit qualifier -> `Trd qualifier
      in
      List.partition3_map module_updates ~f:categorize
    in
    (* We only want to eagerly reparse sources that have been cached. We have to also invalidate
       sources that are now deleted or changed from explicit to implicit. *)
    let reparse_module_paths =
      List.filter changed_module_paths ~f:(fun { ModulePath.qualifier; _ } ->
          RawSources.mem raw_sources qualifier)
    in
    let reparse_modules =
      reparse_module_paths |> List.map ~f:(fun { ModulePath.qualifier; _ } -> qualifier)
    in
    let modules_with_invalidated_raw_source =
      List.concat [removed_modules; new_implicits; reparse_modules]
    in
    (* Because type checking relies on AstEnvironment.UpdateResult.invalidated_modules to determine
       which files require re-type-checking, we have to include all new non-external modules, even
       though they don't really require us to update data in the push phase, or else they'll never
       be checked. *)
    let reparse_modules_union_in_project_modules =
      let fold qualifiers { ModulePath.qualifier; _ } = Set.add qualifiers qualifier in
      List.filter changed_module_paths ~f:ModulePath.should_type_check
      |> List.fold ~init:(Reference.Set.of_list reparse_modules) ~f:fold
      |> Set.to_list
    in
    let invalidated_modules_before_preprocessing =
      List.concat [removed_modules; new_implicits; reparse_modules_union_in_project_modules]
    in
    let update_raw_sources () =
      source_of_module_paths ~scheduler ~ast_environment reparse_module_paths
    in
    let _, raw_source_dependencies =
      PyreProfiling.track_duration_and_shared_memory
        "Parse Raw Sources"
        ~tags:["phase_name", "Parsing"]
        ~f:(fun _ ->
          RawSources.update_and_compute_dependencies
            raw_sources
            modules_with_invalidated_raw_source
            ~update:update_raw_sources
            ~scheduler)
    in
    let triggered_dependencies, invalidated_modules =
      let fold_key registered (triggered_dependencies, invalidated_modules) =
        (* WildcardImport dependencies should be handled internally by converting them
         * to invalidated_modules, which UnannotatedGlobalEnvironment will load. Other dependencies
         * should be forwarded to later environments. *)
        match SharedMemoryKeys.DependencyKey.get_key registered with
        | SharedMemoryKeys.WildcardImport qualifier ->
            triggered_dependencies, RawSources.KeySet.add qualifier invalidated_modules
        | _ ->
            ( SharedMemoryKeys.DependencyKey.RegisteredSet.add registered triggered_dependencies,
              invalidated_modules )
      in
      SharedMemoryKeys.DependencyKey.RegisteredSet.fold
        fold_key
        raw_source_dependencies
        ( SharedMemoryKeys.DependencyKey.RegisteredSet.empty,
          RawSources.KeySet.of_list invalidated_modules_before_preprocessing )
    in
    {
      UpdateResult.triggered_dependencies;
      invalidated_modules = RawSources.KeySet.elements invalidated_modules;
      module_updates;
    }


  let remove_sources { raw_sources; _ } = RawSources.remove_sources raw_sources

  let read_only ({ module_tracker; _ } as ast_environment) =
    { ReadOnly.module_tracker; raw_source_of_qualifier = LazyRawSources.get ~ast_environment }


  let controls { module_tracker; _ } = ModuleTracker.ReadOnly.controls module_tracker
end

module Base = struct
  type t = {
    module_tracker: ModuleTracker.t;
    from_read_only_upstream: FromReadOnlyUpstream.t;
  }

  let from_module_tracker module_tracker =
    {
      module_tracker;
      from_read_only_upstream =
        ModuleTracker.read_only module_tracker |> FromReadOnlyUpstream.create;
    }


  let create controls = ModuleTracker.create controls |> from_module_tracker

  let load controls =
    ModuleTracker.Serializer.from_stored_layouts ~controls () |> from_module_tracker


  let store { module_tracker; _ } = ModuleTracker.Serializer.store_layouts module_tracker

  let update ~scheduler { module_tracker; from_read_only_upstream } events =
    ModuleTracker.update module_tracker ~events
    |> FromReadOnlyUpstream.process_module_updates ~scheduler from_read_only_upstream


  let clear_memory_for_tests ~scheduler { module_tracker; from_read_only_upstream } =
    let _ =
      ModuleTracker.module_paths module_tracker
      |> List.map ~f:ModulePath.qualifier
      |> List.map ~f:(fun qualifier -> ModuleTracker.IncrementalUpdate.Delete qualifier)
      |> FromReadOnlyUpstream.process_module_updates ~scheduler from_read_only_upstream
    in
    ()


  let module_tracker { module_tracker; _ } = module_tracker

  let controls { from_read_only_upstream; _ } =
    FromReadOnlyUpstream.controls from_read_only_upstream


  let remove_sources { from_read_only_upstream; _ } qualifiers =
    FromReadOnlyUpstream.remove_sources from_read_only_upstream qualifiers


  let read_only { from_read_only_upstream; _ } =
    FromReadOnlyUpstream.read_only from_read_only_upstream
end

module Overlay = struct
  type t = {
    parent: ReadOnly.t;
    module_tracker: ModuleTracker.Overlay.t;
    from_read_only_upstream: FromReadOnlyUpstream.t;
  }

  let create parent =
    let module_tracker = ReadOnly.module_tracker parent |> ModuleTracker.Overlay.create in
    let from_read_only_upstream =
      ModuleTracker.Overlay.read_only module_tracker |> FromReadOnlyUpstream.create
    in
    { parent; module_tracker; from_read_only_upstream }


  let module_tracker { module_tracker; _ } = module_tracker

  let update_overlaid_code { module_tracker; from_read_only_upstream; _ } ~code_updates =
    (* No ownership filtering is needed, since raw sources correspond 1:1 with raw code. *)
    ModuleTracker.Overlay.update_overlaid_code module_tracker ~code_updates
    |> FromReadOnlyUpstream.process_module_updates
         ~scheduler:(Scheduler.create_sequential ())
         from_read_only_upstream


  let read_only { module_tracker = overlay_tracker; parent; from_read_only_upstream } =
    let this_read_only = FromReadOnlyUpstream.read_only from_read_only_upstream in
    let raw_source_of_qualifier ?dependency qualifier =
      if ModuleTracker.Overlay.owns_qualifier overlay_tracker qualifier then
        ReadOnly.raw_source_of_qualifier this_read_only ?dependency qualifier
      else
        ReadOnly.raw_source_of_qualifier parent ?dependency qualifier
    in
    { this_read_only with raw_source_of_qualifier }
end

include Base
