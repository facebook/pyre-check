(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Cache: implements caching capabilities for the taint analysis. This is what
 * powers the `--use-cache` command line option. This is basically implemented
 * by writing the shared memory into a file and restoring it later.
 *)

module CamlUnix = Unix
open Core
open Pyre
module PyrePysaApi = Interprocedural.PyrePysaApi
module PyrePysaLogic = Analysis.PyrePysaLogic
module FetchCallables = Interprocedural.FetchCallables
module ClassHierarchyGraph = Interprocedural.ClassHierarchyGraph
module ClassIntervalSetGraph = Interprocedural.ClassIntervalSetGraph
module SaveLoadSharedMemory = Interprocedural.SaveLoadSharedMemory
module Usage = SaveLoadSharedMemory.Usage

module Entry = struct
  type t =
    | TypeEnvironment
    | InitialCallables
    | ClassHierarchyGraph
    | ClassIntervalGraph
    | PreviousAnalysisSetup
    | OverrideGraph
    | CallGraph
    | GlobalConstants
  [@@deriving compare, show { with_path = false }]

  let show_pretty = function
    | TypeEnvironment -> "type environment"
    | InitialCallables -> "initial callables"
    | ClassHierarchyGraph -> "class hierarchy graph"
    | ClassIntervalGraph -> "class interval graph"
    | PreviousAnalysisSetup -> "previous analysis setup"
    | OverrideGraph -> "override graph"
    | CallGraph -> "call graph"
    | GlobalConstants -> "global constants"
end

module EntryStatus = struct
  module Map = Stdlib.Map.Make (Entry)

  type t = Usage.t Map.t

  let empty = Map.empty

  let add ~name ~usage = Map.add name usage

  let get = Map.find

  let to_json entry_status =
    let accumulate name usage so_far = (Entry.show name, `String (Usage.show usage)) :: so_far in
    `Assoc (Map.fold accumulate entry_status [])
end

module AnalysisSetup = struct
  type t = {
    maximum_overrides: int option;
    analyze_all_overrides_targets: Target.Set.t;
    attribute_targets: Target.Set.t;
    skip_type_checking_callables: Ast.Reference.SerializableSet.t;
    skip_analysis_targets: Target.Set.t;
    skip_overrides_targets: Ast.Reference.SerializableSet.t;
    skipped_overrides: Interprocedural.OverrideGraph.skipped_overrides;
    initial_callables: FetchCallables.t;
    initial_models: SharedModels.t option;
    whole_program_call_graph: Interprocedural.CallGraph.WholeProgramCallGraph.t;
  }
end

module SharedMemoryStatus = struct
  type t =
    | InvalidByDecoratorChange
    | InvalidByCodeChange
    | InvalidBySkipAnalysisChange
    | TypeEnvironmentLoadError
    | LoadError
    | UnsupportedForPyrefly
    | NotFound
    | Disabled
    | Loaded of {
        entry_status: EntryStatus.t;
        previous_analysis_setup: AnalysisSetup.t;
      }

  let to_json = function
    | InvalidByDecoratorChange -> `String "InvalidByDecoratorChange"
    | InvalidByCodeChange -> `String "InvalidByCodeChange"
    | InvalidBySkipAnalysisChange -> `String "InvalidBySkipAnalysisChange"
    | TypeEnvironmentLoadError -> `String "TypeEnvironmentLoadError"
    | UnsupportedForPyrefly -> `String "UnsupportedForPyrefly"
    | LoadError -> `String "LoadError"
    | NotFound -> `String "NotFound"
    | Disabled -> `String "Disabled"
    | Loaded { entry_status; _ } -> `Assoc ["Loaded", EntryStatus.to_json entry_status]


  let set_entry_usage ~entry ~usage status =
    match status with
    | Loaded ({ entry_status; _ } as loaded) ->
        let entry_status = EntryStatus.add ~name:entry ~usage entry_status in
        Loaded { loaded with entry_status }
    | _ -> status
end

module PreviousAnalysisSetupSharedMemory = struct
  let entry = Entry.PreviousAnalysisSetup

  module T = SaveLoadSharedMemory.MakeSingleValue (struct
    type t = AnalysisSetup.t

    let prefix = Hack_parallel.Std.Prefix.make ()

    let name = Entry.show_pretty entry
  end)

  let load_from_cache entry_status =
    match T.load_from_cache () with
    | Ok previous_analysis_setup ->
        let entry_status =
          EntryStatus.add ~name:entry ~usage:SaveLoadSharedMemory.Usage.Used entry_status
        in
        SharedMemoryStatus.Loaded { entry_status; previous_analysis_setup }
    | Error error ->
        Log.info
          "Failed to load the previous analysis setup: %s"
          (SaveLoadSharedMemory.Usage.show error);
        SharedMemoryStatus.LoadError


  let save_to_cache = T.save_to_cache
end

module ChangedFiles = struct
  let show_files files = files |> List.map ~f:PyrePath.show |> String.concat ~sep:", "

  let should_invalidate_if_changed path =
    let is_pysa_model path = String.is_suffix ~suffix:".pysa" (PyrePath.get_suffix_path path) in
    let is_taint_config path = String.is_suffix ~suffix:"taint.config" (PyrePath.absolute path) in
    not (is_pysa_model path || is_taint_config path)


  let from_pyre_read_write_api ~scheduler ~scheduler_policies pyre_read_write_api =
    Interprocedural.ChangedPaths.compute_locally_changed_paths
      ~scheduler
      ~scheduler_policies
      ~configuration:(PyrePysaApi.ReadWrite.configuration pyre_read_write_api)
      ~old_module_paths:(PyrePysaApi.ReadWrite.module_paths pyre_read_write_api)
      ~new_module_paths:(PyrePysaApi.ReadWrite.module_paths_from_disk pyre_read_write_api)
    |> List.map ~f:ArtifactPath.raw
    |> List.filter ~f:should_invalidate_if_changed
end

type t = {
  status: SharedMemoryStatus.t;
  save_cache: bool;
  scheduler: Scheduler.t;
  scheduler_policies: Configuration.SchedulerPolicies.t;
  configuration: Configuration.Analysis.t;
  pyrefly_results: PyrePath.t option;
  no_file_changes_reported_by_watchman: bool;
}

exception BuildCacheOnly

let metadata_to_json { status; save_cache; _ } =
  `Assoc ["shared_memory_status", SharedMemoryStatus.to_json status; "save_cache", `Bool save_cache]


let get_save_directory ~configuration =
  PyrePath.create_relative
    ~root:(Configuration.Analysis.log_directory configuration)
    ~relative:".pysa_cache"


let get_shared_memory_save_path ~configuration =
  PyrePath.append (get_save_directory ~configuration) ~element:"sharedmem"


let ignore_result (_ : ('a, 'b) result) = ()

let ensure_save_directory_exists ~configuration =
  let directory = PyrePath.absolute (get_save_directory ~configuration) in
  try CamlUnix.mkdir directory 0o777 with
  (* [mkdir] on MacOSX returns [EISDIR] instead of [EEXIST] if the directory already exists. *)
  | CamlUnix.Unix_error ((EEXIST | EISDIR), _, _) -> ()
  | e -> raise e


module FileLoading = struct
  type t =
    | LocalCache of { cache_path: PyrePath.t }
    | SavedState of {
        cache_path: PyrePath.t;
        file_changes_reported_by_watchman: PyrePath.t list;
      }

  module SavedState = struct
    let fetch_from_project
        ~configuration
        ~saved_state:
          {
            Configuration.StaticAnalysis.SavedState.watchman_root;
            cache_critical_files;
            project_name;
            preset;
          }
      =
      let open Lwt.Infix in
      Lwt.catch
        (fun () ->
          Watchman.Raw.create_exn ()
          >>= fun watchman_raw ->
          Watchman.Raw.with_connection watchman_raw ~f:(fun watchman_connection ->
              match watchman_root, project_name with
              | None, _ -> Lwt.return (Result.Error "Missing watchman root")
              | _, None -> Lwt.return (Result.Error "Missing saved state project name")
              | Some watchman_root, Some project_name ->
                  let saved_state_storage_location = get_shared_memory_save_path ~configuration in
                  let () = ensure_save_directory_exists ~configuration in
                  let watchman_filter =
                    {
                      Watchman.Filter.base_names = [];
                      whole_names = cache_critical_files;
                      suffixes = [];
                    }
                  in
                  Saved_state.Execution.query_and_fetch_exn
                    {
                      Saved_state.Execution.Setting.watchman_root =
                        PyrePath.create_absolute watchman_root;
                      watchman_filter;
                      watchman_connection;
                      project_name;
                      project_metadata =
                        preset
                        (* The encoding of information into `project_metadata` needs to be the same
                           between the usage of the saved state (i.e., here) and the creation of
                           saved states (i.e., in Sandcastle), so that we can locate the saved
                           states that are created with the same information. *);
                      critical_files = [];
                      target = saved_state_storage_location;
                    }
                  >>= fun fetched -> Lwt.return (Result.Ok fetched)))
        (fun exn ->
          let message =
            match exn with
            | Watchman.ConnectionError message
            | Watchman.QueryError message
            | Saved_state.Execution.QueryFailure message ->
                message
            | _ -> Exn.to_string exn
          in
          Lwt.return (Result.Error message))


    let load ~saved_state ~configuration =
      let open Lwt.Infix in
      fetch_from_project ~saved_state ~configuration
      >>= fun fetched ->
      match fetched with
      | Result.Error message ->
          Log.warning "Could not fetch a saved state: %s" message;
          Lwt.return_none
      | Result.Ok { Saved_state.Execution.Fetched.path; changed_files } ->
          let () = Log.info "Fetched saved state" in
          let () =
            if List.is_empty changed_files then
              Log.info "Watchman detected no file changes"
            else
              Log.info "Watchman detected file changes: %s" (ChangedFiles.show_files changed_files)
          in
          Lwt.return_some
            (SavedState { cache_path = path; file_changes_reported_by_watchman = changed_files })
  end

  let locate_cache_and_detect_file_changes ~saved_state ~configuration =
    let local_cache_path = get_shared_memory_save_path ~configuration in
    if PyrePath.file_exists local_cache_path then
      let () = Log.info "Using local cache file `%s`" (PyrePath.absolute local_cache_path) in
      Some (LocalCache { cache_path = local_cache_path })
    else
      let () = Log.info "Could not find local cache. Fetching cache from saved states." in
      SavedState.load ~saved_state ~configuration |> Lwt_main.run
end

let initialize_shared_memory ~path ~configuration =
  match path with
  | Some path when PyrePath.file_exists path ->
      SaveLoadSharedMemory.exception_to_error
        ~error:SharedMemoryStatus.LoadError
        ~message:"loading cached state"
        ~f:(fun () ->
          Log.info "Loading cached state from `%s`" (PyrePath.absolute path);
          Memory.initialize configuration;
          Memory.load_shared_memory ~path:(PyrePath.absolute path) ~configuration;
          Log.info "Cached state successfully loaded.";
          Ok ())
  | _ -> Error SharedMemoryStatus.NotFound


let check_decorator_invalidation ~decorator_configuration:current_configuration =
  match PyrePysaLogic.DecoratorPreprocessing.get_configuration () with
  | Some cached_configuration
    when PyrePysaLogic.DecoratorPreprocessing.Configuration.equal
           cached_configuration
           current_configuration ->
      Ok ()
  | Some _ ->
      (* We need to invalidate the cache since decorator modes (e.g, `@IgnoreDecorator`) are
         implemented as an AST preprocessing step. Any change could lead to a change in the AST,
         which could lead to a different type environment and so on. *)
      Log.warning "Changes to decorator modes detected, ignoring existing cache.";
      Error SharedMemoryStatus.InvalidByDecoratorChange
  | None ->
      Log.warning "Could not find cached decorator modes, ignoring existing cache.";
      Error SharedMemoryStatus.LoadError


let check_skip_type_checking_invalidation ~skip_type_checking_callables = function
  | SharedMemoryStatus.Loaded
      {
        previous_analysis_setup =
          { AnalysisSetup.skip_type_checking_callables = previous_skip_type_checking_callables; _ };
        _;
      }
    when not
           (Ast.Reference.SerializableSet.equal
              skip_type_checking_callables
              previous_skip_type_checking_callables) ->
      Log.info "Changes to SkipAnalysis modes detected. Ignoring existing cache.";
      SharedMemoryStatus.InvalidBySkipAnalysisChange
  | status -> status


let try_load
    ~scheduler
    ~scheduler_policies
    ~saved_state
    ~configuration
    ~pyrefly_results
    ~decorator_configuration
    ~skip_type_checking_callables
    ~enabled
  =
  let save_cache = enabled in
  if not enabled then
    {
      status = SharedMemoryStatus.Disabled;
      save_cache;
      scheduler;
      scheduler_policies;
      configuration;
      pyrefly_results;
      no_file_changes_reported_by_watchman = false;
    }
  else if Option.is_some pyrefly_results then
    let () = Log.info "Caching is not supported when using pyrefly results" in
    {
      status = SharedMemoryStatus.UnsupportedForPyrefly;
      save_cache = false;
      scheduler;
      scheduler_policies;
      configuration;
      pyrefly_results;
      no_file_changes_reported_by_watchman = false;
    }
  else
    let open Result in
    let cache_path, no_file_changes_reported_by_watchman, must_invalidate_cache =
      match FileLoading.locate_cache_and_detect_file_changes ~saved_state ~configuration with
      | Some (FileLoading.SavedState { cache_path; file_changes_reported_by_watchman }) ->
          let exist_file_changes_reported_by_watchman =
            not (List.is_empty file_changes_reported_by_watchman)
          in
          ( Some cache_path,
            not exist_file_changes_reported_by_watchman,
            exist_file_changes_reported_by_watchman )
      | Some (FileLoading.LocalCache { cache_path }) -> Some cache_path, false, false
      | None -> None, false, false
    in
    let status =
      if must_invalidate_cache then
        let () = Log.info "Detected source code changes. Ignoring existing cache." in
        SharedMemoryStatus.InvalidByCodeChange
      else
        match initialize_shared_memory ~path:cache_path ~configuration with
        | Error error -> error
        | Ok () ->
            let status =
              check_decorator_invalidation ~decorator_configuration
              >>| fun () ->
              PreviousAnalysisSetupSharedMemory.load_from_cache EntryStatus.empty
              |> check_skip_type_checking_invalidation ~skip_type_checking_callables
            in
            let status =
              match status with
              | Ok status -> status
              | Error status -> status
            in
            (* Remove initial models from shared memory. *)
            let status =
              match status with
              | Loaded
                  {
                    previous_analysis_setup =
                      { initial_models = Some initial_models; _ } as previous_analysis_setup;
                    entry_status;
                  } ->
                  let () = SharedModels.cleanup ~clean_old:false initial_models in
                  SharedMemoryStatus.Loaded
                    {
                      previous_analysis_setup =
                        { previous_analysis_setup with initial_models = None };
                      entry_status;
                    }
              | _ -> status
            in

            let () =
              match status with
              | Loaded _ -> ()
              | _ ->
                  Log.info "Resetting shared memory";
                  Memory.reset_shared_memory ()
            in
            status
    in
    {
      status;
      save_cache;
      scheduler;
      scheduler_policies;
      configuration;
      pyrefly_results;
      no_file_changes_reported_by_watchman;
    }


let load_pyre_read_write_api ~configuration ~pyrefly_results =
  let open Result in
  SaveLoadSharedMemory.exception_to_error
    ~error:SharedMemoryStatus.TypeEnvironmentLoadError
    ~message:"Loading type environment"
    ~f:(fun () -> Ok (PyrePysaApi.ReadWrite.load_from_cache ~configuration ~pyrefly_results))


let save_pyre_read_write_api ~scheduler ~scheduler_policies pyre_read_write_api =
  SaveLoadSharedMemory.exception_to_error
    ~error:()
    ~message:"saving type environment to cache"
    ~f:(fun () ->
      Memory.SharedMemory.collect `aggressive;
      Interprocedural.ChangedPaths.save_current_paths
        ~scheduler
        ~scheduler_policies
        ~configuration:(PyrePysaApi.ReadWrite.configuration pyre_read_write_api)
        ~module_paths:(PyrePysaApi.ReadWrite.module_paths pyre_read_write_api);
      PyrePysaApi.ReadWrite.save pyre_read_write_api;
      Log.info "Saved type environment to cache shared memory.";
      Ok ())


let pyre_read_write_api
    ({
       status;
       save_cache;
       scheduler;
       scheduler_policies;
       configuration;
       pyrefly_results;
       no_file_changes_reported_by_watchman;
     } as cache)
    f
  =
  let no_file_changes_detected_by_comparing_type_environments old_pyre_read_write_api =
    Log.info "Determining if source files have changed since cache was created.";
    let changed_files =
      ChangedFiles.from_pyre_read_write_api ~scheduler ~scheduler_policies old_pyre_read_write_api
    in
    if List.is_empty changed_files then
      let () = Log.info "No source file change is detected." in
      true
    else
      let () =
        Log.info "Changes to source files detected: %s" (ChangedFiles.show_files changed_files)
      in
      false
  in
  let compute_and_save_pyre_read_write_api () =
    let pyre_read_write_api = f () in
    if save_cache then
      save_pyre_read_write_api ~scheduler ~scheduler_policies pyre_read_write_api |> ignore_result;
    pyre_read_write_api
  in
  let environment, status =
    match status with
    | Loaded _ -> (
        match load_pyre_read_write_api ~configuration ~pyrefly_results with
        | Ok environment ->
            if
              no_file_changes_reported_by_watchman
              || no_file_changes_detected_by_comparing_type_environments environment
            then
              let status =
                SharedMemoryStatus.set_entry_usage
                  ~entry:Entry.TypeEnvironment
                  ~usage:Usage.Used
                  status
              in
              environment, status
            else
              let () = Log.info "Resetting shared memory due to source file changes" in
              Memory.reset_shared_memory ();
              compute_and_save_pyre_read_write_api (), SharedMemoryStatus.InvalidByCodeChange
        | Error error_status ->
            Log.info "Resetting shared memory due to failing to load the type environment";
            Memory.reset_shared_memory ();
            compute_and_save_pyre_read_write_api (), error_status)
    | _ -> compute_and_save_pyre_read_write_api (), status
  in
  environment, { cache with status }


let save_shared_memory ~configuration =
  SaveLoadSharedMemory.exception_to_error
    ~error:()
    ~message:"saving cached state to file"
    ~f:(fun () ->
      let path = get_shared_memory_save_path ~configuration in
      Log.info "Saving shared memory state to cache file...";
      ensure_save_directory_exists ~configuration;
      Memory.SharedMemory.collect `aggressive;
      Memory.save_shared_memory ~path:(PyrePath.absolute path) ~configuration;
      Log.info "Saved shared memory state to cache file: `%s`" (PyrePath.absolute path);
      Ok ())


let save
    ~maximum_overrides
    ~analyze_all_overrides_targets
    ~attribute_targets
    ~skip_type_checking_callables
    ~skip_analysis_targets
    ~skip_overrides_targets
    ~skipped_overrides
    ~override_graph_shared_memory
    ~initial_callables
    ~initial_models
    ~call_graph_shared_memory
    ~whole_program_call_graph
    ~global_constants
    { save_cache; configuration; _ }
  =
  if save_cache then
    let () =
      Interprocedural.OverrideGraph.SharedMemory.save_to_cache override_graph_shared_memory
    in
    let () = Interprocedural.CallGraph.SharedMemory.save_to_cache call_graph_shared_memory in
    let () = Interprocedural.GlobalConstants.SharedMemory.save_to_cache global_constants in
    let () =
      PreviousAnalysisSetupSharedMemory.save_to_cache
        {
          AnalysisSetup.maximum_overrides;
          analyze_all_overrides_targets;
          attribute_targets;
          skip_type_checking_callables;
          skip_analysis_targets;
          skip_overrides_targets;
          skipped_overrides;
          initial_callables;
          initial_models = Some initial_models;
          whole_program_call_graph;
        }
    in
    save_shared_memory ~configuration |> ignore


let set_entry_usage ~entry ~usage ({ status; _ } as cache) =
  let status = SharedMemoryStatus.set_entry_usage ~entry ~usage status in
  { cache with status }


module type CacheEntryType = sig
  type t

  val entry : Entry.t

  val prefix : Hack_parallel.Std.Prefix.t
end

module MakeCacheEntry (CacheEntry : CacheEntryType) = struct
  module T = SaveLoadSharedMemory.MakeSingleValue (struct
    type t = CacheEntry.t

    let prefix = CacheEntry.prefix

    let name = Entry.show_pretty CacheEntry.entry
  end)

  let load_or_compute ({ save_cache; status; _ } as cache) f =
    let value, cache =
      match status with
      | Loaded _ ->
          let value, usage =
            match T.load_from_cache () with
            | Ok value -> value, Usage.Used
            | Error error -> f (), error
          in
          let cache = set_entry_usage ~entry:CacheEntry.entry ~usage cache in
          value, cache
      | _ -> f (), cache
    in
    if save_cache then T.save_to_cache value;
    value, cache
end

module ClassHierarchyGraphSharedMemory = MakeCacheEntry (struct
  type t = ClassHierarchyGraph.Heap.t

  let entry = Entry.ClassHierarchyGraph

  let prefix = Hack_parallel.Std.Prefix.make ()
end)

module ClassIntervalGraphSharedMemory = MakeCacheEntry (struct
  type t = ClassIntervalSetGraph.Heap.t

  let entry = Entry.ClassIntervalGraph

  let prefix = Hack_parallel.Std.Prefix.make ()
end)

let initial_callables ({ status; _ } as cache) compute_value =
  match status with
  | Loaded
      ({ entry_status; previous_analysis_setup = { AnalysisSetup.initial_callables; _ }; _ } as
      loaded) ->
      let entry_status =
        EntryStatus.add
          ~name:Entry.InitialCallables
          ~usage:SaveLoadSharedMemory.Usage.Used
          entry_status
      in
      let status = SharedMemoryStatus.Loaded { loaded with entry_status } in
      initial_callables, { cache with status }
  | _ -> compute_value (), cache


let class_hierarchy_graph = ClassHierarchyGraphSharedMemory.load_or_compute

let class_interval_graph = ClassIntervalGraphSharedMemory.load_or_compute

module OverrideGraphSharedMemory = struct
  let is_reusable
      ~skip_overrides_targets
      ~maximum_overrides
      ~analyze_all_overrides_targets
      {
        AnalysisSetup.maximum_overrides = previous_maximum_overrides;
        analyze_all_overrides_targets = previous_analyze_all_overrides_targets;
        skip_overrides_targets = previous_skip_overrides_targets;
        _;
      }
    =
    let no_change_in_maximum_overrides =
      Option.equal Int.equal maximum_overrides previous_maximum_overrides
    in
    let no_change_in_analyze_all_overrides_targets =
      Target.Set.equal previous_analyze_all_overrides_targets analyze_all_overrides_targets
    in
    let no_change_in_skip_overrides =
      Ast.Reference.SerializableSet.equal previous_skip_overrides_targets skip_overrides_targets
    in
    no_change_in_skip_overrides
    && no_change_in_analyze_all_overrides_targets
    && no_change_in_maximum_overrides


  let load_or_compute_if_unloadable
      ~previous_analysis_setup:{ AnalysisSetup.skipped_overrides; _ }
      ~skip_overrides_targets
      ~maximum_overrides
      ~analyze_all_overrides_targets
      entry_status
      compute_value
    =
    match Interprocedural.OverrideGraph.SharedMemory.load_from_cache () with
    | Ok override_graph_shared_memory ->
        let override_graph_heap =
          Interprocedural.OverrideGraph.SharedMemory.to_heap override_graph_shared_memory
        in
        ( {
            Interprocedural.OverrideGraph.override_graph_heap;
            override_graph_shared_memory;
            skipped_overrides;
          },
          EntryStatus.add
            ~name:Entry.OverrideGraph
            ~usage:SaveLoadSharedMemory.Usage.Used
            entry_status )
    | Error error ->
        ( compute_value ~skip_overrides_targets ~maximum_overrides ~analyze_all_overrides_targets (),
          EntryStatus.add ~name:Entry.OverrideGraph ~usage:error entry_status )


  let remove_previous () =
    match Interprocedural.OverrideGraph.SharedMemory.load_from_cache () with
    | Ok override_graph_shared_memory ->
        Log.info "Removing the previous override graph.";
        Interprocedural.OverrideGraph.SharedMemory.cleanup override_graph_shared_memory
    | Error _ -> Log.warning "Failed to remove the previous override graph."


  let load_or_compute_if_stale_or_unloadable
      ~skip_overrides_targets
      ~maximum_overrides
      ~analyze_all_overrides_targets
      ({ status; _ } as cache)
      compute_value
    =
    match status with
    | Loaded ({ previous_analysis_setup; entry_status } as loaded) ->
        let reusable =
          is_reusable
            ~skip_overrides_targets
            ~analyze_all_overrides_targets
            ~maximum_overrides
            previous_analysis_setup
        in
        if reusable then
          let () = Log.info "Trying to reuse the override graph from the cache." in
          let value, entry_status =
            load_or_compute_if_unloadable
              ~previous_analysis_setup
              ~skip_overrides_targets
              ~maximum_overrides
              ~analyze_all_overrides_targets
              entry_status
              compute_value
          in
          let status = SharedMemoryStatus.Loaded { loaded with entry_status } in
          value, { cache with status }
        else
          let () = Log.info "Override graph from the cache is stale." in
          let () = remove_previous () in
          let cache =
            set_entry_usage
              ~entry:Entry.OverrideGraph
              ~usage:(SaveLoadSharedMemory.Usage.Unused Stale)
              cache
          in
          ( compute_value
              ~skip_overrides_targets
              ~maximum_overrides
              ~analyze_all_overrides_targets
              (),
            cache )
    | _ ->
        ( compute_value ~skip_overrides_targets ~maximum_overrides ~analyze_all_overrides_targets (),
          cache )
end

let override_graph = OverrideGraphSharedMemory.load_or_compute_if_stale_or_unloadable

module CallGraphSharedMemory = struct
  let compare_attribute_targets ~previous_attribute_targets ~attribute_targets =
    let is_equal = Target.Set.equal attribute_targets previous_attribute_targets in
    if not is_equal then
      Log.info "Detected changes in the attribute targets";
    is_equal


  let compare_skip_analysis_targets ~previous_skip_analysis_targets ~skip_analysis_targets =
    let is_equal = Target.HashSet.equal skip_analysis_targets previous_skip_analysis_targets in
    if not is_equal then
      Log.info "Detected changes in the skip analysis targets";
    is_equal


  let is_reusable
      ~attribute_targets
      ~skip_analysis_targets
      entry_status
      {
        AnalysisSetup.attribute_targets = previous_attribute_targets;
        skip_analysis_targets = previous_skip_analysis_targets;
        _;
      }
    =
    (* Technically we should also compare the changes in the definitions, but such comparison is
       unnecessary because we invalidate the cache when there is a source code change -- which
       implies no change in the definitions. *)
    [%compare.equal: SaveLoadSharedMemory.Usage.t]
      (EntryStatus.get Entry.OverrideGraph entry_status)
      SaveLoadSharedMemory.Usage.Used
    && compare_attribute_targets ~previous_attribute_targets ~attribute_targets
    && compare_skip_analysis_targets
         ~previous_skip_analysis_targets:
           (previous_skip_analysis_targets |> Target.Set.elements |> Target.HashSet.of_list)
         ~skip_analysis_targets


  let remove_previous () =
    match Interprocedural.CallGraph.SharedMemory.load_from_cache () with
    | Ok call_graph_shared_memory ->
        Log.info "Removing the previous call graph.";
        Interprocedural.CallGraph.SharedMemory.cleanup call_graph_shared_memory
    | Error _ -> Log.warning "Failed to remove the previous call graph."


  let load_or_compute_if_not_loadable
      ~attribute_targets
      ~skip_analysis_targets
      ~definitions
      ~previous_analysis_setup:{ AnalysisSetup.whole_program_call_graph; _ }
      compute_value
    =
    match Interprocedural.CallGraph.SharedMemory.load_from_cache () with
    | Ok define_call_graphs ->
        ( { Interprocedural.CallGraph.SharedMemory.whole_program_call_graph; define_call_graphs },
          SaveLoadSharedMemory.Usage.Used )
    | Error error -> compute_value ~attribute_targets ~skip_analysis_targets ~definitions (), error


  let load_or_recompute
      ~attribute_targets
      ~skip_analysis_targets
      ~definitions
      ({ status; _ } as cache)
      compute_value
    =
    match status with
    | Loaded ({ previous_analysis_setup; entry_status } as loaded) ->
        let reusable =
          is_reusable ~attribute_targets ~skip_analysis_targets entry_status previous_analysis_setup
        in
        if reusable then
          let () = Log.info "Trying to reuse the call graph from the cache." in
          let value, usage =
            load_or_compute_if_not_loadable
              ~attribute_targets
              ~skip_analysis_targets
              ~definitions
              ~previous_analysis_setup
              compute_value
          in
          let entry_status = EntryStatus.add ~name:Entry.CallGraph ~usage entry_status in
          let status = SharedMemoryStatus.Loaded { loaded with entry_status } in
          value, { cache with status }
        else
          let () = Log.info "Call graph from the cache is stale." in
          let () = remove_previous () in
          let cache =
            set_entry_usage
              ~entry:Entry.CallGraph
              ~usage:(SaveLoadSharedMemory.Usage.Unused Stale)
              cache
          in
          compute_value ~attribute_targets ~skip_analysis_targets ~definitions (), cache
    | _ -> compute_value ~attribute_targets ~skip_analysis_targets ~definitions (), cache
end

let call_graph = CallGraphSharedMemory.load_or_recompute

module GlobalConstantsSharedMemory = struct
  let load_or_compute_if_not_loadable compute_value =
    match Interprocedural.GlobalConstants.SharedMemory.load_from_cache () with
    | Ok global_constants -> global_constants, SaveLoadSharedMemory.Usage.Used
    | Error error -> compute_value (), error


  let load_or_recompute_if_stale_or_not_loadable ({ status; _ } as cache) compute_value =
    match status with
    | Loaded ({ entry_status; _ } as loaded) ->
        let () = Log.info "Trying to reuse the global constants from the cache." in
        let value, usage = load_or_compute_if_not_loadable compute_value in
        let entry_status = EntryStatus.add ~name:Entry.GlobalConstants ~usage entry_status in
        let status = SharedMemoryStatus.Loaded { loaded with entry_status } in
        value, { cache with status }
    | _ -> compute_value (), cache
end

let global_constants = GlobalConstantsSharedMemory.load_or_recompute_if_stale_or_not_loadable
