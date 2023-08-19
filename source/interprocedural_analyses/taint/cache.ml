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

open Core
open Pyre
module TypeEnvironment = Analysis.TypeEnvironment
module AstEnvironment = Analysis.AstEnvironment
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
  [@@deriving compare, show { with_path = false }]

  let show_pretty = function
    | TypeEnvironment -> "type environment"
    | InitialCallables -> "initial callables"
    | ClassHierarchyGraph -> "class hierarchy graph"
    | ClassIntervalGraph -> "class interval graph"
    | PreviousAnalysisSetup -> "previous analysis setup"
end

module EntryStatus = struct
  module Map = Caml.Map.Make (Entry)

  type t = Usage.t Map.t

  let empty = Map.empty

  let add ~name ~usage = Map.add name usage

  let to_json entry_status =
    let accumulate name usage so_far = (Entry.show name, `String (Usage.show usage)) :: so_far in
    `Assoc (Map.fold accumulate entry_status [])
end

module AnalysisSetup = struct
  type t = {
    maximum_overrides: int option;
    initial_models: Registry.t;
  }
end

module SharedMemoryStatus = struct
  type t =
    | InvalidByDecoratorChange
    | InvalidByCodeChange
    | TypeEnvironmentLoadError
    | LoadError
    | NotFound
    | Disabled
    | Loaded of {
        entry_status: EntryStatus.t;
        previous_analysis_setup: AnalysisSetup.t;
      }

  let to_json = function
    | InvalidByDecoratorChange -> `String "InvalidByDecoratorChange"
    | InvalidByCodeChange -> `String "InvalidByCodeChange"
    | TypeEnvironmentLoadError -> `String "TypeEnvironmentLoadError"
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

    let name = Entry.show_pretty entry
  end)

  let load_from_cache entry_status =
    match T.load () with
    | Ok previous_analysis_setup ->
        let entry_status =
          EntryStatus.add ~name:entry ~usage:SaveLoadSharedMemory.Usage.Used entry_status
        in
        SharedMemoryStatus.Loaded { entry_status; previous_analysis_setup }
    | Error error ->
        Log.info
          "Reset shared memory due to failing to load the previous analysis setup: %s"
          (SaveLoadSharedMemory.Usage.show error);
        Memory.reset_shared_memory ();
        SharedMemoryStatus.LoadError


  let save_to_cache = T.save
end

type t = {
  status: SharedMemoryStatus.t;
  save_cache: bool;
  scheduler: Scheduler.t;
  configuration: Configuration.Analysis.t;
}

let metadata_to_json { status; save_cache; _ } =
  `Assoc ["shared_memory_status", SharedMemoryStatus.to_json status; "save_cache", `Bool save_cache]


let get_save_directory ~configuration =
  PyrePath.create_relative
    ~root:(Configuration.Analysis.log_directory configuration)
    ~relative:".pysa_cache"


let get_shared_memory_save_path ~configuration =
  PyrePath.append (get_save_directory ~configuration) ~element:"sharedmem"


let ignore_result (_ : ('a, 'b) result) = ()

let initialize_shared_memory ~configuration =
  let path = get_shared_memory_save_path ~configuration in
  if not (PyrePath.file_exists path) then (
    Log.warning "Could not find a cached state.";
    Error SharedMemoryStatus.NotFound)
  else
    SaveLoadSharedMemory.exception_to_error
      ~error:SharedMemoryStatus.LoadError
      ~message:"loading cached state"
      ~f:(fun () ->
        Log.info
          "Loading cached state from `%s`"
          (PyrePath.absolute (get_save_directory ~configuration));
        let _ = Memory.get_heap_handle configuration in
        Memory.load_shared_memory ~path:(PyrePath.absolute path) ~configuration;
        Log.info "Cached state successfully loaded.";
        Ok ())


let check_decorator_invalidation ~decorator_configuration:current_configuration =
  let open Analysis in
  match DecoratorPreprocessing.get_configuration () with
  | Some cached_configuration
    when DecoratorPreprocessing.Configuration.equal cached_configuration current_configuration ->
      Ok ()
  | Some _ ->
      (* We need to invalidate the cache since decorator modes (e.g, `@IgnoreDecorator` and
         `@SkipDecoratorInlining`) are implemented as an AST preprocessing step. Any change could
         lead to a change in the AST, which could lead to a different type environment and so on. *)
      Log.warning "Changes to decorator modes detected, ignoring existing cache.";
      Error SharedMemoryStatus.InvalidByDecoratorChange
  | None ->
      Log.warning "Could not find cached decorator modes, ignoring existing cache.";
      Error SharedMemoryStatus.LoadError


let try_load ~scheduler ~configuration ~decorator_configuration ~enabled =
  let save_cache = enabled in
  if not enabled then
    { status = SharedMemoryStatus.Disabled; save_cache; scheduler; configuration }
  else
    let open Result in
    let status =
      match initialize_shared_memory ~configuration with
      | Ok () -> (
          match check_decorator_invalidation ~decorator_configuration with
          | Ok () ->
              let entry_status = EntryStatus.empty in
              PreviousAnalysisSetupSharedMemory.load_from_cache entry_status
          | Error error ->
              (* If there exist updates to certain decorators, it wastes memory and might not be
                 safe to leave the old type environment in the shared memory. *)
              Log.info "Reset shared memory";
              Memory.reset_shared_memory ();
              error)
      | Error error -> error
    in
    { status; save_cache; scheduler; configuration }


let load_type_environment ~scheduler ~configuration =
  let open Result in
  let controls = Analysis.EnvironmentControls.create configuration in
  Log.info "Determining if source files have changed since cache was created.";
  SaveLoadSharedMemory.exception_to_error
    ~error:SharedMemoryStatus.TypeEnvironmentLoadError
    ~message:"Loading type environment"
    ~f:(fun () -> Ok (TypeEnvironment.load controls))
  >>= fun type_environment ->
  let old_module_tracker =
    TypeEnvironment.ast_environment type_environment |> AstEnvironment.module_tracker
  in
  let new_module_tracker = Analysis.ModuleTracker.create controls in
  let changed_paths =
    let is_pysa_model path = String.is_suffix ~suffix:".pysa" (PyrePath.get_suffix_path path) in
    let is_taint_config path = String.is_suffix ~suffix:"taint.config" (PyrePath.absolute path) in
    Interprocedural.ChangedPaths.compute_locally_changed_paths
      ~scheduler
      ~configuration
      ~old_module_tracker
      ~new_module_tracker
    |> List.map ~f:ArtifactPath.raw
    |> List.filter ~f:(fun path -> not (is_pysa_model path || is_taint_config path))
  in
  match changed_paths with
  | [] -> Ok type_environment
  | _ ->
      Log.warning "Changes to source files detected, ignoring existing cache.";
      Error SharedMemoryStatus.InvalidByCodeChange


let save_type_environment ~scheduler ~configuration ~environment =
  SaveLoadSharedMemory.exception_to_error
    ~error:()
    ~message:"saving type environment to cache"
    ~f:(fun () ->
      Memory.SharedMemory.collect `aggressive;
      let module_tracker = TypeEnvironment.module_tracker environment in
      Interprocedural.ChangedPaths.save_current_paths ~scheduler ~configuration ~module_tracker;
      TypeEnvironment.store environment;
      Log.info "Saved type environment to cache shared memory.";
      Ok ())


let type_environment ({ status; save_cache; scheduler; configuration } as cache) f =
  let compute_and_save_environment () =
    let environment = f () in
    if save_cache then
      save_type_environment ~scheduler ~configuration ~environment |> ignore_result;
    environment
  in
  let environment, status =
    match status with
    | Loaded _ -> (
        match load_type_environment ~scheduler ~configuration with
        | Ok environment ->
            let status =
              SharedMemoryStatus.set_entry_usage
                ~entry:Entry.TypeEnvironment
                ~usage:Usage.Used
                status
            in
            environment, status
        | Error error_status ->
            Log.info "Reset shared memory due to failing to load the type environment";
            Memory.reset_shared_memory ();
            compute_and_save_environment (), error_status)
    | _ -> compute_and_save_environment (), status
  in
  environment, { cache with status }


let ensure_save_directory_exists ~configuration =
  let directory = PyrePath.absolute (get_save_directory ~configuration) in
  try Core_unix.mkdir directory with
  (* [mkdir] on MacOSX returns [EISDIR] instead of [EEXIST] if the directory already exists. *)
  | Core_unix.Unix_error ((EEXIST | EISDIR), _, _) -> ()
  | e -> raise e


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


let save ~maximum_overrides ~initial_models { save_cache; configuration; _ } =
  if save_cache then
    let () =
      PreviousAnalysisSetupSharedMemory.save_to_cache
        { AnalysisSetup.maximum_overrides; initial_models }
    in
    save_shared_memory ~configuration |> ignore


let set_entry_usage ~entry ~usage ({ status; _ } as cache) =
  let status = SharedMemoryStatus.set_entry_usage ~entry ~usage status in
  { cache with status }


module type CacheEntryType = sig
  type t

  val entry : Entry.t
end

module MakeCacheEntry (CacheEntry : CacheEntryType) = struct
  module T = SaveLoadSharedMemory.MakeSingleValue (struct
    type t = CacheEntry.t

    let name = Entry.show_pretty CacheEntry.entry
  end)

  let load_or_compute ({ save_cache; status; _ } as cache) f =
    let value, cache =
      match status with
      | Loaded _ ->
          let value, usage =
            match T.load () with
            | Ok value -> value, Usage.Used
            | Error error -> f (), error
          in
          let cache = set_entry_usage ~entry:CacheEntry.entry ~usage cache in
          value, cache
      | _ -> f (), cache
    in
    if save_cache then T.save value;
    value, cache
end

module ClassHierarchyGraphSharedMemory = MakeCacheEntry (struct
  type t = ClassHierarchyGraph.Heap.t

  let entry = Entry.ClassHierarchyGraph
end)

module InitialCallablesSharedMemory = MakeCacheEntry (struct
  type t = FetchCallables.t

  let entry = Entry.InitialCallables
end)

module ClassIntervalGraphSharedMemory = MakeCacheEntry (struct
  type t = ClassIntervalSetGraph.Heap.t

  let entry = Entry.ClassIntervalGraph
end)

let initial_callables = InitialCallablesSharedMemory.load_or_compute

let class_hierarchy_graph = ClassHierarchyGraphSharedMemory.load_or_compute

let class_interval_graph = ClassIntervalGraphSharedMemory.load_or_compute
