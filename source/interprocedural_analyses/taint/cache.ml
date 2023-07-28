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

module Entry = struct
  type t =
    | TypeEnvironment
    | InitialCallables
    | ClassHierarchyGraph
    | ClassIntervalGraph
  [@@deriving compare, show { with_path = false }]

  let show_pretty = function
    | TypeEnvironment -> "type environment"
    | InitialCallables -> "initial callables"
    | ClassHierarchyGraph -> "class hierarchy graph"
    | ClassIntervalGraph -> "class interval graph"
end

module EntryUsage = struct
  type error = LoadError [@@deriving show { with_path = false }]

  type t =
    | Used
    | Unused of error
  [@@deriving show { with_path = false }]
end

module EntryStatus = struct
  module Map = Caml.Map.Make (Entry)

  type t = EntryUsage.t Map.t

  let empty = Map.empty

  let add ~name ~usage = Map.add name usage

  let to_json entry_status =
    let accumulate name usage so_far =
      (Entry.show name, `String (EntryUsage.show usage)) :: so_far
    in
    `Assoc (Map.fold accumulate entry_status [])
end

module SharedMemoryStatus = struct
  type t =
    | InvalidByDecoratorChange
    | InvalidByCodeChange
    | TypeEnvironmentLoadError
    | LoadError
    | NotFound
    | Disabled
    | Loaded of EntryStatus.t

  let to_json = function
    | InvalidByDecoratorChange -> `String "InvalidByDecoratorChange"
    | InvalidByCodeChange -> `String "InvalidByCodeChange"
    | TypeEnvironmentLoadError -> `String "TypeEnvironmentLoadError"
    | LoadError -> `String "LoadError"
    | NotFound -> `String "NotFound"
    | Disabled -> `String "Disabled"
    | Loaded entry_status -> `Assoc ["Loaded", EntryStatus.to_json entry_status]
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


let exception_to_error ~error ~message ~f =
  try f () with
  | exception_ ->
      Log.error "Error %s:\n%s" message (Exn.to_string exception_);
      Error error


let ignore_result (_ : ('a, 'b) result) = ()

module type SingleValueSharedMemoryValueType = sig
  type t

  val entry : Entry.t
end

(* Support storing / loading a single OCaml value into / from the shared memory, for caching
   purposes. *)
module SingleValueSharedMemory (Value : SingleValueSharedMemoryValueType) = struct
  let value_entry_name = Entry.show_pretty Value.entry

  module T = Memory.Serializer (struct
    type t = Value.t

    module Serialized = struct
      type t = Value.t

      let prefix = Hack_parallel.Std.Prefix.make ()

      let description = value_entry_name
    end

    let serialize = Fn.id

    let deserialize = Fn.id
  end)

  let load () =
    exception_to_error
      ~error:(EntryUsage.Unused EntryUsage.LoadError)
      ~message:(Format.asprintf "Loading %s from cache" value_entry_name)
      ~f:(fun () ->
        Log.info "Loading %s from cache..." value_entry_name;
        let value = T.load () in
        Log.info "Loaded %s from cache." value_entry_name;
        Ok value)


  let save value =
    exception_to_error
      ~error:()
      ~message:(Format.asprintf "Saving %s to cache" value_entry_name)
      ~f:(fun () ->
        Memory.SharedMemory.collect `aggressive;
        T.store value;
        Log.info "Saved %s to cache." value_entry_name;
        Ok ())


  let load_or_compute ({ status; save_cache; _ } as cache) f =
    let value, status =
      match status with
      | Loaded entry_status ->
          let value, usage =
            match load () with
            | Ok value -> Some value, EntryUsage.Used
            | Error error -> None, error
          in
          let entry_status = EntryStatus.add ~name:Value.entry ~usage entry_status in
          value, SharedMemoryStatus.Loaded entry_status
      | _ -> None, status
    in
    let cache = { cache with status } in
    match value with
    | Some value -> value, cache
    | None ->
        let value = f () in
        if save_cache then
          save value |> ignore_result;
        value, cache
end

let initialize_shared_memory ~configuration =
  let path = get_shared_memory_save_path ~configuration in
  if not (PyrePath.file_exists path) then (
    Log.warning "Could not find a cached state.";
    Error SharedMemoryStatus.NotFound)
  else
    exception_to_error
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
  if not enabled then
    { status = SharedMemoryStatus.Disabled; save_cache = false; scheduler; configuration }
  else
    let open Result in
    let status =
      match initialize_shared_memory ~configuration with
      | Ok () -> (
          match check_decorator_invalidation ~decorator_configuration with
          | Ok () -> SharedMemoryStatus.Loaded EntryStatus.empty
          | Error error ->
              (* If there exist updates to certain decorators, it wastes memory and might not be
                 safe to leave the old type environment in the shared memory. *)
              Log.info "Reset shared memory";
              Memory.reset_shared_memory ();
              error)
      | Error error -> error
    in
    { status; save_cache = true; scheduler; configuration }


let load_type_environment ~scheduler ~configuration =
  let open Result in
  let controls = Analysis.EnvironmentControls.create configuration in
  Log.info "Determining if source files have changed since cache was created.";
  exception_to_error
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
  exception_to_error ~error:() ~message:"saving type environment to cache" ~f:(fun () ->
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
    | Loaded entry_status -> (
        match load_type_environment ~scheduler ~configuration with
        | Ok environment ->
            let entry_status =
              EntryStatus.add ~name:Entry.TypeEnvironment ~usage:EntryUsage.Used entry_status
            in
            environment, SharedMemoryStatus.Loaded entry_status
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
  exception_to_error ~error:() ~message:"saving cached state to file" ~f:(fun () ->
      let path = get_shared_memory_save_path ~configuration in
      Log.info "Saving shared memory state to cache file...";
      ensure_save_directory_exists ~configuration;
      Memory.SharedMemory.collect `aggressive;
      Memory.save_shared_memory ~path:(PyrePath.absolute path) ~configuration;
      Log.info "Saved shared memory state to cache file: `%s`" (PyrePath.absolute path);
      Ok ())


let save { save_cache; configuration; _ } =
  if save_cache then
    save_shared_memory ~configuration |> ignore


module ClassHierarchyGraphSharedMemory = SingleValueSharedMemory (struct
  type t = ClassHierarchyGraph.Heap.t

  let entry = Entry.ClassHierarchyGraph
end)

module InitialCallablesSharedMemory = SingleValueSharedMemory (struct
  type t = FetchCallables.t

  let entry = Entry.InitialCallables
end)

module ClassIntervalGraphSharedMemory = SingleValueSharedMemory (struct
  type t = ClassIntervalSetGraph.Heap.t

  let entry = Entry.ClassIntervalGraph
end)

let initial_callables = InitialCallablesSharedMemory.load_or_compute

let class_hierarchy_graph = ClassHierarchyGraphSharedMemory.load_or_compute

let class_interval_graph = ClassIntervalGraphSharedMemory.load_or_compute
