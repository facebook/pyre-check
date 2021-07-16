(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre
open Ast
open Service
module ModuleTracker = Analysis.ModuleTracker
module AstEnvironment = Analysis.AstEnvironment
module AnnotatedGlobalEnvironment = Analysis.AnnotatedGlobalEnvironment
module TypeEnvironment = Analysis.TypeEnvironment

module SymlinkTargetsToSources = Memory.Serializer (struct
  type t = Path.t String.Table.t

  module Serialized = struct
    type t = (string * Path.t) list

    let prefix = Prefix.make ()

    let description = "SymlinkTargetsToSources"

    let unmarshall value = Marshal.from_string value 0
  end

  let serialize = Hashtbl.to_alist

  let deserialize data = String.Table.of_alist_exn data
end)

module StoredConfiguration = Memory.Serializer (struct
  type t = Configuration.Analysis.t

  module Serialized = struct
    type t = Configuration.Analysis.t

    let prefix = Prefix.make ()

    let description = "Configuration"

    let unmarshall value = Marshal.from_string value 0
  end

  let serialize = Fn.id

  let deserialize = Fn.id
end)

module ServerErrors = Memory.Serializer (struct
  type t = Analysis.AnalysisError.t list Reference.Table.t

  module Serialized = struct
    type t = (Reference.t * Analysis.AnalysisError.t list) list

    let prefix = Prefix.make ()

    let description = "All errors"

    let unmarshall value = Marshal.from_string value 0
  end

  let serialize = Hashtbl.to_alist

  let deserialize data = Reference.Table.of_alist_exn data
end)

exception IncompatibleState of string

(* Return symlinks for files for a pyre project that are links to a separate project root. *)
let restore_symbolic_links ~changed_paths ~source_path ~get_old_link_path =
  let new_paths, removed_paths = List.partition_tf ~f:Path.file_exists changed_paths in
  (* We need to get the deleted paths from shared memory, as the version of the server launched from
     the saved state will have references to this files, whereas they won't be present in the new
     project root, meaning that we need to clean up the environment from them. *)
  let removed_paths =
    List.map removed_paths ~f:(fun path -> get_old_link_path path |> Option.value ~default:path)
  in
  (* Any member of new_paths might not have existed when the saved state was being created. *)
  let new_paths =
    let local_root_links =
      let file_filter file =
        Filename.check_suffix file ".py" || Filename.check_suffix file ".pyi"
      in
      List.concat_map source_path ~f:(fun root -> Path.list ~file_filter ~root ())
      |> fun links -> Path.build_symlink_map ~links
    in
    List.filter_map new_paths ~f:(Map.find local_root_links)
  in
  new_paths @ removed_paths


let load
    ~server_configuration:
      {
        Configuration.Server.configuration =
          {
            Configuration.Analysis.expected_version;
            project_root;
            source_path;
            configuration_file_hash;
            _;
          } as configuration;
        saved_state_action;
        _;
      }
    ~connections
  =
  Log.info "Initializing server from saved state...";
  let shared_memory_path, changed_paths =
    match saved_state_action with
    | Some (Load (LoadFromFiles parameters)) ->
        let { Configuration.Server.shared_memory_path; changed_files_path } = parameters in
        let files =
          let to_path serialized = Path.create_absolute serialized in
          changed_files_path
          >>| File.create
          >>= File.content
          >>| String.split_lines
          >>| List.map ~f:to_path
        in
        shared_memory_path, files
    | Some (Load (LoadFromProject { project_name; metadata })) -> (
        if Option.is_none expected_version then (
          Log.warning "An expected version must be passed in in order to load from saved states.";
          raise (IncompatibleState "version mismatch"));
        Log.log ~section:`Server "Loading from saved state project `%s`..." project_name;
        let target_path = Constants.Server.saved_state_path configuration in
        Log.log ~section:`Server "Loading saved state to `%s`..." (Path.absolute target_path);
        let loaded_state =
          Path.search_upwards
            ~target:".watchmanconfig"
            ~target_type:Path.FileType.File
            ~root:project_root
          >>= fun watchman_root ->
          FetchSavedState.load
            ~watchman_root
            ~project_name
            ~project_metadata:metadata
            ~configuration_file_hash
            ~version:(Option.value_exn expected_version)
            ~target_path
        in
        match loaded_state with
        | Some { FetchSavedState.saved_state_path; changed_files } ->
            (* Heuristic: If saved states with metadata are enabled, we need to deal with
               non-checked in generated code, so ignore watchman's response. *)
            saved_state_path, changed_files
        | None -> raise (IncompatibleState "unable to fetch state"))
    | _ -> raise (IncompatibleState "unexpected saved state parameters")
  in
  let scheduler = Scheduler.create ~configuration () in
  Memory.load_shared_memory ~path:(Path.absolute shared_memory_path) ~configuration;
  let module_tracker = ModuleTracker.SharedMemory.load () in
  let ast_environment = AstEnvironment.load module_tracker in
  let environment = AnnotatedGlobalEnvironment.create ast_environment |> TypeEnvironment.create in
  let old_configuration = StoredConfiguration.load () in
  if not (Configuration.Analysis.equal old_configuration configuration) then
    raise (IncompatibleState "configuration mismatch");
  let symlink_targets_to_sources = SymlinkTargetsToSources.load () in
  Analysis.SharedMemoryKeys.DependencyKey.Registry.load ();
  let changed_paths =
    match changed_paths with
    | Some changed_paths ->
        restore_symbolic_links
          ~changed_paths
          ~source_path:(List.map source_path ~f:SearchPath.to_path)
          ~get_old_link_path:(fun path ->
            Hashtbl.find symlink_targets_to_sources (Path.absolute path))
    | None ->
        (* If we're analyzing generated code, Watchman will be blind to any changes to said code. In
           order to be safe, compute hashes for all files that a fresh Pyre run would analyze. *)
        Log.info "Computing files that changed since the saved state was created.";
        Service.ChangedPaths.compute_locally_changed_paths
          ~scheduler
          ~configuration
          ~module_tracker
          ~ast_environment:(AstEnvironment.read_only ast_environment)
  in
  let errors = ServerErrors.load () in
  Log.info "Reanalyzing %d files and their dependencies." (List.length changed_paths);
  let state =
    {
      State.environment;
      errors;
      symlink_targets_to_sources;
      scheduler;
      last_request_time = Unix.time ();
      last_integrity_check = Unix.time ();
      connections;
      open_documents = Reference.Table.create ();
      server_uuid = None;
    }
  in
  let _ = IncrementalCheck.recheck_with_state ~state ~configuration changed_paths in
  state


let save
    ~configuration
    ~saved_state_path
    { State.errors; environment; symlink_targets_to_sources; _ }
  =
  Log.info "Saving server state to %s" saved_state_path;
  Memory.SharedMemory.collect `aggressive;
  TypeEnvironment.module_tracker environment |> ModuleTracker.SharedMemory.store;
  SymlinkTargetsToSources.store symlink_targets_to_sources;
  TypeEnvironment.ast_environment environment |> AstEnvironment.store;
  StoredConfiguration.store configuration;
  ServerErrors.store errors;
  Analysis.SharedMemoryKeys.DependencyKey.Registry.store ();
  Memory.save_shared_memory ~path:saved_state_path ~configuration
