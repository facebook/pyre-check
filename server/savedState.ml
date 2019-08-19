(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre
open Ast
open Service
module ModuleTracker = Analysis.ModuleTracker
module AstEnvironment = Analysis.AstEnvironment

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
  type t = Analysis.Error.t list Reference.Table.t

  module Serialized = struct
    type t = (Reference.t * Analysis.Error.t list) list

    let prefix = Prefix.make ()

    let description = "All errors"

    let unmarshall value = Marshal.from_string value 0
  end

  let serialize = Hashtbl.to_alist

  let deserialize data = Reference.Table.of_alist_exn data
end)

exception IncompatibleState of string

(* Return symlinks for files for a pyre project that are links to a separate project root. *)
let restore_symbolic_links ~changed_paths ~local_root ~get_old_link_path =
  let new_paths, removed_paths = List.partition_tf ~f:Path.file_exists changed_paths in
  (* We need to get the deleted paths from shared memory, as the version of the server launched
     from the saved state will have references to this files, whereas they won't be present in the
     new project root, meaning that we need to clean up the environment from them. *)
  let removed_paths = List.filter_map removed_paths ~f:get_old_link_path in
  (* Any member of new_paths might not have existed when the saved state was being created. *)
  let new_paths =
    let local_root_links =
      let file_filter file =
        Filename.check_suffix file ".py" || Filename.check_suffix file ".pyi"
      in
      Path.list ~file_filter ~root:local_root () |> fun links -> Path.build_symlink_map ~links
    in
    List.filter_map new_paths ~f:(Map.find local_root_links)
  in
  new_paths @ removed_paths


(* Used for removed path detection *)
module IndexedRelativePath = struct
  module T = struct
    type t = int * string [@@deriving sexp, compare, hash]
  end

  include T
  include Hashable.Make (T)
end

(* If we're analyzing generated code, Watchman will be blind to any changes to said code. In order
   to be safe, compute hashes for all files that a fresh Pyre run would analyze. *)
let compute_locally_changed_paths
    ~scheduler
    ~configuration
    ~module_tracker:old_module_tracker
    ~ast_environment
  =
  Log.info "Computing files that changed since the saved state was created.";
  let timer = Timer.start () in
  let new_module_tracker = ModuleTracker.create configuration in
  let changed_paths changed new_source_paths =
    let changed_path ({ SourcePath.qualifier; _ } as source_path) =
      let old_hash =
        AstEnvironment.ReadOnly.get_source ast_environment qualifier
        >>| fun { Source.metadata = { Source.Metadata.raw_hash; _ }; _ } -> raw_hash
      in
      let path = SourcePath.full_path ~configuration source_path in
      let current_hash = File.hash (File.create path) in
      if Option.equal Int.equal old_hash current_hash then
        None
      else
        Some path
    in
    changed @ List.filter_map new_source_paths ~f:changed_path
  in
  let changed_paths =
    Scheduler.map_reduce
      scheduler
      ~configuration
      ~initial:[]
      ~map:changed_paths
      ~reduce:( @ )
      ~inputs:(ModuleTracker.source_paths new_module_tracker)
      ()
  in
  let removed_paths =
    let tracked_set =
      ModuleTracker.all_source_paths new_module_tracker
      |> List.map ~f:(fun { SourcePath.priority; relative; _ } -> priority, relative)
      |> IndexedRelativePath.Hash_set.of_list
    in
    ModuleTracker.all_source_paths old_module_tracker
    |> List.filter ~f:(fun { SourcePath.priority; relative; _ } ->
           let key = priority, relative in
           not (Hash_set.mem tracked_set key))
    |> List.map ~f:(SourcePath.full_path ~configuration)
  in
  Statistics.performance ~name:"computed files to reanalyze" ~timer ();
  changed_paths @ removed_paths


let load
    ~server_configuration:{
                            Configuration.Server.configuration =
                              {
                                Configuration.Analysis.expected_version;
                                project_root;
                                local_root;
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
          let to_path serialized = Path.create_absolute ~follow_symbolic_links:false serialized in
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
          raise (IncompatibleState "version mismatch") );
        Log.log ~section:`Server "Loading from saved state project `%s`..." project_name;
        let target_path = Constants.Server.saved_state_path configuration in
        Log.log ~section:`Server "Loading saved state to `%s`..." (Path.absolute target_path);
        let loaded_state =
          Path.search_upwards ~target:".watchmanconfig" ~root:project_root
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
        | None -> raise (IncompatibleState "unable to fetch state") )
    | _ -> raise (IncompatibleState "unexpected saved state parameters")
  in
  let scheduler = Scheduler.create ~configuration () in
  Memory.load_shared_memory ~path:(Path.absolute shared_memory_path);
  let module_tracker = ModuleTracker.SharedMemory.load () in
  let ast_environment = AstEnvironment.load module_tracker in
  let environment =
    Analysis.Environment.shared_memory_handler (AstEnvironment.read_only ast_environment)
  in
  let old_configuration = StoredConfiguration.load () in
  if not (Configuration.Analysis.equal old_configuration configuration) then
    raise (IncompatibleState "configuration mismatch");
  let symlink_targets_to_sources = SymlinkTargetsToSources.load () in
  let changed_paths =
    match changed_paths with
    | Some changed_paths ->
        restore_symbolic_links ~changed_paths ~local_root ~get_old_link_path:(fun path ->
            Hashtbl.find symlink_targets_to_sources (Path.absolute path))
    | None ->
        compute_locally_changed_paths
          ~scheduler
          ~configuration
          ~module_tracker
          ~ast_environment:(AstEnvironment.read_only ast_environment)
  in
  let errors = ServerErrors.load () in
  let state =
    {
      State.module_tracker;
      ast_environment;
      environment;
      errors;
      symlink_targets_to_sources;
      scheduler;
      last_request_time = Unix.time ();
      last_integrity_check = Unix.time ();
      connections;
      lookups = String.Table.create ();
      open_documents = Reference.Table.create ();
    }
  in
  Log.info "Reanalyzing %d files and their dependencies." (List.length changed_paths);
  let state, _ = IncrementalCheck.recheck ~state ~configuration changed_paths in
  state


let save
    ~configuration
    ~saved_state_path
    { State.errors; module_tracker; ast_environment; symlink_targets_to_sources; _ }
  =
  Log.info "Saving server state to %s" saved_state_path;
  Memory.SharedMemory.collect `aggressive;
  ModuleTracker.SharedMemory.store module_tracker;
  SymlinkTargetsToSources.store symlink_targets_to_sources;
  AstEnvironment.store ast_environment;
  StoredConfiguration.store configuration;
  ServerErrors.store errors;
  Memory.save_shared_memory ~path:saved_state_path
