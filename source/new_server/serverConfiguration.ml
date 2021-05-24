(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre

module JsonParsing = struct
  open Yojson.Safe.Util

  let with_default ~extract ~extract_optional ?default json =
    match default with
    | None -> extract json
    | Some default -> extract_optional json |> Option.value ~default


  let to_bool_with_default = with_default ~extract:to_bool ~extract_optional:to_bool_option

  let to_int_with_default = with_default ~extract:to_int ~extract_optional:to_int_option

  let to_path json = to_string json |> Path.create_absolute

  (* The absent of explicit `~default` parameter means that the corresponding JSON field is
     mandantory. *)
  let bool_member ?default name json = member name json |> to_bool_with_default ?default

  let int_member ?default name json = member name json |> to_int_with_default ?default

  let optional_string_member name json =
    member name json
    |> function
    | `Null -> None
    | _ as element -> Some (to_string element)


  let path_member name json = member name json |> to_path

  let optional_path_member name json =
    member name json
    |> function
    | `Null -> None
    | _ as element -> Some (to_path element)


  let list_member ?default ~f name json =
    member name json
    |> fun element ->
    match element, default with
    | `Null, Some default -> default
    | _, _ -> convert_each f element


  let string_list_member = list_member ~f:to_string

  let path_list_member = list_member ~f:to_path
end

module Buck = struct
  type t = {
    mode: string option;
    isolation_prefix: string option;
    targets: string list;
    (* This is the buck root of the source directory, i.e. output of `buck root`. *)
    source_root: Path.t;
    (* This is the root of directory where built artifacts will be placed. *)
    artifact_root: Path.t;
  }
  [@@deriving sexp, compare, hash]

  let of_yojson json =
    let open JsonParsing in
    try
      let mode = optional_string_member "mode" json in
      let isolation_prefix = optional_string_member "isolation_prefix" json in
      let targets = string_list_member "targets" json ~default:[] in
      let source_root = path_member "source_root" json in
      let artifact_root = path_member "artifact_root" json in
      Result.Ok { mode; isolation_prefix; targets; source_root; artifact_root }
    with
    | Yojson.Safe.Util.Type_error (message, _)
    | Yojson.Safe.Util.Undefined (message, _) ->
        Result.Error message
    | other_exception -> Result.Error (Exn.to_string other_exception)


  let to_yojson { mode; isolation_prefix; targets; source_root; artifact_root } =
    let result =
      [
        "targets", `List (List.map targets ~f:(fun target -> `String target));
        "source_root", `String (Path.absolute source_root);
        "artifact_root", `String (Path.absolute artifact_root);
      ]
    in
    let result =
      match mode with
      | None -> result
      | Some mode -> ("mode", `String mode) :: result
    in
    let result =
      match isolation_prefix with
      | None -> result
      | Some isolation_prefix -> ("isolation_prefix", `String isolation_prefix) :: result
    in
    `Assoc result
end

module SourcePaths = struct
  type t =
    | Simple of SearchPath.t list
    | Buck of Buck.t
  [@@deriving sexp, compare, hash]

  let of_yojson json =
    let open Yojson.Safe.Util in
    let parsing_failed () =
      let message = Format.sprintf "Malformed source path JSON: %s" (Yojson.Safe.to_string json) in
      Result.Error message
    in
    let parse_search_path_jsons search_path_jsons =
      try
        Result.Ok
          (Simple (List.map search_path_jsons ~f:(fun json -> to_string json |> SearchPath.create)))
      with
      | Type_error _ -> parsing_failed ()
    in
    match json with
    | `List search_path_jsons ->
        (* Recognize this as a shortcut for simple source paths. *)
        parse_search_path_jsons search_path_jsons
    | `Assoc _ -> (
        match member "kind" json with
        | `String "simple" -> (
            match member "paths" json with
            | `List search_path_jsons -> parse_search_path_jsons search_path_jsons
            | _ -> parsing_failed () )
        | `String "buck" -> (
            match Buck.of_yojson json with
            | Result.Ok buck -> Result.Ok (Buck buck)
            | Result.Error error -> Result.Error error )
        | _ -> parsing_failed () )
    | _ -> parsing_failed ()


  let to_yojson = function
    | Simple search_paths ->
        `Assoc
          [
            "kind", `String "simple";
            "paths", [%to_yojson: string list] (List.map search_paths ~f:SearchPath.show);
          ]
    | Buck buck -> Buck.to_yojson buck |> Yojson.Safe.Util.combine (`Assoc ["kind", `String "buck"])
end

module CriticalFile = struct
  type t =
    | BaseName of string
    | Extension of string
    | FullPath of Path.t
  [@@deriving sexp, compare, hash]

  let of_yojson = function
    | `Assoc [("base_name", `String name)] -> Result.Ok (BaseName name)
    | `Assoc [("extension", `String name)] -> Result.Ok (Extension name)
    | `Assoc [("full_path", `String path)] -> Result.Ok (FullPath (Path.create_absolute path))
    | _ as json ->
        let message =
          Format.sprintf "Malformed critical file JSON: %s" (Yojson.Safe.to_string json)
        in
        Result.Error message


  let to_yojson = function
    | BaseName name -> `Assoc ["base_name", `String name]
    | Extension name -> `Assoc ["extension", `String name]
    | FullPath path -> `Assoc ["full_path", `String (Path.absolute path)]


  let matches ~path = function
    | BaseName expect_name ->
        let actual_name = Path.last path in
        String.equal expect_name actual_name
    | Extension extension ->
        let actual_name = Path.last path in
        String.is_suffix actual_name ~suffix:("." ^ extension)
    | FullPath expect_path -> Path.equal expect_path path


  let matches_any ~path patterns = List.exists patterns ~f:(matches ~path)

  let find ~within patterns = List.find within ~f:(fun path -> matches_any ~path patterns)
end

module SavedStateAction = struct
  type t =
    | LoadFromFile of {
        shared_memory_path: Path.t;
        changed_files_path: Path.t option;
      }
    | LoadFromProject of {
        project_name: string;
        project_metadata: string option;
      }
    | SaveToFile of { shared_memory_path: Path.t }
  [@@deriving sexp, compare, hash]

  let of_yojson json =
    let open Yojson.Safe.Util in
    let parsing_failed () =
      let message =
        Format.sprintf "Malformed saved state action JSON: %s" (Yojson.Safe.to_string json)
      in
      Result.Error message
    in
    match json with
    | `List [`String "load_from_file"; load_from_file_options] -> (
        match
          ( member "shared_memory_path" load_from_file_options,
            member "changed_files_path" load_from_file_options )
        with
        | `String shared_memory_path, `String changed_files_path ->
            Result.Ok
              (LoadFromFile
                 {
                   shared_memory_path = Path.create_absolute shared_memory_path;
                   changed_files_path = Some (Path.create_absolute changed_files_path);
                 })
        | `String shared_memory_path, `Null ->
            Result.Ok
              (LoadFromFile
                 {
                   shared_memory_path = Path.create_absolute shared_memory_path;
                   changed_files_path = None;
                 })
        | _, _ -> parsing_failed () )
    | `List [`String "load_from_project"; load_from_project_options] -> (
        match
          ( member "project_name" load_from_project_options,
            member "project_metadata" load_from_project_options )
        with
        | `String project_name, `String project_metadata ->
            Result.Ok (LoadFromProject { project_name; project_metadata = Some project_metadata })
        | `String project_name, `Null ->
            Result.Ok (LoadFromProject { project_name; project_metadata = None })
        | _, _ -> parsing_failed () )
    | `List [`String "save_to_file"; save_to_file_options] -> (
        match member "shared_memory_path" save_to_file_options with
        | `String shared_memory_path ->
            Result.Ok (SaveToFile { shared_memory_path = Path.create_absolute shared_memory_path })
        | _ -> parsing_failed () )
    | _ -> parsing_failed ()


  let to_yojson = function
    | LoadFromFile { shared_memory_path; changed_files_path } ->
        let load_from_file_options =
          let shared_memory_path_option =
            "shared_memory_path", `String (Path.absolute shared_memory_path)
          in
          match changed_files_path with
          | None -> [shared_memory_path_option]
          | Some changed_files_path ->
              let changed_files_path_option =
                "changed_files_path", `String (Path.absolute changed_files_path)
              in
              [shared_memory_path_option; changed_files_path_option]
        in
        `List [`String "load_from_file"; `Assoc load_from_file_options]
    | LoadFromProject { project_name; project_metadata } ->
        let load_from_project_options =
          let project_name_option = "project_name", `String project_name in
          match project_metadata with
          | None -> [project_name_option]
          | Some project_metadata ->
              let project_metadata_option = "project_metadata", `String project_metadata in
              [project_name_option; project_metadata_option]
        in
        `List [`String "load_from_project"; `Assoc load_from_project_options]
    | SaveToFile { shared_memory_path } ->
        let save_to_file_options =
          ["shared_memory_path", `String (Path.absolute shared_memory_path)]
        in
        `List [`String "save_to_file"; `Assoc save_to_file_options]
end

module RemoteLogging = struct
  type t = {
    logger: string;
    identifier: (string[@default ""]);
  }
  [@@deriving sexp, compare, hash, yojson]
end

module PythonVersion = struct
  type t = {
    major: int;
    minor: int;
    micro: int;
  }
  [@@deriving sexp, compare, hash, yojson]

  let default =
    {
      major = Configuration.default_python_major_version;
      minor = Configuration.default_python_minor_version;
      micro = Configuration.default_python_micro_version;
    }
end

module SharedMemory = struct
  type t = {
    heap_size: int;
    dependency_table_power: int;
    hash_table_power: int;
  }
  [@@deriving sexp, compare, hash, yojson]

  let default =
    {
      heap_size = Configuration.default_shared_memory_heap_size;
      dependency_table_power = Configuration.default_shared_memory_dependency_table_power;
      hash_table_power = Configuration.default_shared_memory_hash_table_power;
    }


  let of_yojson json =
    let open JsonParsing in
    Ok
      {
        heap_size = int_member "heap_size" ~default:default.heap_size json;
        dependency_table_power =
          int_member "dependency_table_power" ~default:default.dependency_table_power json;
        hash_table_power = int_member "hash_table_power" ~default:default.hash_table_power json;
      }
end

type t = {
  (* Source file discovery *)
  source_paths: SourcePaths.t;
  search_paths: SearchPath.t list;
  excludes: string list;
  checked_directory_allowlist: Path.t list;
  checked_directory_blocklist: Path.t list;
  extensions: Configuration.Extension.t list;
  (* Auxiliary paths *)
  log_path: Path.t;
  global_root: Path.t;
  local_root: Path.t option;
  watchman_root: Path.t option;
  taint_model_paths: Path.t list;
  (* Type checking controls *)
  debug: bool;
  strict: bool;
  python_version: PythonVersion.t;
  show_error_traces: bool;
  store_type_check_resolution: bool;
  critical_files: CriticalFile.t list;
  saved_state_action: SavedStateAction.t option;
  (* Parallelism controls *)
  parallel: bool;
  number_of_workers: int;
  (* Memory controls *)
  shared_memory: SharedMemory.t;
  (* Logging controls *)
  additional_logging_sections: string list;
  remote_logging: RemoteLogging.t option;
  profiling_output: string option;
  memory_profiling_output: string option;
}
[@@deriving sexp, compare, hash]

let of_yojson json =
  let open Yojson.Safe.Util in
  let open JsonParsing in
  try
    let critial_file_list_member =
      let to_critical_file json = CriticalFile.of_yojson json |> Result.ok_or_failwith in
      list_member ~f:to_critical_file
    in

    (* Parsing logic *)
    let source_paths =
      json |> member "source_paths" |> SourcePaths.of_yojson |> Result.ok_or_failwith
    in
    let search_paths =
      json
      |> list_member
           "search_paths"
           ~f:(fun element -> to_string element |> SearchPath.create)
           ~default:[]
    in
    let excludes = json |> string_list_member "excludes" ~default:[] in
    let checked_directory_allowlist =
      json |> path_list_member "checked_directory_allowlist" ~default:[]
    in
    let checked_directory_blocklist =
      json |> path_list_member "checked_directory_blocklist" ~default:[]
    in
    let extensions =
      json
      |> string_list_member "extensions" ~default:[]
      |> List.map ~f:Configuration.Extension.create_extension
    in
    let log_path = json |> path_member "log_path" in
    let global_root = json |> path_member "global_root" in
    let local_root = json |> optional_path_member "local_root" in
    let watchman_root = json |> optional_path_member "watchman_root" in
    let taint_model_paths = json |> path_list_member "taint_model_paths" ~default:[] in
    let debug = json |> bool_member "debug" ~default:false in
    let strict = json |> bool_member "strict" ~default:false in
    let python_version =
      json
      |> member "python_version"
      |> function
      | `Null -> PythonVersion.default
      | _ as json -> PythonVersion.of_yojson json |> Result.ok_or_failwith
    in
    let show_error_traces = json |> bool_member "show_error_traces" ~default:false in
    let critical_files = json |> critial_file_list_member "critical_files" ~default:[] in
    let saved_state_action =
      json
      |> member "saved_state_action"
      |> function
      | `Null -> None
      | _ as json -> SavedStateAction.of_yojson json |> Result.ok_or_failwith |> Option.some
    in
    let store_type_check_resolution =
      json |> bool_member "store_type_check_resolution" ~default:false
    in
    let parallel = json |> bool_member "parallel" ~default:false in
    let number_of_workers = json |> int_member "number_of_workers" ~default:1 in
    let shared_memory =
      json
      |> member "shared_memory"
      |> function
      | `Null -> SharedMemory.default
      | _ as json -> SharedMemory.of_yojson json |> Result.ok_or_failwith
    in
    let additional_logging_sections =
      json |> string_list_member "additional_logging_sections" ~default:[]
    in
    let remote_logging =
      json
      |> member "remote_logging"
      |> function
      | `Null -> None
      | _ as json -> RemoteLogging.of_yojson json |> Result.ok_or_failwith |> Option.some
    in
    let profiling_output = json |> optional_string_member "profiling_output" in
    let memory_profiling_output = json |> optional_string_member "memory_profiling_output" in
    Result.Ok
      {
        source_paths;
        search_paths;
        excludes;
        checked_directory_allowlist;
        checked_directory_blocklist;
        extensions;
        log_path;
        global_root;
        local_root;
        watchman_root;
        taint_model_paths;
        debug;
        strict;
        python_version;
        show_error_traces;
        critical_files;
        saved_state_action;
        store_type_check_resolution;
        parallel;
        number_of_workers;
        shared_memory;
        additional_logging_sections;
        remote_logging;
        profiling_output;
        memory_profiling_output;
      }
  with
  | Type_error (message, _)
  | Undefined (message, _) ->
      Result.Error message
  | other_exception -> Result.Error (Exn.to_string other_exception)


let to_yojson
    {
      source_paths;
      search_paths;
      excludes;
      checked_directory_allowlist;
      checked_directory_blocklist;
      extensions;
      log_path;
      global_root;
      local_root;
      watchman_root;
      taint_model_paths;
      debug;
      strict;
      python_version;
      show_error_traces;
      critical_files;
      saved_state_action;
      store_type_check_resolution;
      parallel;
      number_of_workers;
      shared_memory;
      additional_logging_sections;
      remote_logging;
      profiling_output;
      memory_profiling_output;
    }
  =
  let result =
    [
      "source_paths", [%to_yojson: SourcePaths.t] source_paths;
      "search_paths", [%to_yojson: string list] (List.map search_paths ~f:SearchPath.show);
      "excludes", [%to_yojson: string list] excludes;
      ( "checked_directory_allowlist",
        [%to_yojson: string list] (List.map checked_directory_allowlist ~f:Path.absolute) );
      ( "checked_directory_blocklist",
        [%to_yojson: string list] (List.map checked_directory_blocklist ~f:Path.absolute) );
      "extensions", [%to_yojson: Configuration.Extension.t list] extensions;
      "log_path", [%to_yojson: string] (Path.absolute log_path);
      "global_root", [%to_yojson: string] (Path.absolute global_root);
      "taint_model_paths", [%to_yojson: string list] (List.map taint_model_paths ~f:Path.absolute);
      "debug", [%to_yojson: bool] debug;
      "strict", [%to_yojson: bool] strict;
      "python_version", [%to_yojson: PythonVersion.t] python_version;
      "show_error_traces", [%to_yojson: bool] show_error_traces;
      "critical_files", [%to_yojson: CriticalFile.t list] critical_files;
      "store_type_check_resolution", [%to_yojson: bool] store_type_check_resolution;
      "parallel", [%to_yojson: bool] parallel;
      "number_of_workers", [%to_yojson: int] number_of_workers;
      "shared_memory", [%to_yojson: SharedMemory.t] shared_memory;
      "additional_logging_sections", [%to_yojson: string list] additional_logging_sections;
    ]
  in
  let result =
    match local_root with
    | None -> result
    | Some local_root -> ("local_root", [%to_yojson: string] (Path.absolute local_root)) :: result
  in
  let result =
    match watchman_root with
    | None -> result
    | Some watchman_root ->
        ("watchman_root", [%to_yojson: string] (Path.absolute watchman_root)) :: result
  in
  let result =
    match saved_state_action with
    | None -> result
    | Some saved_state_action ->
        ("saved_state_action", SavedStateAction.to_yojson saved_state_action) :: result
  in
  let result =
    match remote_logging with
    | None -> result
    | Some remote_logging -> ("remote_logging", RemoteLogging.to_yojson remote_logging) :: result
  in
  let result =
    match profiling_output with
    | None -> result
    | Some profiling_output -> ("profiling_output", [%to_yojson: string] profiling_output) :: result
  in
  let result =
    match memory_profiling_output with
    | None -> result
    | Some memory_profiling_output ->
        ("memory_profiling_output", [%to_yojson: string] memory_profiling_output) :: result
  in
  `Assoc result


let analysis_configuration_of
    {
      source_paths;
      search_paths;
      excludes;
      checked_directory_allowlist;
      checked_directory_blocklist;
      extensions;
      log_path;
      global_root;
      local_root;
      watchman_root = _;
      taint_model_paths;
      debug;
      strict;
      python_version = { PythonVersion.major; minor; micro };
      show_error_traces;
      critical_files = _;
      saved_state_action = _;
      store_type_check_resolution;
      parallel;
      number_of_workers;
      shared_memory = { SharedMemory.heap_size; dependency_table_power; hash_table_power };
      additional_logging_sections = _;
      remote_logging = _;
      profiling_output = _;
      memory_profiling_output = _;
    }
  =
  let source_path =
    match source_paths with
    | SourcePaths.Simple source_paths -> source_paths
    | Buck { Buck.artifact_root; _ } -> [SearchPath.Root artifact_root]
  in
  Configuration.Analysis.create
    ~infer:false
    ~parallel
    ~analyze_external_sources:false
    ~filter_directories:checked_directory_allowlist
    ~ignore_all_errors:checked_directory_blocklist
    ~number_of_workers
    ~local_root:(Option.value local_root ~default:global_root)
    ~project_root:global_root
    ~search_path:(List.map search_paths ~f:SearchPath.normalize)
    ~taint_model_paths
    ~strict
    ~debug
    ~show_error_traces
    ~excludes
    ~extensions
    ~store_type_check_resolution
    ~incremental_style:Configuration.Analysis.FineGrained
    ~include_hints:false
    ~perform_autocompletion:false
    ~log_directory:(Path.absolute log_path)
    ~python_major_version:major
    ~python_minor_version:minor
    ~python_micro_version:micro
    ~shared_memory_heap_size:heap_size
    ~shared_memory_dependency_table_power:dependency_table_power
    ~shared_memory_hash_table_power:hash_table_power
    ~source_path
    ()
