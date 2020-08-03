(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre

module CriticalFiles = struct
  type t = string list [@@deriving sexp, compare, hash]

  let of_yojson = function
    | `Null -> Result.Ok []
    | _ as elements -> [%of_yojson: string list] elements


  let to_yojson = [%to_yojson: string list]

  let find critical_files paths =
    List.find paths ~f:(fun path ->
        let base_name = Path.last path in
        List.exists critical_files ~f:(String.equal base_name))
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
                   shared_memory_path =
                     Path.create_absolute ~follow_symbolic_links:false shared_memory_path;
                   changed_files_path =
                     Some (Path.create_absolute ~follow_symbolic_links:false changed_files_path);
                 })
        | `String shared_memory_path, `Null ->
            Result.Ok
              (LoadFromFile
                 {
                   shared_memory_path =
                     Path.create_absolute ~follow_symbolic_links:false shared_memory_path;
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
end

type t = {
  (* Source file discovery *)
  source_paths: Path.t list;
  search_paths: SearchPath.t list;
  excludes: string list;
  checked_directory_allowlist: Path.t list;
  checked_directory_blocklist: Path.t list;
  extensions: string list;
  (* Auxiliary paths *)
  log_path: Path.t;
  global_root: Path.t;
  local_root: Path.t option;
  watchman_root: Path.t option;
  taint_model_paths: Path.t list;
  (* Type checking controls *)
  debug: bool;
  strict: bool;
  show_error_traces: bool;
  store_type_check_resolution: bool;
  critical_files: CriticalFiles.t;
  saved_state_action: SavedStateAction.t option;
  (* Parallelism controls *)
  parallel: bool;
  number_of_workers: int;
}
[@@deriving sexp, compare, hash]

let of_yojson json =
  let open Yojson.Safe.Util in
  try
    let with_default ~extract ~extract_optional ?default json =
      match default with
      | None -> extract json
      | Some default -> extract_optional json |> Option.value ~default
    in
    let to_bool_with_default = with_default ~extract:to_bool ~extract_optional:to_bool_option in
    let to_int_with_default = with_default ~extract:to_int ~extract_optional:to_int_option in
    let to_path json = to_string json |> Path.create_absolute ~follow_symbolic_links:false in
    (* The absent of explicit `~default` parameter means that the corresponding JSON field is
       mandantory. *)
    let bool_member ?default name json = member name json |> to_bool_with_default ?default in
    let int_member ?default name json = member name json |> to_int_with_default ?default in
    let path_member name json = member name json |> to_path in
    let optional_path_member name json =
      member name json
      |> function
      | `Null -> None
      | _ as element -> Some (to_path element)
    in
    let list_member ?default ~f name json =
      member name json
      |> fun element ->
      match element, default with
      | `Null, Some default -> default
      | _, _ -> convert_each f element
    in
    let string_list_member = list_member ~f:to_string in
    let path_list_member = list_member ~f:to_path in

    (* Parsing logic *)
    let source_paths = json |> path_list_member "source_paths" in
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
    let extensions = json |> string_list_member "extensions" ~default:[] in
    let log_path = json |> path_member "log_path" in
    let global_root = json |> path_member "global_root" in
    let local_root = json |> optional_path_member "local_root" in
    let watchman_root = json |> optional_path_member "watchman_root" in
    let taint_model_paths = json |> path_list_member "taint_model_paths" ~default:[] in
    let debug = json |> bool_member "debug" ~default:false in
    let strict = json |> bool_member "strict" ~default:false in
    let show_error_traces = json |> bool_member "show_error_traces" ~default:false in
    let critical_files =
      json |> member "critical_files" |> CriticalFiles.of_yojson |> Result.ok_or_failwith
    in
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
        show_error_traces;
        critical_files;
        saved_state_action;
        store_type_check_resolution;
        parallel;
        number_of_workers;
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
      show_error_traces;
      critical_files;
      saved_state_action;
      store_type_check_resolution;
      parallel;
      number_of_workers;
    }
  =
  let result =
    [
      "source_paths", [%to_yojson: string list] (List.map source_paths ~f:Path.absolute);
      "search_paths", [%to_yojson: string list] (List.map search_paths ~f:SearchPath.show);
      "excludes", [%to_yojson: string list] excludes;
      ( "checked_directory_allowlist",
        [%to_yojson: string list] (List.map checked_directory_allowlist ~f:Path.absolute) );
      ( "checked_directory_blocklist",
        [%to_yojson: string list] (List.map checked_directory_blocklist ~f:Path.absolute) );
      "extensions", [%to_yojson: string list] extensions;
      "log_path", [%to_yojson: string] (Path.absolute log_path);
      "global_root", [%to_yojson: string] (Path.absolute global_root);
      "taint_model_paths", [%to_yojson: string list] (List.map taint_model_paths ~f:Path.absolute);
      "debug", [%to_yojson: bool] debug;
      "strict", [%to_yojson: bool] strict;
      "show_error_traces", [%to_yojson: bool] show_error_traces;
      "critical_files", [%to_yojson: string list] critical_files;
      "store_type_check_resolution", [%to_yojson: bool] store_type_check_resolution;
      "parallel", [%to_yojson: bool] parallel;
      "number_of_workers", [%to_yojson: int] number_of_workers;
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
      show_error_traces;
      critical_files = _;
      saved_state_action = _;
      store_type_check_resolution;
      parallel;
      number_of_workers;
    }
  =
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
    ~source_path:source_paths
    ()
