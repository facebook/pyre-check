(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre

let default_python_major_version = 3

let default_python_minor_version = 10

let default_python_micro_version = 10

let default_shared_memory_heap_size = 8 * 1024 * 1024 * 1024 (* 8 GiB *)

let default_shared_memory_dependency_table_power = 27

let default_shared_memory_hash_table_power = 26

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
            | _ -> parsing_failed ())
        | `String "buck" -> (
            match Buck.of_yojson json with
            | Result.Ok buck -> Result.Ok (Buck buck)
            | Result.Error error -> Result.Error error)
        | _ -> parsing_failed ())
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
      major = default_python_major_version;
      minor = default_python_minor_version;
      micro = default_python_micro_version;
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
      heap_size = default_shared_memory_heap_size;
      dependency_table_power = default_shared_memory_dependency_table_power;
      hash_table_power = default_shared_memory_hash_table_power;
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

module Features = struct
  type t = {
    click_to_fix: bool;
    go_to_definition: bool;
    hover: bool;
  }
  [@@deriving yojson, show]

  let default = { click_to_fix = false; go_to_definition = false; hover = false }

  let create feature_string =
    feature_string
    >>| (fun feature_string ->
          let json_string = Yojson.Safe.from_string feature_string in
          let default_from_string string =
            let { click_to_fix; go_to_definition; hover } = default in
            match string with
            | "click_to_fix" -> click_to_fix
            | "go_to_definition" -> go_to_definition
            | "hover" -> hover
            | _ -> false
          in
          let parse_feature string =
            match Yojson.Safe.Util.member string json_string with
            | `Bool value -> value
            | _ -> default_from_string string
          in
          let click_to_fix = parse_feature "click_to_fix" in
          let go_to_definition = parse_feature "go_to_definition" in
          let hover = parse_feature "hover" in
          { click_to_fix; go_to_definition; hover })
    |> Option.value ~default
end

module Extension = struct
  type t = {
    suffix: string;
    include_suffix_in_module_qualifier: bool;
  }
  [@@deriving show, eq, sexp, compare, hash]

  let to_yojson { suffix; include_suffix_in_module_qualifier } =
    `Assoc
      [
        "suffix", `String suffix;
        "include_suffix_in_module_qualifier", `Bool include_suffix_in_module_qualifier;
      ]


  let of_yojson = function
    | `Assoc
        [
          ("suffix", `String suffix);
          ("include_suffix_in_module_qualifier", `Bool include_suffix_in_module_qualifier);
        ] ->
        Result.Ok { suffix; include_suffix_in_module_qualifier }
    | _ as json ->
        let message =
          Format.sprintf "Malformed critical file JSON: %s" (Yojson.Safe.to_string json)
        in
        Result.Error message


  let create_extension serialized =
    match String.split serialized ~on:'$' with
    | [suffix] -> { suffix; include_suffix_in_module_qualifier = false }
    | [suffix; options] when String.equal options "include_suffix_in_module_qualifier" ->
        { suffix; include_suffix_in_module_qualifier = true }
    | _ -> failwith (Format.asprintf "Unable to parse extension from %s" serialized)


  let suffix { suffix; _ } = suffix
end

module Analysis = struct
  type incremental_style =
    | Shallow
    | FineGrained
  [@@deriving show]

  type shared_memory = {
    heap_size: int;
    dependency_table_power: int;
    hash_table_power: int;
  }
  [@@deriving show]

  type t = {
    configuration_file_hash: string option;
    parallel: bool;
    analyze_external_sources: bool;
    filter_directories: Path.t list option;
    ignore_all_errors: Path.t list option;
    number_of_workers: int;
    local_root: Path.t;
    debug: bool;
    project_root: Path.t;
    source_path: SearchPath.t list;
    search_path: SearchPath.t list;
    taint_model_paths: Path.t list;
    expected_version: string option;
    strict: bool;
    show_error_traces: bool;
    excludes: Str.regexp list; [@opaque]
    extensions: Extension.t list;
    store_type_check_resolution: bool;
    incremental_style: incremental_style;
    include_hints: bool;
    perform_autocompletion: bool;
    features: Features.t;
    log_directory: Path.t;
    python_major_version: int;
    python_minor_version: int;
    python_micro_version: int;
    shared_memory: shared_memory;
  }
  [@@deriving show]

  let equal first second =
    [%compare.equal: string option] first.expected_version second.expected_version
    && Bool.equal first.strict second.strict


  let create
      ?configuration_file_hash
      ?(parallel = true)
      ?(analyze_external_sources = false)
      ?filter_directories
      ?ignore_all_errors
      ?(number_of_workers = 4)
      ?(local_root = Path.current_working_directory ())
      ?(project_root = Path.create_absolute "/")
      ?(search_path = [])
      ?(taint_model_paths = [])
      ?expected_version
      ?(strict = false)
      ?(debug = false)
      ?(show_error_traces = false)
      ?(excludes = [])
      ?(extensions = [])
      ?(store_type_check_resolution = true)
      ?(incremental_style = Shallow)
      ?(include_hints = false)
      ?(perform_autocompletion = false)
      ?(features = Features.default)
      ?log_directory
      ?(python_major_version = default_python_major_version)
      ?(python_minor_version = default_python_minor_version)
      ?(python_micro_version = default_python_micro_version)
      ?(shared_memory_heap_size = default_shared_memory_heap_size)
      ?(shared_memory_dependency_table_power = default_shared_memory_dependency_table_power)
      ?(shared_memory_hash_table_power = default_shared_memory_hash_table_power)
      ~source_path
      ()
    =
    {
      configuration_file_hash;
      parallel;
      analyze_external_sources;
      filter_directories;
      ignore_all_errors;
      number_of_workers;
      local_root;
      debug;
      project_root;
      source_path;
      search_path;
      taint_model_paths;
      expected_version;
      strict;
      show_error_traces;
      excludes =
        List.map excludes ~f:(fun exclude_regex ->
            Str.global_substitute
              (Str.regexp_string "${SOURCE_DIRECTORY}")
              (fun _ -> Path.absolute local_root)
              exclude_regex
            |> Str.regexp);
      extensions;
      store_type_check_resolution;
      incremental_style;
      include_hints;
      perform_autocompletion;
      features;
      log_directory =
        (match log_directory with
        | Some directory -> Path.create_absolute directory
        | None -> Path.append local_root ~element:".pyre");
      python_major_version;
      python_minor_version;
      python_micro_version;
      shared_memory =
        {
          heap_size = shared_memory_heap_size;
          dependency_table_power = shared_memory_dependency_table_power;
          hash_table_power = shared_memory_hash_table_power;
        };
    }


  let log_directory { log_directory; _ } = log_directory

  let search_path { source_path; search_path; _ } =
    (* Have an ordering of search_path > source_path with the parser. search_path precedes
     * local_root due to the possibility of having a subdirectory of the root in the search path. *)
    search_path @ source_path


  let extension_suffixes { extensions; _ } = List.map ~f:Extension.suffix extensions

  let find_extension { extensions; _ } path =
    List.find extensions ~f:(fun extension ->
        String.is_suffix ~suffix:(Extension.suffix extension) (Path.absolute path))


  let features { features; _ } = features
end

module Server = struct
  type load_parameters = {
    shared_memory_path: Path.t;
    changed_files_path: Path.t option;
  }

  type load =
    | LoadFromFiles of load_parameters
    | LoadFromProject of {
        project_name: string;
        metadata: string option;
      }

  type saved_state_action =
    | Save of string
    | Load of load

  type socket_path = {
    path: Path.t;
    link: Path.t;
  }

  type t = {
    (* Server-specific configuration options *)
    socket: socket_path;
    json_socket: socket_path;
    adapter_socket: socket_path;
    lock_path: Path.t;
    pid_path: Path.t;
    log_path: Path.t;
    daemonize: bool;
    saved_state_action: saved_state_action option;
    (* Analysis configuration *)
    configuration: Analysis.t;
  }

  (* Required to appease the compiler. *)
  let global : t option ref = ref None

  let set_global configuration = global := Some configuration

  let get_global () = !global
end

module StaticAnalysis = struct
  type t = {
    result_json_path: Path.t option;
    dump_call_graph: bool;
    verify_models: bool;
    (* Analysis configuration *)
    configuration: Analysis.t;
    rule_filter: int list option;
    find_missing_flows: string option;
    dump_model_query_results: bool;
    use_cache: bool;
    maximum_trace_length: int option;
    maximum_tito_depth: int option;
  }

  let dump_model_query_results_path
      { configuration = { log_directory; _ }; dump_model_query_results; _ }
    =
    Path.create_relative ~root:log_directory ~relative:"model_query_results.pysa"
    |> Option.some_if dump_model_query_results
end
