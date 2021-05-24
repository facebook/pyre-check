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

let default_shared_memory_heap_size = 16 * 1024 * 1024 * 1024 (* 16 GiB *)

let default_shared_memory_dependency_table_power = 27

let default_shared_memory_hash_table_power = 26

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
    infer: bool;
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
    ignore_infer: Path.t list;
    log_directory: Path.t;
    python_major_version: int;
    python_minor_version: int;
    python_micro_version: int;
    shared_memory: shared_memory;
  }
  [@@deriving show]

  let equal first second =
    Bool.equal first.infer second.infer
    && [%compare.equal: string option] first.expected_version second.expected_version
    && Bool.equal first.strict second.strict


  let create
      ?(infer = false)
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
      ?(ignore_infer = [])
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
      infer;
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
      ignore_infer;
      log_directory =
        ( match log_directory with
        | Some directory -> Path.create_absolute directory
        | None -> Path.append local_root ~element:".pyre" );
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
  }

  let dump_model_query_results_path
      { configuration = { log_directory; _ }; dump_model_query_results; _ }
    =
    Path.create_relative ~root:log_directory ~relative:"model_query_results.pysa"
    |> Option.some_if dump_model_query_results
end
