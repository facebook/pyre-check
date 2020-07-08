(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre

module Features = struct
  type t = {
    click_to_fix: bool;
    go_to_definition: bool;
    hover: bool;
  }
  [@@deriving yojson, show]

  let default = { click_to_fix = true; go_to_definition = false; hover = false }

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

module Analysis = struct
  type incremental_style =
    | Shallow
    | FineGrained
  [@@deriving show]

  type t = {
    start_time: float;
    infer: bool;
    configuration_file_hash: string option;
    parallel: bool;
    analyze_external_sources: bool;
    filter_directories: Path.t list option;
    ignore_all_errors: Path.t list option;
    number_of_workers: int;
    local_root: Path.t;
    sections: string list;
    debug: bool;
    project_root: Path.t;
    search_path: SearchPath.t list;
    taint_model_paths: Path.t list;
    verbose: bool;
    expected_version: string option;
    strict: bool;
    show_error_traces: bool;
    log_identifier: string;
    logger: string option;
    profiling_output: string option;
    memory_profiling_output: string option;
    excludes: Str.regexp list; [@opaque]
    extensions: string list;
    store_type_check_resolution: bool;
    incremental_style: incremental_style;
    include_hints: bool;
    perform_autocompletion: bool;
    features: Features.t;
    ignore_infer: Path.t list;
    log_directory: Path.t;
  }
  [@@deriving show]

  let equal first second =
    Bool.equal first.infer second.infer
    && [%compare.equal: string option] first.expected_version second.expected_version
    && Bool.equal first.strict second.strict


  let create
      ?(start_time = Unix.time ())
      ?(infer = false)
      ?configuration_file_hash
      ?(parallel = true)
      ?(analyze_external_sources = false)
      ?filter_directories
      ?ignore_all_errors
      ?(number_of_workers = 4)
      ?(local_root = Path.current_working_directory ())
      ?(sections = [])
      ?(project_root = Path.create_absolute "/")
      ?(search_path = [])
      ?(taint_model_paths = [])
      ?(verbose = false)
      ?expected_version
      ?(strict = false)
      ?(debug = false)
      ?(show_error_traces = false)
      ?(log_identifier = "")
      ?logger
      ?profiling_output
      ?memory_profiling_output
      ?(excludes = [])
      ?(extensions = [])
      ?(store_type_check_resolution = true)
      ?(incremental_style = Shallow)
      ?(include_hints = false)
      ?(perform_autocompletion = false)
      ?(features = Features.default)
      ?(ignore_infer = [])
      ?log_directory
      ()
    =
    {
      start_time;
      infer;
      configuration_file_hash;
      parallel;
      analyze_external_sources;
      filter_directories;
      ignore_all_errors;
      number_of_workers;
      local_root;
      sections;
      debug;
      project_root;
      search_path;
      taint_model_paths;
      verbose;
      expected_version;
      strict;
      show_error_traces;
      log_identifier;
      logger;
      profiling_output;
      memory_profiling_output;
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
    }


  let global : t option ref = ref None

  let set_global configuration = global := Some configuration

  let get_global () = !global

  let log_directory { log_directory; _ } = log_directory

  let search_path { local_root; search_path; _ } =
    (* Have an ordering of search_path > local_root with the parser. search_path precedes
     * local_root due to the possibility of having a subdirectory of the root in the search path. *)
    search_path @ [SearchPath.Root local_root]


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
    find_obscure_flows: bool;
  }
end
