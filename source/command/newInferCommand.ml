(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
module Path = PyrePath

module InferMode = struct
  type t =
    | Local
    | Interprocedural
  [@@deriving sexp, compare, hash, yojson]
end

module InferConfiguration = struct
  type t = {
    (* Source file discovery *)
    source_paths: Configuration.SourcePaths.t;
    search_paths: SearchPath.t list;
    excludes: string list;
    checked_directory_allowlist: Path.t list;
    checked_directory_blocklist: Path.t list;
    ignore_infer: Path.t list;
    extensions: Configuration.Extension.t list;
    (* Auxiliary paths *)
    log_path: Path.t;
    global_root: Path.t;
    local_root: Path.t option;
    (* Inference controls *)
    infer_mode: InferMode.t;
    debug: bool;
    python_version: Configuration.PythonVersion.t;
    (* Parallelism controls *)
    parallel: bool;
    number_of_workers: int;
    (* Memory controls *)
    shared_memory: Configuration.SharedMemory.t;
    (* Logging controls *)
    remote_logging: Configuration.RemoteLogging.t option;
    profiling_output: string option;
    memory_profiling_output: string option;
  }
  [@@deriving sexp, compare, hash]

  let of_yojson json =
    let open Yojson.Safe.Util in
    let open JsonParsing in
    (* Parsing logic *)
    try
      let source_paths =
        json
        |> member "source_paths"
        |> Configuration.SourcePaths.of_yojson
        |> Result.ok_or_failwith
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
      let ignore_infer = json |> path_list_member "ignore_infer" ~default:[] in
      let extensions =
        json
        |> string_list_member "extensions" ~default:[]
        |> List.map ~f:Configuration.Extension.create_extension
      in
      let log_path = json |> path_member "log_path" in
      let global_root = json |> path_member "global_root" in
      let local_root = json |> optional_path_member "local_root" in
      let infer_mode =
        json |> member "infer_mode" |> InferMode.of_yojson |> Result.ok_or_failwith
      in
      let debug = json |> bool_member "debug" ~default:false in
      let python_version =
        json
        |> member "python_version"
        |> function
        | `Null -> Configuration.PythonVersion.default
        | _ as json -> Configuration.PythonVersion.of_yojson json |> Result.ok_or_failwith
      in
      let parallel = json |> bool_member "parallel" ~default:false in
      let number_of_workers = json |> int_member "number_of_workers" ~default:1 in
      let shared_memory =
        json
        |> member "shared_memory"
        |> function
        | `Null -> Configuration.SharedMemory.default
        | _ as json -> Configuration.SharedMemory.of_yojson json |> Result.ok_or_failwith
      in
      let remote_logging =
        json
        |> member "remote_logging"
        |> function
        | `Null -> None
        | _ as json ->
            Configuration.RemoteLogging.of_yojson json |> Result.ok_or_failwith |> Option.some
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
          ignore_infer;
          extensions;
          log_path;
          global_root;
          local_root;
          infer_mode;
          debug;
          python_version;
          parallel;
          number_of_workers;
          shared_memory;
          remote_logging;
          profiling_output;
          memory_profiling_output;
        }
    with
    | Type_error (message, _)
    | Undefined (message, _) ->
        Result.Error message
    | other_exception -> Result.Error (Exn.to_string other_exception)


  let _analysis_configuration_of
      {
        source_paths;
        search_paths;
        excludes;
        checked_directory_allowlist;
        checked_directory_blocklist;
        ignore_infer;
        extensions;
        log_path;
        global_root;
        local_root;
        infer_mode = _;
        debug;
        python_version = { Configuration.PythonVersion.major; minor; micro };
        parallel;
        number_of_workers;
        shared_memory =
          { Configuration.SharedMemory.heap_size; dependency_table_power; hash_table_power };
        remote_logging = _;
        profiling_output = _;
        memory_profiling_output = _;
      }
    =
    let source_path =
      match source_paths with
      | Configuration.SourcePaths.Simple source_paths -> source_paths
      | Buck { Configuration.Buck.artifact_root; _ } -> [SearchPath.Root artifact_root]
    in
    Configuration.Analysis.create
      ~infer:true
      ~ignore_infer
      ~parallel
      ~analyze_external_sources:false
      ~filter_directories:checked_directory_allowlist
      ~ignore_all_errors:checked_directory_blocklist
      ~number_of_workers
      ~local_root:(Option.value local_root ~default:global_root)
      ~project_root:global_root
      ~search_path:(List.map search_paths ~f:SearchPath.normalize)
      ~strict:false
      ~debug
      ~show_error_traces:false
      ~excludes
      ~extensions
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
end

let run_infer _configuration_file = Log.warning "Comming soon..."

let command =
  let filename_argument = Command.Param.(anon ("filename" %: Filename.arg_type)) in
  Command.basic
    ~summary:"Runs type inference"
    (Command.Param.map filename_argument ~f:(fun filename () -> run_infer filename))
