(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Pyre


module Analysis = struct
  type t = {
    start_time: float;
    infer: bool;
    recursive_infer: bool;
    parallel: bool;
    filter_directories: (Path.t list) option;
    number_of_workers: int;
    local_root: Path.t;
    sections: string list;
    debug: bool;
    project_root: Path.t;
    search_path: Path.t list;
    typeshed: Path.t option;
    verbose: bool;
    expected_version: string option;
    strict: bool;
    declare: bool;
    show_error_traces: bool;
    log_identifier: string;
    logger: string option;
  }
  [@@deriving show]

  let equal first second =
    first.infer = second.infer &&
    first.recursive_infer = second.recursive_infer &&
    first.debug = second.debug &&
    first.expected_version = second.expected_version &&
    first.strict = second.strict &&
    first.declare = second.declare


  let create
      ?(start_time = Unix.time())
      ?(infer = false)
      ?(recursive_infer = false)
      ?(parallel = true)
      ?filter_directories
      ?(number_of_workers = 4)
      ?(local_root = Path.current_working_directory ())
      ?(sections = [])
      ?(project_root = Path.create_absolute "/")
      ?(search_path = [])
      ?typeshed
      ?(verbose = false)
      ?expected_version
      ?(strict = false)
      ?(declare = false)
      ?(debug = false)
      ?(show_error_traces = false)
      ?(log_identifier = "")
      ?logger
      () =
    {
      start_time;
      infer;
      recursive_infer;
      parallel;
      filter_directories;
      number_of_workers;
      local_root;
      sections;
      debug;
      project_root;
      search_path;
      typeshed;
      verbose;
      expected_version;
      strict;
      declare;
      show_error_traces;
      log_identifier;
      logger;
    }


  let global: t option ref =
    ref None


  let set_global configuration =
    global := Some configuration


  let get_global () =
    !global


  let localize ({ debug; strict; _ } as configuration) ~local_debug ~local_strict ~declare =
    {
      configuration with
      debug = debug || local_debug;
      strict = strict || local_strict;
      declare;
    }


  let pyre_root { local_root; _ } =
    Path.append local_root ~element:".pyre"
end


module Server = struct
  type load_parameters = {
    shared_memory_path: Path.t;
    changed_files_path: Path.t;
  }

  type load =
    | LoadFromFiles of load_parameters
    | LoadFromProject of string

  type saved_state =
    | Save of string
    | Load of load

  type t = {
    (* Server-specific configuration options *)
    socket_path: Path.t;
    socket_link: Path.t;
    lock_path: Path.t;
    pid_path: Path.t;
    log_path: Path.t;
    daemonize: bool;
    use_watchman: bool;
    watchman_creation_timeout: float;
    saved_state: saved_state option;
    (* Analysis configuration *)
    configuration: Analysis.t;
  }

  (* Required to appease the compiler. *)
  let global: t option ref = ref None

  let set_global configuration =
    global := Some configuration

  let get_global () = !global
end


module StaticAnalysis = struct
  type t = {
    result_json_path: Path.t option;
    (* Analysis configuration *)
    configuration: Analysis.t;
  }
end
