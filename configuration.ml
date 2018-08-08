(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Pyre


type t = {
  start_time: float;
  infer: bool;
  recursive_infer: bool;
  parallel: bool;
  filter_directories: (Path.t list) option;
  number_of_workers: int;
  source_root: Path.t;
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


let create
    ?(start_time = Unix.time())
    ?(infer = false)
    ?(recursive_infer = false)
    ?(parallel = true)
    ?filter_directories
    ?(number_of_workers = 4)
    ?(source_root = Path.current_working_directory ())
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
    source_root;
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


let global =
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


let pyre_root { source_root; _ } =
  Path.append source_root ~element:".pyre"
