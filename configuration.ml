(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Pyre


type t = {
  start_time: float;
  infer: bool;
  recursive_infer: bool;
  analyze: bool;
  parallel: bool;
  number_of_workers: int;
  source_root: Path.t;
  sections: string list;
  debug: bool;
  project_root: Path.t;
  stub_roots: Path.t list;
  verbose: bool;
  version: string option;
  strict: bool;
  declare: bool;
  show_error_traces: bool;
  log_identifier: string;
}
[@@deriving show]


let create
    ?(start_time = Unix.time())
    ?(infer = false)
    ?(recursive_infer = false)
    ?(analyze = false)
    ?(parallel = true)
    ?(number_of_workers = 4)
    ?(source_root = Path.current_working_directory ())
    ?(sections = [])
    ?(project_root = Path.create_absolute "/")
    ?(stub_roots = [])
    ?(verbose = false)
    ?version
    ?(strict = false)
    ?(declare = false)
    ?(debug = false)
    ?(show_error_traces = false)
    ?(log_identifier = "")
    () =
  {
    start_time;
    infer;
    recursive_infer;
    analyze;
    parallel;
    number_of_workers;
    source_root;
    sections;
    debug;
    project_root;
    stub_roots;
    verbose;
    version;
    strict;
    declare;
    show_error_traces;
    log_identifier;
  }


let localize ({ debug; strict; _ } as configuration) ~local_debug ~local_strict ~declare =
  {
    configuration with
    debug = debug || local_debug;
    strict = strict || local_strict;
    declare;
  }


let pyre_root { source_root; _ } =
  Path.append source_root ~element:".pyre"
