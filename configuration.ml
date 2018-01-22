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
  report_undefined_attributes: bool;
}
[@@deriving show]


let create
    ?(start_time = Unix.time())
    ?(infer = false)
    ?(recursive_infer = false)
    ?(parallel = true)
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
    ?(report_undefined_attributes = false)
    () =
  {
    start_time;
    infer;
    recursive_infer;
    parallel;
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
    report_undefined_attributes;
  }


let localize ({ debug; _ } as configuration) ~local_debug ~strict ~declare =
  {
    configuration with
    debug = debug || local_debug;
    strict;
    declare;
  }


let pyre_root { source_root; _ } =
  Path.append source_root ~element:".pyre"
