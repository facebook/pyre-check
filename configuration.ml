(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Pyre


type t = {
  infer: bool;
  recursive_infer: bool;
  parallel: bool;
  project_root: Path.t;
  sections: string list;
  debug: bool;
  stub_roots: Path.t list;
  verbose: bool;
  version: string option;
  strict: bool;
  declare: bool;
  show_error_traces: bool;
}
[@@deriving show]


let create
    ?(infer = false)
    ?(recursive_infer = false)
    ?(parallel = true)
    ?(project_root = Path.current_working_directory ())
    ?(sections = [])
    ?(stub_roots = [])
    ?(verbose = false)
    ?version
    ?(strict = false)
    ?(declare = false)
    ?(debug = false)
    ?(show_error_traces = false)
    () =
  {
    infer;
    recursive_infer;
    parallel;
    project_root;
    sections;
    debug;
    stub_roots;
    verbose;
    version;
    strict;
    declare;
    show_error_traces
  }

let localize ({ debug; _ } as configuration) ~local_debug ~strict ~declare =
  {
    configuration with
    debug = debug || local_debug;
    strict;
    declare;
  }


let pyre_root { project_root; _ } =
  Path.append project_root ~element:".pyre"
