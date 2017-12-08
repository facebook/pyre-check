(** Copyright 2016-present Facebook. All rights reserved. **)

open Core

open Pyre


type t = {
  gradual: bool;
  infer: bool;
  recursive_infer: bool;
  parallel: bool;
  project_root: Path.t;
  sections: string list;
  debug: bool;
  stub_roots: Path.t list;
  verbose: bool;
  strict: bool;
  declare: bool;
  show_error_traces: bool;
}


let create
    ?(gradual = false)
    ?(infer = false)
    ?(recursive_infer = false)
    ?(parallel = true)
    ?(project_root = Path.current_working_directory ())
    ?(sections = [])
    ?(stub_roots = [])
    ?(verbose = false)
    ?(strict = false)
    ?(declare = false)
    ?(debug = false)
    ?(show_error_traces = false)
    () =
  {
    gradual;
    infer;
    recursive_infer;
    parallel;
    project_root;
    sections;
    debug;
    stub_roots;
    verbose;
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


let apply_if ~condition ~f argument =
  if condition then
    f argument
  else
    argument


let pyre_root { project_root; _ } =
  Path.append project_root ~element:".pyre"
