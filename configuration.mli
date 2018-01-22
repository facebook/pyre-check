(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

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

val create
  :  ?start_time: float
  -> ?infer:bool
  -> ?recursive_infer:bool
  -> ?parallel:bool
  -> ?source_root:Path.t
  -> ?sections:string list
  -> ?project_root:Path.t
  -> ?stub_roots: Path.t list
  -> ?verbose:bool
  -> ?version:string
  -> ?strict:bool
  -> ?declare:bool
  -> ?debug:bool
  -> ?show_error_traces:bool
  -> ?report_undefined_attributes: bool
  -> unit
  -> t

val localize: t -> local_debug:bool -> strict:bool -> declare:bool -> t

val pyre_root: t -> Path.t
