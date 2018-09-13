(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Pyre


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
[@@deriving show, eq]

val create
  :  ?start_time: float
  -> ?infer:bool
  -> ?recursive_infer:bool
  -> ?parallel: bool
  -> ?filter_directories: Path.t list
  -> ?number_of_workers: int
  -> ?local_root:Path.t
  -> ?sections:string list
  -> ?project_root:Path.t
  -> ?search_path: Path.t list
  -> ?typeshed: Path.t
  -> ?verbose:bool
  -> ?expected_version:string
  -> ?strict:bool
  -> ?declare:bool
  -> ?debug:bool
  -> ?show_error_traces:bool
  -> ?log_identifier:string
  -> ?logger:string
  -> unit
  -> t

val set_global: t -> unit
val get_global: unit -> t option

val localize: t -> local_debug:bool -> local_strict:bool -> declare:bool -> t

val pyre_root: t -> Path.t
