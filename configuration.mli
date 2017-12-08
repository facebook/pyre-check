(** Copyright 2016-present Facebook. All rights reserved. **)

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

val create
  :  ?gradual:bool
  -> ?infer:bool
  -> ?recursive_infer:bool
  -> ?parallel:bool
  -> ?project_root:Path.t
  -> ?sections:string list
  -> ?stub_roots: Path.t list
  -> ?verbose:bool
  -> ?strict:bool
  -> ?declare:bool
  -> ?debug:bool
  -> ?show_error_traces:bool
  -> unit
  -> t

val localize: t -> local_debug:bool -> strict:bool -> declare:bool -> t

val apply_if: condition:bool -> f:('a -> 'a) -> 'a -> 'a

val pyre_root: t -> Path.t
