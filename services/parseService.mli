(** Copyright 2016-present Facebook. All rights reserved. **)

open Pyre

val parse_stubs: Service.t -> roots: Path.t list -> File.Handle.t list

val parse_sources: Service.t -> root: Path.t -> File.Handle.t list

val parse_sources_list
  : Service.t
  -> File.t list
  -> root: Path.t
  -> File.Handle.t list * (int * int)
