(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast

val get_by_handle
  :  state: State.t
  -> file: File.t
  -> handle: File.Handle.t
  -> State.lookups_cache_entry option

val evict
  :  state: State.t
  -> configuration: Configuration.Analysis.t
  -> File.t
  -> unit

val find_annotation
  :  state: State.t
  -> configuration: Configuration.Analysis.t
  -> file: File.t
  -> position: Location.position
  -> (Location.Instantiated.t * Type.t) option

val find_all_annotations
  :  state: State.t
  -> configuration: Configuration.Analysis.t
  -> file: File.t
  -> (Location.Instantiated.t * Type.t) list option

val find_definition
   :  state: State.t
  -> configuration: Configuration.Analysis.t
  -> File.t
  -> Location.position
  -> Location.Instantiated.t option
