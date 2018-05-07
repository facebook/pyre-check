(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast


val get_source: File.Handle.t -> Source.t option

val add_source: File.Handle.t -> Source.t -> unit

val remove_paths: File.Handle.t list -> unit
