(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Statement


val add_overriding_types
  :  member: Access.t
  -> subtypes: Access.t list
  -> unit

val get_overriding_types
  :  member: Access.t
  -> Access.t list option

val overrides_exist: Access.t -> bool
