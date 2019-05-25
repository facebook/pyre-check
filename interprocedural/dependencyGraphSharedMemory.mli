(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast

val add_overriding_types : member:Reference.t -> subtypes:Reference.t list -> unit

val get_overriding_types : member:Reference.t -> Reference.t list option

val overrides_exist : Reference.t -> bool
