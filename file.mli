(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre

type t [@@deriving eq, show, hash]

val create : ?content:string -> Path.t -> t

val path : t -> Path.t

val content : t -> string option

val lines : t -> string list option

val hash : t -> int option

val write : t -> unit

val append : lines:string list -> Path.t -> unit

module Set : Set.S with type Elt.t = t
