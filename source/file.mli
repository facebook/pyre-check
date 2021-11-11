(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre

type t [@@deriving show, hash]

val create : ?content:string -> Path.t -> t

val path : t -> Path.t

val content : t -> string option

val content_exn : t -> string

val lines : t -> string list option

val lines_exn : t -> string list

val hash : t -> int option

val write : t -> unit

val append : lines:string list -> Path.t -> unit

module Set : Set.S with type Elt.t = t
