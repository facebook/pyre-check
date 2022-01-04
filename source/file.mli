(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

type t [@@deriving show, hash]

val create : ?content:string -> PyrePath.t -> t

val path : t -> PyrePath.t

val content : t -> string option

val content_exn : t -> string

val lines : t -> string list option

val lines_exn : t -> string list

val hash : t -> int option

val write : t -> unit

val append : lines:string list -> PyrePath.t -> unit

module Set : Set.S with type Elt.t = t
