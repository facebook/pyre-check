(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre

type t = Unix.File_descr.t

module Map : Map.S with type Key.t = t

val initialize_unix_socket : Path.t -> t

val open_connection : Path.t -> [ `Failure | `Success of in_channel * out_channel ]

val write : t -> 'a -> unit

val write_ignoring_epipe : t -> 'a -> unit

val read : t -> 'a
(** From channel without buffering *)
