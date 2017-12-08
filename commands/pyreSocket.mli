(** Copyright 2016-present Facebook. All rights reserved. **)

open Pyre


type t = Unix.file_descr

val initialize_unix_socket: Path.t -> t

val open_connection: Path.t -> [ `Failure | `Success of in_channel * out_channel ]

val write: t -> 'a -> unit

(** From channel without buffering *)
val read: t -> 'a
