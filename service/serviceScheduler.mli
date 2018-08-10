(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


open Hack_parallel.Std

type t

val create: configuration: Configuration.t -> ?bucket_multiplier: int -> unit -> t
val initialize_process: configuration: Configuration.t -> unit

val map_reduce
  :  t
  -> ?bucket_size: int
  -> configuration: Configuration.t
  -> init:'a
  -> map:('a -> 'b list -> 'c)
  -> reduce:('c -> 'a -> 'a)
  -> 'b list
  -> 'a

val iter: t -> configuration: Configuration.t -> f: ('a list -> unit) -> 'a list -> unit

val single_job : t -> f:('a -> 'b) -> 'a -> 'b

val is_parallel : t -> bool

val with_parallel: is_parallel: bool -> t -> t

val workers: t -> Worker.t list

val mock: unit -> t

val destroy : t -> unit
