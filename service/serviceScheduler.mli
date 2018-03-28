(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

type t

module Memory: sig
  (* Between 0.0 and 1.0 *)
  val heap_use_ratio: unit -> float

  val slot_use_ratio: unit -> float
end

val create : is_parallel:bool -> ?bucket_multiplier: int -> unit -> t

val map_reduce:
  t ->
  init:'a ->
  map:('a -> 'b list -> 'c) ->
  reduce:('c -> 'a -> 'a) ->
  'b list ->
  'a

val iter: t -> f: ('a -> unit) -> 'a list -> unit

val single_job : t -> f:('a -> 'b) -> 'a -> 'b

val is_parallel : t -> bool

val with_parallel: is_parallel: bool -> t -> t

val mock: unit -> t

val destroy : t -> unit
