(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

type t

val create : is_parallel:bool -> unit -> t

val map_reduce:
  t ->
  init:'a ->
  map:('a -> 'b list -> 'c) ->
  reduce:('c -> 'a -> 'a) ->
  'b list ->
  'a

val single_job : t -> f:('a -> 'b) -> 'a -> 'b

val is_parallel : t -> bool

val mock: unit -> t

val destroy : t -> unit
