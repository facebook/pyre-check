(** Copyright 2016-present Facebook. All rights reserved. **)

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
