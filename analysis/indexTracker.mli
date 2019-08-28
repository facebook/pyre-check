open Core

type t [@@deriving compare, eq, sexp, show]

module IndexKey : Memory.KeyType with type t = t and type out = t

module Set : Set.S with type Elt.t = t

include Hashable with type t := t

val index : string -> t

val annotation : t -> string
