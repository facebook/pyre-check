(** Copyright 2016-present Facebook. All rights reserved. **)

open Core
open Sexplib.Conv

type t = string
[@@deriving compare, eq, sexp]


module Map = Map.Make(struct
    type nonrec t = t
    let compare = compare
    let sexp_of_t = sexp_of_t
    let t_of_sexp = t_of_sexp
  end)


let pp format identifier =
  Format.fprintf format "`%a`" String.pp identifier

let create name = name
let show name = name


let length =
  String.length

let append ~separator identifier other =
  identifier ^ separator ^ other
