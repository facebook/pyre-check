(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open Sexplib.Conv

type t = string
[@@deriving compare, eq, sexp, hash]


module Map = Map.Make(struct
    type nonrec t = t
    let compare = compare
    let sexp_of_t = sexp_of_t
    let t_of_sexp = t_of_sexp
  end)


module Set = Set.Make(struct
    type nonrec t = t
    let compare = compare
    let sexp_of_t = sexp_of_t
    let t_of_sexp = t_of_sexp
  end)


let pp format identifier =
  Format.fprintf format "`%a`" String.pp identifier

let create name = name
let show name = name

let show_sanitized name =
  let renaming_pattern = Str.regexp "^\\$[a-zA-Z]*_" in
  Str.global_replace renaming_pattern "" name


let length =
  String.length

let append ~separator identifier other =
  identifier ^ separator ^ other

let add_prefix ~prefix identifier =
  prefix ^ identifier

let remove_prefix ~prefix identifier =
  String.chop_prefix ~prefix identifier
  |> Option.value ~default:identifier
