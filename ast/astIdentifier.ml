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


let sanitize name =
  let stars, name =
    if String.is_prefix name ~prefix:"**" then
      "**", String.drop_prefix name 2
    else if String.is_prefix name ~prefix:"*" then
      "*", String.drop_prefix name 1
    else
      "", name
  in
  let renaming_pattern = Str.regexp "^\\$[a-zA-Z]*_\\([0-9]+_\\)?" in
  Str.global_replace renaming_pattern "" name
  |> Format.asprintf "%s%s" stars


let pp_sanitized format name =
  sanitize name
  |> Format.fprintf format "%s"


let show_sanitized name =
  sanitize name


let remove_leading_underscores name =
  let renaming_pattern = Str.regexp "\\(\\$[a-zA-Z]*_\\)_+" in
  Str.global_replace renaming_pattern "\\1" name


let length =
  String.length

let append ~separator identifier other =
  identifier ^ separator ^ other

let add_prefix ~prefix identifier =
  prefix ^ identifier

let remove_prefix ~prefix identifier =
  String.chop_prefix ~prefix identifier
  |> Option.value ~default:identifier
