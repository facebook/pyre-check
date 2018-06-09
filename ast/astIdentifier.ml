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


let sanitization_pattern = Str.regexp "^\\$[a-zA-Z]*_\\([0-9]+_\\)?"


let sanitized name =
  let stars, name =
    if String.is_prefix name ~prefix:"**" then
      "**", String.drop_prefix name 2
    else if String.is_prefix name ~prefix:"*" then
      "*", String.drop_prefix name 1
    else
      "", name
  in
  Str.global_replace sanitization_pattern "" name
  |> Format.asprintf "%s%s" stars


let pp_sanitized format name =
  sanitized name
  |> Format.fprintf format "%s"


let show_sanitized name =
  sanitized name


let remove_leading_underscores name =
  let renaming_pattern = Str.regexp "\\(\\$[a-zA-Z]*_\\)_+" in
  Str.global_replace renaming_pattern "\\1" name


let map identifier ~f =
  f identifier
