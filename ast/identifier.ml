(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Sexplib.Conv

type t = string [@@deriving compare, eq, sexp, hash, to_yojson]

module Map = Map.Make (struct
  type nonrec t = t

  let compare = compare

  let sexp_of_t = sexp_of_t

  let t_of_sexp = t_of_sexp
end)

module SerializableMap = SerializableMap.Make (struct
  type nonrec t = t

  let compare = compare
end)

module Set = Set.Make (struct
  type nonrec t = t

  let compare = compare

  let sexp_of_t = sexp_of_t

  let t_of_sexp = t_of_sexp
end)

include Hashable.Make (struct
  type nonrec t = t

  let compare = compare

  let hash = hash

  let hash_fold_t = hash_fold_t

  let sexp_of_t = sexp_of_t

  let t_of_sexp = t_of_sexp
end)

let pp format identifier = Format.fprintf format "%a" String.pp identifier

let split_star name =
  if String.is_prefix name ~prefix:"**" then
    "**", String.drop_prefix name 2
  else if String.is_prefix name ~prefix:"*" then
    "*", String.drop_prefix name 1
  else
    "", name


let sanitized name =
  let stars, name = split_star name in
  let name =
    match String.is_prefix name ~prefix:"$", String.rindex name '$' with
    | true, Some index when index > 0 -> String.drop_prefix name (index + 1)
    | _ -> name
  in
  stars ^ name


let equal_sanitized left right = equal (sanitized left) (sanitized right)

let pp_sanitized format identifier = Format.fprintf format "%a" String.pp (sanitized identifier)

let remove_leading_underscores name =
  let renaming_pattern = Str.regexp "\\(\\$.*\\$\\)_+" in
  Str.global_replace renaming_pattern "\\1" name
