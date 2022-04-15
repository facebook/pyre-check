(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Sexplib.Conv

module T = struct
  type t = string [@@deriving compare, sexp, hash, to_yojson, show]
end

include T
module Map = Map.Make (T)
module SerializableMap = Data_structures.SerializableMap.Make (T)
module Set = Set.Make (T)
include Hashable.Make (T)

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


let is_sanitized name = String.is_prefix ~prefix:"$" name

let equal = [%compare.equal: t]

let equal_sanitized left right = equal (sanitized left) (sanitized right)

let pp_sanitized format identifier = Format.fprintf format "%a" String.pp (sanitized identifier)

let remove_leading_underscores name =
  let renaming_pattern = Str.regexp "\\(\\$.*\\$\\)_+" in
  Str.global_replace renaming_pattern "\\1" name
