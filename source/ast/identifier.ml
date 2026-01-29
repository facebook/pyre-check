(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

open Core
open Sexplib.Conv

module T = struct
  type t = string [@@deriving compare, sexp, hash, yojson, show]
end

include T

module Map = struct
  include Map.Make (T)

  module Tree = Map.Make_tree (struct
    include T
    include Comparator.Make (T)
  end)
end

module SerializableMap = Data_structures.SerializableMap.Make (T)
module SerializableSet = Data_structures.SerializableSet.Make (T)
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

let pp_sanitized format identifier = Format.fprintf format "%s" (sanitized identifier)

let remove_leading_underscores name =
  let renaming_pattern = Str.regexp "\\(\\$.*\\$\\)_+" in
  Str.global_replace renaming_pattern "\\1" name


let is_private_name name =
  String.is_prefix ~prefix:"__" name && not (String.is_suffix ~suffix:"__" name)


let mangle_private_name ~class_name identifier =
  let class_name = String.lstrip ~drop:(fun character -> Char.equal character '_') class_name in
  "_" ^ class_name ^ identifier


let keywords =
  Set.of_list
    [
      "and";
      "as";
      "assert";
      "async";
      "await";
      "break";
      "case";
      "class";
      "continue";
      "__debug__";
      "def";
      "del";
      "elif";
      "else";
      "except";
      "finally";
      "for";
      "from";
      "global";
      "if";
      "import";
      "in";
      "is";
      "lambda";
      "match";
      "nonlocal";
      "not";
      "or";
      "pass";
      "raise";
      "return";
      "try";
      "type";
      "while";
      "with";
      "yield";
      "False";
      "None";
      "True";
    ]


let is_valid_identifier name =
  let re = Str.regexp "^[a-zA-Z_][a-zA-Z0-9_]*$" in
  Str.string_match re name 0 && not (Core.Set.mem keywords name)
