(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base
module Path = Pyre.Path

type t =
  | BaseName of string
  | Extension of string
  | FullPath of Path.t
[@@deriving sexp, compare, hash]

let of_yojson = function
  | `Assoc [("base_name", `String name)] -> Result.Ok (BaseName name)
  | `Assoc [("extension", `String name)] -> Result.Ok (Extension name)
  | `Assoc [("full_path", `String path)] -> Result.Ok (FullPath (Path.create_absolute path))
  | _ as json ->
      let message =
        Format.sprintf "Malformed critical file JSON: %s" (Yojson.Safe.to_string json)
      in
      Result.Error message


let to_yojson = function
  | BaseName name -> `Assoc ["base_name", `String name]
  | Extension name -> `Assoc ["extension", `String name]
  | FullPath path -> `Assoc ["full_path", `String (Path.absolute path)]


let matches ~path = function
  | BaseName expect_name ->
      let actual_name = Path.last path in
      String.equal expect_name actual_name
  | Extension extension ->
      let actual_name = Path.last path in
      String.is_suffix actual_name ~suffix:("." ^ extension)
  | FullPath expect_path -> Path.equal expect_path path


let matches_any ~path patterns = List.exists patterns ~f:(matches ~path)

let find ~within patterns = List.find within ~f:(fun path -> matches_any ~path patterns)
