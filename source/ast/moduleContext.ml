(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Represent an identifiale local context within a given module *)

open Base

type t =
  | TopLevel
  | Function of {
      parent: t;
      name: string;
    }
  | Class of {
      parent: t;
      name: string;
    }
[@@deriving sexp, compare, equal, hash, show, to_yojson]

let create_toplevel () = TopLevel

let create_function ~parent name = Function { parent; name }

let create_class ~parent name = Class { parent; name }

let is_toplevel = function
  | TopLevel -> true
  | Function _
  | Class _ ->
      false


let is_function = function
  | Function _ -> true
  | TopLevel
  | Class _ ->
      false


let is_class = function
  | Class _ -> true
  | TopLevel
  | Function _ ->
      false


let to_qualifier ~module_name context =
  let rec convert ~sofar = function
    | TopLevel -> sofar
    | Function { parent; name }
    | Class { parent; name } ->
        convert ~sofar:(name :: sofar) parent
  in
  let local_names = convert ~sofar:[] context in
  Reference.combine module_name (Reference.create_from_list local_names)
