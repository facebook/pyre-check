(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Yojson.Safe.Util

let with_default ~extract ~extract_optional ?default json =
  match default with
  | None -> extract json
  | Some default -> extract_optional json |> Option.value ~default


let to_bool_with_default = with_default ~extract:to_bool ~extract_optional:to_bool_option

let to_int_with_default = with_default ~extract:to_int ~extract_optional:to_int_option

let to_string_with_default = with_default ~extract:to_string ~extract_optional:to_string_option

let to_path json = to_string json |> PyrePath.create_absolute

(* The absent of explicit `~default` parameter means that the corresponding JSON field is
   mandantory. *)
let bool_member ?default name json = member name json |> to_bool_with_default ?default

let int_member ?default name json = member name json |> to_int_with_default ?default

let string_member ?default name json = member name json |> to_string_with_default ?default

let optional_member ~f name json =
  member name json
  |> function
  | `Null -> None
  | _ as element -> Some (f element)


let optional_string_member = optional_member ~f:to_string

let optional_int_member = optional_member ~f:to_int

let path_member name json = member name json |> to_path

let optional_path_member = optional_member ~f:to_path

let list_member ?default ~f name json =
  member name json
  |> fun element ->
  match element, default with
  | `Null, Some default -> default
  | _, _ -> convert_each f element


let string_list_member = list_member ~f:to_string

let path_list_member = list_member ~f:to_path
