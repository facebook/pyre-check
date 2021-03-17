(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base
module Path = Pyre.Path

module Partial = struct
  type t = string Hashtbl.M(String).t [@@deriving sexp]

  module MergeResult = struct
    module IncompatibleItem = struct
      type t = {
        key: string;
        left_value: string;
        right_value: string;
      }
      [@@deriving sexp, compare]
    end

    type nonrec t =
      | Ok of t
      | Incompatible of IncompatibleItem.t
  end

  let of_alist_exn items =
    let result =
      let size = List.length items in
      Hashtbl.create (module String) ~size
    in
    let add_mapping (key, value) =
      if String.is_suffix key ~suffix:".py" or String.is_suffix key ~suffix:".pyi" then
        Hashtbl.add_exn result ~key ~data:value
    in
    List.iter items ~f:add_mapping;
    result


  let of_json_exn json =
    let open Yojson.Safe.Util in
    let sources = member "sources" json |> to_assoc in
    let dependencies = member "dependencies" json |> to_assoc in
    List.append sources dependencies
    |> List.map ~f:(fun (key, value) -> key, to_string value)
    |> of_alist_exn


  let of_json json =
    try Result.Ok (of_json_exn json) with
    | Yojson.Safe.Util.Type_error (message, _)
    | Yojson.Safe.Util.Undefined (message, _) ->
        Result.Error message
    | other_exception -> Result.Error (Exn.to_string other_exception)


  let of_json_file_exn path =
    let path = Path.absolute path in
    Yojson.Safe.from_file ~fname:path path |> of_json_exn


  let of_json_file path =
    try Result.Ok (of_json_file_exn path) with
    | Yojson.Safe.Util.Type_error (message, _)
    | Yojson.Safe.Util.Undefined (message, _)
    | Sys_error message ->
        Result.Error message
    | other_exception -> Result.Error (Exn.to_string other_exception)


  exception FoundIncompatibleItem of MergeResult.IncompatibleItem.t

  let merge left right =
    try
      let merge_item ~key = function
        | `Left value
        | `Right value ->
            Some value
        | `Both (left_value, right_value) ->
            if String.equal left_value right_value then
              Some left_value
            else
              raise
                (FoundIncompatibleItem { MergeResult.IncompatibleItem.key; left_value; right_value })
      in
      MergeResult.Ok (Hashtbl.merge left right ~f:merge_item)
    with
    | FoundIncompatibleItem item -> MergeResult.Incompatible item


  let to_alist = Hashtbl.to_alist
end
