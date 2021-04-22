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

  let of_alist_implementation ~add items =
    let result =
      let size = List.length items in
      Hashtbl.create (module String) ~size
    in
    let add_mapping (key, value) =
      if String.is_suffix key ~suffix:".py" or String.is_suffix key ~suffix:".pyi" then
        add result ~key ~data:value
    in
    List.iter items ~f:add_mapping;
    result


  let of_alist_exn items = of_alist_implementation items ~add:Hashtbl.add_exn

  let of_alist_ignoring_duplicates items =
    let add table ~key ~data =
      (* First entry wins. *)
      Hashtbl.add table ~key ~data |> ignore
    in
    of_alist_implementation items ~add


  let empty : t = of_alist_exn []

  let of_json_exn_ignoring_duplicates json =
    let open Yojson.Safe.Util in
    let sources = member "sources" json |> to_assoc in
    let dependencies = member "dependencies" json |> to_assoc in
    List.append sources dependencies
    |> List.map ~f:(fun (key, value) -> key, to_string value)
    |> of_alist_ignoring_duplicates


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

module Indexed = struct
  type t = {
    lookup_source: string -> string option;
    lookup_artifact: string -> string list;
  }

  let lookup_source { lookup_source; _ } = lookup_source

  let lookup_artifact { lookup_artifact; _ } = lookup_artifact
end

module Difference = struct
  module Kind = struct
    type t =
      | New of string
      | Deleted
      | Changed of string
    [@@deriving sexp, compare]
  end

  type t = Kind.t Hashtbl.M(String).t [@@deriving sexp]

  let of_alist_exn items = Hashtbl.of_alist_exn (module String) items

  let to_alist = Hashtbl.to_alist
end

type t = { artifact_to_source: Partial.t }

let create artifact_to_source = { artifact_to_source }

let index { artifact_to_source } =
  let source_to_artifact =
    (* Delay source-to-artifact map computation till the first invocation of `lookup_artifact`. *)
    lazy
      (let result =
         let size = Hashtbl.length artifact_to_source in
         Hashtbl.create (module String) ~size
       in
       let add_item ~key ~data =
         Hashtbl.update result data ~f:(function
             | None -> [key]
             | Some values -> key :: values)
       in
       Hashtbl.iteri artifact_to_source ~f:add_item;
       result)
  in
  let lookup_source = Hashtbl.find artifact_to_source in
  let lookup_artifact source =
    let source_to_artifact = Lazy.force source_to_artifact in
    Hashtbl.find source_to_artifact source |> Option.value ~default:[]
  in
  { Indexed.lookup_source; lookup_artifact }


let to_alist { artifact_to_source } = Hashtbl.to_alist artifact_to_source

let difference ~original:{ artifact_to_source = original } { artifact_to_source = current } =
  let result = Hashtbl.create (module String) in
  let scan_original_item ~key ~data =
    match Hashtbl.find current key with
    | None ->
        (* The key exists in the original map but not in the current one. *)
        Hashtbl.set result ~key ~data:Difference.Kind.Deleted
    | Some current_data ->
        (* The key exists in both the original and the current map but the corresponding values are
           different. *)
        if not (String.equal data current_data) then
          Hashtbl.set result ~key ~data:(Difference.Kind.Changed current_data)
  in
  let scan_current_item ~key ~data =
    match Hashtbl.find original key with
    | None ->
        (* The key exists in the current map but not in the original one. *)
        Hashtbl.set result ~key ~data:(Difference.Kind.New data)
    | Some _ ->
        (* We've already handled this case in `scan_original_item`. *)
        ()
  in
  Hashtbl.iteri original ~f:scan_original_item;
  Hashtbl.iteri current ~f:scan_current_item;
  result
