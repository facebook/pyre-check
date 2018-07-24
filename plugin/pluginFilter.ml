(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Pyre
open Ast
open Statement
open Expression


type class_filter = {
  name: string option;
  bases: string list option;
  decorator: string option;
  docstring: string option;
}


let create_class_filter
    ?(name = None)
    ?(bases = None)
    ?(decorator = None)
    ?(docstring = None)
    () =
  {
    name;
    bases;
    decorator;
    docstring;
  }


let filter_classes ~class_filter:{ name; bases; decorator; docstring; } statements =
  let is_class = function
    | { Node.location; value = Class origin } ->
        Some (Node.create ~location origin)
    | _ ->
        None
  in
  let filter_classes_by_name classes name =
    let has_name { Node.value = { Class.name = class_name; _ }; _ } =
      Access.equal class_name (Access.create name)
    in
    List.filter ~f:has_name classes
  in
  let filter_classes_by_bases classes bases =
    let has_bases { Node.value = { Class.bases = class_bases; _ }; _ } =
      List.equal
        (List.sort
           ~compare:Access.compare
           (List.map ~f:(fun { Argument.value; _ } -> Expression.access value) class_bases))
        (List.sort ~compare:Access.compare (List.map ~f:Access.create bases))
        ~equal:Access.equal
    in
    List.filter ~f:has_bases classes
  in
  let filter_classes_by_decorator classes decorator =
    List.filter ~f:(fun { Node.value; _ } -> Class.has_decorator value decorator) classes
  in
  let filter_classes_by_docstring classes docstring =
    let has_docstring { Node.value = { Class.docstring = class_docstring; _ }; _ } =
      class_docstring
      >>| String.equal docstring
      |> Option.value ~default:false
    in
    List.filter ~f:has_docstring classes
  in
  let apply ~f argument classes =
    argument
    >>| f classes
    |> Option.value ~default:classes
  in
  List.filter_map ~f:is_class statements
  |> apply ~f:filter_classes_by_name name
  |> apply ~f:filter_classes_by_bases bases
  |> apply ~f:filter_classes_by_decorator decorator
  |> apply ~f:filter_classes_by_docstring docstring
