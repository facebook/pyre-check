(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Pyre
open Ast
open Statement
open Expression


type define_filter = {
  name: string option;
  decorator: string option;
  docstring: string option;
  parent: string option;
}


let create_define_filter
    ?(name = None)
    ?(decorator = None)
    ?(docstring = None)
    ?(parent = None)
    () =
  {
    name;
    decorator;
    docstring;
    parent;
  }


let apply ~f argument statements =
  argument
  >>| f statements
  |> Option.value ~default:statements


let filter_defines ~define_filter:{ name; decorator; docstring; parent } statements =
  let is_define = function
    | { Node.location; Node.value = Define origin } ->
        Some (Node.create ~location origin)
    | _ ->
        None
  in
  let filter_defines_by_name defines name =
    let has_name { Node.value = { Define.name = define_name; _ }; _ } =
      Access.equal define_name (Access.create name)
    in
    List.filter ~f:has_name defines
  in
  let filter_defines_by_decorator defines decorator =
    List.filter ~f:(fun { Node.value; _ } -> Define.has_decorator value decorator) defines
  in
  let filter_defines_by_docstring defines docstring =
    let has_docstring { Node.value = { Define.docstring = define_docstring; _ }; _ } =
      define_docstring
      >>| String.equal docstring
      |> Option.value ~default:false
    in
    List.filter ~f:has_docstring defines
  in
  let filter_defines_by_parent defines parent =
    let has_parent { Node.value = { Define.parent = define_parent; _ }; _ } =
      define_parent
      >>| Access.equal (Access.create parent)
      |> Option.value ~default:false
    in
    List.filter ~f:has_parent defines
  in
  List.filter_map ~f:is_define statements
  |> apply ~f:filter_defines_by_name name
  |> apply ~f:filter_defines_by_decorator decorator
  |> apply ~f:filter_defines_by_docstring docstring
  |> apply ~f:filter_defines_by_parent parent


type class_filter = {
  name: string option;
  decorator: string option;
  docstring: string option;
  bases: string list option;
  define_filter: define_filter option;
}


let create_class_filter
    ?(name = None)
    ?(bases = None)
    ?(decorator = None)
    ?(docstring = None)
    ?(define_filter = None)
    () =
  {
    name;
    decorator;
    docstring;
    bases;
    define_filter;
  }


let filter_classes
    ~class_filter:{ name; decorator; docstring; bases; define_filter }
    statements =
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
  let filter_classes_by_method classes define_filter =
    let has_method { Node.value = { Class.body; _ }; _ } =
      not (List.is_empty (filter_defines ~define_filter body))
    in
    List.filter ~f:has_method classes
  in
  List.filter_map ~f:is_class statements
  |> apply ~f:filter_classes_by_name name
  |> apply ~f:filter_classes_by_bases bases
  |> apply ~f:filter_classes_by_decorator decorator
  |> apply ~f:filter_classes_by_docstring docstring
  |> apply ~f:filter_classes_by_method define_filter


type assign_filter = {
  target: string option;
  annotation: string option;
  value_regexp: string option;
  parent: string option;
}


let create_assign_filter
    ?(target = None)
    ?(annotation = None)
    ?(value_regexp = None)
    ?(parent = None)
    () =
  {
    target;
    annotation;
    value_regexp;
    parent;
  }


let filter_assigns ~assign_filter:{ target; annotation; value_regexp; parent } statements =
  let is_assign = function
    | { Node.location; Node.value = Assign origin } ->
        Some (Node.create ~location origin)
    | _ ->
        None
  in
  let filter_assigns_by_target assigns target =
    let has_target { Node.value = { Assign.target = assign_target; _ }; _ } =
      Access.equal (Expression.access assign_target) (Access.create target)
    in
    List.filter ~f:has_target assigns
  in
  let filter_assigns_by_annotation assigns annotation =
    let has_annotatation { Node.value = { Assign.annotation = assign_annotation; _ }; _ } =
      assign_annotation
      >>| Expression.access
      >>| Access.equal (Access.create annotation)
      |> Option.value ~default:false
    in
    List.filter ~f:has_annotatation assigns
  in
  let filter_assigns_by_value_regexp assigns value_regexp =
    let has_regexp { Node.value = { Assign.value; _ }; _ } =
      value
      >>| (fun value -> Str.string_match (Str.regexp value_regexp) (Expression.show value) 0)
      |> Option.value ~default:false
    in
    List.filter ~f:has_regexp assigns
  in
  let filter_assigns_by_parent assigns parent =
    let has_parent { Node.value = { Assign.parent = assign_parent; _ }; _ } =
      assign_parent
      >>| Access.equal (Access.create parent)
      |> Option.value ~default:false
    in
    List.filter ~f:has_parent assigns
  in
  List.filter_map ~f:is_assign statements
  |> apply ~f:filter_assigns_by_target target
  |> apply ~f:filter_assigns_by_annotation annotation
  |> apply ~f:filter_assigns_by_value_regexp value_regexp
  |> apply ~f:filter_assigns_by_parent parent
