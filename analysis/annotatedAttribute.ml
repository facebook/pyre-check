(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast

type read_only =
  | Refinable of { overridable: bool }
  | Unrefinable
[@@deriving eq, show, compare, sexp]

type visibility =
  | ReadOnly of read_only
  | ReadWrite
[@@deriving eq, show, compare, sexp]

type attribute = {
  abstract: bool;
  annotation: Type.t;
  original_annotation: Type.t;
  async: bool;
  class_attribute: bool;
  defined: bool;
  initialized: bool;
  name: Identifier.t;
  parent: Type.Primitive.t;
  visibility: visibility;
  property: bool;
  static: bool;
  value: Expression.t;
}
[@@deriving eq, show, compare, sexp]

type t = attribute Node.t [@@deriving eq, show]

let name { Node.value = { name; _ }; _ } = name

let annotation
    { Node.value = { annotation; original_annotation; async; defined; visibility; _ }; _ }
  =
  let annotation, original =
    if async then
      Type.awaitable annotation, Type.awaitable original_annotation
    else
      annotation, original_annotation
  in
  let mutability =
    if defined then
      let final =
        match visibility with
        | ReadOnly _ -> true
        | ReadWrite -> false
      in
      Annotation.Immutable { scope = Global; original; final }
    else
      (* We need to distinguish between unannotated attributes and non-existent ones - ensure that
         the annotation is viewed as mutable to distinguish from user-defined globals. *)
      Annotation.Mutable
  in
  { Annotation.annotation; mutability }


let parent { Node.value = { parent; _ }; _ } = parent

let value { Node.value = { value; _ }; _ } = value

let initialized { Node.value = { initialized; _ }; _ } = initialized

let location { Node.location; _ } = location

let defined { Node.value = { defined; _ }; _ } = defined

let class_attribute { Node.value = { class_attribute; _ }; _ } = class_attribute

let abstract { Node.value = { abstract; _ }; _ } = abstract

let async { Node.value = { async; _ }; _ } = async

let static { Node.value = { static; _ }; _ } = static

let property { Node.value = { property; _ }; _ } = property

let instantiate
    ({ Node.value = { annotation; original_annotation; _ } as attribute; _ } as attribute_node)
    ~constraints
  =
  let instantiate = Type.instantiate ~constraints in
  {
    attribute_node with
    Node.value =
      {
        attribute with
        annotation = instantiate annotation;
        original_annotation = instantiate original_annotation;
      };
  }


module Table = struct
  type element = t [@@deriving compare]

  type table = (string, element) Caml.Hashtbl.t

  type t = {
    attributes: table;
    names: string list ref;
  }

  let create () = { attributes = Caml.Hashtbl.create 15; names = ref [] }

  let add { attributes; names } ({ Node.value = { name; _ }; _ } as attribute) =
    if Caml.Hashtbl.mem attributes name then
      ()
    else (
      Caml.Hashtbl.add attributes name attribute;
      names := name :: !names )


  let lookup_name { attributes; _ } = Caml.Hashtbl.find_opt attributes

  let to_list { attributes; names } = List.rev_map !names ~f:(Caml.Hashtbl.find attributes)

  let names { names; _ } = !names

  let clear { attributes; names } =
    Caml.Hashtbl.clear attributes;
    names := []


  let compare left right = List.compare compare_element (to_list left) (to_list right)

  let filter_map ~f table =
    let add_attribute attribute = Option.iter (f attribute) ~f:(add table) in
    let attributes = to_list table in
    clear table;
    List.iter attributes ~f:add_attribute
end
