(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast

type property =
  | ReadOnly
  | ReadWrite
[@@deriving eq, show]

type attribute = {
  annotation: Annotation.t;
  async: bool;
  class_attribute: bool;
  defined: bool;
  final: bool;
  initialized: bool;
  name: Identifier.t;
  parent: Type.t;
  property: property option;
  static: bool;
  value: Expression.t;
}
[@@deriving eq, show]

type t = attribute Node.t [@@deriving eq, show]

let name { Node.value = { name; _ }; _ } = name

let annotation { Node.value = { annotation; async; _ }; _ } =
  if async then
    Annotation.annotation annotation |> Type.awaitable |> Annotation.create
  else
    annotation


let parent { Node.value = { parent; _ }; _ } = parent

let value { Node.value = { value; _ }; _ } = value

let initialized { Node.value = { initialized; _ }; _ } = initialized

let location { Node.location; _ } = location

let defined { Node.value = { defined; _ }; _ } = defined

let class_attribute { Node.value = { class_attribute; _ }; _ } = class_attribute

let async { Node.value = { async; _ }; _ } = async

let final { Node.value = { final; _ }; _ } = final

let static { Node.value = { static; _ }; _ } = static

let property { Node.value = { property; _ }; _ } = property

let instantiate ({ Node.value = { annotation; _ } as attribute; _ } as attribute_node) ~constraints
  =
  {
    attribute_node with
    Node.value = { attribute with annotation = Annotation.instantiate annotation ~constraints };
  }


module Table = struct
  type element = t

  type t = {
    attributes: element String.Table.t;
    names: string list ref;
  }

  let create () = { attributes = String.Table.create (); names = ref [] }

  let add { attributes; names } ({ Node.value = { name; _ }; _ } as attribute) =
    match Hashtbl.add attributes ~key:name ~data:attribute with
    | `Ok -> names := name :: !names
    | `Duplicate -> ()


  let lookup_name { attributes; _ } = Hashtbl.find attributes

  let to_list { attributes; names } = List.rev_map !names ~f:(Hashtbl.find_exn attributes)

  let clear { attributes; names } =
    Hashtbl.clear attributes;
    names := []


  let filter_map ~f table =
    let add_attribute attribute = Option.iter (f attribute) ~f:(add table) in
    let attributes = to_list table in
    clear table;
    List.iter attributes ~f:add_attribute
end
