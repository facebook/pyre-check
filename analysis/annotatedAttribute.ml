(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast


type attribute = {
  name: Identifier.t;
  parent: Type.t;
  annotation: Annotation.t;
  value: Expression.t;
  defined: bool;
  class_attribute: bool;
  async: bool;
  initialized: bool;
  property: bool;
}
[@@deriving eq, show]


type t = attribute Node.t
[@@deriving eq, show]


let name { Node.value = { name; _ }; _ } =
  name


let annotation { Node.value = { annotation; async; _ }; _ } =
  if async then
    Annotation.annotation annotation
    |> Type.awaitable
    |> Annotation.create
  else
    annotation


let parent { Node.value = { parent; _ }; _ } =
  parent


let value { Node.value = { value; _ }; _ } =
  value


let initialized { Node.value = { initialized; _ }; _ } =
  initialized


let location { Node.location; _ } =
  location


let defined { Node.value = { defined; _ }; _ } =
  defined


let class_attribute { Node.value = { class_attribute; _ }; _ } =
  class_attribute


let async { Node.value = { async; _ }; _ } =
  async


let instantiate
    ({ Node.value = ({ annotation; _ } as attribute); _ } as attribute_node)
    ~constraints =
  {
    attribute_node with
    Node.value = { attribute with annotation = Annotation.instantiate annotation ~constraints }
  }

module Cache = struct
  type t = {
    transitive: bool;
    class_attributes: bool;
    include_generated_attributes: bool;
    name: Reference.t;
    instantiated: Type.t;
  }
  [@@deriving compare, sexp, hash]


  include Hashable.Make(struct
      type nonrec t = t
      let compare = compare
      let hash = Hashtbl.hash
      let hash_fold_t = hash_fold_t
      let sexp_of_t = sexp_of_t
      let t_of_sexp = t_of_sexp
    end)


  let cache: attribute Node.t list Table.t =
    Table.create ~size:1023 ()


  let clear () =
    Table.clear cache
end
