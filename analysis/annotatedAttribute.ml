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

type t = attribute Node.t [@@deriving eq, show, sexp, compare]

let create
    ~abstract
    ~annotation
    ~original_annotation
    ~async
    ~class_attribute
    ~defined
    ~initialized
    ~name
    ~parent
    ~visibility
    ~property
    ~static
    ~value
    ~location
  =
  {
    Node.location;
    value =
      {
        abstract;
        annotation;
        original_annotation;
        async;
        class_attribute;
        defined;
        initialized;
        name;
        parent;
        visibility;
        property;
        static;
        value;
      };
  }


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

let visibility { Node.value = { visibility; _ }; _ } = visibility

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


let with_value { Node.location; value = attribute } ~value =
  { Node.location; value = { attribute with value } }


let with_location attribute ~location = { attribute with Node.location }
