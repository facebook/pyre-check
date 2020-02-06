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

type 'a t = {
  payload: 'a;
  abstract: bool;
  async: bool;
  class_attribute: bool;
  defined: bool;
  initialized: bool;
  name: Identifier.t;
  parent: Type.Primitive.t;
  visibility: visibility;
  property: bool;
  static: bool;
  has_ellipsis_value: bool;
}
[@@deriving eq, show, compare, sexp]

type instantiated_annotation = {
  annotation: Type.t;
  original_annotation: Type.t;
}
[@@deriving eq, show, compare, sexp]

type instantiated = instantiated_annotation t [@@deriving eq, show, compare, sexp]

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
    ~has_ellipsis_value
  =
  {
    payload = { annotation; original_annotation };
    abstract;
    async;
    class_attribute;
    defined;
    initialized;
    name;
    parent;
    visibility;
    property;
    static;
    has_ellipsis_value;
  }


let create_uninstantiated
    ~abstract
    ~uninstantiated_annotation
    ~async
    ~class_attribute
    ~defined
    ~initialized
    ~name
    ~parent
    ~visibility
    ~property
    ~static
    ~has_ellipsis_value
  =
  {
    payload = uninstantiated_annotation;
    abstract;
    async;
    class_attribute;
    defined;
    initialized;
    name;
    parent;
    visibility;
    property;
    static;
    has_ellipsis_value;
  }


let annotation { payload = { annotation; original_annotation }; async; defined; visibility; _ } =
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
      Annotation.Immutable { original; final }
    else
      (* We need to distinguish between unannotated attributes and non-existent ones - ensure that
         the annotation is viewed as mutable to distinguish from user-defined globals. *)
      Annotation.Mutable
  in
  { Annotation.annotation; mutability }


let uninstantiated_annotation { payload; _ } = payload

let name { name; _ } = name

let parent { parent; _ } = parent

let initialized { initialized; _ } = initialized

let defined { defined; _ } = defined

let class_attribute { class_attribute; _ } = class_attribute

let abstract { abstract; _ } = abstract

let async { async; _ } = async

let static { static; _ } = static

let property { property; _ } = property

let visibility { visibility; _ } = visibility

let has_ellipsis_value { has_ellipsis_value; _ } = has_ellipsis_value

let instantiate attribute ~annotation ~original_annotation =
  { attribute with payload = { annotation; original_annotation } }


let ignore_callable_define_locations ({ payload = { annotation; original_annotation }; _ } as value)
  =
  let remove =
    let constraints = function
      | Type.Callable ({ implementation; overloads; _ } as callable) ->
          let callable =
            let remove callable = { callable with Type.Callable.define_location = None } in
            {
              callable with
              implementation = remove implementation;
              overloads = List.map overloads ~f:remove;
            }
          in
          Some (Type.Callable callable)
      | _ -> None
    in
    Type.instantiate ~constraints
  in

  let payload =
    { annotation = remove annotation; original_annotation = remove original_annotation }
  in
  { value with payload }
