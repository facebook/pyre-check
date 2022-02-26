(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

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

type initialized =
  | OnClass
  | OnlyOnInstance
  | NotInitialized
[@@deriving eq, show, compare, sexp]

type invalid_decorator_reason =
  | CouldNotResolve
  | CouldNotResolveArgument of { argument_index: int }
  | NonCallableDecoratorFactory of Type.t
  | NonCallableDecorator of Type.t
  | FactorySignatureSelectionFailed of {
      reason: SignatureSelectionTypes.reason option;
      callable: Type.Callable.t;
    }
  | ApplicationFailed of {
      callable: Type.Callable.t;
      reason: SignatureSelectionTypes.reason option;
    }
[@@deriving eq, show, compare, sexp]

type problem =
  | DifferingDecorators of { offender: Type.t Type.Callable.overload }
  | InvalidDecorator of {
      index: int;
      reason: invalid_decorator_reason;
    }
[@@deriving eq, show, compare, sexp]

type 'a t = {
  payload: 'a;
  abstract: bool;
  async_property: bool;
  class_variable: bool;
  defined: bool;
  initialized: initialized;
  name: Identifier.t;
  parent: Type.Primitive.t;
  visibility: visibility;
  property: bool;
  undecorated_signature: Type.Callable.t option;
  problem: problem option;
}
[@@deriving eq, show, compare, sexp]

type instantiated_annotation = {
  annotation: Type.t;
  original_annotation: Type.t;
  uninstantiated_annotation: Type.t option;
}
[@@deriving eq, show, compare, sexp]

type instantiated = instantiated_annotation t [@@deriving eq, show, compare, sexp]

let create
    ~abstract
    ~annotation
    ~original_annotation
    ~async_property
    ~class_variable
    ~defined
    ~initialized
    ~name
    ~parent
    ~visibility
    ~property
    ~uninstantiated_annotation
    ~undecorated_signature
    ~problem
  =
  {
    payload = { annotation; original_annotation; uninstantiated_annotation };
    abstract;
    async_property;
    class_variable;
    defined;
    initialized;
    name;
    parent;
    visibility;
    property;
    undecorated_signature;
    problem;
  }


let create_uninstantiated
    ~abstract
    ~uninstantiated_annotation
    ~async_property
    ~class_variable
    ~defined
    ~initialized
    ~name
    ~parent
    ~visibility
    ~property
    ~undecorated_signature
    ~problem
  =
  {
    payload = uninstantiated_annotation;
    abstract;
    async_property;
    class_variable;
    defined;
    initialized;
    name;
    parent;
    visibility;
    property;
    undecorated_signature;
    problem;
  }


let annotation
    { payload = { annotation; original_annotation; _ }; async_property; defined; visibility; _ }
  =
  let annotation, original =
    if async_property then
      let coroutine annotation =
        Type.coroutine [Single Type.Any; Single Type.Any; Single annotation]
      in
      coroutine annotation, coroutine original_annotation
    else
      annotation, original_annotation
  in
  if defined then
    let final =
      match visibility with
      | ReadOnly _ -> true
      | ReadWrite -> false
    in
    Annotation.create_immutable ~original:(Some original) ~final annotation
  else
    (* We need to distinguish between unannotated attributes and non-existent ones - ensure that the
       annotation is viewed as mutable to distinguish from user-defined globals. *)
    Annotation.create_mutable annotation


let uninstantiated_annotation { payload; _ } = payload

let with_uninstantiated_annotation ~uninstantiated_annotation attribute =
  { attribute with payload = uninstantiated_annotation }


let with_undecorated_signature attribute ~undecorated_signature =
  { attribute with undecorated_signature }


let name { name; _ } = name

let parent { parent; _ } = parent

let parent_name { parent; _ } =
  let type_name = Type.primitive_name (Type.Primitive parent) in
  Option.value_exn type_name


let parent_prefix attribute =
  parent_name attribute |> Reference.create |> Reference.last |> fun name -> "_" ^ name


let initialized { initialized; _ } = initialized

let defined { defined; _ } = defined

let class_variable { class_variable; _ } = class_variable

let abstract { abstract; _ } = abstract

let async_property { async_property; _ } = async_property

let static { payload = { uninstantiated_annotation; _ }; _ } =
  match uninstantiated_annotation with
  | Some (Type.Parametric { name = "typing.StaticMethod"; _ }) -> true
  | _ -> false


let property { property; _ } = property

let visibility { visibility; _ } = visibility

let undecorated_signature { undecorated_signature; _ } = undecorated_signature

let problem { problem; _ } = problem

let is_private ({ name; _ } as attribute) =
  let parent_prefix = parent_prefix attribute in
  String.is_prefix ~prefix:(parent_prefix ^ "__") name


let public_name ({ name; _ } as attribute) =
  let parent_prefix = parent_prefix attribute in
  if is_private attribute then
    String.drop_prefix name (String.length parent_prefix)
  else
    name


let is_final { visibility; _ } =
  match visibility with
  | ReadOnly (Refinable _) -> true
  | _ -> false


let instantiate attribute ~annotation ~original_annotation ~uninstantiated_annotation =
  { attribute with payload = { annotation; original_annotation; uninstantiated_annotation } }


let with_initialized attribute ~initialized = { attribute with initialized }
