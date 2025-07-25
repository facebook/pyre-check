(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* An AnnotatedAttribute.t represents an attribute for static analysis. This includes:
 * - the type of the attribute (in payload, either a plain type or an
 *   instantiated type)
 * - various flags indicating things like initialization, whether this is a
 *   classvar, etc
 * - any problems arising from us resolving the attribute, which need to be
 *   exposed in type checking
 *
 * Here attribute can refer to a "plain" attribute or a method, they are handled the same way.
 *)

open Core
open Ast

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
[@@deriving equal, show, compare, sexp]

type problem =
  | DifferingDecorators of { offender: Type.t Type.Callable.overload }
  | InvalidDecorator of {
      index: int;
      reason: invalid_decorator_reason;
    }
[@@deriving equal, show, compare, sexp]

type decorated_method = {
  undecorated_signature: Type.Callable.t;
  decorators: (Expression.t list, problem) Result.t;
}
[@@deriving compare, sexp]

module UninstantiatedAnnotation = struct
  type property_annotation = {
    self: Type.t option;
    value: Type.t option;
  }
  [@@deriving compare, sexp]

  type kind =
    | Attribute of Type.t
    | DecoratedMethod of decorated_method
    | Property of {
        getter: property_annotation;
        setter: property_annotation option;
      }
  [@@deriving compare, sexp]

  type t = {
    accessed_via_metaclass: bool;
    kind: kind;
  }
  [@@deriving compare, sexp]
end

module InstantiatedAnnotation = struct
  type t = {
    annotation: Type.t;
    original_annotation: Type.t;
    uninstantiated_annotation: Type.t option;
    problem: problem option;
  }
  [@@deriving equal, show, compare, sexp]
end

(* Note: the read_only and visibility flags here are related to `Final` attributes, they are not
   directly related to `ReadOnly` types (although the two do interact) *)
type read_only =
  | Refinable of { overridable: bool }
  | Unrefinable
[@@deriving equal, show, compare, sexp]

type visibility =
  | ReadOnly of read_only
  | ReadWrite
[@@deriving equal, show, compare, sexp]

type initialized =
  | OnClass
  | OnlyOnInstance
  | NotInitialized
[@@deriving equal, show, compare, sexp]

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
}
[@@deriving equal, show, compare, sexp]

type uninstantiated = UninstantiatedAnnotation.t t [@@deriving compare, sexp]

type instantiated = InstantiatedAnnotation.t t [@@deriving equal, show, compare, sexp]

let create_instantiated
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
  =
  {
    payload =
      InstantiatedAnnotation.
        { annotation; original_annotation; uninstantiated_annotation; problem = None };
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
  }


let annotation
    {
      payload = InstantiatedAnnotation.{ annotation; original_annotation; _ };
      async_property;
      defined;
      visibility;
      _;
    }
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
    TypeInfo.Unit.create_immutable ~original:(Some original) ~final annotation
  else
    (* We need to distinguish between unannotated attributes and non-existent ones - ensure that the
       annotation is viewed as mutable to distinguish from user-defined globals. *)
    TypeInfo.Unit.create_mutable annotation


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

let visibility { visibility; _ } = visibility

let defined { defined; _ } = defined

let class_variable { class_variable; _ } = class_variable

let abstract { abstract; _ } = abstract

let async_property { async_property; _ } = async_property

let static { payload = InstantiatedAnnotation.{ uninstantiated_annotation; _ }; _ } =
  match uninstantiated_annotation with
  | Some (Type.Parametric { name = "typing.StaticMethod"; _ }) -> true
  | _ -> false


let property { property; _ } = property

let undecorated_signature { undecorated_signature; _ } = undecorated_signature

let problem { payload = InstantiatedAnnotation.{ problem; _ }; _ } = problem

(* In Python, there's a convention that any attribute starting with a single underscore is "private" name.
 *
 * This helper allows us to capture that convention for certain use cases, like variance inference.
 *)
let is_private_field { name; _ } =
  match name with
  | "" -> false
  | _ when Char.equal name.[0] '_' -> not (String.is_suffix ~suffix:"__" name)
  | _ -> false


(* There is a (rarely-used) runtime mechanism to mangle names of attributes that start with *two* underscores.
 *
 * This helper allows us to model that by allowing certain behaviors that would otherwise be
 * unsound, as well as capturing a few edge cases (e.g. sqlalchemy constructor generation) where
 * such private attributes are handles specially.
 *)
let is_mangled_private_field ({ name; _ } as attribute) =
  let parent_prefix = parent_prefix attribute in
  String.is_prefix ~prefix:(parent_prefix ^ "__") name


let public_name ({ name; _ } as attribute) =
  let parent_prefix = parent_prefix attribute in
  if is_mangled_private_field attribute then
    String.drop_prefix name (String.length parent_prefix)
  else
    name


let is_final { visibility; _ } =
  match visibility with
  | ReadOnly (Refinable _) -> true
  | _ -> false


let instantiate attribute ~annotation ~original_annotation ~uninstantiated_annotation ~problem =
  {
    attribute with
    payload =
      InstantiatedAnnotation.{ annotation; original_annotation; uninstantiated_annotation; problem };
  }


let with_initialized attribute ~initialized = { attribute with initialized }

let with_visibility attribute ~visibility = { attribute with visibility }
