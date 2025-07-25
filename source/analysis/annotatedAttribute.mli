(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

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

module UninstantiatedAnnotation : sig
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

module InstantiatedAnnotation : sig
  type t [@@deriving show]
end

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

type 'a t [@@deriving equal, show, compare, sexp]

type uninstantiated = UninstantiatedAnnotation.t t [@@deriving compare, sexp]

type instantiated = InstantiatedAnnotation.t t [@@deriving equal, show, compare, sexp]

val create_instantiated
  :  abstract:bool ->
  annotation:Type.t ->
  original_annotation:Type.t ->
  async_property:bool ->
  class_variable:bool ->
  defined:bool ->
  initialized:initialized ->
  name:Identifier.t ->
  parent:Type.Primitive.t ->
  visibility:visibility ->
  property:bool ->
  uninstantiated_annotation:Type.t option ->
  undecorated_signature:Type.Callable.t option ->
  instantiated

val create_uninstantiated
  :  abstract:bool ->
  uninstantiated_annotation:'a ->
  async_property:bool ->
  class_variable:bool ->
  defined:bool ->
  initialized:initialized ->
  name:Identifier.t ->
  parent:Type.Primitive.t ->
  visibility:visibility ->
  property:bool ->
  undecorated_signature:Type.Callable.t option ->
  'a t

val annotation : instantiated -> TypeInfo.Unit.t

val uninstantiated_annotation : 'a t -> 'a

val with_uninstantiated_annotation : uninstantiated_annotation:'a -> 'a t -> 'a t

val with_undecorated_signature : 'a t -> undecorated_signature:Type.Callable.t option -> 'a t

val name : 'a t -> Identifier.t

val abstract : 'a t -> bool

val async_property : 'a t -> bool

val parent : 'a t -> Type.Primitive.t

val parent_name : 'a t -> Identifier.t

val initialized : 'a t -> initialized

val defined : 'a t -> bool

val class_variable : 'a t -> bool

val static : instantiated -> bool

val property : 'a t -> bool

val visibility : 'a t -> visibility

val is_private_field : 'a t -> bool

val is_mangled_private_field : 'a t -> bool

val public_name : 'a t -> Identifier.t

val is_final : 'a t -> bool

val with_initialized : 'a t -> initialized:initialized -> 'a t

val with_visibility : 'a t -> visibility:visibility -> 'a t

val undecorated_signature : 'a t -> Type.Callable.t option

val problem : instantiated -> problem option

val instantiate
  :  'a t ->
  annotation:Type.t ->
  original_annotation:Type.t ->
  uninstantiated_annotation:Type.t option ->
  problem:problem option ->
  instantiated
