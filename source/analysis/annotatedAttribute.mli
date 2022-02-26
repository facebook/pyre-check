(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

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

type 'a t [@@deriving eq, show, compare, sexp]

type instantiated_annotation

type instantiated = instantiated_annotation t [@@deriving eq, show, compare, sexp]

val create
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
  problem:problem option ->
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
  problem:problem option ->
  'a t

val annotation : instantiated -> Annotation.t

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

val is_private : 'a t -> bool

val public_name : 'a t -> Identifier.t

val is_final : 'a t -> bool

val with_initialized : 'a t -> initialized:initialized -> 'a t

val undecorated_signature : 'a t -> Type.Callable.t option

val problem : 'a t -> problem option

val instantiate
  :  'a t ->
  annotation:Type.t ->
  original_annotation:Type.t ->
  uninstantiated_annotation:Type.t option ->
  instantiated
