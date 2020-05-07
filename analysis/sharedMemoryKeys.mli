(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast

module IntKey : Memory.KeyType with type t = int and type out = int

module StringKey : Memory.KeyType with type t = string and type out = string

module ReferenceKey : Memory.KeyType with type t = Reference.t and type out = Reference.t

module AttributeTableKey : sig
  type t = {
    include_generated_attributes: bool;
    in_test: bool;
    accessed_via_metaclass: bool;
    name: Type.Primitive.t;
    assumptions: Assumptions.t;
  }

  include Memory.KeyType with type t := t

  module Set : Core.Set.S with type Elt.t = t

  include Core.Hashable with type t := t
end

module ParseAnnotationKey : sig
  type type_validation_policy =
    | NoValidation
    | ValidatePrimitives
    | ValidatePrimitivesAndTypeParameters

  type t = {
    assumptions: Assumptions.t;
    validation: type_validation_policy;
    expression: Expression.t;
  }

  include Memory.KeyType with type t := t

  module Set : Core.Set.S with type Elt.t = t

  include Core.Hashable with type t := t
end

module ReferenceSet : Set.S with type elt = Reference.t

module ReferenceDependencyKey :
  DependencyTrackedMemory.DependencyKey.S
    with type key = Reference.t
     and type registered = Reference.t
     and module RegisteredSet = ReferenceSet
     and module KeySet = ReferenceSet

type dependency =
  | TypeCheckDefine of Reference.t
  | AliasRegister of Reference.t
  | ClassConnect of Type.Primitive.t
  | RegisterClassMetadata of Type.Primitive.t
  | UndecoratedFunction of Reference.t
  | AnnotateGlobal of Reference.t
  | AnnotateGlobalLocation of Reference.t
  | FromEmptyStub of Reference.t
  | AttributeTable of AttributeTableKey.t
  | ParseAnnotation of ParseAnnotationKey.t
[@@deriving show, compare, sexp]

module DependencySet : Set.S with type elt = dependency

module DependencyKey :
  DependencyTrackedMemory.DependencyKey.S
    with type key = dependency
     and type registered = dependency
     and module RegisteredSet = DependencySet
     and module KeySet = DependencySet

module LocationKey : Memory.KeyType with type t = Location.t
