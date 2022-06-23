(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast

module IntKey : Memory.KeyType with type t = int

module StringKey : Memory.KeyType with type t = string

module ReferenceKey : sig
  type t = Reference.t [@@deriving sexp]

  include Memory.KeyType with type t := Reference.t
end

module AttributeTableKey : sig
  type t = {
    include_generated_attributes: bool;
    accessed_via_metaclass: bool;
    name: Type.Primitive.t;
  }
  [@@deriving sexp]

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
    validation: type_validation_policy;
    expression: Expression.t;
  }
  [@@deriving sexp]

  include Memory.KeyType with type t := t

  module Set : Core.Set.S with type Elt.t = t

  include Core.Hashable with type t := t
end

type dependency =
  | CreateModuleErrors of Reference.t
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
  | Metaclass of Type.Primitive.t
  | WildcardImport of Reference.t
[@@deriving show, compare, sexp]

module DependencyKey : sig
  include DependencyTrackedMemory.DependencyKey.S with type key = dependency

  val get_key : registered -> key

  module Registry : sig
    val register : key -> registered

    type serialized

    val store : unit -> unit

    val load : unit -> unit

    val collected_map_reduce
      :  Scheduler.t ->
      policy:Scheduler.Policy.t ->
      initial:'state ->
      map:('state -> 'input list -> 'intermediate) ->
      reduce:('intermediate -> 'state -> 'state) ->
      inputs:'input list ->
      unit ->
      'state

    val collected_iter
      :  Scheduler.t ->
      policy:Scheduler.Policy.t ->
      f:('input list -> unit) ->
      inputs:'input list ->
      unit
  end
end

module LocationKey : Memory.KeyType with type t = Location.t
