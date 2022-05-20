(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast

module IntKey = struct
  type t = int

  let to_string = Int.to_string

  let compare = Int.compare

  let from_string = Core.Int.of_string
end

module StringKey = struct
  type t = string

  let to_string = Fn.id

  let compare = String.compare

  let from_string x = x
end

module ReferenceKey = struct
  type t = Reference.t [@@deriving compare, sexp]

  let to_string = Reference.show

  let from_string name = Reference.create name
end

module AttributeTableKey = struct
  module T = struct
    type t = {
      include_generated_attributes: bool;
      accessed_via_metaclass: bool;
      name: Type.Primitive.t;
    }
    [@@deriving compare, sexp, hash, show]
  end

  include T
  module Set = Set.Make (T)
  include Hashable.Make (T)

  let to_string key = sexp_of_t key |> Sexp.to_string

  let from_string sexp = Sexp.of_string sexp |> t_of_sexp
end

module ParseAnnotationKey = struct
  type type_validation_policy =
    | NoValidation
    | ValidatePrimitives
    | ValidatePrimitivesAndTypeParameters
  [@@deriving compare, sexp, hash, show]

  module T = struct
    type t = {
      validation: type_validation_policy;
      expression: Expression.t;
    }
    [@@deriving compare, sexp, hash, show]
  end

  include T
  module Set = Set.Make (T)
  include Hashable.Make (T)

  let to_string key = sexp_of_t key |> Sexp.to_string

  let from_string sexp = Sexp.of_string sexp |> t_of_sexp
end

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
  | Metaclass of Type.Primitive.t
  | WildcardImport of Reference.t
[@@deriving show, compare, sexp, hash]

module DependencyKey = DependencyTrackedMemory.DependencyKey.Make (struct
  type t = dependency [@@deriving compare, sexp, hash]
end)

module LocationKey = struct
  type t = Location.t

  let to_string key = Location.sexp_of_t key |> Sexp.to_string

  let compare = Location.compare

  let from_string sexp_string = Sexp.of_string sexp_string |> Location.t_of_sexp
end
