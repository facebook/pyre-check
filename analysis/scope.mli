(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast

(** Data structure that represents a name binding in Python. Constructors are kept private
    intentionally -- The [Scope] class is supposed to do the creation in batch. *)
module Binding : sig
  module Kind : sig
    type t =
      | AssignTarget
      | ClassName
      | ComprehensionTarget
      | DefineName
      | ExceptTarget
      | ForTarget
      | ImportName
      | ParameterName
      | WithTarget
    [@@deriving sexp, compare, hash]
  end

  type t = private {
    kind: Kind.t;
    name: Identifier.t;
    location: Location.t;
    annotation: Expression.t option;
  }
  [@@deriving sexp, compare, hash]
end

(** Data structure that aggregates all bindings in a single code block (excluding all the nesting
    and nested blocks). *)
module Scope : sig
  module Kind : sig
    type t =
      | Module
      | Define
      | Lambda
      | Comprehension
    [@@deriving sexp, compare, hash]
  end

  type t = private {
    kind: Kind.t;
    globals: Identifier.Set.t;
    nonlocals: Identifier.Set.t;
    bindings: Binding.t Identifier.Map.t;
  }
  [@@deriving sexp, compare]

  val of_define : Statement.Define.t -> t option

  val of_define_exn : Statement.Define.t -> t

  val of_expression : Expression.t -> t option

  val of_expression_exn : Expression.t -> t

  val of_source : Source.t -> t

  (* Look up bindings directly, skipping global and nonlocal check *)
  (* Mostly for testing *)
  val lookup_bindings : t -> Identifier.t -> Binding.t option
end
