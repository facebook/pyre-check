(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast

(** Data structure that represents a name binding in Python. Constructors are kept private
    intentionally -- The [Scope] class is supposed to do the creation in batch. *)
module Binding : sig
  module Kind : sig
    module Star : sig
      type t =
        | Once
        | Twice
      [@@deriving sexp, compare, hash]
    end

    module Import : sig
      type t =
        | From of Reference.t
        | Module
      [@@deriving sexp, compare, hash]
    end

    type t =
      | AssignTarget of Expression.t option
      | ClassName
      | ComprehensionTarget
      | DefineName of Statement.Define.Signature.t
      | ExceptTarget of Expression.t option
      | ForTarget
      | ImportName of Import.t
      | MatchTarget
      | ParameterName of {
          index: int;
          annotation: Expression.t option;
          star: Star.t option;
        }
      | WalrusTarget
      | WithTarget
    [@@deriving sexp, compare, hash]
  end

  type t = private {
    kind: Kind.t;
    name: Identifier.t;
    location: Location.t;
  }
  [@@deriving sexp, compare, hash]

  val name : t -> Identifier.t

  val of_statement : t list -> Statement.t -> t list

  val of_parameters : t list -> Expression.Parameter.parameter Node.t list -> t list
end

(** Data structure that aggregates all bindings in a single code block (excluding all the nesting
    and nested blocks). *)
module Scope : sig
  module Kind : sig
    type t =
      | Module
      | Define of Statement.Define.Signature.t
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

(** Data structure that represents the result of a binding lookup in a stack of scopes *)
module Access : sig
  module Locality : sig
    type t =
      | Local
      | Nonlocal
      | Global
    [@@deriving sexp, compare, hash]
  end

  module Kind : sig
    type t =
      | CurrentScope
      | OuterScope of Locality.t
    [@@deriving sexp, compare, hash]
  end

  type t = private {
    kind: Kind.t;
    binding: Binding.t;  (** The binding corresponding to the name looked up *)
    scope: Scope.t;  (** The scope where the binding is found *)
  }
  [@@deriving sexp, compare]
end

(** Data structure that aggregates all bindings in a code block as well as all blocks that
    (transitively) nest it. *)
module ScopeStack : sig
  type t

  val create : Source.t -> t

  val global_scope : t -> Scope.t

  val current_scope : t -> Scope.t

  val extend : with_:Scope.t -> t -> t

  val lookup : t -> Identifier.t -> Access.t option
end

module Builtins : sig
  val mem : Identifier.t -> bool
  (** Returns whether the given name belongs to the builtin scope. *)
end
