(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

module Identifier = AstIdentifier
module Location = AstLocation
module Parameter = AstParameter
module Node = AstNode

module BooleanOperator : sig
  type operator =
    | And
    | Or
  [@@deriving compare, eq, sexp, show, hash]

  type 'expression t = {
    left: 'expression;
    operator: operator;
    right: 'expression;
  }
  [@@deriving compare, eq, sexp, show, hash]
end

module Record : sig
  module Argument : sig
    type 'expression record = {
      name: (Identifier.t Node.t) option;
      value: 'expression;
    }
    [@@deriving compare, eq, sexp, show, hash]
  end

  module Access : sig
    type 'expression access =
      | Call of (('expression Argument.record) list) Node.t
      | Expression of 'expression
      | Identifier of Identifier.t
    [@@deriving compare, eq, sexp, show, hash]

    type 'expression record = ('expression access) list
    [@@deriving compare, eq, sexp, show, hash]
  end

  module ComparisonOperator : sig
    type operator =
      | Equals
      | GreaterThan
      | GreaterThanOrEquals
      | In
      | Is
      | IsNot
      | LessThan
      | LessThanOrEquals
      | NotEquals
      | NotIn
    [@@deriving compare, eq, sexp, show, hash]

    type 'expression record = {
      left: 'expression;
      right: (operator * 'expression) list;
    }
    [@@deriving compare, eq, sexp, show, hash]
  end

  module UnaryOperator : sig
    type operator =
      | Invert
      | Negative
      | Not
      | Positive
    [@@deriving compare, eq, sexp, show, hash]

    type 'expression record = {
      operator: operator;
      operand: 'expression;
    }
    [@@deriving compare, eq, sexp, show, hash]
  end
end

module Lambda : sig
  type 'expression t = {
    parameters: ('expression Parameter.t) list;
    body: 'expression;
  }
  [@@deriving compare, eq, sexp, show, hash]
end

module Ternary : sig
  type 'expression t = {
    target: 'expression;
    test: 'expression;
    alternative: 'expression;
  }
  [@@deriving compare, eq, sexp, show, hash]
end

module Dictionary : sig
  type 'expression entry = {
    key: 'expression;
    value: 'expression;
  }
  [@@deriving compare, eq, sexp, show, hash]

  type 'expression t = {
    entries: ('expression entry) list;
    keywords: 'expression option;
  }
  [@@deriving compare, eq, sexp, show, hash]
end

module Comprehension : sig
  type 'expression generator = {
    target: 'expression;
    iterator: 'expression;
    conditions: 'expression list;
    async: bool;
  }
  [@@deriving compare, eq, sexp, show, hash]

  type ('element, 'expression) t = {
    element: 'element;
    generators: ('expression generator) list;
  }
  [@@deriving compare, eq, sexp, show, hash]
end

module Starred : sig
  type 'expression t =
    | Once of 'expression
    | Twice of 'expression
  [@@deriving compare, eq, sexp, show, hash]
end

type expression =
  | Access of t Record.Access.record
  | Await of t
  | BooleanOperator of t BooleanOperator.t
  | Bytes of string
  | ComparisonOperator of t Record.ComparisonOperator.record
  | Complex of float
  | Dictionary of t Dictionary.t
  | DictionaryComprehension of ((t Dictionary.entry), t) Comprehension.t
  | False
  | Float of float
  | Format of string
  | Generator of (t, t) Comprehension.t
  | Integer of int
  | Lambda of t Lambda.t
  | List of t list
  | ListComprehension of (t, t) Comprehension.t
  | Set of t list
  | SetComprehension of (t, t) Comprehension.t
  | Starred of t Starred.t
  | String of string
  | Ternary of t Ternary.t
  | True
  | Tuple of t list
  | UnaryOperator of t Record.UnaryOperator.record
  | Yield of t option

and t = expression Node.t
[@@deriving compare, eq, sexp, show, hash]

and expression_t = t
[@@deriving compare, eq, sexp, show, hash]

module Argument : sig
  include module type of struct include Record.Argument end

  type t = expression_t Record.Argument.record
  [@@deriving compare, eq, sexp, show, hash]
end

module Access : sig
  include module type of struct include Record.Access end

  type t = expression_t Record.Access.record
  [@@deriving compare, eq, sexp, show, hash]

  module Set: Set.S with type Elt.t = t

  module Map: Map.S with type Key.t = t

  include Hashable with type t := t

  val create: string -> t
  val create_from_identifiers: Identifier.t list -> t
  val create_from_expression: expression_t -> t

  val show_sanitized: ?sanitized: bool -> t -> string

  val add_prefix: prefix: string -> t -> t
  val remove_prefix: prefix: string -> t -> t
  val starts_with: prefix: string -> t -> bool

  val is_strict_prefix: prefix: t -> t -> bool
  val drop_prefix: t -> prefix: t -> t

  val call: ?arguments: Argument.t list -> location: Location.t ->  name: string -> unit -> t
  (* Calls like `__add__` have backups that are called on exceptions. *)
  val backup: arguments: Argument.t list -> name: t -> (Argument.t list * t) option
  (* Some calls are redirected to method calls, e.g. `repr(x)` will call
     `x.__repr__()`. *)
  val redirect: arguments: Argument.t list -> location: Location.t -> name: t -> t option
end

val access: t -> Access.t

module ComparisonOperator : sig
  include module type of struct include Record.ComparisonOperator end

  type t = expression_t Record.ComparisonOperator.record
  [@@deriving compare, eq, sexp, show, hash]

  val override: t -> (expression_t option) list
end

module UnaryOperator : sig
  include module type of struct include Record.UnaryOperator end

  type t = expression_t Record.UnaryOperator.record
  [@@deriving compare, eq, sexp, show, hash]

  val override: t -> expression_t option
end

val negate: t -> t

val normalize: t -> t

val pp : Format.formatter -> t -> unit

val show : t -> string

val pp_expression_list : Format.formatter -> t list -> unit

val pp_expression_access_list : Format.formatter -> Access.t -> unit

val pp_expression_argument_list : Format.formatter -> (t Argument.record) list -> unit

val pp_expression_parameter_list :
  Format.formatter -> expression Node.t Parameter.t list -> unit
