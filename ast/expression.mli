(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


module Call : sig
  type 'expression t = {
    name: 'expression;
    arguments: ('expression Argument.t) list;
  }
  [@@deriving compare, eq, sexp, show]
end

module BooleanOperator : sig
  type operator =
    | And
    | Or
  [@@deriving compare, eq, sexp, show]

  type 'expression t = {
    left: 'expression;
    operator: operator;
    right: 'expression;
  }
  [@@deriving compare, eq, sexp, show]
end

module BinaryOperator : sig
  type operator =
    | Add
    | At
    | BitAnd
    | BitOr
    | BitXor
    | Divide
    | FloorDivide
    | LeftShift
    | Modulo
    | Multiply
    | Power
    | RightShift
    | Subtract
  [@@deriving compare, eq, sexp, show]

  type 'expression t = {
    left: 'expression;
    operator: operator;
    right: 'expression;
  }
  [@@deriving compare, eq, sexp, show]

  val pp_binary_operator : Format.formatter -> operator -> unit
end

module UnaryOperator : sig
  type operator =
    | Invert
    | Negative
    | Not
    | Positive
  [@@deriving compare, eq, sexp, show]

  type 'expression t = {
    operator: operator;
    operand: 'expression;
  }
  [@@deriving compare, eq, sexp, show]
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
  [@@deriving compare, eq, sexp, show]

  type 'expression t = {
    left: 'expression;
    right: (operator * 'expression) list;
  }
  [@@deriving compare, eq, sexp, show]
end

module Access : sig
  type 'expression slice = {
    lower: 'expression option;
    upper: 'expression option;
    step: 'expression option;
  }
  [@@deriving compare, eq, sexp, show]

  type 'expression subscript =
    | Index of 'expression
    | Slice of 'expression slice
  [@@deriving compare, eq, sexp, show]

  type 'expression access =
    | Call of ('expression Call.t) Node.t
    | Expression of 'expression
    | Identifier of Identifier.t
    | Subscript of ('expression subscript) list
  [@@deriving compare, eq, sexp, show]

  type 'expression t = ('expression access) list
  [@@deriving compare, eq, sexp, show]
end

module Lambda : sig
  type 'expression t = {
    parameters: ('expression Parameter.t) list;
    body: 'expression;
  }
  [@@deriving compare, eq, sexp, show]
end

module Ternary : sig
  type 'expression t = {
    target: 'expression;
    test: 'expression;
    alternative: 'expression;
  }
  [@@deriving compare, eq, sexp, show]
end

module Dictionary : sig
  type 'expression entry = {
    key: 'expression;
    value: 'expression;
  }
  [@@deriving compare, eq, sexp, show]

  type 'expression t = {
    entries: ('expression entry) list;
    keywords: 'expression option;
  }
  [@@deriving compare, eq, sexp, show]
end

module Comprehension : sig
  type 'expression generator = {
    target: 'expression;
    iterator: 'expression;
    conditions: 'expression list;
    async: bool;
  }
  [@@deriving compare, eq, sexp, show]

  type ('element, 'expression) t = {
    element: 'element;
    generators: ('expression generator) list;
  }
  [@@deriving compare, eq, sexp, show]
end

module Starred : sig
  type 'expression t =
    | Once of 'expression
    | Twice of 'expression
  [@@deriving compare, eq, sexp, show]
end

type expression =
  | Access of t Access.t
  | Await of t
  | BinaryOperator of t BinaryOperator.t
  | BooleanOperator of t BooleanOperator.t
  | Bytes of string
  | ComparisonOperator of t ComparisonOperator.t
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
  | UnaryOperator of t UnaryOperator.t
  | Yield of t option

and t = expression Node.t
[@@deriving compare, eq, sexp, show]

type access = t Access.t
[@@deriving compare, eq, sexp, show]

val negate: t -> t

val normalize: t -> t

val pp : Format.formatter -> t -> unit

val show : t -> string

val pp_expression_list : Format.formatter -> t list -> unit

val pp_expression_access_list : Format.formatter -> t Access.t -> unit

val pp_expression_argument_list : Format.formatter -> (t Argument.t) list -> unit

val pp_expression_parameter_list :
  Format.formatter -> expression Node.t Parameter.t list -> unit
