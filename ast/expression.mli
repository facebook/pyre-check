(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

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
      operator: operator;
      right: 'expression;
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

  module Call : sig
    module RecordArgument : sig
      type 'expression record = {
        name: Identifier.t Node.t option;
        value: 'expression;
      }
      [@@deriving compare, eq, sexp, show, hash, to_yojson]
    end

    type 'expression record = {
      callee: 'expression;
      arguments: 'expression RecordArgument.record list;
    }
    [@@deriving compare, eq, sexp, show, hash]
  end
end

module Name : sig
  module Attribute : sig
    type 'expression t = {
      base: 'expression;
      attribute: Identifier.t;
      special: bool;
    }
    [@@deriving compare, eq, sexp, show, hash]
  end

  type 'expression t =
    | Attribute of 'expression Attribute.t
    | Identifier of Identifier.t
  [@@deriving compare, eq, sexp, show, hash]
end

module Lambda : sig
  type 'expression t = {
    parameters: 'expression Parameter.t list;
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
    entries: 'expression entry list;
    keywords: 'expression list;
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
    generators: 'expression generator list;
  }
  [@@deriving compare, eq, sexp, show, hash]
end

module Starred : sig
  type 'expression t =
    | Once of 'expression
    | Twice of 'expression
  [@@deriving compare, eq, sexp, show, hash]
end

module StringLiteral : sig
  module Substring : sig
    type kind =
      | Literal
      | Format
    [@@deriving compare, eq, sexp, show, hash]

    type t = {
      value: string;
      kind: kind;
    }
    [@@deriving compare, eq, sexp, show, hash]
  end

  type 'expression kind =
    | String
    | Bytes
    | Format of 'expression list
    | Mixed of Substring.t Node.t list

  and 'expression t = {
    value: string;
    kind: 'expression kind;
  }
  [@@deriving compare, eq, sexp, show, hash]

  val create : ?bytes:bool -> ?expressions:'expression list -> string -> 'expression t

  val create_mixed : Substring.t Node.t list -> 'expression t
end

type expression =
  | Await of t
  | BooleanOperator of t BooleanOperator.t
  | Call of t Record.Call.record
  | ComparisonOperator of t Record.ComparisonOperator.record
  | Complex of float
  | Dictionary of t Dictionary.t
  | DictionaryComprehension of (t Dictionary.entry, t) Comprehension.t
  | Ellipsis
  | False
  | Float of float
  | Generator of (t, t) Comprehension.t
  | Integer of int
  | Lambda of t Lambda.t
  | List of t list
  | ListComprehension of (t, t) Comprehension.t
  | Name of t Name.t
  | Set of t list
  | SetComprehension of (t, t) Comprehension.t
  | Starred of t Starred.t
  | String of t StringLiteral.t
  | Ternary of t Ternary.t
  | True
  | Tuple of t list
  | UnaryOperator of t Record.UnaryOperator.record
  | Yield of t option

and t = expression Node.t [@@deriving compare, eq, sexp, show, hash, to_yojson]

and expression_t = t [@@deriving compare, eq, sexp, show, hash]

module ComparisonOperator : sig
  include module type of struct
    include Record.ComparisonOperator
  end

  type t = expression_t Record.ComparisonOperator.record [@@deriving compare, eq, sexp, show, hash]

  val override : t -> expression_t option
end

module UnaryOperator : sig
  include module type of struct
    include Record.UnaryOperator
  end

  type t = expression_t Record.UnaryOperator.record [@@deriving compare, eq, sexp, show, hash]

  val override : t -> expression_t option
end

module Call : sig
  module Argument : sig
    include module type of struct
      include Record.Call.RecordArgument
    end

    type t = expression_t record [@@deriving compare, eq, sexp, show, hash, to_yojson]
  end

  include module type of struct
    include Record.Call
  end

  type t = expression_t record [@@deriving compare, eq, sexp, show, hash, to_yojson]
end

val negate : t -> t

val normalize : t -> t

val create_name_from_identifiers : Identifier.t Node.t list -> expression_t Name.t

val create_name : location:Location.t -> string -> expression_t Name.t

val create_name_from_reference : location:Location.t -> Reference.t -> expression_t Name.t

val from_reference : location:Location.t -> Reference.t -> expression_t

val name_to_identifiers : expression_t Name.t -> Identifier.t list option

val name_to_reference : expression_t Name.t -> Reference.t option

val name_to_reference_exn : expression_t Name.t -> Reference.t

val is_simple_name : expression_t Name.t -> bool

val get_identifier_base : t -> Identifier.t option

val has_identifier_base : t -> bool

val name_is : name:string -> t -> bool

val sanitized : t -> t

val delocalize : t -> t

val delocalize_qualified : t -> t

val exists_in_list : ?match_prefix:bool -> expression_list:t list -> string -> bool

val arguments_location : Call.t -> Location.t

val show_sanitized : t -> string

val pp_sanitized : Format.formatter -> t -> unit

val pp_expression_list : Format.formatter -> t list -> unit

val pp_expression_argument_list : Format.formatter -> Call.Argument.t list -> unit

val pp_expression_parameter_list : Format.formatter -> expression Node.t Parameter.t list -> unit
