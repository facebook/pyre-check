(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

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

  val is_all_literal : t Node.t list -> bool
end

module rec BooleanOperator : sig
  type operator =
    | And
    | Or
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  type t = {
    left: Expression.t;
    operator: operator;
    right: Expression.t;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  val pp_boolean_operator : Format.formatter -> operator -> unit

  val inverse : operator -> operator

  val location_insensitive_compare : t -> t -> int
end

and Call : sig
  module Argument : sig
    type t = {
      name: Identifier.t Node.t option;
      value: Expression.t;
    }
    [@@deriving compare, eq, sexp, show, hash, to_yojson]

    type kind =
      | SingleStar
      | DoubleStar
      | Named of string Node.t
      | Positional

    val location_insensitive_compare : t -> t -> int

    val unpack : t -> Expression.t * kind
  end

  type t = {
    callee: Expression.t;
    arguments: Argument.t list;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int
end

and ComparisonOperator : sig
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
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  type t = {
    left: Expression.t;
    operator: operator;
    right: Expression.t;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  val inverse : operator -> operator

  val pp_comparison_operator : Format.formatter -> operator -> unit

  val override : location:Location.t -> t -> Expression.t option

  val location_insensitive_compare : t -> t -> int
end

and Comprehension : sig
  module Generator : sig
    type t = {
      target: Expression.t;
      iterator: Expression.t;
      conditions: Expression.t list;
      async: bool;
    }
    [@@deriving compare, eq, sexp, show, hash, to_yojson]

    val location_insensitive_compare : t -> t -> int
  end

  type 'element t = {
    element: 'element;
    generators: Generator.t list;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  val location_insensitive_compare
    :  ('element -> 'element -> int) ->
    'element t ->
    'element t ->
    int
end

and Dictionary : sig
  module Entry : sig
    type t = {
      key: Expression.t;
      value: Expression.t;
    }
    [@@deriving compare, eq, sexp, show, hash, to_yojson]

    val location_insensitive_compare : t -> t -> int
  end

  type t = {
    entries: Entry.t list;
    keywords: Expression.t list;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int
end

and Lambda : sig
  type t = {
    parameters: Parameter.t list;
    body: Expression.t;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]
end

and Name : sig
  module Attribute : sig
    type t = {
      base: Expression.t;
      attribute: Identifier.t;
      special: bool;
    }
    [@@deriving compare, eq, sexp, show, hash, to_yojson]

    val location_insensitive_compare : t -> t -> int
  end

  type t =
    | Attribute of Attribute.t
    | Identifier of Identifier.t
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int

  val last : t -> string
end

and Parameter : sig
  type parameter = {
    name: Identifier.t;
    value: Expression.t option;
    annotation: Expression.t option;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  type t = parameter Node.t [@@deriving compare, eq, sexp, show, hash, to_yojson]

  val create
    :  location:Location.t ->
    ?value:Expression.t ->
    ?annotation:Expression.t ->
    name:Identifier.t ->
    unit ->
    t

  val name : t -> Identifier.t

  val location_insensitive_compare : t -> t -> int
end

and Starred : sig
  type t =
    | Once of Expression.t
    | Twice of Expression.t
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int
end

and StringLiteral : sig
  type kind =
    | String
    | Bytes
    | Format of Expression.t list
    | Mixed of Substring.t Node.t list
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  type t = {
    value: string;
    kind: kind;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int

  val create : ?bytes:bool -> ?expressions:Expression.t list -> string -> t

  val create_mixed : Substring.t Node.t list -> t
end

and Ternary : sig
  type t = {
    target: Expression.t;
    test: Expression.t;
    alternative: Expression.t;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int
end

and UnaryOperator : sig
  type operator =
    | Invert
    | Negative
    | Not
    | Positive
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  type t = {
    operator: operator;
    operand: Expression.t;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int

  val pp_unary_operator : Format.formatter -> operator -> unit

  val override : t -> Expression.t option
end

and WalrusOperator : sig
  type t = {
    target: Expression.t;
    value: Expression.t;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int
end

and Expression : sig
  type expression =
    | Await of t
    | BooleanOperator of BooleanOperator.t
    | Call of Call.t
    | ComparisonOperator of ComparisonOperator.t
    | Complex of float
    | Dictionary of Dictionary.t
    | DictionaryComprehension of Dictionary.Entry.t Comprehension.t
    | Ellipsis
    | False
    | Float of float
    | Generator of t Comprehension.t
    | Integer of int
    | Lambda of Lambda.t
    | List of t list
    | ListComprehension of t Comprehension.t
    | Name of Name.t
    | Set of t list
    | SetComprehension of t Comprehension.t
    | Starred of Starred.t
    | String of StringLiteral.t
    | Ternary of Ternary.t
    | True
    | Tuple of t list
    | UnaryOperator of UnaryOperator.t
    | WalrusOperator of WalrusOperator.t
    | Yield of t option

  and t = expression Node.t [@@deriving compare, eq, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int
end

type t = Expression.t [@@deriving compare, eq, sexp, show, hash, to_yojson]

type expression = Expression.expression [@@deriving compare, eq, sexp, show, hash, to_yojson]

val location_insensitive_compare : t -> t -> int

val negate : t -> t

val normalize : t -> t

val create_name_from_identifiers : Identifier.t Node.t list -> Name.t

val create_name : location:Location.t -> string -> Name.t

val create_name_from_reference : location:Location.t -> Reference.t -> Name.t

val from_reference : location:Location.t -> Reference.t -> t

val name_to_identifiers : Name.t -> Identifier.t list option

val name_to_reference : Name.t -> Reference.t option

val name_to_reference_exn : Name.t -> Reference.t

val is_simple_name : Name.t -> bool

val get_identifier_base : t -> Identifier.t option

val has_identifier_base : t -> bool

val name_is : name:string -> t -> bool

val sanitized : t -> t

val delocalize : t -> t

val delocalize_qualified : t -> t

val exists_in_list : ?match_prefix:bool -> expression_list:t list -> string -> bool

val arguments_location : Call.t -> Location.t

val get_item_call : string -> expression Node.t list -> location:Location.t -> expression

val is_dunder_attribute : string -> bool

val pp_expression_list : Format.formatter -> t list -> unit

val pp_expression_argument_list : Format.formatter -> Call.Argument.t list -> unit

val pp_expression_parameter_list : Format.formatter -> Parameter.t list -> unit

val inverse_operator : string -> string option

val is_operator : string -> bool

val operator_name_to_symbol : string -> string option
