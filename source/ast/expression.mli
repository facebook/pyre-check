(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module StringLiteral : sig
  type kind =
    | String
    | Bytes
  [@@deriving compare, sexp, show, hash, to_yojson]

  type t = {
    value: string;
    kind: kind;
  }
  [@@deriving compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int

  val create : ?bytes:bool -> string -> t
end

module Constant : sig
  type t =
    | NoneLiteral
    | Ellipsis
    | False
    | True
    | Integer of int
    | Float of float
    | Complex of float
    | String of StringLiteral.t
  [@@deriving compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int
end

module rec BooleanOperator : sig
  type operator =
    | And
    | Or
  [@@deriving compare, sexp, show, hash, to_yojson]

  type t = {
    left: Expression.t;
    operator: operator;
    right: Expression.t;
  }
  [@@deriving compare, sexp, show, hash, to_yojson]

  val pp_boolean_operator : Format.formatter -> operator -> unit

  val inverse : operator -> operator

  val location_insensitive_compare : t -> t -> int
end

and Call : sig
  module Argument : sig
    type t = {
      (* NOTE(grievejia): Location here refers to the location of the entire argument, not the
         location of the argument name itself. *)
      name: Identifier.t Node.t option;
      value: Expression.t;
    }
    [@@deriving compare, sexp, show, hash, to_yojson]

    type kind =
      | SingleStar
      | DoubleStar
      | Named of string Node.t
      | Positional
    [@@deriving compare, show]

    val location_insensitive_compare : t -> t -> int

    val unpack : t -> Expression.t * kind
  end

  type t = {
    callee: Expression.t;
    arguments: Argument.t list;
  }
  [@@deriving compare, sexp, show, hash, to_yojson]

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
  [@@deriving compare, sexp, show, hash, to_yojson]

  type t = {
    left: Expression.t;
    operator: operator;
    right: Expression.t;
  }
  [@@deriving compare, sexp, show, hash, to_yojson]

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
    [@@deriving compare, sexp, show, hash, to_yojson]

    val location_insensitive_compare : t -> t -> int
  end

  type 'element t = {
    element: 'element;
    generators: Generator.t list;
  }
  [@@deriving compare, sexp, show, hash, to_yojson]

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
    [@@deriving compare, sexp, show, hash, to_yojson]

    val location_insensitive_compare : t -> t -> int
  end

  type t = {
    entries: Entry.t list;
    keywords: Expression.t list;
  }
  [@@deriving compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int
end

and Lambda : sig
  type t = {
    parameters: Parameter.t list;
    body: Expression.t;
  }
  [@@deriving compare, sexp, show, hash, to_yojson]
end

and Name : sig
  module Attribute : sig
    type t = {
      base: Expression.t;
      attribute: Identifier.t;
      special: bool;
    }
    [@@deriving compare, sexp, show, hash, to_yojson]

    val location_insensitive_compare : t -> t -> int
  end

  type t =
    | Attribute of Attribute.t
    | Identifier of Identifier.t
  [@@deriving compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int

  val last : t -> string
end

and Parameter : sig
  type parameter = {
    name: Identifier.t;
    value: Expression.t option;
    annotation: Expression.t option;
  }
  [@@deriving compare, sexp, show, hash, to_yojson]

  type t = parameter Node.t [@@deriving compare, sexp, show, hash, to_yojson]

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
  [@@deriving compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int
end

and Substring : sig
  type t =
    | Literal of string Node.t
    | Format of Expression.t
  [@@deriving compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int
end

and Ternary : sig
  type t = {
    target: Expression.t;
    test: Expression.t;
    alternative: Expression.t;
  }
  [@@deriving compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int
end

and UnaryOperator : sig
  type operator =
    | Invert
    | Negative
    | Not
    | Positive
  [@@deriving compare, sexp, show, hash, to_yojson]

  type t = {
    operator: operator;
    operand: Expression.t;
  }
  [@@deriving compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int

  val pp_unary_operator : Format.formatter -> operator -> unit

  val override : t -> Expression.t option
end

and WalrusOperator : sig
  type t = {
    target: Expression.t;
    value: Expression.t;
  }
  [@@deriving compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int
end

and Expression : sig
  type expression =
    | Await of t
    | BooleanOperator of BooleanOperator.t
    | Call of Call.t
    | ComparisonOperator of ComparisonOperator.t
    | Constant of Constant.t
    | Dictionary of Dictionary.t
    | DictionaryComprehension of Dictionary.Entry.t Comprehension.t
    | Generator of t Comprehension.t
    | FormatString of Substring.t list
    | Lambda of Lambda.t
    | List of t list
    | ListComprehension of t Comprehension.t
    | Name of Name.t
    | Set of t list
    | SetComprehension of t Comprehension.t
    | Starred of Starred.t
    | Ternary of Ternary.t
    | Tuple of t list
    | UnaryOperator of UnaryOperator.t
    | WalrusOperator of WalrusOperator.t
    | Yield of t option
    | YieldFrom of t

  and t = expression Node.t [@@deriving compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int
end

module Folder : sig
  type 'a t

  val fold : folder:'a t -> state:'a -> Expression.t -> 'a

  val fold_list : folder:'a t -> state:'a -> Expression.t list -> 'a

  val fold_option : folder:'a t -> state:'a -> Expression.t option -> 'a

  val create
    :  ?fold_await:(folder:'a t -> state:'a -> location:Location.t -> Expression.t -> 'a) ->
    ?fold_boolean_operator:
      (folder:'a t -> state:'a -> location:Location.t -> BooleanOperator.t -> 'a) ->
    ?fold_call:(folder:'a t -> state:'a -> location:Location.t -> Call.t -> 'a) ->
    ?fold_comparison_operator:
      (folder:'a t -> state:'a -> location:Location.t -> ComparisonOperator.t -> 'a) ->
    ?fold_constant:(folder:'a t -> state:'a -> location:Location.t -> Constant.t -> 'a) ->
    ?fold_dictionary:(folder:'a t -> state:'a -> location:Location.t -> Dictionary.t -> 'a) ->
    ?fold_dictionary_comprehension:
      (folder:'a t -> state:'a -> location:Location.t -> Dictionary.Entry.t Comprehension.t -> 'a) ->
    ?fold_generator:
      (folder:'a t -> state:'a -> location:Location.t -> Expression.t Comprehension.t -> 'a) ->
    ?fold_format_string:(folder:'a t -> state:'a -> location:Location.t -> Substring.t list -> 'a) ->
    ?fold_lambda:(folder:'a t -> state:'a -> location:Location.t -> Lambda.t -> 'a) ->
    ?fold_list:(folder:'a t -> state:'a -> location:Location.t -> Expression.t list -> 'a) ->
    ?fold_list_comprehension:
      (folder:'a t -> state:'a -> location:Location.t -> Expression.t Comprehension.t -> 'a) ->
    ?fold_name:(folder:'a t -> state:'a -> location:Location.t -> Name.t -> 'a) ->
    ?fold_set:(folder:'a t -> state:'a -> location:Location.t -> Expression.t list -> 'a) ->
    ?fold_set_comprehension:
      (folder:'a t -> state:'a -> location:Location.t -> Expression.t Comprehension.t -> 'a) ->
    ?fold_starred:(folder:'a t -> state:'a -> location:Location.t -> Starred.t -> 'a) ->
    ?fold_ternary:(folder:'a t -> state:'a -> location:Location.t -> Ternary.t -> 'a) ->
    ?fold_tuple:(folder:'a t -> state:'a -> location:Location.t -> Expression.t list -> 'a) ->
    ?fold_unary_operator:(folder:'a t -> state:'a -> location:Location.t -> UnaryOperator.t -> 'a) ->
    ?fold_walrus_operator:(folder:'a t -> state:'a -> location:Location.t -> WalrusOperator.t -> 'a) ->
    ?fold_yield:(folder:'a t -> state:'a -> location:Location.t -> Expression.t option -> 'a) ->
    ?fold_yield_from:(folder:'a t -> state:'a -> location:Location.t -> Expression.t -> 'a) ->
    unit ->
    'a t

  val create_with_uniform_location_fold
    :  ?fold_await:(folder:'a t -> state:'a -> Expression.t -> 'a) ->
    ?fold_boolean_operator:(folder:'a t -> state:'a -> BooleanOperator.t -> 'a) ->
    ?fold_call:(folder:'a t -> state:'a -> Call.t -> 'a) ->
    ?fold_comparison_operator:(folder:'a t -> state:'a -> ComparisonOperator.t -> 'a) ->
    ?fold_constant:(folder:'a t -> state:'a -> Constant.t -> 'a) ->
    ?fold_dictionary:(folder:'a t -> state:'a -> Dictionary.t -> 'a) ->
    ?fold_dictionary_comprehension:
      (folder:'a t -> state:'a -> Dictionary.Entry.t Comprehension.t -> 'a) ->
    ?fold_generator:(folder:'a t -> state:'a -> Expression.t Comprehension.t -> 'a) ->
    ?fold_format_string:(folder:'a t -> state:'a -> Substring.t list -> 'a) ->
    ?fold_lambda:(folder:'a t -> state:'a -> Lambda.t -> 'a) ->
    ?fold_list:(folder:'a t -> state:'a -> Expression.t list -> 'a) ->
    ?fold_list_comprehension:(folder:'a t -> state:'a -> Expression.t Comprehension.t -> 'a) ->
    ?fold_name:(folder:'a t -> state:'a -> Name.t -> 'a) ->
    ?fold_set:(folder:'a t -> state:'a -> Expression.t list -> 'a) ->
    ?fold_set_comprehension:(folder:'a t -> state:'a -> Expression.t Comprehension.t -> 'a) ->
    ?fold_starred:(folder:'a t -> state:'a -> Starred.t -> 'a) ->
    ?fold_ternary:(folder:'a t -> state:'a -> Ternary.t -> 'a) ->
    ?fold_tuple:(folder:'a t -> state:'a -> Expression.t list -> 'a) ->
    ?fold_unary_operator:(folder:'a t -> state:'a -> UnaryOperator.t -> 'a) ->
    ?fold_walrus_operator:(folder:'a t -> state:'a -> WalrusOperator.t -> 'a) ->
    ?fold_yield:(folder:'a t -> state:'a -> Expression.t option -> 'a) ->
    ?fold_yield_from:(folder:'a t -> state:'a -> Expression.t -> 'a) ->
    ?fold_location:(state:'a -> Location.t -> 'a) ->
    unit ->
    'a t
end

type t = Expression.t [@@deriving compare, sexp, show, hash, to_yojson]

type expression = Expression.expression [@@deriving compare, sexp, show, hash, to_yojson]

val location_insensitive_compare : t -> t -> int

val negate : t -> t

val normalize : t -> t

val is_false : t -> bool

val is_none : t -> bool

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
