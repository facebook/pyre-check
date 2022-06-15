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
    | BigInteger of string
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

  val string_literal_keys : Entry.t list -> (string * Expression.t) list option
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

module Mapper : sig
  type 'a t

  val map : mapper:'a t -> Expression.t -> 'a

  val map_list : mapper:'a t -> Expression.t list -> 'a list

  val map_option : mapper:'a t -> Expression.t option -> 'a option

  val create
    :  map_await:(mapper:'a t -> location:Location.t -> Expression.t -> 'a) ->
    map_boolean_operator:(mapper:'a t -> location:Location.t -> BooleanOperator.t -> 'a) ->
    map_call:(mapper:'a t -> location:Location.t -> Call.t -> 'a) ->
    map_comparison_operator:(mapper:'a t -> location:Location.t -> ComparisonOperator.t -> 'a) ->
    map_constant:(mapper:'a t -> location:Location.t -> Constant.t -> 'a) ->
    map_dictionary:(mapper:'a t -> location:Location.t -> Dictionary.t -> 'a) ->
    map_dictionary_comprehension:
      (mapper:'a t -> location:Location.t -> Dictionary.Entry.t Comprehension.t -> 'a) ->
    map_generator:(mapper:'a t -> location:Location.t -> Expression.t Comprehension.t -> 'a) ->
    map_format_string:(mapper:'a t -> location:Location.t -> Substring.t list -> 'a) ->
    map_lambda:(mapper:'a t -> location:Location.t -> Lambda.t -> 'a) ->
    map_list:(mapper:'a t -> location:Location.t -> Expression.t list -> 'a) ->
    map_list_comprehension:
      (mapper:'a t -> location:Location.t -> Expression.t Comprehension.t -> 'a) ->
    map_name:(mapper:'a t -> location:Location.t -> Name.t -> 'a) ->
    map_set:(mapper:'a t -> location:Location.t -> Expression.t list -> 'a) ->
    map_set_comprehension:(mapper:'a t -> location:Location.t -> Expression.t Comprehension.t -> 'a) ->
    map_starred:(mapper:'a t -> location:Location.t -> Starred.t -> 'a) ->
    map_ternary:(mapper:'a t -> location:Location.t -> Ternary.t -> 'a) ->
    map_tuple:(mapper:'a t -> location:Location.t -> Expression.t list -> 'a) ->
    map_unary_operator:(mapper:'a t -> location:Location.t -> UnaryOperator.t -> 'a) ->
    map_walrus_operator:(mapper:'a t -> location:Location.t -> WalrusOperator.t -> 'a) ->
    map_yield:(mapper:'a t -> location:Location.t -> Expression.t option -> 'a) ->
    map_yield_from:(mapper:'a t -> location:Location.t -> Expression.t -> 'a) ->
    unit ->
    'a t

  val create_default
    :  ?map_await:(mapper:Expression.t t -> location:Location.t -> Expression.t -> Expression.t) ->
    ?map_boolean_operator:
      (mapper:Expression.t t -> location:Location.t -> BooleanOperator.t -> Expression.t) ->
    ?map_call:(mapper:Expression.t t -> location:Location.t -> Call.t -> Expression.t) ->
    ?map_comparison_operator:
      (mapper:Expression.t t -> location:Location.t -> ComparisonOperator.t -> Expression.t) ->
    ?map_constant:(mapper:Expression.t t -> location:Location.t -> Constant.t -> Expression.t) ->
    ?map_dictionary:(mapper:Expression.t t -> location:Location.t -> Dictionary.t -> Expression.t) ->
    ?map_dictionary_comprehension:
      (mapper:Expression.t t ->
      location:Location.t ->
      Dictionary.Entry.t Comprehension.t ->
      Expression.t) ->
    ?map_generator:
      (mapper:Expression.t t -> location:Location.t -> Expression.t Comprehension.t -> Expression.t) ->
    ?map_format_string:
      (mapper:Expression.t t -> location:Location.t -> Substring.t list -> Expression.t) ->
    ?map_lambda:(mapper:Expression.t t -> location:Location.t -> Lambda.t -> Expression.t) ->
    ?map_list:(mapper:Expression.t t -> location:Location.t -> Expression.t list -> Expression.t) ->
    ?map_list_comprehension:
      (mapper:Expression.t t -> location:Location.t -> Expression.t Comprehension.t -> Expression.t) ->
    ?map_name:(mapper:Expression.t t -> location:Location.t -> Name.t -> Expression.t) ->
    ?map_set:(mapper:Expression.t t -> location:Location.t -> Expression.t list -> Expression.t) ->
    ?map_set_comprehension:
      (mapper:Expression.t t -> location:Location.t -> Expression.t Comprehension.t -> Expression.t) ->
    ?map_starred:(mapper:Expression.t t -> location:Location.t -> Starred.t -> Expression.t) ->
    ?map_ternary:(mapper:Expression.t t -> location:Location.t -> Ternary.t -> Expression.t) ->
    ?map_tuple:(mapper:Expression.t t -> location:Location.t -> Expression.t list -> Expression.t) ->
    ?map_unary_operator:
      (mapper:Expression.t t -> location:Location.t -> UnaryOperator.t -> Expression.t) ->
    ?map_walrus_operator:
      (mapper:Expression.t t -> location:Location.t -> WalrusOperator.t -> Expression.t) ->
    ?map_yield:(mapper:Expression.t t -> location:Location.t -> Expression.t option -> Expression.t) ->
    ?map_yield_from:(mapper:Expression.t t -> location:Location.t -> Expression.t -> Expression.t) ->
    unit ->
    Expression.t t

  val create_transformer
    :  ?map_await:(mapper:Expression.t t -> Expression.t -> Expression.t) ->
    ?map_boolean_operator:(mapper:Expression.t t -> BooleanOperator.t -> BooleanOperator.t) ->
    ?map_call:(mapper:Expression.t t -> Call.t -> Call.t) ->
    ?map_comparison_operator:(mapper:Expression.t t -> ComparisonOperator.t -> ComparisonOperator.t) ->
    ?map_constant:(mapper:Expression.t t -> Constant.t -> Constant.t) ->
    ?map_dictionary:(mapper:Expression.t t -> Dictionary.t -> Dictionary.t) ->
    ?map_dictionary_comprehension:
      (mapper:Expression.t t ->
      Dictionary.Entry.t Comprehension.t ->
      Dictionary.Entry.t Comprehension.t) ->
    ?map_generator:
      (mapper:Expression.t t -> Expression.t Comprehension.t -> Expression.t Comprehension.t) ->
    ?map_format_string:(mapper:Expression.t t -> Substring.t list -> Substring.t list) ->
    ?map_lambda:(mapper:Expression.t t -> Lambda.t -> Lambda.t) ->
    ?map_list:(mapper:Expression.t t -> Expression.t list -> Expression.t list) ->
    ?map_list_comprehension:
      (mapper:Expression.t t -> Expression.t Comprehension.t -> Expression.t Comprehension.t) ->
    ?map_name:(mapper:Expression.t t -> Name.t -> Name.t) ->
    ?map_set:(mapper:Expression.t t -> Expression.t list -> Expression.t list) ->
    ?map_set_comprehension:
      (mapper:Expression.t t -> Expression.t Comprehension.t -> Expression.t Comprehension.t) ->
    ?map_starred:(mapper:Expression.t t -> Starred.t -> Starred.t) ->
    ?map_ternary:(mapper:Expression.t t -> Ternary.t -> Ternary.t) ->
    ?map_tuple:(mapper:Expression.t t -> Expression.t list -> Expression.t list) ->
    ?map_unary_operator:(mapper:Expression.t t -> UnaryOperator.t -> UnaryOperator.t) ->
    ?map_walrus_operator:(mapper:Expression.t t -> WalrusOperator.t -> WalrusOperator.t) ->
    ?map_yield:(mapper:Expression.t t -> Expression.t option -> Expression.t option) ->
    ?map_yield_from:(mapper:Expression.t t -> Expression.t -> Expression.t) ->
    ?map_location:(Location.t -> Location.t) ->
    unit ->
    Expression.t t
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

val is_self_call : callee:t -> bool

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
