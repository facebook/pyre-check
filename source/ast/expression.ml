(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Sexplib.Std
open Pyre

module StringLiteral = struct
  type kind =
    | String
    | Bytes
  [@@deriving compare, sexp, show, hash, to_yojson]

  type t = {
    value: string;
    kind: kind;
  }
  [@@deriving compare, sexp, show, hash, to_yojson]

  let create ?(bytes = false) value =
    let kind =
      if bytes then
        Bytes
      else
        String
    in
    { value; kind }


  let location_insensitive_compare_kind left right =
    match left, right with
    | String, String
    | Bytes, Bytes ->
        0
    | String, _ -> -1
    | Bytes, _ -> 1


  let location_insensitive_compare left right =
    match String.compare left.value right.value with
    | x when not (Int.equal x 0) -> x
    | _ -> location_insensitive_compare_kind left.kind right.kind
end

module Constant = struct
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

  let location_insensitive_compare left right =
    match left, right with
    | NoneLiteral, NoneLiteral
    | Ellipsis, Ellipsis
    | False, False
    | True, True ->
        0
    | Integer left, Integer right -> Int.compare left right
    | Float left, Float right
    | Complex left, Complex right ->
        Float.compare left right
    | String left, String right -> StringLiteral.compare left right
    | BigInteger left, BigInteger right -> String.compare left right
    | NoneLiteral, _ -> -1
    | Ellipsis, _ -> -1
    | False, _ -> -1
    | True, _ -> -1
    | Integer _, _ -> -1
    | BigInteger _, _ -> 1
    | Float _, _ -> -1
    | Complex _, _ -> -1
    | String _, _ -> 1


  let pp formatter = function
    | String { StringLiteral.value; kind } ->
        let bytes =
          match kind with
          | StringLiteral.Bytes -> "b"
          | _ -> ""
        in
        Format.fprintf formatter "%s\"%s\"" bytes value
    | Ellipsis -> Format.fprintf formatter "..."
    | Float float_value -> Format.fprintf formatter "%f" float_value
    | Complex float_value -> Format.fprintf formatter "%fj" float_value
    | False -> Format.fprintf formatter "%s" "False"
    | Integer integer -> Format.fprintf formatter "%d" integer
    | BigInteger value -> Format.fprintf formatter "%s" value
    | NoneLiteral -> Format.fprintf formatter "None"
    | True -> Format.fprintf formatter "%s" "True"
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
end = struct
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

  let pp_boolean_operator formatter operator =
    Format.fprintf
      formatter
      "%s"
      (match operator with
      | And -> "and"
      | Or -> "or")


  let inverse = function
    | And -> Or
    | Or -> And


  let location_insensitive_compare left right =
    match Expression.location_insensitive_compare left.left right.left with
    | x when not (Int.equal x 0) -> x
    | _ -> (
        match [%compare: operator] left.operator right.operator with
        | x when not (Int.equal x 0) -> x
        | _ -> Expression.location_insensitive_compare left.right right.right)
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
end = struct
  module Argument = struct
    type t = {
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

    let location_insensitive_compare left right =
      match
        Option.compare
          (Node.location_insensitive_compare [%compare: Identifier.t])
          left.name
          right.name
      with
      | x when not (Int.equal x 0) -> x
      | _ -> Expression.location_insensitive_compare left.value right.value


    let unpack = function
      | { value = { Node.value = Starred (Starred.Once expression); _ }; _ } ->
          expression, SingleStar
      | { value = { Node.value = Starred (Starred.Twice expression); _ }; _ } ->
          expression, DoubleStar
      | { value; name = Some name } -> value, Named name
      | { value; name = None } -> value, Positional
  end

  type t = {
    callee: Expression.t;
    arguments: Argument.t list;
  }
  [@@deriving compare, sexp, show, hash, to_yojson]

  let location_insensitive_compare left right =
    match Expression.location_insensitive_compare left.callee right.callee with
    | x when not (Int.equal x 0) -> x
    | _ -> List.compare Argument.location_insensitive_compare left.arguments right.arguments
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
end = struct
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

  let inverse = function
    | Equals -> NotEquals
    | GreaterThan -> LessThanOrEquals
    | GreaterThanOrEquals -> LessThan
    | In -> NotIn
    | Is -> IsNot
    | IsNot -> Is
    | LessThan -> GreaterThanOrEquals
    | LessThanOrEquals -> GreaterThan
    | NotEquals -> Equals
    | NotIn -> In


  let pp_comparison_operator formatter operator =
    Format.fprintf
      formatter
      "%s"
      (match operator with
      | Equals -> "=="
      | GreaterThan -> ">"
      | GreaterThanOrEquals -> ">="
      | In -> "in"
      | Is -> "is"
      | IsNot -> "is not"
      | LessThan -> "<"
      | LessThanOrEquals -> "<="
      | NotEquals -> "!="
      | NotIn -> "not in")


  let override ~location { left; operator; right } =
    let left, right =
      match operator with
      | In -> right, left
      | _ -> left, right
    in
    let operator =
      match operator with
      | Equals -> Some "__eq__"
      | GreaterThan -> Some "__gt__"
      | GreaterThanOrEquals -> Some "__ge__"
      | Is
      | IsNot ->
          None
      | LessThan -> Some "__lt__"
      | LessThanOrEquals -> Some "__le__"
      | NotEquals -> Some "__ne__"
      | In -> None
      | NotIn -> None
    in
    operator
    >>| fun name ->
    let arguments = [{ Call.Argument.name = None; value = right }] in
    Expression.Call
      {
        callee =
          {
            Node.location;
            value = Name (Name.Attribute { base = left; attribute = name; special = true });
          };
        arguments;
      }
    |> Node.create ~location


  let location_insensitive_compare left right =
    match Expression.location_insensitive_compare left.left right.left with
    | x when not (Int.equal x 0) -> x
    | _ -> (
        match [%compare: operator] left.operator right.operator with
        | x when not (Int.equal x 0) -> x
        | _ -> Expression.location_insensitive_compare left.right right.right)
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
end = struct
  module Generator = struct
    type t = {
      target: Expression.t;
      iterator: Expression.t;
      conditions: Expression.t list;
      async: bool;
    }
    [@@deriving compare, sexp, show, hash, to_yojson]

    let location_insensitive_compare left right =
      match Expression.location_insensitive_compare left.target right.target with
      | x when not (Int.equal x 0) -> x
      | _ -> (
          match Expression.location_insensitive_compare left.iterator right.iterator with
          | x when not (Int.equal x 0) -> x
          | _ -> (
              match
                List.compare
                  Expression.location_insensitive_compare
                  left.conditions
                  right.conditions
              with
              | x when not (Int.equal x 0) -> x
              | _ -> Bool.compare left.async right.async))
  end

  type 'element t = {
    element: 'element;
    generators: Generator.t list;
  }
  [@@deriving compare, sexp, show, hash, to_yojson]

  let location_insensitive_compare compare_element left right =
    match compare_element left.element right.element with
    | x when not (Int.equal x 0) -> x
    | _ -> List.compare Generator.location_insensitive_compare left.generators right.generators
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
end = struct
  module Entry = struct
    type t = {
      key: Expression.t;
      value: Expression.t;
    }
    [@@deriving compare, sexp, show, hash, to_yojson]

    let location_insensitive_compare left right =
      match Expression.location_insensitive_compare left.key right.key with
      | x when not (Int.equal x 0) -> x
      | _ -> Expression.location_insensitive_compare left.value right.value
  end

  type t = {
    entries: Entry.t list;
    keywords: Expression.t list;
  }
  [@@deriving compare, sexp, show, hash, to_yojson]

  let location_insensitive_compare left right =
    match List.compare Entry.location_insensitive_compare left.entries right.entries with
    | x when not (Int.equal x 0) -> x
    | _ -> List.compare Expression.location_insensitive_compare left.keywords right.keywords


  let string_literal_keys entries =
    let rec match_literal_key entries accumulated_result =
      match entries with
      | {
          Dictionary.Entry.key =
            {
              Node.value = Expression.Constant (Constant.String { StringLiteral.value = key; _ });
              _;
            };
          value;
        }
        :: tail ->
          match_literal_key tail ((key, value) :: accumulated_result)
      | [] -> Some (List.rev accumulated_result)
      | _ -> None
    in
    match_literal_key entries []
end

and Lambda : sig
  type t = {
    parameters: Parameter.t list;
    body: Expression.t;
  }
  [@@deriving compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int
end = struct
  type t = {
    parameters: Parameter.t list;
    body: Expression.t;
  }
  [@@deriving compare, sexp, show, hash, to_yojson]

  let location_insensitive_compare left right =
    match List.compare Parameter.location_insensitive_compare left.parameters right.parameters with
    | x when not (Int.equal x 0) -> x
    | _ -> Expression.location_insensitive_compare left.body right.body
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
end = struct
  module Attribute = struct
    type t = {
      base: Expression.t;
      attribute: Identifier.t;
      special: bool;
    }
    [@@deriving compare, sexp, show, hash, to_yojson]

    let location_insensitive_compare left right =
      match Expression.location_insensitive_compare left.base right.base with
      | x when not (Int.equal x 0) -> x
      | _ -> (
          match [%compare: Identifier.t] left.attribute right.attribute with
          | x when not (Int.equal x 0) -> x
          | _ -> Bool.compare left.special right.special)
  end

  type t =
    | Attribute of Attribute.t
    | Identifier of Identifier.t
  [@@deriving compare, sexp, show, hash, to_yojson]

  let location_insensitive_compare left right =
    match left, right with
    | Attribute left, Attribute right -> Attribute.location_insensitive_compare left right
    | Identifier left, Identifier right -> [%compare: Identifier.t] left right
    | Attribute _, Identifier _ -> -1
    | Identifier _, Attribute _ -> 1


  let last = function
    | Identifier name -> name
    | Attribute { attribute; _ } -> attribute
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
end = struct
  type parameter = {
    name: Identifier.t;
    value: Expression.t option;
    annotation: Expression.t option;
  }
  [@@deriving compare, sexp, show, hash, to_yojson]

  type t = parameter Node.t [@@deriving compare, sexp, show, hash, to_yojson]

  let create ~location ?value ?annotation ~name () =
    { Node.location; value = { name; value; annotation } }


  let name { Node.value = { name; _ }; _ } = name

  let location_insensitive_compare_parameter left right =
    match [%compare: Identifier.t] left.name right.name with
    | x when not (Int.equal x 0) -> x
    | _ -> (
        match Option.compare Expression.location_insensitive_compare left.value right.value with
        | x when not (Int.equal x 0) -> x
        | _ ->
            Option.compare Expression.location_insensitive_compare left.annotation right.annotation)


  let location_insensitive_compare =
    Node.location_insensitive_compare location_insensitive_compare_parameter
end

and Starred : sig
  type t =
    | Once of Expression.t
    | Twice of Expression.t
  [@@deriving compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int
end = struct
  type t =
    | Once of Expression.t
    | Twice of Expression.t
  [@@deriving compare, sexp, show, hash, to_yojson]

  let location_insensitive_compare left right =
    match left, right with
    | Once left, Once right
    | Twice left, Twice right ->
        Expression.location_insensitive_compare left right
    | Once _, Twice _ -> -1
    | Twice _, Once _ -> 1
end

and Substring : sig
  type t =
    | Literal of string Node.t
    | Format of Expression.t
  [@@deriving compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int
end = struct
  type t =
    | Literal of string Node.t
    | Format of Expression.t
  [@@deriving compare, sexp, show, hash, to_yojson]

  let location_insensitive_compare left right =
    match left, right with
    | Literal left, Literal right -> Node.location_insensitive_compare String.compare left right
    | Format left, Format right -> Expression.location_insensitive_compare left right
    | Literal _, _ -> -1
    | Format _, _ -> 1
end

and Ternary : sig
  type t = {
    target: Expression.t;
    test: Expression.t;
    alternative: Expression.t;
  }
  [@@deriving compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int
end = struct
  type t = {
    target: Expression.t;
    test: Expression.t;
    alternative: Expression.t;
  }
  [@@deriving compare, sexp, show, hash, to_yojson]

  let location_insensitive_compare left right =
    match Expression.location_insensitive_compare left.target right.target with
    | x when not (Int.equal x 0) -> x
    | _ -> (
        match Expression.location_insensitive_compare left.test right.test with
        | x when not (Int.equal x 0) -> x
        | _ -> Expression.location_insensitive_compare left.alternative right.alternative)
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

  val pp_unary_operator : Format.formatter -> operator -> unit

  val override : t -> Expression.t option

  val location_insensitive_compare : t -> t -> int
end = struct
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

  let pp_unary_operator formatter operator =
    Format.fprintf
      formatter
      "%s"
      (match operator with
      | Invert -> "~"
      | Negative -> "-"
      | Not -> "not"
      | Positive -> "+")


  let override { operator; operand = { Node.location; _ } as operand } =
    (match operator with
    | Invert -> Some "__invert__"
    | Negative -> Some "__neg__"
    | Not -> None
    | Positive -> Some "__pos__")
    >>| fun name ->
    Expression.Call
      {
        callee =
          {
            Node.location;
            value = Name (Name.Attribute { base = operand; attribute = name; special = true });
          };
        arguments = [];
      }
    |> Node.create ~location


  let location_insensitive_compare left right =
    match [%compare: operator] left.operator right.operator with
    | x when not (Int.equal x 0) -> x
    | _ -> Expression.location_insensitive_compare left.operand right.operand
end

and WalrusOperator : sig
  type t = {
    target: Expression.t;
    value: Expression.t;
  }
  [@@deriving compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int
end = struct
  type t = {
    target: Expression.t;
    value: Expression.t;
  }
  [@@deriving compare, sexp, show, hash, to_yojson]

  let location_insensitive_compare left right =
    match Expression.location_insensitive_compare left.target right.target with
    | x when not (Int.equal x 0) -> x
    | _ -> Expression.location_insensitive_compare left.value right.value
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

  val pp_expression_list : Format.formatter -> t list -> unit

  val pp_expression_argument_list : Format.formatter -> Call.Argument.t list -> unit

  val pp_expression_parameter_list : Format.formatter -> Parameter.t list -> unit
end = struct
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

  let _ = show (* shadowed below *)

  let rec location_insensitive_compare_expression left right =
    match left, right with
    | Await left, Await right -> location_insensitive_compare left right
    | BooleanOperator left, BooleanOperator right ->
        BooleanOperator.location_insensitive_compare left right
    | Call left, Call right -> Call.location_insensitive_compare left right
    | ComparisonOperator left, ComparisonOperator right ->
        ComparisonOperator.location_insensitive_compare left right
    | Constant left, Constant right -> Constant.compare left right
    | Dictionary left, Dictionary right -> Dictionary.location_insensitive_compare left right
    | DictionaryComprehension left, DictionaryComprehension right ->
        Comprehension.location_insensitive_compare
          Dictionary.Entry.location_insensitive_compare
          left
          right
    | Generator left, Generator right ->
        Comprehension.location_insensitive_compare location_insensitive_compare left right
    | FormatString left, FormatString right ->
        List.compare Substring.location_insensitive_compare left right
    | Lambda left, Lambda right -> Lambda.location_insensitive_compare left right
    | List left, List right -> List.compare location_insensitive_compare left right
    | ListComprehension left, ListComprehension right ->
        Comprehension.location_insensitive_compare location_insensitive_compare left right
    | Name left, Name right -> Name.location_insensitive_compare left right
    | Set left, Set right -> List.compare location_insensitive_compare left right
    | SetComprehension left, SetComprehension right ->
        Comprehension.location_insensitive_compare location_insensitive_compare left right
    | Starred left, Starred right -> Starred.location_insensitive_compare left right
    | Ternary left, Ternary right -> Ternary.location_insensitive_compare left right
    | Tuple left, Tuple right -> List.compare location_insensitive_compare left right
    | UnaryOperator left, UnaryOperator right ->
        UnaryOperator.location_insensitive_compare left right
    | WalrusOperator left, WalrusOperator right ->
        WalrusOperator.location_insensitive_compare left right
    | Yield left, Yield right -> Option.compare location_insensitive_compare left right
    | YieldFrom left, YieldFrom right -> location_insensitive_compare left right
    | Await _, _ -> -1
    | BooleanOperator _, _ -> -1
    | Call _, _ -> -1
    | ComparisonOperator _, _ -> -1
    | Constant _, _ -> -1
    | Dictionary _, _ -> -1
    | DictionaryComprehension _, _ -> -1
    | Generator _, _ -> -1
    | FormatString _, _ -> -1
    | Lambda _, _ -> -1
    | List _, _ -> -1
    | ListComprehension _, _ -> -1
    | Name _, _ -> -1
    | Set _, _ -> -1
    | SetComprehension _, _ -> -1
    | Starred _, _ -> -1
    | Ternary _, _ -> -1
    | Tuple _, _ -> -1
    | UnaryOperator _, _ -> -1
    | WalrusOperator _, _ -> -1
    | Yield _, _ -> 1
    | YieldFrom _, _ -> 1


  and location_insensitive_compare left right =
    Node.location_insensitive_compare location_insensitive_compare_expression left right


  module PrettyPrinter = struct
    let rec pp_expression_t formatter expression_t =
      match expression_t with
      | { Node.value = expression; _ } -> Format.fprintf formatter "%a" pp_expression expression


    and pp_argument formatter { Call.Argument.name; value } =
      match name with
      | Some name -> Format.fprintf formatter "%s = %a" (Node.value name) pp_expression_t value
      | None -> Format.fprintf formatter "%a" pp_expression_t value


    and pp_argument_list formatter argument_list =
      match argument_list with
      | [] -> ()
      | [argument] -> Format.fprintf formatter "%a" pp_argument argument
      | argument :: argument_list ->
          Format.fprintf formatter "%a, %a" pp_argument argument pp_argument_list argument_list


    and pp_dictionary_entry formatter { Dictionary.Entry.key; value } =
      Format.fprintf formatter "%a:%a" pp_expression_t key pp_expression_t value


    and pp_dictionary formatter dictionary =
      match dictionary with
      | [] -> ()
      | [entry] -> pp_dictionary_entry formatter entry
      | entry :: dictionary ->
          Format.fprintf formatter "%a,%a" pp_dictionary_entry entry pp_dictionary dictionary


    and pp_expression_list formatter expression_list =
      match expression_list with
      | [] -> ()
      | [expression] -> Format.fprintf formatter "%a" pp_expression_t expression
      | expression :: expression_list ->
          Format.fprintf
            formatter
            "%a, %a"
            pp_expression_t
            expression
            pp_expression_list
            expression_list


    and pp_generator formatter { Comprehension.Generator.target; iterator; conditions; async } =
      Format.fprintf
        formatter
        "generator(%s%a in %a if %a)"
        (if async then "async " else "")
        pp_expression_t
        target
        pp_expression_t
        iterator
        pp_expression_list
        conditions


    and pp_generators formatter generators =
      match generators with
      | [] -> ()
      | [generator] -> Format.fprintf formatter "generators(%a)" pp_generator generator
      | generator :: xs ->
          Format.fprintf formatter "generators(%a, %a)" pp_generator generator pp_generators xs


    and pp_keywords formatter keywords =
      match keywords with
      | [] -> ()
      | [keyword] -> Format.fprintf formatter ", %a" pp_expression_t keyword
      | keyword :: keywords ->
          Format.fprintf formatter ", %a%a" pp_expression_t keyword pp_keywords keywords


    and pp_parameter formatter { Node.value = { Parameter.name; value; annotation }; _ } =
      let identifier = name in
      match value, annotation with
      | Some expression, Some annotation ->
          Format.fprintf
            formatter
            "%s: %a=%a"
            identifier
            pp_expression_t
            annotation
            pp_expression_t
            expression
      | None, Some annotation ->
          Format.fprintf formatter "%s: %a" identifier pp_expression_t annotation
      | Some expression, None ->
          Format.fprintf formatter "%s=%a" identifier pp_expression_t expression
      | None, None -> Format.fprintf formatter "%s" identifier


    and pp_parameter_list formatter parameter_list =
      match parameter_list with
      | [] -> ()
      | [parameter] -> Format.fprintf formatter "%a" pp_parameter parameter
      | parameter :: parameter_list ->
          Format.fprintf formatter "%a, %a" pp_parameter parameter pp_parameter_list parameter_list


    and pp_basic_comprehension formatter { Comprehension.element; generators } =
      (* shortcut for pretty printing (expression Node.t, expression Node.t) Comprehension.t *)
      Format.fprintf
        formatter
        "comprehension(%a for %a)"
        pp_expression_t
        element
        pp_generators
        generators


    and pp_starred formatter starred =
      match starred with
      | Starred.Once expression -> Format.fprintf formatter "*%a" pp_expression_t expression
      | Starred.Twice expression -> Format.fprintf formatter "**%a" pp_expression_t expression


    and pp_ternary formatter { Ternary.target; test; alternative } =
      Format.fprintf
        formatter
        "%a if %a else %a"
        pp_expression_t
        target
        pp_expression_t
        test
        pp_expression_t
        alternative


    and pp_expression formatter expression =
      match expression with
      | Await expression -> Format.fprintf formatter "await %a" pp_expression_t expression
      | BooleanOperator { BooleanOperator.left; operator; right } ->
          Format.fprintf
            formatter
            "%a %a %a"
            pp_expression_t
            left
            BooleanOperator.pp_boolean_operator
            operator
            pp_expression_t
            right
      | Call { Call.callee; arguments } -> (
          match Node.value callee with
          | Name (Name.Attribute { base; attribute = "__getitem__"; special = true }) ->
              Format.fprintf formatter "%a[%a]" pp_expression_t base pp_argument_list arguments
          | _ -> Format.fprintf formatter "%a(%a)" pp_expression_t callee pp_argument_list arguments
          )
      | FormatString substrings ->
          let pp_substring formatter = function
            | Substring.Literal { Node.value; _ } -> Format.fprintf formatter "\"%s\"" value
            | Substring.Format expression ->
                Format.fprintf formatter "f\"{%a}\"" pp_expression_t expression
          in
          List.iter substrings ~f:(pp_substring formatter)
      | ComparisonOperator { ComparisonOperator.left; operator; right } ->
          Format.fprintf
            formatter
            "%a %a %a"
            pp_expression_t
            left
            ComparisonOperator.pp_comparison_operator
            operator
            pp_expression_t
            right
      | Constant constant -> Format.fprintf formatter "%a" Constant.pp constant
      | Dictionary { Dictionary.entries; keywords } ->
          Format.fprintf formatter "{ %a%a }" pp_dictionary entries pp_keywords keywords
      | DictionaryComprehension { Comprehension.element; generators } ->
          Format.fprintf formatter "{ %a: %a }" pp_dictionary_entry element pp_generators generators
      | Generator generator -> Format.fprintf formatter "%a" pp_basic_comprehension generator
      | Lambda { Lambda.parameters; body } ->
          Format.fprintf
            formatter
            "lambda (%a) (%a)"
            pp_parameter_list
            parameters
            pp_expression_t
            body
      | List expression_list -> Format.fprintf formatter "[%a]" pp_expression_list expression_list
      | ListComprehension list_comprehension ->
          Format.fprintf formatter "%a" pp_basic_comprehension list_comprehension
      | Name (Name.Identifier name) -> Format.fprintf formatter "%s" name
      | Name (Name.Attribute { base; attribute; _ }) ->
          Format.fprintf formatter "%a.%s" pp_expression (Node.value base) attribute
      | Set set -> Format.fprintf formatter "set(%a)" pp_expression_list set
      | SetComprehension set_comprehension ->
          Format.fprintf formatter "set(%a)" pp_basic_comprehension set_comprehension
      | Starred starred -> Format.fprintf formatter "%a" pp_starred starred
      | Ternary ternary -> Format.fprintf formatter "%a" pp_ternary ternary
      | Tuple tuple -> Format.fprintf formatter "(%a)" pp_expression_list tuple
      | UnaryOperator { UnaryOperator.operator; operand } ->
          Format.fprintf
            formatter
            "%a %a"
            UnaryOperator.pp_unary_operator
            operator
            pp_expression_t
            operand
      | WalrusOperator { target; value } ->
          Format.fprintf formatter "%a := %a" pp_expression_t target pp_expression_t value
      | Yield yield -> (
          match yield with
          | Some yield -> Format.fprintf formatter "(yield %a)" pp_expression_t yield
          | None -> Format.fprintf formatter "(yield)")
      | YieldFrom yield -> Format.fprintf formatter "(yield from %a)" pp_expression_t yield


    let pp = pp_expression_t
  end

  let pp formatter expression = Format.fprintf formatter "%a" PrettyPrinter.pp expression

  let show expression = Format.asprintf "%a" pp expression

  let pp_expression_list formatter expression_list =
    Format.fprintf formatter "%a" PrettyPrinter.pp_expression_list expression_list


  let pp_expression_argument_list formatter expression_argument_list =
    Format.fprintf formatter "%a" PrettyPrinter.pp_argument_list expression_argument_list


  let pp_expression_parameter_list formatter expression_parameter_list =
    Format.fprintf formatter "%a" PrettyPrinter.pp_parameter_list expression_parameter_list
end

module Mapper = struct
  type 'a t = {
    map_await: mapper:'a t -> location:Location.t -> Expression.t -> 'a;
    map_boolean_operator: mapper:'a t -> location:Location.t -> BooleanOperator.t -> 'a;
    map_call: mapper:'a t -> location:Location.t -> Call.t -> 'a;
    map_comparison_operator: mapper:'a t -> location:Location.t -> ComparisonOperator.t -> 'a;
    map_constant: mapper:'a t -> location:Location.t -> Constant.t -> 'a;
    map_dictionary: mapper:'a t -> location:Location.t -> Dictionary.t -> 'a;
    map_dictionary_comprehension:
      mapper:'a t -> location:Location.t -> Dictionary.Entry.t Comprehension.t -> 'a;
    map_generator: mapper:'a t -> location:Location.t -> Expression.t Comprehension.t -> 'a;
    map_format_string: mapper:'a t -> location:Location.t -> Substring.t list -> 'a;
    map_lambda: mapper:'a t -> location:Location.t -> Lambda.t -> 'a;
    map_list: mapper:'a t -> location:Location.t -> Expression.t list -> 'a;
    map_list_comprehension:
      mapper:'a t -> location:Location.t -> Expression.t Comprehension.t -> 'a;
    map_name: mapper:'a t -> location:Location.t -> Name.t -> 'a;
    map_set: mapper:'a t -> location:Location.t -> Expression.t list -> 'a;
    map_set_comprehension: mapper:'a t -> location:Location.t -> Expression.t Comprehension.t -> 'a;
    map_starred: mapper:'a t -> location:Location.t -> Starred.t -> 'a;
    map_ternary: mapper:'a t -> location:Location.t -> Ternary.t -> 'a;
    map_tuple: mapper:'a t -> location:Location.t -> Expression.t list -> 'a;
    map_unary_operator: mapper:'a t -> location:Location.t -> UnaryOperator.t -> 'a;
    map_walrus_operator: mapper:'a t -> location:Location.t -> WalrusOperator.t -> 'a;
    map_yield: mapper:'a t -> location:Location.t -> Expression.t option -> 'a;
    map_yield_from: mapper:'a t -> location:Location.t -> Expression.t -> 'a;
  }

  let map
      ~mapper:
        ({
           map_await;
           map_boolean_operator;
           map_call;
           map_comparison_operator;
           map_constant;
           map_dictionary;
           map_dictionary_comprehension;
           map_generator;
           map_format_string;
           map_lambda;
           map_list;
           map_list_comprehension;
           map_name;
           map_set;
           map_set_comprehension;
           map_starred;
           map_ternary;
           map_tuple;
           map_unary_operator;
           map_walrus_operator;
           map_yield;
           map_yield_from;
         } as mapper)
      { Node.value; location }
    =
    match value with
    | Expression.Await expression -> map_await ~mapper ~location expression
    | Expression.BooleanOperator boolean_operator ->
        map_boolean_operator ~mapper ~location boolean_operator
    | Expression.Call call -> map_call ~mapper ~location call
    | Expression.FormatString substrings -> map_format_string ~mapper ~location substrings
    | Expression.ComparisonOperator comparison_operator ->
        map_comparison_operator ~mapper ~location comparison_operator
    | Expression.Constant constant -> map_constant ~mapper ~location constant
    | Expression.Dictionary dictionary -> map_dictionary ~mapper ~location dictionary
    | Expression.DictionaryComprehension dictionary_comprehension ->
        map_dictionary_comprehension ~mapper ~location dictionary_comprehension
    | Expression.Generator generator -> map_generator ~mapper ~location generator
    | Expression.Lambda lambda -> map_lambda ~mapper ~location lambda
    | Expression.List expression_list -> map_list ~mapper ~location expression_list
    | Expression.ListComprehension list_comprehension ->
        map_list_comprehension ~mapper ~location list_comprehension
    | Expression.Name name -> map_name ~mapper ~location name
    | Expression.Set set -> map_set ~mapper ~location set
    | Expression.SetComprehension set_comprehension ->
        map_set_comprehension ~mapper ~location set_comprehension
    | Expression.Starred starred -> map_starred ~mapper ~location starred
    | Expression.Ternary ternary -> map_ternary ~mapper ~location ternary
    | Expression.Tuple tuple -> map_tuple ~mapper ~location tuple
    | Expression.UnaryOperator unary_operator -> map_unary_operator ~mapper ~location unary_operator
    | Expression.WalrusOperator walrus_operator ->
        map_walrus_operator ~mapper ~location walrus_operator
    | Expression.Yield yield -> map_yield ~mapper ~location yield
    | Expression.YieldFrom yield_from -> map_yield_from ~mapper ~location yield_from


  let create_list_mapper f ~mapper items = List.map items ~f:(f ~mapper)

  let map_list ~mapper items = (create_list_mapper map) ~mapper items

  let map_option ~mapper = Option.map ~f:(map ~mapper)

  let default_map_dictionary_entry ~mapper { Dictionary.Entry.key; value } =
    { Dictionary.Entry.key = map ~mapper key; value = map ~mapper value }


  let default_map_dictionary_entries ~mapper entries =
    (create_list_mapper default_map_dictionary_entry) ~mapper entries


  let default_map_argument ~mapper { Call.Argument.name; value } =
    { Call.Argument.name; value = map ~mapper value }


  let default_map_arguments ~mapper arguments =
    (create_list_mapper default_map_argument) ~mapper arguments


  let default_map_parameter_with_location
      ~mapper
      ~map_location
      { Node.value = { Parameter.name; value; annotation }; location }
    =
    {
      Node.value =
        {
          Parameter.name;
          value = map_option ~mapper value;
          annotation = map_option ~mapper annotation;
        };
      location = map_location location;
    }


  let default_map_parameters_with_location ~mapper ~map_location parameters =
    (create_list_mapper (default_map_parameter_with_location ~map_location)) ~mapper parameters


  let default_map_parameters ~mapper parameters =
    default_map_parameters_with_location ~mapper ~map_location:Fn.id parameters


  let default_map_comprehension_generator
      ~mapper
      { Comprehension.Generator.target; iterator; conditions; async }
    =
    {
      Comprehension.Generator.target = map ~mapper target;
      iterator = map ~mapper iterator;
      conditions = map_list ~mapper conditions;
      async;
    }


  let default_map_comprehension_generators ~mapper generators =
    (create_list_mapper default_map_comprehension_generator) ~mapper generators


  let default_map_comprehension ~map_element ~mapper { Comprehension.element; generators } =
    {
      Comprehension.element = map_element ~mapper element;
      generators = default_map_comprehension_generators ~mapper generators;
    }


  let default_map_substring_with_location ~mapper ~map_location = function
    | Substring.Literal { Node.value; location } ->
        Substring.Literal { Node.value; location = map_location location }
    | Substring.Format expression -> Substring.Format (map ~mapper expression)


  let default_map_substrings_with_location ~mapper ~map_location substrings =
    (create_list_mapper (default_map_substring_with_location ~map_location)) ~mapper substrings


  let default_map_substrings ~mapper substrings =
    default_map_substrings_with_location ~mapper ~map_location:Fn.id substrings


  let default_map_await ~mapper awaited = map ~mapper awaited

  let default_map_await_node ~mapper ~location awaited =
    Node.create ~location (Expression.Await (default_map_await ~mapper awaited))


  let default_map_boolean_operator ~mapper { BooleanOperator.left; operator; right } =
    { BooleanOperator.left = map ~mapper left; operator; right = map ~mapper right }


  let default_map_boolean_operator_node ~mapper ~location boolean_operator =
    Node.create
      ~location
      (Expression.BooleanOperator (default_map_boolean_operator ~mapper boolean_operator))


  let default_map_call ~mapper { Call.callee; arguments } =
    { Call.callee = map ~mapper callee; arguments = default_map_arguments ~mapper arguments }


  let default_map_call_node ~mapper ~location call =
    Node.create ~location (Expression.Call (default_map_call ~mapper call))


  let default_map_comparison_operator ~mapper { ComparisonOperator.left; operator; right } =
    { ComparisonOperator.left = map ~mapper left; operator; right = map ~mapper right }


  let default_map_comparison_operator_node ~mapper ~location comparison_operator =
    Node.create
      ~location
      (Expression.ComparisonOperator (default_map_comparison_operator ~mapper comparison_operator))


  let default_map_constant ~mapper:_ constant = constant

  let default_map_constant_node ~mapper ~location constant =
    Node.create ~location (Expression.Constant (default_map_constant ~mapper constant))


  let default_map_dictionary ~mapper { Dictionary.entries; keywords } =
    {
      Dictionary.entries = default_map_dictionary_entries ~mapper entries;
      keywords = map_list ~mapper keywords;
    }


  let default_map_dictionary_node ~mapper ~location dictionary =
    Node.create ~location (Expression.Dictionary (default_map_dictionary ~mapper dictionary))


  let default_map_dictionary_comprehension ~mapper comprehension =
    default_map_comprehension ~map_element:default_map_dictionary_entry ~mapper comprehension


  let default_map_dictionary_comprehension_node ~mapper ~location comprehension =
    Node.create
      ~location
      (Expression.DictionaryComprehension
         (default_map_dictionary_comprehension ~mapper comprehension))


  let default_map_format_string ~mapper substrings = default_map_substrings ~mapper substrings

  let default_map_format_string_node ~mapper ~location format_string =
    Node.create
      ~location
      (Expression.FormatString (default_map_format_string ~mapper format_string))


  let default_map_generator ~mapper comprehension =
    default_map_comprehension ~map_element:map ~mapper comprehension


  let default_map_generator_node ~mapper ~location generator =
    Node.create ~location (Expression.Generator (default_map_generator ~mapper generator))


  let default_map_lambda ~mapper { Lambda.parameters; body } =
    { Lambda.parameters = default_map_parameters ~mapper parameters; body = map ~mapper body }


  let default_map_lambda_node ~mapper ~location lambda =
    Node.create ~location (Expression.Lambda (default_map_lambda ~mapper lambda))


  let default_map_list ~mapper expression_list = map_list ~mapper expression_list

  let default_map_list_node ~mapper ~location expression_list =
    Node.create ~location (Expression.List (default_map_list ~mapper expression_list))


  let default_map_list_comprehension ~mapper comprehension =
    default_map_comprehension ~map_element:map ~mapper comprehension


  let default_map_list_comprehension_node ~mapper ~location comprehension =
    Node.create
      ~location
      (Expression.ListComprehension (default_map_list_comprehension ~mapper comprehension))


  let default_map_name ~mapper = function
    | Name.Identifier _ as identifier -> identifier
    | Name.Attribute { base; attribute; special } ->
        Name.Attribute { base = map ~mapper base; attribute; special }


  let default_map_name_node ~mapper ~location name =
    Node.create ~location (Expression.Name (default_map_name ~mapper name))


  let default_map_set ~mapper expression_list = map_list ~mapper expression_list

  let default_map_set_node ~mapper ~location expression_list =
    Node.create ~location (Expression.Set (default_map_set ~mapper expression_list))


  let default_map_set_comprehension ~mapper comprehension =
    default_map_comprehension ~map_element:map ~mapper comprehension


  let default_map_set_comprehension_node ~mapper ~location comprehension =
    Node.create
      ~location
      (Expression.SetComprehension (default_map_set_comprehension ~mapper comprehension))


  let default_map_starred ~mapper = function
    | Starred.Once expression -> Starred.Once (map ~mapper expression)
    | Starred.Twice expression -> Starred.Twice (map ~mapper expression)


  let default_map_starred_node ~mapper ~location starred =
    Node.create ~location (Expression.Starred (default_map_starred ~mapper starred))


  let default_map_ternary ~mapper { Ternary.target; test; alternative } =
    {
      Ternary.target = map ~mapper target;
      test = map ~mapper test;
      alternative = map ~mapper alternative;
    }


  let default_map_ternary_node ~mapper ~location ternary =
    Node.create ~location (Expression.Ternary (default_map_ternary ~mapper ternary))


  let default_map_tuple ~mapper expression_list = map_list ~mapper expression_list

  let default_map_tuple_node ~mapper ~location expression_list =
    Node.create ~location (Expression.Tuple (default_map_tuple ~mapper expression_list))


  let default_map_unary_operator ~mapper { UnaryOperator.operator; operand } =
    { UnaryOperator.operator; operand = map ~mapper operand }


  let default_map_unary_operator_node ~mapper ~location unary_operator =
    Node.create
      ~location
      (Expression.UnaryOperator (default_map_unary_operator ~mapper unary_operator))


  let default_map_walrus_operator ~mapper { WalrusOperator.target; value } =
    { WalrusOperator.target = map ~mapper target; value = map ~mapper value }


  let default_map_walrus_operator_node ~mapper ~location walrus_operator =
    Node.create
      ~location
      (Expression.WalrusOperator (default_map_walrus_operator ~mapper walrus_operator))


  let default_map_yield ~mapper yield = map_option ~mapper yield

  let default_map_yield_node ~mapper ~location yield =
    Node.create ~location (Expression.Yield (default_map_yield ~mapper yield))


  let default_map_yield_from ~mapper yield_from = map ~mapper yield_from

  let default_map_yield_from_node ~mapper ~location yield_from =
    Node.create ~location (Expression.YieldFrom (default_map_yield_from ~mapper yield_from))


  let create
      ~map_await
      ~map_boolean_operator
      ~map_call
      ~map_comparison_operator
      ~map_constant
      ~map_dictionary
      ~map_dictionary_comprehension
      ~map_generator
      ~map_format_string
      ~map_lambda
      ~map_list
      ~map_list_comprehension
      ~map_name
      ~map_set
      ~map_set_comprehension
      ~map_starred
      ~map_ternary
      ~map_tuple
      ~map_unary_operator
      ~map_walrus_operator
      ~map_yield
      ~map_yield_from
      ()
    =
    {
      map_await;
      map_boolean_operator;
      map_call;
      map_comparison_operator;
      map_constant;
      map_dictionary;
      map_dictionary_comprehension;
      map_generator;
      map_format_string;
      map_lambda;
      map_list;
      map_list_comprehension;
      map_name;
      map_set;
      map_set_comprehension;
      map_starred;
      map_ternary;
      map_tuple;
      map_unary_operator;
      map_walrus_operator;
      map_yield;
      map_yield_from;
    }


  let create_default
      ?(map_await = default_map_await_node)
      ?(map_boolean_operator = default_map_boolean_operator_node)
      ?(map_call = default_map_call_node)
      ?(map_comparison_operator = default_map_comparison_operator_node)
      ?(map_constant = default_map_constant_node)
      ?(map_dictionary = default_map_dictionary_node)
      ?(map_dictionary_comprehension = default_map_dictionary_comprehension_node)
      ?(map_generator = default_map_generator_node)
      ?(map_format_string = default_map_format_string_node)
      ?(map_lambda = default_map_lambda_node)
      ?(map_list = default_map_list_node)
      ?(map_list_comprehension = default_map_list_comprehension_node)
      ?(map_name = default_map_name_node)
      ?(map_set = default_map_set_node)
      ?(map_set_comprehension = default_map_set_comprehension_node)
      ?(map_starred = default_map_starred_node)
      ?(map_ternary = default_map_ternary_node)
      ?(map_tuple = default_map_tuple_node)
      ?(map_unary_operator = default_map_unary_operator_node)
      ?(map_walrus_operator = default_map_walrus_operator_node)
      ?(map_yield = default_map_yield_node)
      ?(map_yield_from = default_map_yield_from_node)
      ()
    =
    {
      map_await;
      map_boolean_operator;
      map_call;
      map_comparison_operator;
      map_constant;
      map_dictionary;
      map_dictionary_comprehension;
      map_generator;
      map_format_string;
      map_lambda;
      map_list;
      map_list_comprehension;
      map_name;
      map_set;
      map_set_comprehension;
      map_starred;
      map_ternary;
      map_tuple;
      map_unary_operator;
      map_walrus_operator;
      map_yield;
      map_yield_from;
    }


  let create_transformer
      ?(map_await = default_map_await)
      ?(map_boolean_operator = default_map_boolean_operator)
      ?(map_call = default_map_call)
      ?(map_comparison_operator = default_map_comparison_operator)
      ?(map_constant = default_map_constant)
      ?(map_dictionary = default_map_dictionary)
      ?(map_dictionary_comprehension = default_map_dictionary_comprehension)
      ?(map_generator = default_map_generator)
      ?map_format_string
      ?map_lambda
      ?(map_list = default_map_list)
      ?(map_list_comprehension = default_map_list_comprehension)
      ?(map_name = default_map_name)
      ?(map_set = default_map_set)
      ?(map_set_comprehension = default_map_set_comprehension)
      ?(map_starred = default_map_starred)
      ?(map_ternary = default_map_ternary)
      ?(map_tuple = default_map_tuple)
      ?(map_unary_operator = default_map_unary_operator)
      ?(map_walrus_operator = default_map_walrus_operator)
      ?(map_yield = default_map_yield)
      ?(map_yield_from = default_map_yield_from)
      ?(map_location = Fn.id)
      ()
    =
    let map_await ~mapper ~location awaited =
      Node.create ~location:(map_location location) (Expression.Await (map_await ~mapper awaited))
    in
    let map_boolean_operator ~mapper ~location boolean_operator =
      Node.create
        ~location:(map_location location)
        (Expression.BooleanOperator (map_boolean_operator ~mapper boolean_operator))
    in
    let map_call ~mapper ~location call =
      Node.create ~location:(map_location location) (Expression.Call (map_call ~mapper call))
    in
    let map_comparison_operator ~mapper ~location comparison_operator =
      Node.create
        ~location:(map_location location)
        (Expression.ComparisonOperator (map_comparison_operator ~mapper comparison_operator))
    in
    let map_constant ~mapper ~location constant =
      Node.create
        ~location:(map_location location)
        (Expression.Constant (map_constant ~mapper constant))
    in
    let map_dictionary ~mapper ~location dictionary =
      Node.create
        ~location:(map_location location)
        (Expression.Dictionary (map_dictionary ~mapper dictionary))
    in
    let map_dictionary_comprehension ~mapper ~location comprehension =
      Node.create
        ~location:(map_location location)
        (Expression.DictionaryComprehension (map_dictionary_comprehension ~mapper comprehension))
    in
    let map_format_string ~mapper ~location format_string =
      let map_format_string =
        match map_format_string with
        | Some map_format_string -> map_format_string
        | None ->
            fun ~mapper substrings ->
              default_map_substrings_with_location ~mapper ~map_location substrings
      in
      Node.create
        ~location:(map_location location)
        (Expression.FormatString (map_format_string ~mapper format_string))
    in
    let map_generator ~mapper ~location generator =
      Node.create
        ~location:(map_location location)
        (Expression.Generator (map_generator ~mapper generator))
    in
    let map_lambda ~mapper ~location lambda =
      let map_lambda =
        match map_lambda with
        | Some map_lambda -> map_lambda
        | None ->
            fun ~mapper { Lambda.parameters; body } ->
              {
                Lambda.parameters =
                  default_map_parameters_with_location ~mapper ~map_location parameters;
                body = map ~mapper body;
              }
      in
      Node.create ~location:(map_location location) (Expression.Lambda (map_lambda ~mapper lambda))
    in
    let map_list ~mapper ~location expression_list =
      Node.create
        ~location:(map_location location)
        (Expression.List (map_list ~mapper expression_list))
    in
    let map_list_comprehension ~mapper ~location comprehension =
      Node.create
        ~location:(map_location location)
        (Expression.ListComprehension (map_list_comprehension ~mapper comprehension))
    in
    let map_name ~mapper ~location name =
      Node.create ~location:(map_location location) (Expression.Name (map_name ~mapper name))
    in
    let map_set ~mapper ~location expression_list =
      Node.create
        ~location:(map_location location)
        (Expression.Set (map_set ~mapper expression_list))
    in
    let map_set_comprehension ~mapper ~location comprehension =
      Node.create
        ~location:(map_location location)
        (Expression.SetComprehension (map_set_comprehension ~mapper comprehension))
    in
    let map_starred ~mapper ~location starred =
      Node.create
        ~location:(map_location location)
        (Expression.Starred (map_starred ~mapper starred))
    in
    let map_ternary ~mapper ~location ternary =
      Node.create
        ~location:(map_location location)
        (Expression.Ternary (map_ternary ~mapper ternary))
    in
    let map_tuple ~mapper ~location expression_list =
      Node.create
        ~location:(map_location location)
        (Expression.Tuple (map_tuple ~mapper expression_list))
    in
    let map_unary_operator ~mapper ~location unary_operator =
      Node.create
        ~location:(map_location location)
        (Expression.UnaryOperator (map_unary_operator ~mapper unary_operator))
    in
    let map_walrus_operator ~mapper ~location walrus_operator =
      Node.create
        ~location:(map_location location)
        (Expression.WalrusOperator (map_walrus_operator ~mapper walrus_operator))
    in
    let map_yield ~mapper ~location yield =
      Node.create ~location:(map_location location) (Expression.Yield (map_yield ~mapper yield))
    in
    let map_yield_from ~mapper ~location yield_from =
      Node.create
        ~location:(map_location location)
        (Expression.YieldFrom (map_yield_from ~mapper yield_from))
    in
    create_default
      ~map_await
      ~map_boolean_operator
      ~map_call
      ~map_comparison_operator
      ~map_constant
      ~map_dictionary
      ~map_dictionary_comprehension
      ~map_generator
      ~map_format_string
      ~map_lambda
      ~map_list
      ~map_list_comprehension
      ~map_name
      ~map_set
      ~map_set_comprehension
      ~map_starred
      ~map_ternary
      ~map_tuple
      ~map_unary_operator
      ~map_walrus_operator
      ~map_yield
      ~map_yield_from
      ()
end

module Folder = struct
  type 'a t = {
    fold_await: folder:'a t -> state:'a -> location:Location.t -> Expression.t -> 'a;
    fold_boolean_operator:
      folder:'a t -> state:'a -> location:Location.t -> BooleanOperator.t -> 'a;
    fold_call: folder:'a t -> state:'a -> location:Location.t -> Call.t -> 'a;
    fold_comparison_operator:
      folder:'a t -> state:'a -> location:Location.t -> ComparisonOperator.t -> 'a;
    fold_constant: folder:'a t -> state:'a -> location:Location.t -> Constant.t -> 'a;
    fold_dictionary: folder:'a t -> state:'a -> location:Location.t -> Dictionary.t -> 'a;
    fold_dictionary_comprehension:
      folder:'a t -> state:'a -> location:Location.t -> Dictionary.Entry.t Comprehension.t -> 'a;
    fold_generator:
      folder:'a t -> state:'a -> location:Location.t -> Expression.t Comprehension.t -> 'a;
    fold_format_string: folder:'a t -> state:'a -> location:Location.t -> Substring.t list -> 'a;
    fold_lambda: folder:'a t -> state:'a -> location:Location.t -> Lambda.t -> 'a;
    fold_list: folder:'a t -> state:'a -> location:Location.t -> Expression.t list -> 'a;
    fold_list_comprehension:
      folder:'a t -> state:'a -> location:Location.t -> Expression.t Comprehension.t -> 'a;
    fold_name: folder:'a t -> state:'a -> location:Location.t -> Name.t -> 'a;
    fold_set: folder:'a t -> state:'a -> location:Location.t -> Expression.t list -> 'a;
    fold_set_comprehension:
      folder:'a t -> state:'a -> location:Location.t -> Expression.t Comprehension.t -> 'a;
    fold_starred: folder:'a t -> state:'a -> location:Location.t -> Starred.t -> 'a;
    fold_ternary: folder:'a t -> state:'a -> location:Location.t -> Ternary.t -> 'a;
    fold_tuple: folder:'a t -> state:'a -> location:Location.t -> Expression.t list -> 'a;
    fold_unary_operator: folder:'a t -> state:'a -> location:Location.t -> UnaryOperator.t -> 'a;
    fold_walrus_operator: folder:'a t -> state:'a -> location:Location.t -> WalrusOperator.t -> 'a;
    fold_yield: folder:'a t -> state:'a -> location:Location.t -> Expression.t option -> 'a;
    fold_yield_from: folder:'a t -> state:'a -> location:Location.t -> Expression.t -> 'a;
  }

  let fold
      ~folder:
        ({
           fold_await;
           fold_boolean_operator;
           fold_call;
           fold_comparison_operator;
           fold_constant;
           fold_dictionary;
           fold_dictionary_comprehension;
           fold_generator;
           fold_format_string;
           fold_lambda;
           fold_list;
           fold_list_comprehension;
           fold_name;
           fold_set;
           fold_set_comprehension;
           fold_starred;
           fold_ternary;
           fold_tuple;
           fold_unary_operator;
           fold_walrus_operator;
           fold_yield;
           fold_yield_from;
         } as folder)
      ~state
      { Node.value; location }
    =
    match value with
    | Expression.Await expression -> fold_await ~folder ~state ~location expression
    | Expression.BooleanOperator boolean_operator ->
        fold_boolean_operator ~folder ~state ~location boolean_operator
    | Expression.Call call -> fold_call ~folder ~state ~location call
    | Expression.FormatString substrings -> fold_format_string ~folder ~state ~location substrings
    | Expression.ComparisonOperator comparison_operator ->
        fold_comparison_operator ~folder ~state ~location comparison_operator
    | Expression.Constant constant -> fold_constant ~folder ~state ~location constant
    | Expression.Dictionary dictionary -> fold_dictionary ~folder ~state ~location dictionary
    | Expression.DictionaryComprehension dictionary_comprehension ->
        fold_dictionary_comprehension ~folder ~state ~location dictionary_comprehension
    | Expression.Generator generator -> fold_generator ~folder ~state ~location generator
    | Expression.Lambda lambda -> fold_lambda ~folder ~state ~location lambda
    | Expression.List expression_list -> fold_list ~folder ~state ~location expression_list
    | Expression.ListComprehension list_comprehension ->
        fold_list_comprehension ~folder ~state ~location list_comprehension
    | Expression.Name name -> fold_name ~folder ~state ~location name
    | Expression.Set set -> fold_set ~folder ~state ~location set
    | Expression.SetComprehension set_comprehension ->
        fold_set_comprehension ~folder ~state ~location set_comprehension
    | Expression.Starred starred -> fold_starred ~folder ~state ~location starred
    | Expression.Ternary ternary -> fold_ternary ~folder ~state ~location ternary
    | Expression.Tuple tuple -> fold_tuple ~folder ~state ~location tuple
    | Expression.UnaryOperator unary_operator ->
        fold_unary_operator ~folder ~state ~location unary_operator
    | Expression.WalrusOperator walrus_operator ->
        fold_walrus_operator ~folder ~state ~location walrus_operator
    | Expression.Yield yield -> fold_yield ~folder ~state ~location yield
    | Expression.YieldFrom yield_from -> fold_yield_from ~folder ~state ~location yield_from


  let create_list_folder f ~folder ~state items =
    List.fold items ~init:state ~f:(fun state item -> f ~folder ~state item)


  let fold_list ~folder ~state items = (create_list_folder fold) ~folder ~state items

  let fold_option ~folder ~state =
    Option.fold ~init:state ~f:(fun state item -> fold ~folder ~state item)


  let default_fold_dictionary_entry ~folder ~state { Dictionary.Entry.key; value } =
    let state = fold ~folder ~state key in
    fold ~folder ~state value


  let default_fold_dictionary_entries ~folder ~state entries =
    (create_list_folder default_fold_dictionary_entry) ~folder ~state entries


  let default_fold_argument ~folder ~state { Call.Argument.name = _; value } =
    fold ~folder ~state value


  let default_fold_arguments ~folder ~state arguments =
    (create_list_folder default_fold_argument) ~folder ~state arguments


  let default_fold_location ~state _ = state

  let default_fold_parameter_with_location
      ~folder
      ~state
      ~fold_location
      { Node.value = { Parameter.name = _; value; annotation }; location }
    =
    let state = fold_location ~state location in
    let state = fold_option ~folder ~state value in
    fold_option ~folder ~state annotation


  let default_fold_parameters_with_location ~folder ~state ~fold_location parameters =
    (create_list_folder (default_fold_parameter_with_location ~fold_location))
      ~folder
      ~state
      parameters


  let default_fold_parameters ~folder ~state parameters =
    default_fold_parameters_with_location
      ~folder
      ~state
      ~fold_location:default_fold_location
      parameters


  let default_fold_comprehension_generator
      ~folder
      ~state
      { Comprehension.Generator.target; iterator; conditions; async = _ }
    =
    let state = fold ~folder ~state target in
    let state = fold ~folder ~state iterator in
    fold_list ~folder ~state conditions


  let default_fold_comprehension_generators ~folder ~state generators =
    (create_list_folder default_fold_comprehension_generator) ~folder ~state generators


  let default_fold_comprehension ~fold_element ~folder ~state { Comprehension.element; generators } =
    let state = fold_element ~folder ~state element in
    default_fold_comprehension_generators ~folder ~state generators


  let default_fold_substring_with_location ~folder ~state ~fold_location = function
    | Substring.Literal { Node.value = _; location } -> fold_location ~state location
    | Substring.Format expression -> fold ~folder ~state expression


  let default_fold_substrings_with_location ~folder ~state ~fold_location substrings =
    (create_list_folder (default_fold_substring_with_location ~fold_location))
      ~folder
      ~state
      substrings


  let default_fold_substrings ~folder ~state substrings =
    default_fold_substrings_with_location
      ~folder
      ~state
      ~fold_location:default_fold_location
      substrings


  let default_fold_await ~folder ~state awaited = fold ~folder ~state awaited

  let default_fold_boolean_operator ~folder ~state { BooleanOperator.left; operator = _; right } =
    let state = fold ~folder ~state left in
    fold ~folder ~state right


  let default_fold_call ~folder ~state { Call.callee; arguments } =
    let state = fold ~folder ~state callee in
    default_fold_arguments ~folder ~state arguments


  let default_fold_comparison_operator
      ~folder
      ~state
      { ComparisonOperator.left; operator = _; right }
    =
    let state = fold ~folder ~state left in
    fold ~folder ~state right


  let default_fold_constant ~folder:_ ~state _ = state

  let default_fold_dictionary ~folder ~state { Dictionary.entries; keywords } =
    let state = default_fold_dictionary_entries ~folder ~state entries in
    fold_list ~folder ~state keywords


  let default_fold_dictionary_comprehension ~folder ~state comprehension =
    default_fold_comprehension
      ~fold_element:default_fold_dictionary_entry
      ~folder
      ~state
      comprehension


  let default_fold_format_string ~folder ~state substrings =
    default_fold_substrings ~folder ~state substrings


  let default_fold_generator ~folder ~state comprehension =
    default_fold_comprehension ~fold_element:fold ~folder ~state comprehension


  let default_fold_lambda ~folder ~state { Lambda.parameters; body } =
    let state = default_fold_parameters ~folder ~state parameters in
    fold ~folder ~state body


  let default_fold_list ~folder ~state expression_list = fold_list ~folder ~state expression_list

  let default_fold_name ~folder ~state = function
    | Name.Identifier _ -> state
    | Name.Attribute { base; attribute = _; special = _ } -> fold ~folder ~state base


  let default_fold_starred ~folder ~state = function
    | Starred.Once expression
    | Starred.Twice expression ->
        fold ~folder ~state expression


  let default_fold_ternary ~folder ~state { Ternary.target; test; alternative } =
    let state = fold ~folder ~state target in
    let state = fold ~folder ~state test in
    fold ~folder ~state alternative


  let default_fold_unary_operator ~folder ~state { UnaryOperator.operator = _; operand } =
    fold ~folder ~state operand


  let default_fold_walrus_operator ~folder ~state { WalrusOperator.target; value } =
    let state = fold ~folder ~state target in
    fold ~folder ~state value


  let default_fold_yield ~folder ~state yield = fold_option ~folder ~state yield

  let default_fold_yield_from ~folder ~state yield_from = fold ~folder ~state yield_from

  let fold_ignoring_location f ~folder ~state ~location:_ value = f ~folder ~state value

  let create
      ?(fold_await = fold_ignoring_location default_fold_await)
      ?(fold_boolean_operator = fold_ignoring_location default_fold_boolean_operator)
      ?(fold_call = fold_ignoring_location default_fold_call)
      ?(fold_comparison_operator = fold_ignoring_location default_fold_comparison_operator)
      ?(fold_constant = fold_ignoring_location default_fold_constant)
      ?(fold_dictionary = fold_ignoring_location default_fold_dictionary)
      ?(fold_dictionary_comprehension =
        fold_ignoring_location default_fold_dictionary_comprehension)
      ?(fold_generator = fold_ignoring_location default_fold_generator)
      ?(fold_format_string = fold_ignoring_location default_fold_format_string)
      ?(fold_lambda = fold_ignoring_location default_fold_lambda)
      ?(fold_list = fold_ignoring_location default_fold_list)
      ?(fold_list_comprehension = fold_ignoring_location default_fold_generator)
      ?(fold_name = fold_ignoring_location default_fold_name)
      ?(fold_set = fold_ignoring_location default_fold_list)
      ?(fold_set_comprehension = fold_ignoring_location default_fold_generator)
      ?(fold_starred = fold_ignoring_location default_fold_starred)
      ?(fold_ternary = fold_ignoring_location default_fold_ternary)
      ?(fold_tuple = fold_ignoring_location default_fold_list)
      ?(fold_unary_operator = fold_ignoring_location default_fold_unary_operator)
      ?(fold_walrus_operator = fold_ignoring_location default_fold_walrus_operator)
      ?(fold_yield = fold_ignoring_location default_fold_yield)
      ?(fold_yield_from = fold_ignoring_location default_fold_yield_from)
      ()
    =
    {
      fold_await;
      fold_boolean_operator;
      fold_call;
      fold_comparison_operator;
      fold_constant;
      fold_dictionary;
      fold_dictionary_comprehension;
      fold_generator;
      fold_format_string;
      fold_lambda;
      fold_list;
      fold_list_comprehension;
      fold_name;
      fold_set;
      fold_set_comprehension;
      fold_starred;
      fold_ternary;
      fold_tuple;
      fold_unary_operator;
      fold_walrus_operator;
      fold_yield;
      fold_yield_from;
    }


  let create_with_uniform_location_fold
      ?(fold_await = default_fold_await)
      ?(fold_boolean_operator = default_fold_boolean_operator)
      ?(fold_call = default_fold_call)
      ?(fold_comparison_operator = default_fold_comparison_operator)
      ?(fold_constant = default_fold_constant)
      ?(fold_dictionary = default_fold_dictionary)
      ?(fold_dictionary_comprehension = default_fold_dictionary_comprehension)
      ?(fold_generator = default_fold_generator)
      ?fold_format_string
      ?fold_lambda
      ?(fold_list = default_fold_list)
      ?(fold_list_comprehension = default_fold_generator)
      ?(fold_name = default_fold_name)
      ?(fold_set = default_fold_list)
      ?(fold_set_comprehension = default_fold_generator)
      ?(fold_starred = default_fold_starred)
      ?(fold_ternary = default_fold_ternary)
      ?(fold_tuple = default_fold_list)
      ?(fold_unary_operator = default_fold_unary_operator)
      ?(fold_walrus_operator = default_fold_walrus_operator)
      ?(fold_yield = default_fold_yield)
      ?(fold_yield_from = default_fold_yield_from)
      ?(fold_location = default_fold_location)
      ()
    =
    let fold_lambda =
      match fold_lambda with
      | Some fold_lambda -> fold_lambda
      | None ->
          fun ~folder ~state { Lambda.parameters; body } ->
            let state =
              default_fold_parameters_with_location ~folder ~state ~fold_location parameters
            in
            fold ~folder ~state body
    in
    let fold_format_string =
      match fold_format_string with
      | Some fold_format_string -> fold_format_string
      | None ->
          fun ~folder ~state substrings ->
            default_fold_substrings_with_location ~folder ~state ~fold_location substrings
    in
    let fold_with_location f ~folder ~state ~location item =
      let state = fold_location ~state location in
      f ~folder ~state item
    in
    {
      fold_await = fold_with_location fold_await;
      fold_boolean_operator = fold_with_location fold_boolean_operator;
      fold_call = fold_with_location fold_call;
      fold_comparison_operator = fold_with_location fold_comparison_operator;
      fold_constant = fold_with_location fold_constant;
      fold_dictionary = fold_with_location fold_dictionary;
      fold_dictionary_comprehension = fold_with_location fold_dictionary_comprehension;
      fold_generator = fold_with_location fold_generator;
      fold_format_string = fold_with_location fold_format_string;
      fold_lambda = fold_with_location fold_lambda;
      fold_list = fold_with_location fold_list;
      fold_list_comprehension = fold_with_location fold_list_comprehension;
      fold_name = fold_with_location fold_name;
      fold_set = fold_with_location fold_set;
      fold_set_comprehension = fold_with_location fold_set_comprehension;
      fold_starred = fold_with_location fold_starred;
      fold_ternary = fold_with_location fold_ternary;
      fold_tuple = fold_with_location fold_tuple;
      fold_unary_operator = fold_with_location fold_unary_operator;
      fold_walrus_operator = fold_with_location fold_walrus_operator;
      fold_yield = fold_with_location fold_yield;
      fold_yield_from = fold_with_location fold_yield_from;
    }
end

include Expression

let negate ({ Node.location; value } as node) =
  match value with
  | UnaryOperator { UnaryOperator.operator = UnaryOperator.Not; operand } -> operand
  | ComparisonOperator { ComparisonOperator.operator = ComparisonOperator.IsNot; left; right } ->
      {
        Node.location;
        value =
          ComparisonOperator { ComparisonOperator.operator = ComparisonOperator.Is; left; right };
      }
  | ComparisonOperator { ComparisonOperator.operator = ComparisonOperator.Is; left; right } ->
      {
        Node.location;
        value =
          ComparisonOperator { ComparisonOperator.operator = ComparisonOperator.IsNot; left; right };
      }
  | _ ->
      {
        Node.location;
        value = UnaryOperator { UnaryOperator.operator = UnaryOperator.Not; operand = node };
      }


(* Changes boolean expressions to negation normal form *)
let rec normalize { Node.location; value } =
  let normalized =
    match value with
    | BooleanOperator { BooleanOperator.operator; left; right } ->
        BooleanOperator { BooleanOperator.operator; left = normalize left; right = normalize right }
    | UnaryOperator { UnaryOperator.operator = UnaryOperator.Not; operand = { Node.value; _ } } as
      unary -> (
        match value with
        | ComparisonOperator { ComparisonOperator.left; operator; right } ->
            ComparisonOperator
              { ComparisonOperator.left; operator = ComparisonOperator.inverse operator; right }
        | Constant Constant.False -> Constant Constant.True
        | Constant Constant.True -> Constant Constant.False
        | UnaryOperator { UnaryOperator.operator = UnaryOperator.Not; operand = { Node.value; _ } }
          ->
            value
        | BooleanOperator { BooleanOperator.left; operator; right } ->
            BooleanOperator
              {
                BooleanOperator.operator = BooleanOperator.inverse operator;
                left = normalize (negate left);
                right = normalize (negate right);
              }
        | _ -> unary)
    | _ -> value
  in
  { Node.location; value = normalized }


let is_false { Node.value; _ } =
  match value with
  | Constant Constant.False -> true
  | _ -> false


let is_none { Node.value; _ } =
  match value with
  | Constant Constant.NoneLiteral -> true
  | _ -> false


let create_name_from_identifiers identifiers =
  let rec create = function
    | [] -> failwith "Name must have non-zero identifiers."
    | [{ Node.location; value = identifier }] ->
        Name (Name.Identifier identifier) |> Node.create ~location
    | { Node.location; value = identifier } :: rest ->
        Name (Name.Attribute { base = create rest; attribute = identifier; special = false })
        |> Node.create ~location
  in
  match create (List.rev identifiers) with
  | { Node.value = Name name; _ } -> name
  | _ -> failwith "Impossible."


let create_name ~location name =
  let identifier_names name =
    if String.equal name "..." then
      [name]
    else
      String.split ~on:'.' name
  in
  identifier_names name |> List.map ~f:(Node.create ~location) |> create_name_from_identifiers


let create_name_from_reference ~location reference =
  let rec create = function
    | [] -> Name (Name.Identifier "") |> Node.create ~location
    | [identifier] -> Name (Name.Identifier identifier) |> Node.create ~location
    | identifier :: rest ->
        Name (Name.Attribute { base = create rest; attribute = identifier; special = false })
        |> Node.create ~location
  in
  match create (List.rev (Reference.as_list reference)) with
  | { Node.value = Name name; _ } -> name
  | _ -> failwith "Impossible."


let from_reference ~location reference =
  create_name_from_reference ~location reference |> (fun name -> Name name) |> Node.create ~location


let name_to_identifiers name =
  let rec collect sofar name =
    match sofar, name with
    | Some sofar, Name (Name.Identifier identifier) -> Some (identifier :: sofar)
    | Some sofar, Name (Name.Attribute { base; attribute; _ }) ->
        collect (Some (attribute :: sofar)) (Node.value base)
    | _ -> None
  in
  collect (Some []) (Name name)


let name_to_reference name =
  let rec get_reversed_identifiers = function
    | Name.Identifier identifier -> Some [identifier]
    | Name.Attribute { base = { Node.value = Name base; _ }; attribute; _ } -> (
        match get_reversed_identifiers base with
        | Some sofar -> Some (attribute :: sofar)
        | None -> None)
    | _ -> None
  in
  get_reversed_identifiers name >>| List.rev >>| Reference.create_from_list


let name_to_reference_exn name =
  match name_to_reference name with
  | Some name -> name
  | None ->
      failwith
        (Format.sprintf
           "Cannot convert expression %s with non-identifiers to reference."
           (Name.show name))


let is_simple_name name = Option.is_some (name_to_identifiers name)

let rec get_identifier_base expression =
  match Node.value expression with
  | Call { callee; _ } -> get_identifier_base callee
  | Name (Name.Attribute { base; _ }) -> get_identifier_base base
  | Name (Name.Identifier identifier) -> Some identifier
  | _ -> None


let has_identifier_base expression = get_identifier_base expression |> Option.is_some

let name_is ~name expression =
  let identifiers =
    if String.equal name "" then
      []
    else if String.equal name "..." then
      [name]
    else
      String.split ~on:'.' name
  in
  let rec check_match = function
    | [name], Name (Name.Identifier identifier) when Identifier.equal name identifier -> true
    | name :: remaining, Name (Name.Attribute { base; attribute; _ })
      when Identifier.equal name attribute ->
        check_match (remaining, Node.value base)
    | _ -> false
  in
  check_match (List.rev identifiers, Node.value expression)


let rec sanitized ({ Node.value; location } as expression) =
  match value with
  | Name (Name.Identifier identifier) ->
      Name (Name.Identifier (Identifier.sanitized identifier)) |> Node.create ~location
  | Name (Name.Attribute { base; attribute; special }) ->
      Name
        (Name.Attribute
           { base = sanitized base; attribute = Identifier.sanitized attribute; special })
      |> Node.create ~location
  | Call { callee; arguments } ->
      let sanitize_argument ({ Call.Argument.name; value } as argument) =
        let name =
          match name with
          | Some { Node.value; location } ->
              Some { Node.value = Identifier.sanitized value; location }
          | None -> None
        in
        { argument with Call.Argument.name; value = sanitized value }
      in
      Call { callee = sanitized callee; arguments = List.map ~f:sanitize_argument arguments }
      |> Node.create ~location
  | _ -> expression


let rec delocalize ({ Node.value; location } as expression) =
  let value =
    match value with
    | Call { callee; arguments } ->
        let delocalize_argument ({ Call.Argument.value; _ } as argument) =
          { argument with Call.Argument.value = delocalize value }
        in
        Call { callee = delocalize callee; arguments = List.map ~f:delocalize_argument arguments }
    | Name (Name.Identifier identifier) when identifier |> String.is_prefix ~prefix:"$local_$" ->
        let sanitized = Identifier.sanitized identifier in
        Name (Name.Identifier sanitized)
    | Name (Name.Identifier identifier) when identifier |> String.is_prefix ~prefix:"$local_" ->
        let sanitized = Identifier.sanitized identifier in
        if Str.string_match Reference.local_qualifier_pattern identifier 0 then
          let qualifier =
            Str.matched_group 1 identifier
            |> String.substr_replace_all ~pattern:"?" ~with_:"."
            |> create_name ~location
            |> fun name -> Name name |> Node.create ~location
          in
          Name (Name.Attribute { base = qualifier; attribute = sanitized; special = false })
        else (
          Log.debug "Unable to extract qualifier from %s" identifier;
          Name (Name.Identifier sanitized))
    | Name (Name.Identifier identifier) -> Name (Name.Identifier identifier)
    | Name (Name.Attribute ({ base; _ } as name)) ->
        Name (Name.Attribute { name with base = delocalize base })
    | List elements -> List (List.map elements ~f:delocalize)
    | Tuple elements -> Tuple (List.map elements ~f:delocalize)
    | _ -> value
  in
  { expression with Node.value }


let delocalize_qualified = function
  | { Node.location; value = Name (Name.Identifier identifier) } ->
      { Node.location; value = Name (Name.Identifier (Identifier.sanitized identifier)) }
  | { Node.location; value = Name (Name.Attribute ({ attribute; _ } as name)) } ->
      {
        Node.location;
        value = Name (Name.Attribute { name with attribute = Identifier.sanitized attribute });
      }
  | expression -> expression


(* TODO(T114580705): Better precision when deciding if an expression is self *)
let is_self_call ~callee:{ Node.value; _ } =
  match value with
  | Expression.Name
      (Name.Attribute { base = { Node.value = Name (Name.Identifier identifier); _ }; _ }) ->
      String.equal (Identifier.sanitized identifier) "self"
  | _ -> false


let exists_in_list ?(match_prefix = false) ~expression_list target_string =
  let flatten =
    let rec flatten flattened expression =
      match flattened, Node.value expression with
      | Some flattened, Name (Name.Identifier identifier) -> Some (identifier :: flattened)
      | Some flattened, Name (Name.Attribute { base; attribute; _ }) ->
          flatten (Some (attribute :: flattened)) base
      | Some flattened, Call { callee; _ } ->
          flatten (Some ("$call_placeholder" :: flattened)) callee
      | _ -> None
    in
    flatten (Some [])
  in
  let rec matches expected actual =
    match expected, actual with
    | expected :: expected_tail, actual :: actual_tail when String.equal expected actual ->
        if List.is_empty expected_tail && (match_prefix || List.is_empty actual_tail) then
          true
        else
          matches expected_tail actual_tail
    | _ -> false
  in
  List.map ~f:delocalize_qualified expression_list
  |> List.filter_map ~f:flatten
  |> List.exists ~f:(matches (String.split ~on:'.' target_string))


let arguments_location
    { Call.callee = { Node.location = { Location.stop = callee_end; _ }; _ }; arguments }
  =
  match List.rev arguments with
  | [] ->
      {
        Location.start = callee_end;
        stop = { callee_end with column = callee_end.Location.column + 2 };
      }
  | { Call.Argument.value = { Node.location = { Location.stop; _ }; _ }; _ } :: _ ->
      { Location.start = callee_end; stop = { stop with column = stop.Location.column + 1 } }


let get_item_call base arguments ~location =
  let create_name name = Name (create_name ~location name) in
  let arguments =
    if List.length arguments > 1 then
      Tuple arguments
      |> Node.create_with_default_location
      |> fun tuple -> [{ Call.Argument.name = None; value = tuple }]
    else
      let create argument = { Call.Argument.name = None; value = argument } in
      List.map ~f:create arguments
  in
  Call
    {
      callee =
        {
          Node.location;
          value =
            Name
              (Name.Attribute
                 {
                   base = { Node.location; value = create_name base };
                   attribute = "__getitem__";
                   special = true;
                 });
        };
      arguments;
    }


let is_dunder_attribute attribute_name =
  String.is_prefix ~prefix:"__" attribute_name && String.is_suffix ~suffix:"__" attribute_name


let inverse_operator = function
  (* cf. https://docs.python.org/3/reference/datamodel.html#object.__radd__ *)
  | "__add__" -> Some "__radd__"
  | "__radd__" -> Some "__add__"
  | "__sub__" -> Some "__rsub__"
  | "__rsub__" -> Some "__sub__"
  | "__mul__" -> Some "__rmul__"
  | "__rmul__" -> Some "__mul__"
  | "__matmul__" -> Some "__rmatmul__"
  | "__rmatmul__" -> Some "__matmul__"
  | "__truediv__" -> Some "__rtruediv__"
  | "__rtruediv__" -> Some "__truediv__"
  | "__floordiv__" -> Some "__rfloordiv__"
  | "__rfloordiv__" -> Some "__floordiv__"
  | "__mod__" -> Some "__rmod__"
  | "__rmod__" -> Some "__mod__"
  | "__divmod__" -> Some "__rdivmod__"
  | "__rdivmod__" -> Some "__divmod__"
  | "__pow__" -> Some "__rpow__"
  | "__rpow__" -> Some "__pow__"
  | "__lshift__" -> Some "__rlshift__"
  | "__rlshift__" -> Some "__lshift__"
  | "__rshift__" -> Some "__rrshift__"
  | "__rrshift__" -> Some "__rshift__"
  | "__and__" -> Some "__rand__"
  | "__rand__" -> Some "__and__"
  | "__xor__" -> Some "__rxor__"
  | "__rxor__" -> Some "__xor__"
  | "__or__" -> Some "__ror__"
  | "__ror__" -> Some "__or__"
  (* cf. https://docs.python.org/3/reference/datamodel.html#object.__lt__ *)
  | "__lt__" -> Some "__gt__"
  | "__gt__" -> Some "__lt__"
  | "__le__" -> Some "__ge__"
  | "__ge__" -> Some "__le__"
  | _ -> None


let is_operator = function
  | "__eq__"
  | "__ne__" ->
      true
  | name -> Option.is_some (inverse_operator name)


let operator_name_to_symbol = function
  | "__lt__" -> Some "<"
  | "__gt__" -> Some ">"
  | "__le__" -> Some "<="
  | "__ge__" -> Some ">="
  | "__eq__" -> Some "=="
  | "__ne__" -> Some "!="
  | "__add__" -> Some "+"
  | "__sub__" -> Some "-"
  | "__mul__" -> Some "*"
  | "__matmul__" -> Some "@"
  | "__truediv__" -> Some "/"
  | "__floordiv__" -> Some "//"
  | "__mod__" -> Some "%"
  | "__divmod__" -> Some "divmod"
  | "__pow__" -> Some "**"
  | "__lshift__" -> Some "<<"
  | "__rshift__" -> Some ">>"
  | "__and__" -> Some "&"
  | "__xor__" -> Some "^"
  | "__or__" -> Some "|"
  | _ -> None
