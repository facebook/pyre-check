(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Expression: an expression represented in Pyre's OCaml implementation of the Python AST
 * See `expr` in https://docs.python.org/3/library/ast.html
 * - Expressions are very closely related to Statements. See Statement.Statement.t.
 * - An expression is what you would typically think of as the parts of a line
 *   in Python that can be broken up and repeated, and don't contain most keywords
 *   (except for `None`, `False`, `True`, `and`, `or`, `in`, `is`, `not`, and `lambda`).
 *   This is unlike Python statements, which can't be repeated more than once
 *   (think of trying to do an `if` statement and `for` loop on the same line without `;`).
 * - Examples of AST representations:
 *   - Note: you can get JSON representations of runtime values as a string with
 *     `(Yojson.Safe.to_string ([%yojson_of: Expression.t] <your_expression>))`.
 *     - The same can be done with sexp with
 *       `(Sexp.to_string ([%sexp_of: Expression.t] <your_expression>))`.
 *   - Note: existing examples of representations can be seen in expressionTest.ml.
 *)

open Core
open Sexplib.Std
open Pyre

module StringLiteral = struct
  type kind =
    | String
    | Bytes
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  type t = {
    value: string;
    kind: kind;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

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
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

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
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  type t = {
    left: Expression.t;
    operator: operator;
    right: Expression.t;
    (* If this AST node was created from lowering down another AST node (for instance, `a + b` is
       turned into `a.__add__(b)`), `origin` stores the location of the original AST node and the
       reason for lowering it. *)
    origin: Origin.t option;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  val pp_boolean_operator : Format.formatter -> operator -> unit

  val inverse : operator -> operator

  val location_insensitive_compare : t -> t -> int
end = struct
  type operator =
    | And
    | Or
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  type t = {
    left: Expression.t;
    operator: operator;
    right: Expression.t;
    origin: Origin.t option;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

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
      name: Identifier.t Node.t option;
      value: Expression.t;
    }
    [@@deriving equal, compare, sexp, show, hash, to_yojson]

    type kind =
      | SingleStar
      | DoubleStar
      | Named of {
          name: string Node.t;
          requires_default: bool;
        }
      | Positional
    [@@deriving equal, compare, show]

    val location_insensitive_compare : t -> t -> int

    val unpack : t -> Expression.t * kind
  end

  type t = {
    callee: Expression.t;
    arguments: Argument.t list;
    (* If this AST node was created from lowering down another AST node (for instance, `a + b` is
       turned into `a.__add__(b)`), `origin` stores the location of the original AST node and the
       reason for lowering it. *)
    origin: Origin.t option;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int
end = struct
  module Argument = struct
    type t = {
      name: Identifier.t Node.t option;
      value: Expression.t;
    }
    [@@deriving equal, compare, sexp, show, hash, to_yojson]

    type kind =
      | SingleStar
      | DoubleStar
      | Named of {
          name: string Node.t;
          requires_default: bool;
        }
      | Positional
    [@@deriving equal, compare, show]

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
      | { value = { Node.value = Expression.Starred (Starred.Once expression); _ }; _ } ->
          expression, SingleStar
      | { value = { Node.value = Expression.Starred (Starred.Twice expression); _ }; _ } ->
          expression, DoubleStar
      | { value; name = Some name } -> value, Named { name; requires_default = false }
      | { value; name = None } -> value, Positional
  end

  type t = {
    callee: Expression.t;
    arguments: Argument.t list;
    origin: Origin.t option;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  let location_insensitive_compare left right =
    (* We ignore locations, but also the origin. *)
    match Expression.location_insensitive_compare left.callee right.callee with
    | x when not (Int.equal x 0) -> x
    | _ -> List.compare Argument.location_insensitive_compare left.arguments right.arguments
end

and Await : sig
  type t = {
    operand: Expression.t;
    (* If this AST node was created from lowering down another AST node (for instance, `a + b` is
       turned into `a.__add__(b)`), `origin` stores the location of the original AST node and the
       reason for lowering it. *)
    origin: Origin.t option;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int
end = struct
  type t = {
    operand: Expression.t;
    origin: Origin.t option;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  let location_insensitive_compare left right =
    Expression.location_insensitive_compare left.operand right.operand
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
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  type t = {
    left: Expression.t;
    operator: operator;
    right: Expression.t;
    (* If this AST node was created from lowering down another AST node (for instance, `a + b` is
       turned into `a.__add__(b)`), `origin` stores the location of the original AST node and the
       reason for lowering it. *)
    origin: Origin.t option;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  val inverse : operator -> operator

  val pp_comparison_operator : Format.formatter -> operator -> unit

  val lower_to_expression
    :  location:Location.t ->
    callee_location:Location.t ->
    t ->
    Expression.t option

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
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  type t = {
    left: Expression.t;
    operator: operator;
    right: Expression.t;
    origin: Origin.t option;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

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


  let lower_to_expression ~location ~callee_location { left; operator; right; origin = base_origin }
    =
    let left, right =
      match operator with
      | In -> right, left
      | _ -> left, right
    in
    let dunder_method =
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
    dunder_method
    >>| fun dunder_method ->
    let arguments = [{ Call.Argument.name = None; value = right }] in
    let origin = Some (Origin.create ?base:base_origin ~location Origin.ComparisonOperator) in
    Expression.Call
      {
        Call.callee =
          {
            Node.location = callee_location;
            value =
              Expression.Name
                (Name.Attribute { Name.Attribute.base = left; attribute = dunder_method; origin });
          };
        arguments;
        origin;
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

and BinaryOperator : sig
  type operator =
    | Add
    | Sub
    | Mult
    | MatMult
    | Div
    | Mod
    | Pow
    | LShift
    | RShift
    | BitOr
    | BitXor
    | BitAnd
    | FloorDiv
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  type t = {
    left: Expression.t;
    operator: operator;
    right: Expression.t;
    (* If this AST node was created from lowering down another AST node (for instance, `a + b` is
       turned into `a.__add__(b)`), `origin` stores the location of the original AST node and the
       reason for lowering it. *)
    origin: Origin.t option;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  val pp_binary_operator : Format.formatter -> operator -> unit

  val location_insensitive_compare : t -> t -> int

  val lower_to_call : location:Location.t -> callee_location:Location.t -> t -> Call.t

  val lower_to_expression : location:Location.t -> callee_location:Location.t -> t -> Expression.t

  val binary_operator_method : operator -> string
end = struct
  type operator =
    | Add
    | Sub
    | Mult
    | MatMult
    | Div
    | Mod
    | Pow
    | LShift
    | RShift
    | BitOr
    | BitXor
    | BitAnd
    | FloorDiv
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  type t = {
    left: Expression.t;
    operator: operator;
    right: Expression.t;
    origin: Origin.t option;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  let pp_binary_operator formatter operator =
    Format.fprintf
      formatter
      "%s"
      (match operator with
      | Add -> "+"
      | Sub -> "-"
      | Mult -> "*"
      | MatMult -> "@"
      | Div -> "/"
      | Mod -> "%"
      | Pow -> "**"
      | LShift -> "<<"
      | RShift -> ">>"
      | BitOr -> "|"
      | BitXor -> "^"
      | BitAnd -> "&"
      | FloorDiv -> "//")


  let location_insensitive_compare left right =
    match Expression.location_insensitive_compare left.left right.left with
    | x when not (Int.equal x 0) -> x
    | _ -> (
        match [%compare: operator] left.operator right.operator with
        | x when not (Int.equal x 0) -> x
        | _ -> Expression.location_insensitive_compare left.right right.right)


  let binary_operator_method = function
    | Add -> "__add__"
    | Sub -> "__sub__"
    | Mult -> "__mul__"
    | MatMult -> "__matmul__"
    | Div -> "__truediv__"
    | Mod -> "__mod__"
    | Pow -> "__pow__"
    | LShift -> "__lshift__"
    | RShift -> "__rshift__"
    | BitOr -> "__or__"
    | BitXor -> "__xor__"
    | BitAnd -> "__and__"
    | FloorDiv -> "__floordiv__"


  let lower_to_call ~location ~callee_location { left; operator; right; origin } =
    let arguments = [{ Call.Argument.name = None; value = right }] in
    let origin = Some (Origin.create ?base:origin ~location Origin.BinaryOperator) in
    {
      Call.callee =
        {
          Node.location = callee_location;
          value =
            Expression.Name
              (Name.Attribute
                 { Name.Attribute.base = left; attribute = binary_operator_method operator; origin });
        };
      arguments;
      origin;
    }


  let lower_to_expression ~location ~callee_location operator =
    Node.create ~location (Expression.Call (lower_to_call ~location ~callee_location operator))
end

and Comprehension : sig
  module Generator : sig
    type t = {
      target: Expression.t;
      iterator: Expression.t;
      conditions: Expression.t list;
      async: bool;
    }
    [@@deriving equal, compare, sexp, show, hash, to_yojson]

    val location_insensitive_compare : t -> t -> int
  end

  type 'element t = {
    element: 'element;
    generators: Generator.t list;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

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
    [@@deriving equal, compare, sexp, show, hash, to_yojson]

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
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  let location_insensitive_compare compare_element left right =
    match compare_element left.element right.element with
    | x when not (Int.equal x 0) -> x
    | _ -> List.compare Generator.location_insensitive_compare left.generators right.generators
end

and Dictionary : sig
  module Entry : sig
    module KeyValue : sig
      type t = {
        key: Expression.t;
        value: Expression.t;
      }
      [@@deriving equal, compare, sexp, show, hash, to_yojson]

      val location_insensitive_compare : t -> t -> int
    end
    [@@deriving equal, compare, sexp, show, hash, to_yojson]

    type t =
      | KeyValue of KeyValue.t
      | Splat of Expression.t
    [@@deriving equal, compare, sexp, show, hash, to_yojson]

    val location_insensitive_compare : t -> t -> int
  end

  type t = Entry.t list [@@deriving equal, compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int

  val string_literal_keys : t -> (string * Expression.t) list option

  val has_no_keywords : t -> bool
end = struct
  module Entry = struct
    module KeyValue = struct
      type t = {
        key: Expression.t;
        value: Expression.t;
      }
      [@@deriving equal, compare, sexp, show, hash, to_yojson]

      let location_insensitive_compare left right =
        match Expression.location_insensitive_compare left.key right.key with
        | x when not (Int.equal x 0) -> x
        | _ -> Expression.location_insensitive_compare left.value right.value
    end

    type t =
      | KeyValue of KeyValue.t
      | Splat of Expression.t
    [@@deriving equal, compare, sexp, show, hash, to_yojson]

    let location_insensitive_compare left right =
      match left, right with
      | KeyValue kv1, KeyValue kv2 -> KeyValue.location_insensitive_compare kv1 kv2
      | Splat s1, Splat s2 -> Expression.location_insensitive_compare s1 s2
      | Splat _, _ -> 1
      | KeyValue _, _ -> -1
  end

  type t = Entry.t list [@@deriving equal, compare, sexp, show, hash, to_yojson]

  let location_insensitive_compare left right =
    List.compare Entry.location_insensitive_compare left right


  let rec has_no_keywords entries =
    match entries with
    | [] -> true
    | Entry.Splat _ :: _ -> false
    | Entry.KeyValue _ :: tail -> has_no_keywords tail


  let string_literal_keys entries =
    let rec match_literal_key entries accumulated_result =
      match entries with
      | Entry.KeyValue
          Dictionary.Entry.KeyValue.
            {
              key =
                {
                  Node.value =
                    Expression.Constant (Constant.String { StringLiteral.value = key; _ });
                  _;
                };
              value;
            }
        :: tail ->
          match_literal_key tail ((key, value) :: accumulated_result)
      | _ :: tail -> match_literal_key tail accumulated_result
      | [] -> Some (List.rev accumulated_result)
    in
    match_literal_key entries []
end

and Lambda : sig
  type t = {
    parameters: Parameter.t list;
    body: Expression.t;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int
end = struct
  type t = {
    parameters: Parameter.t list;
    body: Expression.t;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

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
      (* If this AST node was created from lowering down another AST node (for instance, `a + b` is
         turned into `a.__add__(b)`), `origin` stores the location of the original AST node and the
         reason for lowering it.

         We could store the full original expression but this could lead to a cyclic dependency
         between Statement and Expression. *)
      origin: Origin.t option;
    }
    [@@deriving equal, compare, sexp, show, hash, to_yojson]

    val location_insensitive_compare : t -> t -> int
  end

  type t =
    | Attribute of Attribute.t
    | Identifier of Identifier.t
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int

  val last : t -> string
end = struct
  module Attribute = struct
    type t = {
      base: Expression.t;
      attribute: Identifier.t;
      origin: Origin.t option;
    }
    [@@deriving equal, compare, sexp, show, hash, to_yojson]

    let location_insensitive_compare left right =
      (* We ignore locations, but also the origin. *)
      match Expression.location_insensitive_compare left.base right.base with
      | x when not (Int.equal x 0) -> x
      | _ -> [%compare: Identifier.t] left.attribute right.attribute
  end

  type t =
    | Attribute of Attribute.t
    | Identifier of Identifier.t
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  let location_insensitive_compare left right =
    match left, right with
    | Attribute left, Attribute right -> Attribute.location_insensitive_compare left right
    | Identifier left, Identifier right -> [%compare: Identifier.t] left right
    | Attribute _, Identifier _ -> -1
    | Identifier _, Attribute _ -> 1


  let last = function
    | Identifier name -> name
    | Attribute { Attribute.attribute; _ } -> attribute
end

and Parameter : sig
  type parameter = {
    name: Identifier.t;
    value: Expression.t option;
    annotation: Expression.t option;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  type t = parameter Node.t [@@deriving equal, compare, sexp, show, hash, to_yojson]

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
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  type t = parameter Node.t [@@deriving equal, compare, sexp, show, hash, to_yojson]

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
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int
end = struct
  type t =
    | Once of Expression.t
    | Twice of Expression.t
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  let location_insensitive_compare left right =
    match left, right with
    | Once left, Once right
    | Twice left, Twice right ->
        Expression.location_insensitive_compare left right
    | Once _, Twice _ -> -1
    | Twice _, Once _ -> 1
end

and Slice : sig
  type t = {
    start: Expression.t option;
    stop: Expression.t option;
    step: Expression.t option;
    (* If this AST node was created from lowering down another AST node (for instance, `a + b` is
       turned into `a.__add__(b)`), `origin` stores the location of the original AST node and the
       reason for lowering it. *)
    origin: Origin.t option;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int

  val lower_to_call : location:Location.t -> t -> Call.t

  val lower_to_expression : location:Location.t -> t -> Expression.t
end = struct
  type t = {
    start: Expression.t option;
    stop: Expression.t option;
    step: Expression.t option;
    origin: Origin.t option;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  let location_insensitive_compare
      { start = left_start; stop = left_stop; step = left_step; origin = _ }
      { start = right_start; stop = right_stop; step = right_step; origin = _ }
    =
    match Option.compare Expression.location_insensitive_compare left_start right_start with
    | x when not (Int.equal x 0) -> x
    | _ -> (
        match Option.compare Expression.location_insensitive_compare left_stop right_stop with
        | x when not (Int.equal x 0) -> x
        | _ -> Option.compare Expression.location_insensitive_compare left_step right_step)


  let lower_to_call ~location { Slice.start; stop; step; origin } =
    let default_none = function
      | None -> Expression.Constant Constant.NoneLiteral |> Node.create ~location
      | Some expr -> expr
    in
    {
      Call.callee = Expression.Name (Name.Identifier "slice") |> Node.create ~location;
      arguments =
        [
          { Call.Argument.value = default_none start; name = None };
          { Call.Argument.value = default_none stop; name = None };
          { Call.Argument.value = default_none step; name = None };
        ];
      origin = Some (Origin.create ?base:origin ~location Origin.Slice);
    }


  let lower_to_expression ~location slice =
    Expression.Call (lower_to_call ~location slice) |> Node.create ~location
end

and Subscript : sig
  type t = {
    base: Expression.t;
    index: Expression.t;
    (* If this AST node was created from lowering down another AST node (for instance, `a + b` is
       turned into `a.__add__(b)`), `origin` stores the location of the original AST node and the
       reason for lowering it. *)
    origin: Origin.t option;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int
end = struct
  type t = {
    base: Expression.t;
    index: Expression.t;
    origin: Origin.t option;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  let location_insensitive_compare
      { base = left_base; index = left_index; origin = _ }
      { base = right_base; index = right_index; origin = _ }
    =
    match Expression.location_insensitive_compare left_base right_base with
    | 0 -> Expression.location_insensitive_compare left_index right_index
    | nonzero -> nonzero
end

and Substring : sig
  type t =
    | Literal of string Node.t
    | Format of {
        value: Expression.t;
        format_spec: Expression.t option;
      }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int
end = struct
  type t =
    | Literal of string Node.t
    | Format of {
        value: Expression.t;
        format_spec: Expression.t option;
      }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  let location_insensitive_compare left right =
    match left, right with
    | Literal left, Literal right -> Node.location_insensitive_compare String.compare left right
    | Format left, Format right -> begin
        match Expression.location_insensitive_compare left.value right.value with
        | x when not (Int.equal x 0) -> x
        | _ ->
            Option.compare
              Expression.location_insensitive_compare
              left.format_spec
              right.format_spec
      end
    | Literal _, _ -> -1
    | Format _, _ -> 1
end

and Ternary : sig
  type t = {
    target: Expression.t;
    test: Expression.t;
    alternative: Expression.t;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int
end = struct
  type t = {
    target: Expression.t;
    test: Expression.t;
    alternative: Expression.t;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

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
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  type t = {
    operator: operator;
    operand: Expression.t;
    (* If this AST node was created from lowering down another AST node (for instance, `a + b` is
       turned into `a.__add__(b)`), `origin` stores the location of the original AST node and the
       reason for lowering it. *)
    origin: Origin.t option;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  val pp_unary_operator : Format.formatter -> operator -> unit

  val lower_to_expression
    :  location:Location.t ->
    callee_location:Location.t ->
    t ->
    Expression.t option

  val location_insensitive_compare : t -> t -> int
end = struct
  type operator =
    | Invert
    | Negative
    | Not
    | Positive
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  type t = {
    operator: operator;
    operand: Expression.t;
    origin: Origin.t option;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  let pp_unary_operator formatter operator =
    Format.fprintf
      formatter
      "%s"
      (match operator with
      | Invert -> "~"
      | Negative -> "-"
      | Not -> "not"
      | Positive -> "+")


  let lower_to_expression ~location ~callee_location { operator; operand; origin = base_origin } =
    (match operator with
    | Invert -> Some "__invert__"
    | Negative -> Some "__neg__"
    | Not -> None
    | Positive -> Some "__pos__")
    >>| fun name ->
    let origin = Some (Origin.create ?base:base_origin ~location Origin.UnaryOperator) in
    Expression.Call
      {
        Call.callee =
          {
            Node.location = callee_location;
            value =
              Expression.Name
                (Name.Attribute { Name.Attribute.base = operand; attribute = name; origin });
          };
        arguments = [];
        origin;
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
    (* If this AST node was created from lowering down another AST node (for instance, `a + b` is
       turned into `a.__add__(b)`), `origin` stores the location of the original AST node and the
       reason for lowering it. *)
    origin: Origin.t option;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int
end = struct
  type t = {
    target: Expression.t;
    value: Expression.t;
    origin: Origin.t option;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  let location_insensitive_compare left right =
    match Expression.location_insensitive_compare left.target right.target with
    | x when not (Int.equal x 0) -> x
    | _ -> Expression.location_insensitive_compare left.value right.value
end

and TypeParam : sig
  type type_var = {
    name: Identifier.t;
    bound: Expression.t option;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  type type_param =
    | TypeVar of type_var
    | ParamSpec of Identifier.t
    | TypeVarTuple of Identifier.t
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  type t = type_param Node.t [@@deriving equal, compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int
end = struct
  type type_var = {
    name: Identifier.t;
    bound: Expression.t option;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  type type_param =
    | TypeVar of type_var
    | ParamSpec of Identifier.t
    | TypeVarTuple of Identifier.t
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  type t = type_param Node.t [@@deriving equal, compare, sexp, show, hash, to_yojson]

  let location_insensitive_compare left right =
    match Node.value left, Node.value right with
    | ( TypeVar { name = left_name; bound = left_bound },
        TypeVar { name = right_name; bound = right_bound } ) -> begin
        match [%compare: Identifier.t] left_name right_name with
        | x when not (Int.equal x 0) -> x
        | _ -> Option.compare Expression.location_insensitive_compare left_bound right_bound
      end
    | ParamSpec left_name, ParamSpec right_name -> [%compare: Identifier.t] left_name right_name
    | TypeVarTuple left_name, TypeVarTuple right_name ->
        [%compare: Identifier.t] left_name right_name
    | _ -> -1
end

and Origin : sig
  (* During the analysis, we create artificial nodes that were not present
   * in the original code. This type is used to describe the original node
   * that originated the artificial node. *)
  type kind =
    | ComparisonOperator (* `a == b` is turned into `a.__eq__(b)` *)
    | BinaryOperator (* `a + b` is turned into `a.__add__(b)` *)
    | UnaryOperator (* `-a` is turned into `a.__neg__()` *)
    | AugmentedAssignDunderCall (* `a += b` is turned into `a = a.__add__(b)` *)
    | AugmentedAssignLHS (* left hand side `a` in `a = a.__add__(b)` *)
    | AugmentedAssignRHS (* right hand side `a` in `a = a.__add__(b)` *)
    | AugmentedAssignStatement
    | ChainedAssign of { index: int } (* `x = y = z` is turned into `x = z; y = z` *)
    | Qualification of string list (* all symbols are turned into their fully qualified version *)
    | SubscriptSetItem (* `d[a] = b` is turned into `d.__setitem__(a, b)` *)
    | SubscriptGetItem (* `d[a]` is turned into `d.__getitem__(a)` *)
    | ForIter (* `for e in l:` is turned into `l.__iter__().__next__()` *)
    | ForNext (* `for e in l:` is turned into `l.__iter__().__next__()` *)
    | ForAwait (* `for e in l:` might be turned into `await l.__iter__().__next__()` *)
    | ForAssign (* `for e in l:` is turned into `e = l.__iter__().__next__()` *)
    | GeneratorIter (* `(e for e in l)` is turned into `l.__iter__().__next__()` *)
    | GeneratorNext (* `(e for e in l)` is turned into `l.__iter__().__next__()` *)
    | GeneratorAwait (* `(e for e in l)` might be turned into `await l.__aiter__().__anext__()` *)
    | GeneratorAssign (* `(e for e in l)` is turned into `e = l.__iter__().__next__()` *)
    | WithEnter (* `with e1 as e2` is turned into `e2 = e1.__enter__()` *)
    | WithAssign (* `with e1 as e2` is turned into `e2 = e1.__enter__()` *)
    | InContains (* `e in l` can be turned into `l.__contains__(e)` *)
    | InIter (* `e in l` can be turned into `l.__iter__().__next__().__eq__(e)` *)
    | InGetItem (* `e in l` can be turned into `l.__getitem__(0).__eq__(e)` *)
    | InGetItemEq (* `e in l` can be turned into `l.__getitem__(0).__eq__(e)` *)
    | Slice (* `1:2` is turned into `slice(1,2,None)` *)
    | TopLevelTupleAssign (* `(x, y) = (a, b)` might be turned into `x = a; y = b` *)
    | MissingStubCallable
    | TypedDictImplicitClass
    | NamedTupleImplicitFields
    | PyTorchRegisterBuffer
    | UnionShorthand (* `a | b` is turned into `typing.Union[a, b]` when in typing context *)
    | Negate (* `if cond:` is turned into `assert(cond)` and `assert(not cond)` *)
    | NegateIs (* `not(a is not b)` is turned into `a is b` *)
    | NegateIsNot (* `not(a is b)` is turned into `a is not b` *)
    | Normalize (* we turn boolean expressions into negation normal form *)
    | NormalizeNotComparison (* `not(a < b)` is turned into `a >= b` *)
    | NormalizeNotBoolOperator (* `not(a == b)` is turned into `a != b` *)
    | TryHandlerIsInstance (* `try..except X as e` is turned into `assert(isinstance(X, e))` *)
    | TryHandlerAssign (* `try: .. except X as e` is turned into `e = ...` *)
    | NamedTupleConstructorAssignment of string
      (* `collections.namedtuple('T', 'a b')` is turned into `def __init__(self, a, b): self.a = a;
         self.b = b` *)
    | DataclassImplicitField
      (* `@dataclass` on `class A: x: int` is turned into `x: int = dataclasses.field()` *)
    | DataclassImplicitDefault
    (* `x: int = dataclasses.field(default_factory=f)` is turned into `x = f()` in the implicit
       constructor *)
    (* All the origins below are used to translate `match` statements *)
    | MatchTypingSequence
      (* `match x: case [..]:` is turned into `isinstance(x, typing.Sequence)` *)
    | MatchTypingMapping (* `match x: case {...}:` is turned into `isinstance(x, typing.Mapping)` *)
    | MatchAsComparisonEquals
    | MatchAsWithCondition
    | MatchClassArgs of int
    | MatchClassGetAttr of int
    | MatchClassArgsSubscript of int
    | MatchClassKeywordAttribute of string
    | MatchClassIsInstance
    | MatchClassJoinConditions
    | MatchMappingKeySubscript
    | MatchMappingRestDict of string
    | MatchMappingRestComparisonEquals of string
    | MatchMappingIsInstance
    | MatchMappingJoinConditions
    | MatchOrJoinConditions
    | MatchSingletonComparisonIs
    | MatchSequenceRestList of string
    | MatchSequenceRestSubscript of string
    | MatchSequenceRestSlice of string
    | MatchSequenceRestComparisonEquals of string
    | MatchSequencePrefix of int
    | MatchSequenceSuffix of int
    | MatchSequenceIsInstance
    | MatchSequenceJoinConditions
    | MatchValueComparisonEquals
    | MatchConditionWithGuard
    | ResolveStrCall
    | StrCallToDunderMethod (* `str(x)` is turned into `x.__str__()` or `x.__repr__()` *)
    | ReprCall (* `repr(x)` is turned into `x.__repr__()` *)
    | AbsCall (* `abs(x)` is turned into `x.__abs__()` *)
    | IterCall (* `iter(x)` is turned into `x.__iter__()` *)
    | NextCall (* `next(x)` is turned into `x.__next__()` *)
    | ImplicitInitCall (* `A(x)` is turned into `A.__init__(..., x)` *)
    | SelfImplicitTypeVar of string
    | SelfImplicitTypeVarAssign
      (* `def f(self):` is turned into `def f(self: TSelf):` with `TSelf = TypeVar["self",
         bound=MyClass])` *)
    | SelfImplicitTypeVarQualification of string * string list
      (* `def f(self):` is turned into `def f(self: TSelf):` with `TSelf = TypeVar["self",
         bound=MyClass])` *)
    | FunctionalEnumImplicitAuto of string list
    (* `Enum("Color", ("RED", "GREEN", "BLUE"))` is turned into `class Color: RED = enum.auto();
       ...` *)
    | FunctionalEnumImplicitAutoAssign
    | DecoratorInlining (* Pysa inlines decorator during preprocessing *)
    | ForDecoratedTarget (* `@foo def f(): ...` is turned into `def f@decorated(): return foo(f)` *)
    | ForDecoratedTargetCallee of
        string list (* `@foo def f(): ...` is turned into `def f@decorated(): return foo(f)` *)
    | FormatStringImplicitStr (* f"{x}" is turned into f"{x.__str__()}" *)
    | FormatStringImplicitRepr (* f"{x}" is turned into f"{x.__repr__()}" *)
    | GetAttrConstantLiteral (* getattr(x, "foo") is turned into x.foo *)
    | SetAttrConstantLiteral (* object.__setattr__(x, "foo", value) is turned into x.foo = value *)
    | PysaCallRedirect of string (* hardcoded AST rewrite made for Pysa analysis *)
    | PysaHigherOrderParameter of
        int (* `f(g)` might be turned into `f(g())` for the taint analysis *)
    | ForTestPurpose (* AST node created when running tests *)
    | ForTypeChecking (* AST node created internally during a type check of an expression *)
    | Nested of {
        head: kind;
        tail: kind;
      }
    (* In some rare cases, AST lowering happens in multiple steps.
     * For instance: `match x: case 0: pass` turns into `if x == 0:` which then turns into `if x.__equals__(0):`.
     * We keep a trace of all AST transforms. *)
    | PysaReturnShim (* Additional ASTs on the return statements. *)
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  type t = {
    kind: kind;
    location: Location.t;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  val create : ?base:t -> location:Location.t -> kind -> t

  val is_dunder_method : t -> bool

  val pp_kind_json : Format.formatter -> kind -> unit

  val kind_from_json : string -> (kind, string) Result.t

  val map_location : f:(Location.t -> Location.t) -> t -> t
end = struct
  type kind =
    | ComparisonOperator
    | BinaryOperator
    | UnaryOperator
    | AugmentedAssignDunderCall
    | AugmentedAssignLHS
    | AugmentedAssignRHS
    | AugmentedAssignStatement
    | ChainedAssign of { index: int }
    | Qualification of string list
    | SubscriptSetItem
    | SubscriptGetItem
    | ForIter
    | ForNext
    | ForAwait
    | ForAssign
    | GeneratorIter
    | GeneratorNext
    | GeneratorAwait
    | GeneratorAssign
    | WithEnter
    | WithAssign
    | InContains
    | InIter
    | InGetItem
    | InGetItemEq
    | Slice
    | TopLevelTupleAssign
    | MissingStubCallable
    | TypedDictImplicitClass
    | NamedTupleImplicitFields
    | PyTorchRegisterBuffer
    | UnionShorthand
    | Negate
    | NegateIs
    | NegateIsNot
    | Normalize
    | NormalizeNotComparison
    | NormalizeNotBoolOperator
    | TryHandlerIsInstance
    | TryHandlerAssign
    | NamedTupleConstructorAssignment of string
    | DataclassImplicitField
    | DataclassImplicitDefault
    | MatchTypingSequence
    | MatchTypingMapping
    | MatchAsComparisonEquals
    | MatchAsWithCondition
    | MatchClassArgs of int
    | MatchClassGetAttr of int
    | MatchClassArgsSubscript of int
    | MatchClassKeywordAttribute of string
    | MatchClassIsInstance
    | MatchClassJoinConditions
    | MatchMappingKeySubscript
    | MatchMappingRestDict of string
    | MatchMappingRestComparisonEquals of string
    | MatchMappingIsInstance
    | MatchMappingJoinConditions
    | MatchOrJoinConditions
    | MatchSingletonComparisonIs
    | MatchSequenceRestList of string
    | MatchSequenceRestSubscript of string
    | MatchSequenceRestSlice of string
    | MatchSequenceRestComparisonEquals of string
    | MatchSequencePrefix of int
    | MatchSequenceSuffix of int
    | MatchSequenceIsInstance
    | MatchSequenceJoinConditions
    | MatchValueComparisonEquals
    | MatchConditionWithGuard
    | ResolveStrCall
    | StrCallToDunderMethod
    | ReprCall
    | AbsCall
    | IterCall
    | NextCall
    | ImplicitInitCall
    | SelfImplicitTypeVar of string
    | SelfImplicitTypeVarAssign
    | SelfImplicitTypeVarQualification of string * string list
    | FunctionalEnumImplicitAuto of string list
    | FunctionalEnumImplicitAutoAssign
    | DecoratorInlining
    | ForDecoratedTarget
    | ForDecoratedTargetCallee of string list
    | FormatStringImplicitStr
    | FormatStringImplicitRepr
    | GetAttrConstantLiteral
    | SetAttrConstantLiteral
    | PysaCallRedirect of string
    | PysaHigherOrderParameter of int
    | ForTestPurpose
    | ForTypeChecking
    | Nested of {
        head: kind;
        tail: kind;
      }
    | PysaReturnShim
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  type t = {
    kind: kind;
    location: Location.t;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  let create ?base ~location kind =
    match base with
    | Some { kind = tail; location } -> { kind = Nested { head = kind; tail }; location }
    | None -> { kind; location }


  let is_dunder_method { kind; _ } =
    let rec is_dunder_method_kind = function
      | ComparisonOperator
      | BinaryOperator
      | UnaryOperator
      | AugmentedAssignDunderCall
      | SubscriptSetItem
      | SubscriptGetItem
      | ForIter
      | ForNext
      | GeneratorIter
      | GeneratorNext
      | WithEnter
      | InContains
      | InIter
      | InGetItem
      | InGetItemEq
      | StrCallToDunderMethod
      | ReprCall
      | AbsCall
      | IterCall
      | NextCall
      | ImplicitInitCall
      | FormatStringImplicitStr
      | FormatStringImplicitRepr ->
          true
      | Nested { head; _ } -> is_dunder_method_kind head
      | _ -> false
    in
    is_dunder_method_kind kind


  let rec pp_kind_json formatter = function
    | ComparisonOperator -> Format.fprintf formatter "comparison"
    | BinaryOperator -> Format.fprintf formatter "binary"
    | UnaryOperator -> Format.fprintf formatter "unary"
    | AugmentedAssignDunderCall -> Format.fprintf formatter "augmented-assign-dunder-call"
    | AugmentedAssignLHS -> Format.fprintf formatter "augmented-assign-lhs"
    | AugmentedAssignRHS -> Format.fprintf formatter "augmented-assign-rhs"
    | AugmentedAssignStatement -> Format.fprintf formatter "augmented-assign-statement"
    | ChainedAssign { index } -> Format.fprintf formatter "chained-assign:%d" index
    | Qualification identifiers ->
        Format.fprintf formatter "qualification:%s" (String.concat ~sep:"." (List.rev identifiers))
    | SubscriptSetItem -> Format.fprintf formatter "subscript-set-item"
    | SubscriptGetItem -> Format.fprintf formatter "subscript-get-item"
    | ForIter -> Format.fprintf formatter "for-iter"
    | ForNext -> Format.fprintf formatter "for-next"
    | ForAwait -> Format.fprintf formatter "for-await"
    | ForAssign -> Format.fprintf formatter "for-assign"
    | GeneratorIter -> Format.fprintf formatter "generator-iter"
    | GeneratorNext -> Format.fprintf formatter "generator-next"
    | GeneratorAwait -> Format.fprintf formatter "generator-await"
    | GeneratorAssign -> Format.fprintf formatter "generator-assign"
    | WithEnter -> Format.fprintf formatter "with-enter"
    | WithAssign -> Format.fprintf formatter "with-assign"
    | InContains -> Format.fprintf formatter "in-contains"
    | InIter -> Format.fprintf formatter "in-iter"
    | InGetItem -> Format.fprintf formatter "in-get-item"
    | InGetItemEq -> Format.fprintf formatter "in-get-item-eq"
    | Slice -> Format.fprintf formatter "slice"
    | TopLevelTupleAssign -> Format.fprintf formatter "top-level-tuple-assign"
    | MissingStubCallable -> Format.fprintf formatter "missing-stub-callable"
    | TypedDictImplicitClass -> Format.fprintf formatter "typed-dict-implicit-class"
    | NamedTupleImplicitFields -> Format.fprintf formatter "named-tuple-implicit-fields"
    | PyTorchRegisterBuffer -> Format.fprintf formatter "pytorch-register-buffer"
    | UnionShorthand -> Format.fprintf formatter "union-shorthand"
    | Negate -> Format.fprintf formatter "negate"
    | NegateIs -> Format.fprintf formatter "negate-is"
    | NegateIsNot -> Format.fprintf formatter "negate-is-not"
    | Normalize -> Format.fprintf formatter "normalize"
    | NormalizeNotComparison -> Format.fprintf formatter "normalize-not-comparison"
    | NormalizeNotBoolOperator -> Format.fprintf formatter "normalize-not-bool-operator"
    | TryHandlerIsInstance -> Format.fprintf formatter "try-handler-isinstance"
    | TryHandlerAssign -> Format.fprintf formatter "try-handler-assign"
    | NamedTupleConstructorAssignment attribute ->
        Format.fprintf formatter "named-tuple-constructor-assignment:%s" attribute
    | DataclassImplicitField -> Format.fprintf formatter "dataclass-implicit-field"
    | DataclassImplicitDefault -> Format.fprintf formatter "dataclass-implicit-default"
    | MatchTypingSequence -> Format.fprintf formatter "match-typing-sequence"
    | MatchTypingMapping -> Format.fprintf formatter "match-typing-mapping"
    | MatchAsComparisonEquals -> Format.fprintf formatter "match-as-comparison-equals"
    | MatchAsWithCondition -> Format.fprintf formatter "match-as-with-condition"
    | MatchClassArgs index -> Format.fprintf formatter "match-class-args:%d" index
    | MatchClassGetAttr index -> Format.fprintf formatter "match-class-get-attr:%d" index
    | MatchClassArgsSubscript index ->
        Format.fprintf formatter "match-class-args-subscript:%d" index
    | MatchClassKeywordAttribute attribute ->
        Format.fprintf formatter "match-class-keyword-attribute:%s" attribute
    | MatchClassIsInstance -> Format.fprintf formatter "match-class-isinstance"
    | MatchClassJoinConditions -> Format.fprintf formatter "match-class-join-conditions"
    | MatchMappingKeySubscript -> Format.fprintf formatter "match-mapping-key-subscript"
    | MatchMappingRestDict attribute ->
        Format.fprintf formatter "match-mapping-rest-dict:%s" attribute
    | MatchMappingRestComparisonEquals attribute ->
        Format.fprintf formatter "match-mapping-rest-comparison-equals:%s" attribute
    | MatchMappingIsInstance -> Format.fprintf formatter "match-mapping-isinstance"
    | MatchMappingJoinConditions -> Format.fprintf formatter "match-mapping-join-conditions"
    | MatchOrJoinConditions -> Format.fprintf formatter "match-or-join-conditions"
    | MatchSingletonComparisonIs -> Format.fprintf formatter "match-singleton-comparison-is"
    | MatchSequenceRestList attribute ->
        Format.fprintf formatter "match-sequence-rest-list:%s" attribute
    | MatchSequenceRestSubscript attribute ->
        Format.fprintf formatter "match-sequence-rest-subscript:%s" attribute
    | MatchSequenceRestSlice attribute ->
        Format.fprintf formatter "match-sequence-rest-slice:%s" attribute
    | MatchSequenceRestComparisonEquals attribute ->
        Format.fprintf formatter "match-sequence-rest-comparison-equals:%s" attribute
    | MatchSequencePrefix index -> Format.fprintf formatter "match-sequence-prefix:%d" index
    | MatchSequenceSuffix index -> Format.fprintf formatter "match-sequence-suffix:%d" index
    | MatchSequenceIsInstance -> Format.fprintf formatter "match-sequence-isinstance"
    | MatchSequenceJoinConditions -> Format.fprintf formatter "match-sequence-join-conditions"
    | MatchValueComparisonEquals -> Format.fprintf formatter "match-value-comparison-equals"
    | MatchConditionWithGuard -> Format.fprintf formatter "match-condition-with-guard"
    | ResolveStrCall -> Format.fprintf formatter "resolve-str-call"
    | StrCallToDunderMethod -> Format.fprintf formatter "str-call-to-dunder-method"
    | ReprCall -> Format.fprintf formatter "repr-call"
    | AbsCall -> Format.fprintf formatter "abs-call"
    | IterCall -> Format.fprintf formatter "iter-call"
    | NextCall -> Format.fprintf formatter "next-call"
    | ImplicitInitCall -> Format.fprintf formatter "implicit-init-call"
    | SelfImplicitTypeVar name -> Format.fprintf formatter "self-implicit-typevar:%s" name
    | SelfImplicitTypeVarQualification (name, attributes) ->
        Format.fprintf
          formatter
          "self-implicit-typevar-qualification:%s:%s"
          name
          (String.concat ~sep:"." (List.rev attributes))
    | SelfImplicitTypeVarAssign -> Format.fprintf formatter "self-implicit-typevar-assign"
    | FunctionalEnumImplicitAuto attributes ->
        Format.fprintf
          formatter
          "functional-enum-implicit-auto:%s"
          (String.concat ~sep:"." attributes)
    | FunctionalEnumImplicitAutoAssign ->
        Format.fprintf formatter "functional-enum-implicit-auto-assign"
    | DecoratorInlining -> Format.fprintf formatter "decorator-inlining"
    | ForDecoratedTarget -> Format.fprintf formatter "for-decorated-target"
    | ForDecoratedTargetCallee identifiers ->
        Format.fprintf
          formatter
          "for-decorated-target-callee:%s"
          (String.concat ~sep:"." (List.rev identifiers))
    | FormatStringImplicitStr -> Format.fprintf formatter "format-string-implicit-str"
    | FormatStringImplicitRepr -> Format.fprintf formatter "format-string-implicit-repr"
    | GetAttrConstantLiteral -> Format.fprintf formatter "get-attr-constant-literal"
    | SetAttrConstantLiteral -> Format.fprintf formatter "set-attr-constant-literal"
    | PysaCallRedirect name -> Format.fprintf formatter "pysa-call-redirect:%s" name
    | PysaHigherOrderParameter index ->
        Format.fprintf formatter "pysa-higher-order-parameter:%d" index
    | ForTestPurpose -> Format.fprintf formatter "for-test-purpose"
    | ForTypeChecking -> Format.fprintf formatter "for-type-checking"
    | Nested { head; tail } -> Format.fprintf formatter "%a>%a" pp_kind_json tail pp_kind_json head
    | PysaReturnShim -> Format.fprintf formatter "return-statement"


  let rec kind_from_json str =
    let open Core.Result.Monad_infix in
    let strip_prefix ~prefix str = String.drop_prefix str (String.length prefix) in
    let parse_int str =
      try Ok (Int.of_string str) with
      | _ -> Error (Format.sprintf "Expected integer, got '%s'" str)
    in
    let parse_identifier_list str = String.split ~on:'.' str |> List.rev in
    match str with
    | "comparison" -> Ok ComparisonOperator
    | "binary" -> Ok BinaryOperator
    | "unary" -> Ok UnaryOperator
    | "augmented-assign-dunder-call" -> Ok AugmentedAssignDunderCall
    | "augmented-assign-lhs" -> Ok AugmentedAssignLHS
    | "augmented-assign-rhs" -> Ok AugmentedAssignRHS
    | "augmented-assign-statement" -> Ok AugmentedAssignStatement
    | "subscript-set-item" -> Ok SubscriptSetItem
    | "subscript-get-item" -> Ok SubscriptGetItem
    | "for-iter" -> Ok ForIter
    | "for-next" -> Ok ForNext
    | "for-await" -> Ok ForAwait
    | "for-assign" -> Ok ForAssign
    | "generator-iter" -> Ok GeneratorIter
    | "generator-next" -> Ok GeneratorNext
    | "generator-await" -> Ok GeneratorAwait
    | "generator-assign" -> Ok GeneratorAssign
    | "with-enter" -> Ok WithEnter
    | "with-assign" -> Ok WithAssign
    | "in-contains" -> Ok InContains
    | "in-iter" -> Ok InIter
    | "in-get-item" -> Ok InGetItem
    | "in-get-item-eq" -> Ok InGetItemEq
    | "slice" -> Ok Slice
    | "top-level-tuple-assign" -> Ok TopLevelTupleAssign
    | "missing-stub-callable" -> Ok MissingStubCallable
    | "typed-dict-implicit-class" -> Ok TypedDictImplicitClass
    | "named-tuple-implicit-fields" -> Ok NamedTupleImplicitFields
    | "pytorch-register-buffer" -> Ok PyTorchRegisterBuffer
    | "union-shorthand" -> Ok UnionShorthand
    | "negate" -> Ok Negate
    | "negate-is" -> Ok NegateIs
    | "negate-is-not" -> Ok NegateIsNot
    | "normalize" -> Ok Normalize
    | "normalize-not-comparison" -> Ok NormalizeNotComparison
    | "normalize-not-bool-operator" -> Ok NormalizeNotBoolOperator
    | "try-handler-isinstance" -> Ok TryHandlerIsInstance
    | "try-handler-assign" -> Ok TryHandlerAssign
    | "dataclass-implicit-field" -> Ok DataclassImplicitField
    | "dataclass-implicit-default" -> Ok DataclassImplicitDefault
    | "match-typing-sequence" -> Ok MatchTypingSequence
    | "match-typing-mapping" -> Ok MatchTypingMapping
    | "match-as-comparison-equals" -> Ok MatchAsComparisonEquals
    | "match-as-with-condition" -> Ok MatchAsWithCondition
    | "match-class-isinstance" -> Ok MatchClassIsInstance
    | "match-class-join-conditions" -> Ok MatchClassJoinConditions
    | "match-mapping-key-subscript" -> Ok MatchMappingKeySubscript
    | "match-mapping-isinstance" -> Ok MatchMappingIsInstance
    | "match-mapping-join-conditions" -> Ok MatchMappingJoinConditions
    | "match-or-join-conditions" -> Ok MatchOrJoinConditions
    | "match-singleton-comparison-is" -> Ok MatchSingletonComparisonIs
    | "match-sequence-isinstance" -> Ok MatchSequenceIsInstance
    | "match-sequence-join-conditions" -> Ok MatchSequenceJoinConditions
    | "match-value-comparison-equals" -> Ok MatchValueComparisonEquals
    | "match-condition-with-guard" -> Ok MatchConditionWithGuard
    | "resolve-str-call" -> Ok ResolveStrCall
    | "str-call-to-dunder-method" -> Ok StrCallToDunderMethod
    | "repr-call" -> Ok ReprCall
    | "abs-call" -> Ok AbsCall
    | "iter-call" -> Ok IterCall
    | "next-call" -> Ok NextCall
    | "implicit-init-call" -> Ok ImplicitInitCall
    | "self-implicit-typevar-assign" -> Ok SelfImplicitTypeVarAssign
    | "functional-enum-implicit-auto-assign" -> Ok FunctionalEnumImplicitAutoAssign
    | "decorator-inlining" -> Ok DecoratorInlining
    | "for-decorated-target" -> Ok ForDecoratedTarget
    | "format-string-implicit-str" -> Ok FormatStringImplicitStr
    | "format-string-implicit-repr" -> Ok FormatStringImplicitRepr
    | "get-attr-constant-literal" -> Ok GetAttrConstantLiteral
    | "set-attr-constant-literal" -> Ok SetAttrConstantLiteral
    | "for-test-purpose" -> Ok ForTestPurpose
    | "for-type-checking" -> Ok ForTypeChecking
    | "return-statement" -> Ok PysaReturnShim
    | _ when String.is_prefix str ~prefix:"chained-assign:" ->
        strip_prefix ~prefix:"chained-assign:" str
        |> parse_int
        >>= fun index -> Ok (ChainedAssign { index })
    | _ when String.is_prefix str ~prefix:"qualification:" ->
        strip_prefix ~prefix:"qualification:" str
        |> parse_identifier_list
        |> fun identifiers -> Ok (Qualification identifiers)
    | _ when String.is_prefix str ~prefix:"named-tuple-constructor-assignment:" ->
        strip_prefix ~prefix:"named-tuple-constructor-assignment:" str
        |> fun attribute -> Ok (NamedTupleConstructorAssignment attribute)
    | _ when String.is_prefix str ~prefix:"match-class-args:" ->
        strip_prefix ~prefix:"match-class-args:" str
        |> parse_int
        >>= fun index -> Ok (MatchClassArgs index)
    | _ when String.is_prefix str ~prefix:"match-class-get-attr:" ->
        strip_prefix ~prefix:"match-class-get-attr:" str
        |> parse_int
        >>= fun index -> Ok (MatchClassGetAttr index)
    | _ when String.is_prefix str ~prefix:"match-class-args-subscript:" ->
        strip_prefix ~prefix:"match-class-args-subscript:" str
        |> parse_int
        >>= fun index -> Ok (MatchClassArgsSubscript index)
    | _ when String.is_prefix str ~prefix:"match-class-keyword-attribute:" ->
        strip_prefix ~prefix:"match-class-keyword-attribute:" str
        |> fun attribute -> Ok (MatchClassKeywordAttribute attribute)
    | _ when String.is_prefix str ~prefix:"match-mapping-rest-dict:" ->
        strip_prefix ~prefix:"match-mapping-rest-dict:" str
        |> fun attribute -> Ok (MatchMappingRestDict attribute)
    | _ when String.is_prefix str ~prefix:"match-mapping-rest-comparison-equals:" ->
        strip_prefix ~prefix:"match-mapping-rest-comparison-equals:" str
        |> fun attribute -> Ok (MatchMappingRestComparisonEquals attribute)
    | _ when String.is_prefix str ~prefix:"match-sequence-rest-list:" ->
        strip_prefix ~prefix:"match-sequence-rest-list:" str
        |> fun attribute -> Ok (MatchSequenceRestList attribute)
    | _ when String.is_prefix str ~prefix:"match-sequence-rest-subscript:" ->
        strip_prefix ~prefix:"match-sequence-rest-subscript:" str
        |> fun attribute -> Ok (MatchSequenceRestSubscript attribute)
    | _ when String.is_prefix str ~prefix:"match-sequence-rest-slice:" ->
        strip_prefix ~prefix:"match-sequence-rest-slice:" str
        |> fun attribute -> Ok (MatchSequenceRestSlice attribute)
    | _ when String.is_prefix str ~prefix:"match-sequence-rest-comparison-equals:" ->
        strip_prefix ~prefix:"match-sequence-rest-comparison-equals:" str
        |> fun attribute -> Ok (MatchSequenceRestComparisonEquals attribute)
    | _ when String.is_prefix str ~prefix:"match-sequence-prefix:" ->
        strip_prefix ~prefix:"match-sequence-prefix:" str
        |> parse_int
        >>= fun index -> Ok (MatchSequencePrefix index)
    | _ when String.is_prefix str ~prefix:"match-sequence-suffix:" ->
        strip_prefix ~prefix:"match-sequence-suffix:" str
        |> parse_int
        >>= fun index -> Ok (MatchSequenceSuffix index)
    | _ when String.is_prefix str ~prefix:"self-implicit-typevar:" ->
        strip_prefix ~prefix:"self-implicit-typevar:" str
        |> fun name -> Ok (SelfImplicitTypeVar name)
    | _ when String.is_prefix str ~prefix:"self-implicit-typevar-qualification:" -> (
        let str = strip_prefix ~prefix:"self-implicit-typevar-qualification:" str in
        match String.split ~on:':' str with
        | name :: rest ->
            parse_identifier_list (String.concat ~sep:":" rest)
            |> fun attributes -> Ok (SelfImplicitTypeVarQualification (name, attributes))
        | [] -> Error "Invalid self-implicit-typevar-qualification format")
    | _ when String.is_prefix str ~prefix:"functional-enum-implicit-auto:" ->
        strip_prefix ~prefix:"functional-enum-implicit-auto:" str
        |> parse_identifier_list
        |> fun attributes -> Ok (FunctionalEnumImplicitAuto attributes)
    | _ when String.is_prefix str ~prefix:"for-decorated-target-callee:" ->
        strip_prefix ~prefix:"for-decorated-target-callee:" str
        |> parse_identifier_list
        |> fun identifiers -> Ok (ForDecoratedTargetCallee identifiers)
    | _ when String.is_prefix str ~prefix:"pysa-call-redirect:" ->
        strip_prefix ~prefix:"pysa-call-redirect:" str |> fun name -> Ok (PysaCallRedirect name)
    | _ when String.is_prefix str ~prefix:"pysa-higher-order-parameter:" ->
        strip_prefix ~prefix:"pysa-higher-order-parameter:" str
        |> parse_int
        >>= fun index -> Ok (PysaHigherOrderParameter index)
    | _ when String.is_substring str ~substring:">" ->
        let index = Option.value_exn (String.rfindi str ~f:(fun _ c -> Char.equal c '>')) in
        let tail_str = String.prefix str index in
        let head_str = String.suffix str (String.length str - index - 1) in
        kind_from_json tail_str
        >>= fun tail -> kind_from_json head_str >>= fun head -> Ok (Nested { head; tail })
    | _ -> Error (Format.sprintf "Unknown origin kind: `%s`" str)


  let map_location ~f { kind; location } = { kind; location = f location }
end

and Expression : sig
  type expression =
    | Await of Await.t
    | BinaryOperator of BinaryOperator.t
    | BooleanOperator of BooleanOperator.t
    | Call of Call.t
    | ComparisonOperator of ComparisonOperator.t
    | Constant of Constant.t
    | Dictionary of Dictionary.t
    | DictionaryComprehension of Dictionary.Entry.KeyValue.t Comprehension.t
    | Generator of t Comprehension.t
    | FormatString of Substring.t list
    | Lambda of Lambda.t
    | List of t list
    | ListComprehension of t Comprehension.t
    | Name of Name.t
    | Set of t list
    | SetComprehension of t Comprehension.t
    | Slice of Slice.t
    | Starred of Starred.t
    | Subscript of Subscript.t
    | Ternary of Ternary.t
    | Tuple of t list
    | UnaryOperator of UnaryOperator.t
    | WalrusOperator of WalrusOperator.t
    | Yield of t option
    | YieldFrom of t

  and t = expression Node.t [@@deriving equal, compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int

  val pp_expression_list : Format.formatter -> t list -> unit

  val pp_expression_argument_list : Format.formatter -> Call.Argument.t list -> unit

  val pp_expression_parameter_list : Format.formatter -> Parameter.t list -> unit

  val pp_type_param_list : Format.formatter -> TypeParam.t list -> unit
end = struct
  type expression =
    | Await of Await.t
    | BinaryOperator of BinaryOperator.t
    | BooleanOperator of BooleanOperator.t
    | Call of Call.t
    | ComparisonOperator of ComparisonOperator.t
    | Constant of Constant.t
    | Dictionary of Dictionary.t
    | DictionaryComprehension of Dictionary.Entry.KeyValue.t Comprehension.t
    | Generator of t Comprehension.t
    | FormatString of Substring.t list
    | Lambda of Lambda.t
    | List of t list
    | ListComprehension of t Comprehension.t
    | Name of Name.t
    | Set of t list
    | SetComprehension of t Comprehension.t
    | Slice of Slice.t
    | Starred of Starred.t
    | Subscript of Subscript.t
    | Ternary of Ternary.t
    | Tuple of t list
    | UnaryOperator of UnaryOperator.t
    | WalrusOperator of WalrusOperator.t
    | Yield of t option
    | YieldFrom of t

  and t = expression Node.t [@@deriving equal, compare, sexp, show, hash, to_yojson]

  let _ = show (* shadowed below *)

  let rec location_insensitive_compare_expression left right =
    match left, right with
    | Await left, Await right -> Await.location_insensitive_compare left right
    | BinaryOperator left, BinaryOperator right ->
        BinaryOperator.location_insensitive_compare left right
    | BooleanOperator left, BooleanOperator right ->
        BooleanOperator.location_insensitive_compare left right
    | Call left, Call right -> Call.location_insensitive_compare left right
    | ComparisonOperator left, ComparisonOperator right ->
        ComparisonOperator.location_insensitive_compare left right
    | Constant left, Constant right -> Constant.compare left right
    | Dictionary left, Dictionary right -> Dictionary.location_insensitive_compare left right
    | DictionaryComprehension left, DictionaryComprehension right ->
        Comprehension.location_insensitive_compare
          Dictionary.Entry.KeyValue.location_insensitive_compare
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
    | Slice left, Slice right -> Slice.location_insensitive_compare left right
    | Starred left, Starred right -> Starred.location_insensitive_compare left right
    | Subscript left, Subscript right -> Subscript.location_insensitive_compare left right
    | Ternary left, Ternary right -> Ternary.location_insensitive_compare left right
    | Tuple left, Tuple right -> List.compare location_insensitive_compare left right
    | UnaryOperator left, UnaryOperator right ->
        UnaryOperator.location_insensitive_compare left right
    | WalrusOperator left, WalrusOperator right ->
        WalrusOperator.location_insensitive_compare left right
    | Yield left, Yield right -> Option.compare location_insensitive_compare left right
    | YieldFrom left, YieldFrom right -> location_insensitive_compare left right
    | Await _, _ -> -1
    | BinaryOperator _, _ -> -1
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
    | Slice _, _ -> -1
    | Starred _, _ -> -1
    | Subscript _, _ -> -1
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


    and pp_type_param_t formatter type_param_t =
      match type_param_t with
      | { Node.value = type_param; _ } -> Format.fprintf formatter "%a" pp_type_param type_param


    and pp_type_param formatter type_param =
      match type_param with
      | TypeParam.TypeVar { TypeParam.name; bound = None } -> Format.fprintf formatter "%s" name
      | TypeParam.TypeVar { TypeParam.name; bound = Some bound } ->
          Format.fprintf formatter "%s: %a" name pp_expression_t bound
      | TypeParam.TypeVarTuple name -> Format.fprintf formatter "*%s" name
      | TypeParam.ParamSpec name -> Format.fprintf formatter "**%s" name


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


    and pp_dictionary_entry formatter entry =
      let open Dictionary.Entry in
      match entry with
      | KeyValue KeyValue.{ key; value } ->
          Format.fprintf formatter "%a:%a" pp_expression_t key pp_expression_t value
      | Splat s -> Format.fprintf formatter "%a" pp_expression_t s


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


    and pp_type_param_list formatter type_param_list =
      let rec pp_type_param_list_helper formatter type_param_list =
        match type_param_list with
        | [] -> ()
        | [type_param] -> Format.fprintf formatter "%a" pp_type_param_t type_param
        | type_param :: type_param_list ->
            Format.fprintf
              formatter
              "%a, %a"
              pp_type_param_t
              type_param
              pp_type_param_list_helper
              type_param_list
      in
      match type_param_list with
      | [] -> Format.fprintf formatter ""
      | _ -> Format.fprintf formatter "[%a]" pp_type_param_list_helper type_param_list


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


    and pp_slice formatter = function
      | { Slice.start = None; stop = None; step = None; origin = _ } -> Format.fprintf formatter ":"
      | { Slice.start = Some start; stop = None; step = None; origin = _ } ->
          Format.fprintf formatter "%a:" pp_expression_t start
      | { Slice.start = Some start; stop = Some stop; step = None; origin = _ } ->
          Format.fprintf formatter "%a:%a" pp_expression_t start pp_expression_t stop
      | { Slice.start = None; stop = Some stop; step = None; origin = _ } ->
          Format.fprintf formatter ":%a" pp_expression_t stop
      | { Slice.start = Some start; stop = None; step = Some step; origin = _ } ->
          Format.fprintf formatter "%a::%a" pp_expression_t start pp_expression_t step
      | { Slice.start = Some start; stop = Some stop; step = Some step; origin = _ } ->
          Format.fprintf
            formatter
            "%a:%a:%a"
            pp_expression_t
            start
            pp_expression_t
            stop
            pp_expression_t
            step
      | { Slice.start = None; stop = Some stop; step = Some step; origin = _ } ->
          Format.fprintf formatter ":%a:%a" pp_expression_t stop pp_expression_t step
      | { Slice.start = None; stop = None; step = Some step; origin = _ } ->
          Format.fprintf formatter ":%a" pp_expression_t step


    and pp_subscript formatter { Subscript.base; index; origin = _ } =
      Format.fprintf formatter "%a[%a]" pp_expression_t base pp_expression_t index


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
      | Await { Await.operand; _ } -> Format.fprintf formatter "await %a" pp_expression_t operand
      | BinaryOperator { BinaryOperator.left; operator; right; origin = _ } ->
          Format.fprintf
            formatter
            "%a %a %a"
            pp_expression_t
            left
            BinaryOperator.pp_binary_operator
            operator
            pp_expression_t
            right
      | BooleanOperator { BooleanOperator.left; operator; right; origin = _ } ->
          Format.fprintf
            formatter
            "%a %a %a"
            pp_expression_t
            left
            BooleanOperator.pp_boolean_operator
            operator
            pp_expression_t
            right
      | Call { Call.callee; arguments; origin = _ } ->
          Format.fprintf formatter "%a(%a)" pp_expression_t callee pp_argument_list arguments
      | FormatString substrings ->
          let pp_substring formatter = function
            | Substring.Literal { Node.value; _ } -> Format.fprintf formatter "\"%s\"" value
            | Substring.Format { value = expression; format_spec = None } ->
                Format.fprintf formatter "f\"{%a}\"" pp_expression_t expression
            | Substring.Format { value = expression; format_spec = Some format_expr } ->
                Format.fprintf
                  formatter
                  "f\"{%a:{%a}}\""
                  pp_expression_t
                  expression
                  pp_expression_t
                  format_expr
          in
          List.iter substrings ~f:(pp_substring formatter)
      | ComparisonOperator { ComparisonOperator.left; operator; right; origin = _ } ->
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
      | Dictionary entries -> Format.fprintf formatter "{ %a }" pp_dictionary entries
      | DictionaryComprehension { Comprehension.element; generators } ->
          Format.fprintf
            formatter
            "{ %a: %a }"
            pp_dictionary_entry
            (Dictionary.Entry.KeyValue element)
            pp_generators
            generators
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
      | Name (Name.Attribute { Name.Attribute.base; attribute; _ }) ->
          Format.fprintf formatter "%a.%s" pp_expression (Node.value base) attribute
      | Set set -> Format.fprintf formatter "set(%a)" pp_expression_list set
      | SetComprehension set_comprehension ->
          Format.fprintf formatter "set(%a)" pp_basic_comprehension set_comprehension
      | Slice slice -> Format.fprintf formatter "%a" pp_slice slice
      | Starred starred -> Format.fprintf formatter "%a" pp_starred starred
      | Subscript subscript -> Format.fprintf formatter "%a" pp_subscript subscript
      | Ternary ternary -> Format.fprintf formatter "%a" pp_ternary ternary
      | Tuple tuple -> Format.fprintf formatter "(%a)" pp_expression_list tuple
      | UnaryOperator { UnaryOperator.operator; operand; origin = _ } ->
          Format.fprintf
            formatter
            "%a %a"
            UnaryOperator.pp_unary_operator
            operator
            pp_expression_t
            operand
      | WalrusOperator { WalrusOperator.target; value; origin = _ } ->
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


  let pp_type_param_list formatter type_param_list =
    Format.fprintf formatter "%a" PrettyPrinter.pp_type_param_list type_param_list


  let pp_expression_parameter_list formatter expression_parameter_list =
    Format.fprintf formatter "%a" PrettyPrinter.pp_parameter_list expression_parameter_list
end

module Mapper = struct
  type 'a t = {
    map_await: mapper:'a t -> location:Location.t -> Await.t -> 'a;
    map_binary_operator: mapper:'a t -> location:Location.t -> BinaryOperator.t -> 'a;
    map_boolean_operator: mapper:'a t -> location:Location.t -> BooleanOperator.t -> 'a;
    map_call: mapper:'a t -> location:Location.t -> Call.t -> 'a;
    map_comparison_operator: mapper:'a t -> location:Location.t -> ComparisonOperator.t -> 'a;
    map_constant: mapper:'a t -> location:Location.t -> Constant.t -> 'a;
    map_dictionary: mapper:'a t -> location:Location.t -> Dictionary.t -> 'a;
    map_dictionary_comprehension:
      mapper:'a t -> location:Location.t -> Dictionary.Entry.KeyValue.t Comprehension.t -> 'a;
    map_generator: mapper:'a t -> location:Location.t -> Expression.t Comprehension.t -> 'a;
    map_format_string: mapper:'a t -> location:Location.t -> Substring.t list -> 'a;
    map_lambda: mapper:'a t -> location:Location.t -> Lambda.t -> 'a;
    map_list: mapper:'a t -> location:Location.t -> Expression.t list -> 'a;
    map_list_comprehension:
      mapper:'a t -> location:Location.t -> Expression.t Comprehension.t -> 'a;
    map_name: mapper:'a t -> location:Location.t -> Name.t -> 'a;
    map_set: mapper:'a t -> location:Location.t -> Expression.t list -> 'a;
    map_set_comprehension: mapper:'a t -> location:Location.t -> Expression.t Comprehension.t -> 'a;
    map_slice: mapper:'a t -> location:Location.t -> Slice.t -> 'a;
    map_starred: mapper:'a t -> location:Location.t -> Starred.t -> 'a;
    map_subscript: mapper:'a t -> location:Location.t -> Subscript.t -> 'a;
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
           map_binary_operator;
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
           map_slice;
           map_starred;
           map_subscript;
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
    | Expression.Await await -> map_await ~mapper ~location await
    | Expression.BinaryOperator binary_operator ->
        map_binary_operator ~mapper ~location binary_operator
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
    | Expression.Slice slice -> map_slice ~mapper ~location slice
    | Expression.Starred starred -> map_starred ~mapper ~location starred
    | Expression.Subscript subscript -> map_subscript ~mapper ~location subscript
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

  let default_map_dictionary_entry ~mapper entry =
    let open Dictionary.Entry in
    match entry with
    | KeyValue KeyValue.{ key; value } ->
        KeyValue KeyValue.{ key = map ~mapper key; value = map ~mapper value }
    | Splat s -> Splat (map ~mapper s)


  let default_map_dictionary_keyvalue ~mapper Dictionary.Entry.KeyValue.{ key; value } =
    Dictionary.Entry.KeyValue.{ key = map ~mapper key; value = map ~mapper value }


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
    | Substring.Format { value; format_spec } ->
        Substring.Format { value = map ~mapper value; format_spec = map_option ~mapper format_spec }


  let default_map_substrings_with_location ~mapper ~map_location substrings =
    (create_list_mapper (default_map_substring_with_location ~map_location)) ~mapper substrings


  let default_map_substrings ~mapper substrings =
    default_map_substrings_with_location ~mapper ~map_location:Fn.id substrings


  let default_map_await ~mapper { Await.operand; origin } =
    { Await.operand = map ~mapper operand; origin }


  let default_map_await_node ~mapper ~location awaited =
    Node.create ~location (Expression.Await (default_map_await ~mapper awaited))


  let default_map_binary_operator ~mapper { BinaryOperator.left; operator; right; origin } =
    { BinaryOperator.left = map ~mapper left; operator; right = map ~mapper right; origin }


  let default_map_binary_operator_node ~mapper ~location binary_operator =
    Node.create
      ~location
      (Expression.BinaryOperator (default_map_binary_operator ~mapper binary_operator))


  let default_map_boolean_operator ~mapper { BooleanOperator.left; operator; right; origin } =
    { BooleanOperator.left = map ~mapper left; operator; right = map ~mapper right; origin }


  let default_map_boolean_operator_node ~mapper ~location boolean_operator =
    Node.create
      ~location
      (Expression.BooleanOperator (default_map_boolean_operator ~mapper boolean_operator))


  let default_map_call ~mapper { Call.callee; arguments; origin } =
    {
      Call.callee = map ~mapper callee;
      arguments = default_map_arguments ~mapper arguments;
      origin;
    }


  let default_map_call_node ~mapper ~location call =
    Node.create ~location (Expression.Call (default_map_call ~mapper call))


  let default_map_comparison_operator ~mapper { ComparisonOperator.left; operator; right; origin } =
    { ComparisonOperator.left = map ~mapper left; operator; right = map ~mapper right; origin }


  let default_map_comparison_operator_node ~mapper ~location comparison_operator =
    Node.create
      ~location
      (Expression.ComparisonOperator (default_map_comparison_operator ~mapper comparison_operator))


  let default_map_constant ~mapper:_ constant = constant

  let default_map_constant_node ~mapper ~location constant =
    Node.create ~location (Expression.Constant (default_map_constant ~mapper constant))


  let default_map_dictionary ~mapper entries =
    (create_list_mapper default_map_dictionary_entry) ~mapper entries


  let default_map_dictionary_node ~mapper ~location dictionary =
    Node.create ~location (Expression.Dictionary (default_map_dictionary ~mapper dictionary))


  let default_map_dictionary_comprehension ~mapper comprehension =
    default_map_comprehension ~map_element:default_map_dictionary_keyvalue ~mapper comprehension


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
    | Name.Attribute { Name.Attribute.base; attribute; origin } ->
        Name.Attribute { Name.Attribute.base = map ~mapper base; attribute; origin }


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


  let default_map_slice ~mapper { Slice.start; stop; step; origin } =
    {
      Slice.start = map_option ~mapper start;
      stop = map_option ~mapper stop;
      step = map_option ~mapper step;
      origin;
    }


  let default_map_slice_node ~mapper ~location slice =
    Node.create ~location (Expression.Slice (default_map_slice ~mapper slice))


  let default_map_starred ~mapper = function
    | Starred.Once expression -> Starred.Once (map ~mapper expression)
    | Starred.Twice expression -> Starred.Twice (map ~mapper expression)


  let default_map_starred_node ~mapper ~location starred =
    Node.create ~location (Expression.Starred (default_map_starred ~mapper starred))


  let default_map_subscript ~mapper { Subscript.base; index; origin } =
    { Subscript.base = map ~mapper base; index = map ~mapper index; origin }


  let default_map_subscript_node ~mapper ~location subscript =
    Node.create ~location (Expression.Subscript (default_map_subscript ~mapper subscript))


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


  let default_map_unary_operator ~mapper { UnaryOperator.operator; operand; origin } =
    { UnaryOperator.operator; operand = map ~mapper operand; origin }


  let default_map_unary_operator_node ~mapper ~location unary_operator =
    Node.create
      ~location
      (Expression.UnaryOperator (default_map_unary_operator ~mapper unary_operator))


  let default_map_walrus_operator ~mapper { WalrusOperator.target; value; origin } =
    { WalrusOperator.target = map ~mapper target; value = map ~mapper value; origin }


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
      ~map_binary_operator
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
      ~map_slice
      ~map_starred
      ~map_subscript
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
      map_binary_operator;
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
      map_slice;
      map_starred;
      map_subscript;
      map_ternary;
      map_tuple;
      map_unary_operator;
      map_walrus_operator;
      map_yield;
      map_yield_from;
    }


  let create_default
      ?(map_await = default_map_await_node)
      ?(map_binary_operator = default_map_binary_operator_node)
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
      ?(map_slice = default_map_slice_node)
      ?(map_starred = default_map_starred_node)
      ?(map_subscript = default_map_subscript_node)
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
      map_binary_operator;
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
      map_slice;
      map_starred;
      map_subscript;
      map_ternary;
      map_tuple;
      map_unary_operator;
      map_walrus_operator;
      map_yield;
      map_yield_from;
    }


  let create_transformer
      ?(map_await = default_map_await)
      ?(map_binary_operator = default_map_binary_operator)
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
      ?(map_slice = default_map_slice)
      ?(map_starred = default_map_starred)
      ?(map_subscript = default_map_subscript)
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
    let map_binary_operator ~mapper ~location binary_operator =
      Node.create
        ~location:(map_location location)
        (Expression.BinaryOperator (map_binary_operator ~mapper binary_operator))
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
    let map_slice ~mapper ~location slice =
      Node.create ~location:(map_location location) (Expression.Slice (map_slice ~mapper slice))
    in
    let map_starred ~mapper ~location starred =
      Node.create
        ~location:(map_location location)
        (Expression.Starred (map_starred ~mapper starred))
    in
    let map_subscript ~mapper ~location subscript =
      Node.create
        ~location:(map_location location)
        (Expression.Subscript (map_subscript ~mapper subscript))
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
      ~map_binary_operator
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
      ~map_slice
      ~map_starred
      ~map_subscript
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
    fold_await: folder:'a t -> state:'a -> location:Location.t -> Await.t -> 'a;
    fold_binary_operator: folder:'a t -> state:'a -> location:Location.t -> BinaryOperator.t -> 'a;
    fold_boolean_operator:
      folder:'a t -> state:'a -> location:Location.t -> BooleanOperator.t -> 'a;
    fold_call: folder:'a t -> state:'a -> location:Location.t -> Call.t -> 'a;
    fold_comparison_operator:
      folder:'a t -> state:'a -> location:Location.t -> ComparisonOperator.t -> 'a;
    fold_constant: folder:'a t -> state:'a -> location:Location.t -> Constant.t -> 'a;
    fold_dictionary: folder:'a t -> state:'a -> location:Location.t -> Dictionary.t -> 'a;
    fold_dictionary_comprehension:
      folder:'a t ->
      state:'a ->
      location:Location.t ->
      Dictionary.Entry.KeyValue.t Comprehension.t ->
      'a;
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
    fold_slice: folder:'a t -> state:'a -> location:Location.t -> Slice.t -> 'a;
    fold_starred: folder:'a t -> state:'a -> location:Location.t -> Starred.t -> 'a;
    fold_subscript: folder:'a t -> state:'a -> location:Location.t -> Subscript.t -> 'a;
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
           fold_binary_operator;
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
           fold_slice;
           fold_starred;
           fold_subscript;
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
    | Expression.Await await -> fold_await ~folder ~state ~location await
    | Expression.BinaryOperator binary_operator ->
        fold_binary_operator ~folder ~state ~location binary_operator
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
    | Expression.Slice slice -> fold_slice ~folder ~state ~location slice
    | Expression.Starred starred -> fold_starred ~folder ~state ~location starred
    | Expression.Subscript subscript -> fold_subscript ~folder ~state ~location subscript
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


  let default_fold_dictionary_entry ~folder ~state entry =
    let open Dictionary.Entry in
    match entry with
    | KeyValue KeyValue.{ key; value } ->
        let state = fold ~folder ~state key in
        fold ~folder ~state value
    | Splat s -> fold ~folder ~state s


  let default_fold_dictionary_keyvalue ~folder ~state Dictionary.Entry.KeyValue.{ key; value } =
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
    | Substring.Format { value; format_spec } ->
        let state = fold ~folder ~state value in
        fold_option ~folder ~state format_spec


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


  let default_fold_await ~folder ~state { Await.operand; origin = _ } = fold ~folder ~state operand

  let default_fold_binary_operator
      ~folder
      ~state
      { BinaryOperator.left; operator = _; right; origin = _ }
    =
    let state = fold ~folder ~state left in
    fold ~folder ~state right


  let default_fold_boolean_operator
      ~folder
      ~state
      { BooleanOperator.left; operator = _; right; origin = _ }
    =
    let state = fold ~folder ~state left in
    fold ~folder ~state right


  let default_fold_call ~folder ~state { Call.callee; arguments; origin = _ } =
    let state = fold ~folder ~state callee in
    default_fold_arguments ~folder ~state arguments


  let default_fold_comparison_operator
      ~folder
      ~state
      { ComparisonOperator.left; operator = _; right; origin = _ }
    =
    let state = fold ~folder ~state left in
    fold ~folder ~state right


  let default_fold_constant ~folder:_ ~state _ = state

  let default_fold_dictionary ~folder ~state entries =
    default_fold_dictionary_entries ~folder ~state entries


  let default_fold_dictionary_comprehension ~folder ~state comprehension =
    default_fold_comprehension
      ~fold_element:default_fold_dictionary_keyvalue
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
    | Name.Attribute { Name.Attribute.base; attribute = _; origin = _ } -> fold ~folder ~state base


  let default_fold_slice ~folder ~state { Slice.start; stop; step; origin = _ } =
    let state = fold_option ~folder ~state start in
    let state = fold_option ~folder ~state stop in
    fold_option ~folder ~state step


  let default_fold_starred ~folder ~state = function
    | Starred.Once expression
    | Starred.Twice expression ->
        fold ~folder ~state expression


  let default_fold_subscript ~folder ~state { Subscript.base; index; origin = _ } =
    let state = fold ~folder ~state base in
    fold ~folder ~state index


  let default_fold_ternary ~folder ~state { Ternary.target; test; alternative } =
    let state = fold ~folder ~state target in
    let state = fold ~folder ~state test in
    fold ~folder ~state alternative


  let default_fold_unary_operator ~folder ~state { UnaryOperator.operator = _; operand; origin = _ }
    =
    fold ~folder ~state operand


  let default_fold_walrus_operator ~folder ~state { WalrusOperator.target; value; origin = _ } =
    let state = fold ~folder ~state target in
    fold ~folder ~state value


  let default_fold_yield ~folder ~state yield = fold_option ~folder ~state yield

  let default_fold_yield_from ~folder ~state yield_from = fold ~folder ~state yield_from

  let fold_ignoring_location f ~folder ~state ~location:_ value = f ~folder ~state value

  let create
      ?(fold_await = fold_ignoring_location default_fold_await)
      ?(fold_binary_operator = fold_ignoring_location default_fold_binary_operator)
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
      ?(fold_slice = fold_ignoring_location default_fold_slice)
      ?(fold_starred = fold_ignoring_location default_fold_starred)
      ?(fold_subscript = fold_ignoring_location default_fold_subscript)
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
      fold_binary_operator;
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
      fold_slice;
      fold_starred;
      fold_subscript;
      fold_ternary;
      fold_tuple;
      fold_unary_operator;
      fold_walrus_operator;
      fold_yield;
      fold_yield_from;
    }


  let create_with_uniform_location_fold
      ?(fold_await = default_fold_await)
      ?(fold_binary_operator = default_fold_binary_operator)
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
      ?(fold_slice = default_fold_slice)
      ?(fold_starred = default_fold_starred)
      ?(fold_subscript = default_fold_subscript)
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
      fold_binary_operator = fold_with_location fold_binary_operator;
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
      fold_slice = fold_with_location fold_slice;
      fold_starred = fold_with_location fold_starred;
      fold_subscript = fold_with_location fold_subscript;
      fold_ternary = fold_with_location fold_ternary;
      fold_tuple = fold_with_location fold_tuple;
      fold_unary_operator = fold_with_location fold_unary_operator;
      fold_walrus_operator = fold_with_location fold_walrus_operator;
      fold_yield = fold_with_location fold_yield;
      fold_yield_from = fold_with_location fold_yield_from;
    }
end

include Expression

let origin { Node.value; _ } =
  match value with
  | Expression.Name (Name.Attribute { Name.Attribute.origin; _ }) -> origin
  | Expression.Call { Call.origin; _ } -> origin
  | Expression.ComparisonOperator { ComparisonOperator.origin; _ } -> origin
  | Expression.BinaryOperator { BinaryOperator.origin; _ } -> origin
  | Expression.UnaryOperator { UnaryOperator.origin; _ } -> origin
  | Expression.BooleanOperator { BooleanOperator.origin; _ } -> origin
  | Expression.Subscript { Subscript.origin; _ } -> origin
  | Expression.WalrusOperator { WalrusOperator.origin; _ } -> origin
  | Expression.Slice { Slice.origin; _ } -> origin
  | Expression.Await { Await.origin; _ } -> origin
  | _ -> None


let map_origin ~f ({ Node.value; location } as expression) =
  match value with
  | Expression.Name (Name.Attribute ({ Name.Attribute.origin; _ } as attribute_access)) ->
      Expression.Name (Name.Attribute { attribute_access with Name.Attribute.origin = f origin })
      |> Node.create ~location
  | Expression.Call ({ Call.origin; _ } as call) ->
      Expression.Call { call with Call.origin = f origin } |> Node.create ~location
  | Expression.ComparisonOperator ({ ComparisonOperator.origin; _ } as comparison) ->
      Expression.ComparisonOperator { comparison with ComparisonOperator.origin = f origin }
      |> Node.create ~location
  | Expression.BinaryOperator ({ BinaryOperator.origin; _ } as binary) ->
      Expression.BinaryOperator { binary with BinaryOperator.origin = f origin }
      |> Node.create ~location
  | Expression.UnaryOperator ({ UnaryOperator.origin; _ } as unary) ->
      Expression.UnaryOperator { unary with UnaryOperator.origin = f origin }
      |> Node.create ~location
  | Expression.BooleanOperator ({ BooleanOperator.origin; _ } as boolean) ->
      Expression.BooleanOperator { boolean with BooleanOperator.origin = f origin }
      |> Node.create ~location
  | Expression.Subscript ({ Subscript.origin; _ } as subscript) ->
      Expression.Subscript { subscript with Subscript.origin = f origin } |> Node.create ~location
  | Expression.WalrusOperator ({ WalrusOperator.origin; _ } as walrus) ->
      Expression.WalrusOperator { walrus with WalrusOperator.origin = f origin }
      |> Node.create ~location
  | Expression.Slice ({ Slice.origin; _ } as slice) ->
      Expression.Slice { slice with Slice.origin = f origin } |> Node.create ~location
  | Expression.Await ({ Await.origin; _ } as await) ->
      Expression.Await { await with Await.origin = f origin } |> Node.create ~location
  | _ -> expression


let negate ~normalize ({ Node.location; value } as node) =
  match value with
  | UnaryOperator { UnaryOperator.operator = UnaryOperator.Not; operand; origin = _ } -> operand
  | ComparisonOperator
      { ComparisonOperator.operator = ComparisonOperator.IsNot; left; right; origin = base_origin }
    when normalize ->
      {
        Node.location;
        value =
          ComparisonOperator
            {
              ComparisonOperator.operator = ComparisonOperator.Is;
              left;
              right;
              origin = Some (Origin.create ?base:base_origin ~location Origin.NegateIs);
            };
      }
  | ComparisonOperator
      { ComparisonOperator.operator = ComparisonOperator.Is; left; right; origin = base_origin }
    when normalize ->
      {
        Node.location;
        value =
          ComparisonOperator
            {
              ComparisonOperator.operator = ComparisonOperator.IsNot;
              left;
              right;
              origin = Some (Origin.create ?base:base_origin ~location Origin.NegateIsNot);
            };
      }
  | _ ->
      {
        Node.location;
        value =
          UnaryOperator
            {
              UnaryOperator.operator = UnaryOperator.Not;
              operand = node;
              origin = Some (Origin.create ?base:(origin node) ~location Origin.Negate);
            };
      }


(* Changes boolean expressions to negation normal form *)
let rec normalize { Node.location; value } =
  let normalized =
    match value with
    | BooleanOperator { BooleanOperator.operator; left; right; origin } ->
        BooleanOperator
          {
            BooleanOperator.operator;
            left = normalize left;
            right = normalize right;
            origin = Some (Origin.create ?base:origin ~location Origin.Normalize);
          }
    | UnaryOperator
        {
          UnaryOperator.operator = UnaryOperator.Not;
          operand = { Node.value = operand; location = operand_location };
          origin = _;
        } as unary -> (
        match operand with
        | ComparisonOperator
            { ComparisonOperator.left; operator; right; origin = comparison_origin } ->
            ComparisonOperator
              {
                ComparisonOperator.left;
                operator = ComparisonOperator.inverse operator;
                right;
                origin =
                  Some
                    (Origin.create
                       ?base:comparison_origin
                       ~location:operand_location
                       Origin.NormalizeNotComparison);
              }
        | Constant Constant.False -> Constant Constant.True
        | Constant Constant.True -> Constant Constant.False
        | UnaryOperator
            { UnaryOperator.operator = UnaryOperator.Not; operand = { Node.value; _ }; origin = _ }
          ->
            value
        | BooleanOperator { BooleanOperator.left; operator; right; origin = bool_origin } ->
            BooleanOperator
              {
                BooleanOperator.operator = BooleanOperator.inverse operator;
                left = normalize (negate ~normalize:true left);
                right = normalize (negate ~normalize:true right);
                origin =
                  Some
                    (Origin.create
                       ?base:bool_origin
                       ~location:operand_location
                       Origin.NormalizeNotBoolOperator);
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


let create_name_from_identifiers ~location ~create_origin identifiers =
  let rec create = function
    | [] -> failwith "Name must have non-zero identifiers."
    | [identifier] -> Name (Name.Identifier identifier) |> Node.create ~location
    | identifier :: rest as current_identifiers ->
        Name
          (Name.Attribute
             {
               Name.Attribute.base = create rest;
               attribute = identifier;
               origin = create_origin current_identifiers;
             })
        |> Node.create ~location
  in
  match create (List.rev identifiers) with
  | { Node.value = Expression.Name name; _ } -> name
  | _ -> failwith "Impossible."


let create_name ~location ~create_origin name =
  let identifiers =
    if String.equal name "..." then
      [name]
    else
      String.split ~on:'.' name
  in
  create_name_from_identifiers ~location ~create_origin identifiers


let create_name_from_reference ~location ~create_origin reference =
  let rec create = function
    | [] -> Name (Name.Identifier "") |> Node.create ~location
    | [identifier] -> Name (Name.Identifier identifier) |> Node.create ~location
    | identifier :: rest as current_reference ->
        Name
          (Name.Attribute
             {
               Name.Attribute.base = create rest;
               attribute = identifier;
               origin = create_origin current_reference;
             })
        |> Node.create ~location
  in
  match create (List.rev (Reference.as_list reference)) with
  | { Node.value = Name name; _ } -> name
  | _ -> failwith "Impossible."


let from_reference ~location ~create_origin reference =
  create_name_from_reference ~location ~create_origin reference
  |> (fun name -> Name name)
  |> Node.create ~location


let name_to_identifiers name =
  let rec collect sofar name =
    match sofar, name with
    | Some sofar, Name (Name.Identifier identifier) -> Some (identifier :: sofar)
    | Some sofar, Name (Name.Attribute { Name.Attribute.base; attribute; _ }) ->
        collect (Some (attribute :: sofar)) (Node.value base)
    | _ -> None
  in
  collect (Some []) (Name name)


let name_to_reference name = name_to_identifiers name >>| Reference.create_from_list

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
  | Call { Call.callee; _ } -> get_identifier_base callee
  | Subscript { Subscript.base; _ } -> get_identifier_base base
  | Name (Name.Attribute { Name.Attribute.base; _ }) -> get_identifier_base base
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
    | name :: remaining, Name (Name.Attribute { Name.Attribute.base; attribute; _ })
      when Identifier.equal name attribute ->
        check_match (remaining, Node.value base)
    | _ -> false
  in
  check_match (List.rev identifiers, Node.value expression)


let rec sanitized ({ Node.value; location } as expression) =
  match value with
  | Name (Name.Identifier identifier) ->
      Name (Name.Identifier (Identifier.sanitized identifier)) |> Node.create ~location
  | Name (Name.Attribute { Name.Attribute.base; attribute; origin }) ->
      Name
        (Name.Attribute
           {
             Name.Attribute.base = sanitized base;
             attribute = Identifier.sanitized attribute;
             origin;
           })
      |> Node.create ~location
  | Call { Call.callee; arguments; origin } ->
      let sanitize_argument { Call.Argument.name; value } =
        let name =
          match name with
          | Some { Node.value; location } ->
              Some { Node.value = Identifier.sanitized value; location }
          | None -> None
        in
        { Call.Argument.name; value = sanitized value }
      in
      Call
        {
          Call.callee = sanitized callee;
          arguments = List.map ~f:sanitize_argument arguments;
          origin;
        }
      |> Node.create ~location
  | _ -> expression


let rec delocalize ~create_origin ({ Node.value; location } as expression) =
  let delocalize = delocalize ~create_origin in
  let value =
    match value with
    | Subscript { Subscript.base; index; origin } ->
        Subscript { Subscript.base = delocalize base; index = delocalize index; origin }
    | Slice { Slice.start; stop; step; origin } ->
        Slice
          {
            Slice.start = Option.map ~f:delocalize start;
            stop = Option.map ~f:delocalize stop;
            step = Option.map ~f:delocalize step;
            origin;
          }
    | Call { Call.callee; arguments; origin } ->
        let delocalize_argument ({ Call.Argument.value; _ } as argument) =
          { argument with Call.Argument.value = delocalize value }
        in
        Call
          {
            Call.callee = delocalize callee;
            arguments = List.map ~f:delocalize_argument arguments;
            origin;
          }
    | Name (Name.Identifier identifier) when identifier |> String.is_prefix ~prefix:"$local_$" ->
        let sanitized = Identifier.sanitized identifier in
        Name (Name.Identifier sanitized)
    | Name (Name.Identifier identifier) when identifier |> String.is_prefix ~prefix:"$local_" ->
        let sanitized = Identifier.sanitized identifier in
        if Str.string_match Reference.local_qualifier_pattern identifier 0 then
          let qualifier =
            Str.matched_group 1 identifier
            |> String.substr_replace_all ~pattern:"?" ~with_:"."
            |> create_name ~location ~create_origin:(create_origin ~expression)
            |> fun name -> Name name |> Node.create ~location
          in
          Name
            (Name.Attribute
               {
                 Name.Attribute.base = qualifier;
                 attribute = sanitized;
                 origin = create_origin ~expression [identifier];
               })
        else (
          Log.debug "Unable to extract qualifier from %s" identifier;
          Name (Name.Identifier sanitized))
    | Name (Name.Identifier identifier) -> Name (Name.Identifier identifier)
    | Name (Name.Attribute ({ Name.Attribute.base; _ } as name)) ->
        Name (Name.Attribute { name with Name.Attribute.base = delocalize base })
    | List elements -> List (List.map elements ~f:delocalize)
    | Tuple elements -> Tuple (List.map elements ~f:delocalize)
    | Starred (Starred.Once target) -> Starred (Starred.Once (delocalize target))
    | Starred (Starred.Twice target) -> Starred (Starred.Twice (delocalize target))
    | Await _
    | BinaryOperator _
    | BooleanOperator _
    | ComparisonOperator _
    | Constant _
    | Dictionary _
    | DictionaryComprehension _
    | Generator _
    | Lambda _
    | FormatString _
    | ListComprehension _
    | Set _
    | SetComprehension _
    | Ternary _
    | UnaryOperator _
    | WalrusOperator _
    | Yield _
    | YieldFrom _ ->
        value
  in
  { expression with Node.value }


let delocalize_qualified = function
  | { Node.location; value = Name (Name.Identifier identifier) } ->
      { Node.location; value = Name (Name.Identifier (Identifier.sanitized identifier)) }
  | { Node.location; value = Name (Name.Attribute ({ Name.Attribute.attribute; _ } as name)) } ->
      {
        Node.location;
        value =
          Name
            (Name.Attribute { name with Name.Attribute.attribute = Identifier.sanitized attribute });
      }
  | expression -> expression


let exists_in_list ?(match_prefix = false) ~expression_list target_string =
  let flatten =
    let rec flatten flattened expression =
      match flattened, Node.value expression with
      | Some flattened, Name (Name.Identifier identifier) -> Some (identifier :: flattened)
      | Some flattened, Name (Name.Attribute { Name.Attribute.base; attribute; _ }) ->
          flatten (Some (attribute :: flattened)) base
      | Some flattened, Call { Call.callee; _ } ->
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
    {
      Call.callee = { Node.location = { Location.stop = callee_end; _ }; _ };
      arguments;
      origin = _;
    }
  =
  match List.rev arguments with
  | [] ->
      {
        Location.start = callee_end;
        stop = { callee_end with Location.column = callee_end.Location.column + 2 };
      }
  | { Call.Argument.value = { Node.location = { Location.stop; _ }; _ }; _ } :: _ ->
      {
        Location.start = callee_end;
        stop = { stop with Location.column = stop.Location.column + 1 };
      }


let subscript base indices ~location ~create_origin_for_base ~origin =
  let create_name name = Name (create_name ~location ~create_origin:create_origin_for_base name) in
  let index =
    match indices with
    | [index] -> index
    | multiple_indices -> Tuple multiple_indices |> Node.create_with_default_location
  in
  Subscript { Subscript.base = { Node.location; value = create_name base }; index; origin }


let subscript_for_annotation base indices ~location =
  subscript base indices ~location ~create_origin_for_base:(fun _ -> None) ~origin:None


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


let remove_origins expression =
  let map_name ~mapper = function
    | Name.Identifier _ as identifier -> identifier
    | Name.Attribute { Name.Attribute.base; attribute; origin = _ } ->
        Name.Attribute { Name.Attribute.base = Mapper.map ~mapper base; attribute; origin = None }
  in
  let map_call ~mapper { Call.callee; arguments; origin = _ } =
    {
      Call.callee = Mapper.map ~mapper callee;
      arguments =
        List.map
          ~f:(fun { Call.Argument.name; value } ->
            { Call.Argument.name; value = Mapper.map ~mapper value })
          arguments;
      origin = None;
    }
  in
  let map_comparison_operator ~mapper { ComparisonOperator.left; operator; right; origin = _ } =
    {
      ComparisonOperator.left = Mapper.map ~mapper left;
      operator;
      right = Mapper.map ~mapper right;
      origin = None;
    }
  in
  let map_binary_operator ~mapper { BinaryOperator.left; operator; right; origin = _ } =
    {
      BinaryOperator.left = Mapper.map ~mapper left;
      operator;
      right = Mapper.map ~mapper right;
      origin = None;
    }
  in
  let map_boolean_operator ~mapper { BooleanOperator.left; operator; right; origin = _ } =
    {
      BooleanOperator.left = Mapper.map ~mapper left;
      operator;
      right = Mapper.map ~mapper right;
      origin = None;
    }
  in
  let map_unary_operator ~mapper { UnaryOperator.operator; operand; origin = _ } =
    { UnaryOperator.operand = Mapper.map ~mapper operand; operator; origin = None }
  in
  let map_subscript ~mapper { Subscript.base; index; origin = _ } =
    { Subscript.base = Mapper.map ~mapper base; index = Mapper.map ~mapper index; origin = None }
  in
  let map_walrus_operator ~mapper { WalrusOperator.target; value; origin = _ } =
    {
      WalrusOperator.target = Mapper.map ~mapper target;
      value = Mapper.map ~mapper value;
      origin = None;
    }
  in
  let map_slice ~mapper { Slice.start; stop; step; origin = _ } =
    {
      Slice.start = Option.map ~f:(Mapper.map ~mapper) start;
      stop = Option.map ~f:(Mapper.map ~mapper) stop;
      step = Option.map ~f:(Mapper.map ~mapper) step;
      origin = None;
    }
  in
  let map_await ~mapper { Await.operand; origin = _ } =
    { Await.operand = Mapper.map ~mapper operand; origin = None }
  in
  Mapper.map
    ~mapper:
      (Mapper.create_transformer
         ~map_name
         ~map_call
         ~map_comparison_operator
         ~map_binary_operator
         ~map_boolean_operator
         ~map_unary_operator
         ~map_subscript
         ~map_walrus_operator
         ~map_slice
         ~map_await
         ())
    expression
