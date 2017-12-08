(** Copyright 2016-present Facebook. All rights reserved. **)

open Core
open Sexplib.Std


module Call = struct
  type 'expression t = {
    name: 'expression;
    arguments: ('expression Argument.t) list;
  }
  [@@deriving compare, eq, sexp, show]
end


module BooleanOperator = struct
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


  let pp_boolean_operator formatter operator =
    Format.fprintf formatter
      "%s"
      (match operator with
       | And -> "and"
       | Or -> "or")


  let inverse = function
    | And -> Or
    | Or -> And
end


module BinaryOperator = struct
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


  let pp_binary_operator formatter operator =
    Format.fprintf
      formatter
      "%s"
      (match operator with
       | Add -> "+"
       | At -> "@"
       | BitAnd -> "&"
       | BitOr -> "|"
       | BitXor -> "^"
       | Divide -> "/"
       | FloorDivide -> "//"
       | LeftShift -> "<<"
       | Modulo -> "%"
       | Multiply -> "*"
       | Power -> "**"
       | RightShift -> ">>"
       | Subtract -> "-")
end


module UnaryOperator = struct
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


  let pp_unary_operator formatter operator =
    Format.fprintf
      formatter "%s"
      (match operator with
       | Invert -> "~"
       | Negative -> "-"
       | Not -> "not"
       | Positive -> "+")
end


module ComparisonOperator = struct
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
       | Equals -> "="
       | GreaterThan -> ">"
       | GreaterThanOrEquals -> ">="
       | In -> "in"
       | Is -> "is"
       | IsNot -> "is not"
       | LessThan -> "<"
       | LessThanOrEquals -> "<="
       | NotEquals -> "!="
       | NotIn -> "not in")
end


module Access = struct
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


  let show _ access =
    let identifier = function
      | Identifier identifier ->
          Identifier.show identifier
      | _ ->
          "?" in
    List.map ~f:identifier access
    |> String.concat ~sep:"."


  let pp print_expression format access =
    Format.fprintf format "%s" (show print_expression access)
end


module Lambda = struct
  type 'expression t = {
    parameters: ('expression Parameter.t) list;
    body: 'expression;
  }
  [@@deriving compare, eq, sexp, show]
end


module Ternary = struct
  type 'expression t = {
    target: 'expression;
    test: 'expression;
    alternative: 'expression;
  }
  [@@deriving compare, eq, sexp, show]
end


module Dictionary = struct
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


module Comprehension = struct
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


module Starred = struct
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


let negate ({ Node.location; _ } as node) =
  {
    Node.location;
    value = UnaryOperator {
        UnaryOperator.operator = UnaryOperator.Not;
        operand = node;
      };
  }


(* Changes boolean expressions to negation normal form *)
let rec normalize { Node.location; value } =
  let normalized =
    match value with
    | BooleanOperator {
        BooleanOperator.operator = BooleanOperator.And;
        left;
        right;
      } ->
        BooleanOperator {
          BooleanOperator.operator = BooleanOperator.And;
          left = normalize left;
          right = normalize right;
        }
    | BooleanOperator {
        BooleanOperator.operator = BooleanOperator.Or;
        left;
        right;
      } ->
        BooleanOperator {
          BooleanOperator.operator = BooleanOperator.And;
          left = normalize (negate left);
          right = normalize (negate right);
        }
    | UnaryOperator { UnaryOperator.operator = UnaryOperator.Not; operand = { Node.value; _ };
                    } ->
        begin
          match value with
          | ComparisonOperator {
              ComparisonOperator.left;
              right;
            } ->
              let invert (operator, expression) =
                ComparisonOperator.inverse operator, expression in
              ComparisonOperator {
                ComparisonOperator.left;
                right = List.map ~f:invert right;
              }
          | False ->
              True
          | True ->
              False
          | UnaryOperator {
              UnaryOperator.operator = UnaryOperator.Not;
              operand = { Node.value; _ };
            } ->
              value
          | BooleanOperator { BooleanOperator.left; operator; right } ->
              BooleanOperator {
                BooleanOperator.operator = BooleanOperator.inverse operator;
                left = normalize (negate left);
                right = normalize (negate right);
              }
          | _ ->
              UnaryOperator {
                UnaryOperator.operator = UnaryOperator.Not;
                operand = { Node.location; value };
              }
        end
    | _ ->
        value
  in
  { Node.location; value = normalized }


module PrettyPrinter = struct
  let rec pp_expression_node formatter expression_node =
    match expression_node with
    | { Node.value = expression; _ } ->
        Format.fprintf formatter "%a" pp_expression expression


  and pp_slice formatter { Access.lower; upper; step } =
    match lower,upper,step with
    | Some lower,None,None ->
        Format.fprintf formatter "%a" pp_expression_node lower
    | Some lower,Some upper,None ->
        Format.fprintf
          formatter
          "%a:%a"
          pp_expression_node lower
          pp_expression_node upper
    | Some lower,Some upper,Some step ->
        Format.fprintf formatter "%a:%a:%a"
          pp_expression_node lower
          pp_expression_node upper
          pp_expression_node step
    | Some lower, None, Some step ->
        Format.fprintf
          formatter
          "%a::%a"
          pp_expression_node lower
          pp_expression_node step
    | None,Some upper,None ->
        Format.fprintf formatter ":%a" pp_expression_node upper
    | None,None,Some step ->
        Format.fprintf formatter "::%a" pp_expression_node step
    | None,Some upper,Some step ->
        Format.fprintf
          formatter
          ":%a:%a"
          pp_expression_node upper
          pp_expression_node step
    | None,None,None -> ()


  and pp_subscript formatter subscript =
    match subscript with
    | Access.Index index ->
        Format.fprintf formatter "%a" pp_expression_node index
    | Access.Slice slice ->
        Format.fprintf formatter "%a" pp_slice slice


  and pp_subscript_list formatter subscript_list =
    match subscript_list with
    | [] -> ()
    | subscript :: [] -> Format.fprintf formatter "%a" pp_subscript subscript
    | subscript :: subscript_list ->
        Format.fprintf
          formatter
          "%a,%a"
          pp_subscript subscript
          pp_subscript_list subscript_list


  and pp_access formatter access =
    match access with
    | Access.Call { Node.value = { Call.name; arguments }; _ } ->
        Format.fprintf
          formatter
          "%a(%a)"
          pp_expression_node name
          pp_argument_list arguments
    | Access.Expression expression ->
        Format.fprintf formatter "%a" pp_expression_node expression
    | Access.Identifier identifier ->
        Format.fprintf formatter "%s" @@ Identifier.show identifier
    | Access.Subscript subscript_list ->
        Format.fprintf formatter "%a" pp_subscript_list subscript_list


  and pp_access_list formatter access_list =
    match access_list with
    | [] -> ()
    | access :: [] -> Format.fprintf formatter "%a" pp_access access
    | access :: ([ Access.Subscript _ ] as access_list) ->
        Format.fprintf
          formatter
          "%a[%a]"
          pp_access access
          pp_access_list access_list
    | access :: access_list ->
        Format.fprintf
          formatter
          "%a.%a"
          pp_access access
          pp_access_list access_list


  and pp_argument formatter { Argument.name; value } =
    match name with
    | Some name ->
        Format.fprintf
          formatter
          "%s = %a"
          (Identifier.show name)
          pp_expression_node value
    | None ->
        Format.fprintf formatter "%a" pp_expression_node value


  and pp_argument_list formatter argument_list =
    match argument_list with
    | [] -> ()
    | argument :: [] -> Format.fprintf formatter "%a" pp_argument argument
    | argument :: argument_list ->
        Format.fprintf
          formatter
          "%a,%a"
          pp_argument argument
          pp_argument_list argument_list


  and pp_comparison_list formatter comparison_list =
    match comparison_list with
    | [] -> ()
    | (operator,expression) :: [] ->
        Format.fprintf
          formatter
          "%a %a"
          ComparisonOperator.pp_comparison_operator operator
          pp_expression_node expression
    | (operator,expression) :: comparison_list ->
        Format.fprintf
          formatter
          "%a %a %a"
          ComparisonOperator.pp_comparison_operator operator
          pp_expression_node expression
          pp_comparison_list comparison_list


  and pp_dictionary_entry formatter { Dictionary.key; value } =
    Format.fprintf
      formatter
      "%a:%a"
      pp_expression_node key
      pp_expression_node value


  and pp_dictionary formatter dictionary =
    match dictionary with
    | [] -> ()
    | entry :: [] -> pp_dictionary_entry formatter entry
    | entry :: dictionary ->
        Format.fprintf
          formatter
          "%a,%a"
          pp_dictionary_entry entry
          pp_dictionary dictionary


  and pp_expression_list formatter expression_list =
    match expression_list with
    | [] -> ()
    | expression :: [] ->
        Format.fprintf formatter "%a" pp_expression_node expression
    | expression :: expression_list ->
        Format.fprintf
          formatter
          "%a, %a"
          pp_expression_node expression
          pp_expression_list expression_list


  and pp_generator formatter { Comprehension.target; iterator; conditions; async } =
    Format.fprintf
      formatter
      "generator(%s%a in %a if %a)"
      (if async then "async " else "")
      pp_expression_node target
      pp_expression_node iterator
      pp_expression_list conditions


  and pp_generators formatter generators =
    match generators with
    | [] -> ()
    | generator :: [] ->
        Format.fprintf
          formatter
          "generators(%a)"
          pp_generator generator
    | generator :: xs ->
        Format.fprintf
          formatter
          "generators(%a, %a)"
          pp_generator generator
          pp_generators xs

  and pp_parameter formatter { Node.value = { Parameter.name; value; annotation }; _ } =
    let identifier = Identifier.show name in
    match value,annotation with
    | Some expression, Some annotation ->
        Format.fprintf
          formatter
          "%s: %a=%a"
          identifier
          pp_expression_node annotation
          pp_expression_node expression
    | None, Some annotation ->
        Format.fprintf
          formatter
          "%s: %a"
          identifier
          pp_expression_node annotation
    | Some expression, None ->
        Format.fprintf
          formatter
          "%s=%a"
          identifier
          pp_expression_node expression
    | None, None ->
        Format.fprintf formatter "%s" identifier


  and pp_parameter_list formatter parameter_list =
    match parameter_list with
    | [] -> ()
    | parameter :: [] -> Format.fprintf formatter "%a" pp_parameter parameter
    | parameter :: parameter_list ->
        Format.fprintf
          formatter
          "%a, %a"
          pp_parameter parameter
          pp_parameter_list parameter_list


  and pp_basic_comprehension formatter { Comprehension.element ; generators } =
    (* shortcut for pretty printing (expression Node.t, expression Node.t) Comprehension.t *)
    Format.fprintf
      formatter
      "comprehension(%a for %a)"
      pp_expression_node element
      pp_generators generators


  and pp_starred formatter starred =
    match starred with
    | Starred.Once expression ->
        Format.fprintf
          formatter
          "%a"
          pp_expression_node expression
    | Starred.Twice expression ->
        Format.fprintf
          formatter
          "%a"
          pp_expression_node expression


  and pp_ternary formatter { Ternary.target; test; alternative } =
    Format.fprintf
      formatter
      "%a if %a else %a"
      pp_expression_node target
      pp_expression_node test
      pp_expression_node alternative


  and pp_expression formatter expression =
    match expression with
    | Access access_list ->
        pp_access_list formatter access_list

    | Await expression ->
        Format.fprintf
          formatter
          "%a"
          pp_expression_node expression

    | BinaryOperator { BinaryOperator.left; operator; right } ->
        Format.fprintf
          formatter
          "%a %a %a"
          pp_expression_node left
          BinaryOperator.pp_binary_operator operator
          pp_expression_node right

    | BooleanOperator { BooleanOperator.left; operator; right } ->
        Format.fprintf
          formatter
          "%a %a %a"
          pp_expression_node left
          BooleanOperator.pp_boolean_operator operator
          pp_expression_node right

    | Bytes string
    | String string
    | Format string ->
        Format.fprintf formatter "%S" string

    | ComparisonOperator { ComparisonOperator.left; right } ->
        Format.fprintf
          formatter
          "%a %a"
          pp_expression_node left
          pp_comparison_list right

    | Float float_value

    | Complex float_value -> Format.fprintf formatter "%f" float_value

    | Dictionary { Dictionary.entries; keywords } ->
        let pp_keywords format = function
          | Some keywords ->
              Format.fprintf format ", %a" pp_expression_node keywords
          | None ->
              () in
        Format.fprintf
          formatter
          "Dictionary { %a%a }"
          pp_dictionary entries
          pp_keywords keywords

    | DictionaryComprehension { Comprehension.element; generators } ->
        Format.fprintf
          formatter
          "Dictionary comprehension { %a: %a }"
          pp_dictionary_entry element
          pp_generators generators

    | False -> Format.fprintf formatter "%s" "False"

    | Generator generator ->
        Format.fprintf formatter "%a" pp_basic_comprehension generator

    | Integer integer -> Format.fprintf formatter "%d" integer

    | Lambda { Lambda.parameters; body } ->
        Format.fprintf
          formatter
          "lambda (%a) (%a)"
          pp_parameter_list parameters
          pp_expression_node body

    | List expression_list ->
        Format.fprintf formatter "[%a]" pp_expression_list expression_list

    | ListComprehension list_comprehension ->
        Format.fprintf formatter "%a" pp_basic_comprehension list_comprehension

    | Set set ->
        Format.fprintf formatter "set(%a)" pp_expression_list set

    | SetComprehension set_comprehension ->
        Format.fprintf formatter "set(%a)" pp_basic_comprehension set_comprehension

    | Starred starred ->
        Format.fprintf formatter "%a" pp_starred starred

    | Ternary ternary ->
        Format.fprintf formatter "%a" pp_ternary ternary

    | True ->
        Format.fprintf formatter "%s" "True"

    | Tuple tuple ->
        Format.fprintf formatter "(%a)" pp_expression_list tuple

    | UnaryOperator { UnaryOperator.operator; operand } ->
        Format.fprintf
          formatter
          "%a %a"
          UnaryOperator.pp_unary_operator operator
          pp_expression_node operand

    | Yield yield ->
        match yield with
        | Some yield ->
            Format.fprintf
              formatter
              "yield %a"
              pp_expression_node yield
        | None ->
            Format.fprintf formatter "%s" "yield"

  let pp = pp_expression_node
end


let pp formatter expression =
  Format.fprintf formatter "%a" PrettyPrinter.pp expression


let show expression = Format.asprintf "%a" pp expression


let pp_expression_list formatter expression_list =
  Format.fprintf
    formatter
    "%a"
    PrettyPrinter.pp_expression_list expression_list


let pp_expression_access_list formatter expression_access_list =
  Format.fprintf
    formatter
    "%a"
    PrettyPrinter.pp_access_list expression_access_list


let pp_expression_argument_list formatter expression_argument_list =
  Format.fprintf
    formatter
    "%a"
    PrettyPrinter.pp_argument_list expression_argument_list


let pp_expression_parameter_list formatter expression_parameter_list =
  Format.fprintf
    formatter
    "%a"
    PrettyPrinter.pp_parameter_list expression_parameter_list
