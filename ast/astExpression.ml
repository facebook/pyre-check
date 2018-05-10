(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open Sexplib.Std

open Pyre

module Identifier = AstIdentifier
module Location = AstLocation
module Parameter = AstParameter
module Node = AstNode


module BooleanOperator = struct
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


  let pp_boolean_operator formatter operator =
    Format.fprintf formatter
      "%s"
      begin
        match operator with
        | And -> "and"
        | Or -> "or"
      end


  let inverse = function
    | And -> Or
    | Or -> And
end


module Record = struct
  module Argument = struct
    type 'expression record = {
      name: (Identifier.t Node.t) option;
      value: 'expression;
    }
    [@@deriving compare, eq, sexp, show, hash]
  end

  module Access = struct
    type 'expression slice = {
      lower: 'expression option;
      upper: 'expression option;
      step: 'expression option;
    }
    [@@deriving compare, eq, sexp, show, hash]


    type 'expression subscript =
      | Index of 'expression
      | Slice of 'expression slice
    [@@deriving compare, eq, sexp, show, hash]


    type 'expression access =
      | Call of (('expression Argument.record) list) Node.t
      | Expression of 'expression
      | Identifier of Identifier.t
      | Subscript of ('expression subscript) list
    [@@deriving compare, eq, sexp, show, hash]


    type 'expression record = ('expression access) list
    [@@deriving compare, eq, sexp, show, hash]
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
    [@@deriving compare, eq, sexp, show, hash]


    type 'expression record = {
      left: 'expression;
      operator: operator;
      right: 'expression;
    }
    [@@deriving compare, eq, sexp, show, hash]


    let pp_binary_operator formatter operator =
      Format.fprintf
        formatter
        "%s"
        begin
          match operator with
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
          | Subtract -> "-"
        end
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
    [@@deriving compare, eq, sexp, show, hash]


    type 'expression record = {
      left: 'expression;
      right: (operator * 'expression) list;
    }
    [@@deriving compare, eq, sexp, show, hash]


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
        begin
          match operator with
          | Equals -> "="
          | GreaterThan -> ">"
          | GreaterThanOrEquals -> ">="
          | In -> "in"
          | Is -> "is"
          | IsNot -> "is not"
          | LessThan -> "<"
          | LessThanOrEquals -> "<="
          | NotEquals -> "!="
          | NotIn -> "not in"
        end
  end


  module UnaryOperator = struct
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


    let pp_unary_operator formatter operator =
      Format.fprintf
        formatter "%s"
        begin
          match operator with
          | Invert -> "~"
          | Negative -> "-"
          | Not -> "not"
          | Positive -> "+"
        end
  end
end


module Lambda = struct
  type 'expression t = {
    parameters: ('expression Parameter.t) list;
    body: 'expression;
  }
  [@@deriving compare, eq, sexp, show, hash]
end


module Ternary = struct
  type 'expression t = {
    target: 'expression;
    test: 'expression;
    alternative: 'expression;
  }
  [@@deriving compare, eq, sexp, show, hash]
end


module Dictionary = struct
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


module Comprehension = struct
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


module Starred = struct
  type 'expression t =
    | Once of 'expression
    | Twice of 'expression
  [@@deriving compare, eq, sexp, show, hash]
end


type expression =
  | Access of t Record.Access.record
  | Await of t
  | BinaryOperator of t Record.BinaryOperator.record
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


let equal_t = equal


let pp_t = pp


type expression_t = t
[@@deriving compare, eq, sexp, show, hash]


module Argument = struct
  include Record.Argument

  type t = expression_t Record.Argument.record
  [@@deriving compare, eq, sexp, show, hash]
end


module Access = struct
  include Record.Access

  type t = expression_t Record.Access.record
  [@@deriving compare, eq, sexp, show, hash]


  let rec show access =
    let identifier (element: expression_t Record.Access.access): string =
      match element with
      | Identifier identifier ->
          Identifier.show identifier
      | Call _ ->
          Format.asprintf "(...)"
      | _ ->
          "?" in
    List.map ~f:identifier access
    |> String.concat ~sep:"."


  let pp format access =
    Format.fprintf format "%s" (show access)


  let add_prefix ~prefix access =
    match access with
    | (Identifier head) :: tail ->
        Identifier (Identifier.add_prefix ~prefix head) :: tail
    | _ ->
        access


  let remove_prefix ~prefix access =
    match access with
    | (Identifier head) :: tail ->
        Identifier (Identifier.remove_prefix ~prefix head) :: tail
    | _ ->
        access


  let starts_with ~prefix access =
    String.is_prefix ~prefix (show access)


  let rec is_strict_prefix ~prefix access =
    match prefix, access with
    | [], _ :: _ ->
        true
    | prefix_head :: prefix, head :: access
      when equal [prefix_head] [head] ->
        is_strict_prefix ~prefix access
    | _ ->
        false


  let drop_prefix access ~prefix =
    let rec strip access prefix =
      match prefix, access with
      | prefix_head :: prefix_tail, head :: tail
        when equal [prefix_head] [head] ->
          strip tail prefix_tail
      | [], access ->
          Some access
      | _ ->
          None
    in
    strip access prefix
    |> Option.value ~default:access


  module Set = Set.Make(struct
      type nonrec t = t
      let compare = compare
      let sexp_of_t = sexp_of_t
      let t_of_sexp = t_of_sexp
    end)


  module Map = Map.Make(struct
      type nonrec t = t
      let compare = compare
      let sexp_of_t = sexp_of_t
      let t_of_sexp = t_of_sexp
    end)


  include Hashable.Make(struct
      type nonrec t = t
      let compare = compare
      let hash = Hashtbl.hash
      let hash_fold_t = hash_fold_t
      let sexp_of_t = sexp_of_t
      let t_of_sexp = t_of_sexp
    end)


  let create name =
    if String.equal name "..." then
      [Identifier (Identifier.create name)]
    else
      String.split ~on:'.' name
      |> List.map ~f:(fun name -> Identifier (Identifier.create name))


  let create_from_identifiers identifiers =
    List.map ~f:(fun identifier -> Identifier identifier) identifiers


  let create_from_expression ({ Node.value; _ } as expression) =
    match value with
    | Access access -> access
    | _ -> [Expression expression]


  let call ?(arguments = []) ~location ~name () =
    [Identifier (Identifier.create name); Call { Node.location; value = arguments }]


  let backup ~arguments ~name =
    match List.rev name with
    | (Identifier name) :: _ ->
        (* cf. https://docs.python.org/3/reference/datamodel.html#object.__radd__ *)
        begin
          match Identifier.show name with
          | "__add__" -> Some "__radd__"
          | "__sub__" -> Some "__rsub__"
          | "__mul__" -> Some "__rmul__"
          | "__matmul__" -> Some "__rmatmul__"
          | "__truediv__" -> Some "__rtruediv__"
          | "__floordiv__" -> Some "__rfloordiv__"
          | "__mod__" -> Some "__rmod__"
          | "__divmod__" -> Some "__rdivmod__"
          | "__pow__" -> Some "__rpow__"
          | "__lshift__" -> Some "__rlshift__"
          | "__rshift__" -> Some "__rrshift__"
          | "__and__" -> Some "__rand__"
          | "__xor__" -> Some "__rxor__"
          | "__or__" -> Some "__ror__"
          | _ -> None
        end
        >>| fun name -> List.rev arguments, [Identifier (Identifier.create name)]
    | _ ->
        None


  let redirect ~arguments ~location ~name =
    match name, arguments with
    | [Identifier name], [{ Argument.value; _ }] ->
        begin
          match Identifier.show name with
          | "abs" -> Some "__abs__"
          | "repr" -> Some "__repr__"
          | "str" -> Some "__str__"
          | _ -> None
        end
        >>| fun name ->
        (create_from_expression value) @ (call ~arguments:[] ~location ~name ())
    | _ -> None
end


let access =
  Access.create_from_expression


module BinaryOperator = struct
  include Record.BinaryOperator


  type t = expression_t Record.BinaryOperator.record
  [@@deriving compare, eq, sexp, show, hash]


  let override {
      left = ({ Node.location; _ } as left);
      operator;
      right;
    } =
    let name =
      match operator with
      | Add -> "__add__"
      | At -> "__matmul__"
      | BitAnd -> "__and__"
      | BitOr -> "__or__"
      | BitXor -> "__xor__"
      | Divide -> "__truediv__"
      | FloorDivide -> "__floordiv__"
      | LeftShift -> "__lshift__"
      | Modulo -> "__mod__"
      | Multiply -> "__mul__"
      | Power -> "__pow__"
      | RightShift -> "__rshift__"
      | Subtract -> "__sub__"
    in
    let arguments = [{ Argument.name = None; value = right }] in
    Access ((access left) @ (Access.call ~arguments ~location ~name ()))
    |> Node.create ~location
end


module ComparisonOperator = struct
  include Record.ComparisonOperator


  type t = expression_t Record.ComparisonOperator.record
  [@@deriving compare, eq, sexp, show, hash]


  let override { left; right } =
    let simple_override ({ Node.location; _ } as left) (operator, right) =
      begin
        match operator with
        | Equals -> Some "__eq__"
        | GreaterThan -> Some "__gt__"
        | GreaterThanOrEquals -> Some "__ge__"
        | In -> Some "__contains__"
        | Is
        | IsNot -> None
        | LessThan -> Some "__lt__"
        | LessThanOrEquals -> Some "__le__"
        | NotEquals -> Some "__ne__"
        | NotIn -> None
      end
      >>| fun name ->
      let arguments = [{ Argument.name = None; value = right }] in
      Access ((access left) @ Access.call ~arguments ~location ~name ())
      |> Node.create ~location
    in
    let rec collect left operands =
      match operands with
      | (operator, right) :: operands ->
          (simple_override left (operator, right)) :: (collect right operands)
      | [] -> []
    in
    collect left right
end


module UnaryOperator = struct
  include Record.UnaryOperator


  type t = expression_t Record.UnaryOperator.record
  [@@deriving compare, eq, sexp, show, hash]


  let override { operator; operand = ({ Node.location; _ } as operand) } =
    begin
      match operator with
      | Invert -> Some "__invert__"
      | Negative -> Some "__neg__"
      | Not -> None
      | Positive -> Some "__pos__"
    end
    >>| fun name ->
    Access ((access operand) @ (Access.call ~name ~location ()))
    |> Node.create ~location
end


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
    | BooleanOperator { BooleanOperator.operator; left; right } ->
        BooleanOperator {
          BooleanOperator.operator;
          left = normalize left;
          right = normalize right;
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
  let rec pp_expression_t formatter expression_t =
    match expression_t with
    | { Node.value = expression; _ } ->
        Format.fprintf formatter "%a" pp_expression expression


  and pp_slice formatter { Access.lower; upper; step } =
    match lower,upper,step with
    | Some lower,None,None ->
        Format.fprintf formatter "%a" pp_expression_t lower
    | Some lower,Some upper,None ->
        Format.fprintf
          formatter
          "%a:%a"
          pp_expression_t lower
          pp_expression_t upper
    | Some lower,Some upper,Some step ->
        Format.fprintf formatter "%a:%a:%a"
          pp_expression_t lower
          pp_expression_t upper
          pp_expression_t step
    | Some lower, None, Some step ->
        Format.fprintf
          formatter
          "%a::%a"
          pp_expression_t lower
          pp_expression_t step
    | None,Some upper,None ->
        Format.fprintf formatter ":%a" pp_expression_t upper
    | None,None,Some step ->
        Format.fprintf formatter "::%a" pp_expression_t step
    | None,Some upper,Some step ->
        Format.fprintf
          formatter
          ":%a:%a"
          pp_expression_t upper
          pp_expression_t step
    | None,None,None -> ()


  and pp_subscript formatter subscript =
    match subscript with
    | Access.Index index ->
        Format.fprintf formatter "%a" pp_expression_t index
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
    | Access.Call { Node.value = arguments; _ } ->
        Format.fprintf
          formatter
          "(%a)"
          pp_argument_list arguments
    | Access.Expression expression ->
        Format.fprintf formatter "%a" pp_expression_t expression
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
          (Node.value name |> Identifier.show)
          pp_expression_t value
    | None ->
        Format.fprintf formatter "%a" pp_expression_t value


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
          pp_expression_t expression
    | (operator,expression) :: comparison_list ->
        Format.fprintf
          formatter
          "%a %a %a"
          ComparisonOperator.pp_comparison_operator operator
          pp_expression_t expression
          pp_comparison_list comparison_list


  and pp_dictionary_entry formatter { Dictionary.key; value } =
    Format.fprintf
      formatter
      "%a:%a"
      pp_expression_t key
      pp_expression_t value


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
        Format.fprintf formatter "%a" pp_expression_t expression
    | expression :: expression_list ->
        Format.fprintf
          formatter
          "%a, %a"
          pp_expression_t expression
          pp_expression_list expression_list


  and pp_generator formatter { Comprehension.target; iterator; conditions; async } =
    Format.fprintf
      formatter
      "generator(%s%a in %a if %a)"
      (if async then "async " else "")
      pp_expression_t target
      pp_expression_t iterator
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
          pp_expression_t annotation
          pp_expression_t expression
    | None, Some annotation ->
        Format.fprintf
          formatter
          "%s: %a"
          identifier
          pp_expression_t annotation
    | Some expression, None ->
        Format.fprintf
          formatter
          "%s=%a"
          identifier
          pp_expression_t expression
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
      pp_expression_t element
      pp_generators generators


  and pp_starred formatter starred =
    match starred with
    | Starred.Once expression ->
        Format.fprintf
          formatter
          "%a"
          pp_expression_t expression
    | Starred.Twice expression ->
        Format.fprintf
          formatter
          "%a"
          pp_expression_t expression


  and pp_ternary formatter { Ternary.target; test; alternative } =
    Format.fprintf
      formatter
      "%a if %a else %a"
      pp_expression_t target
      pp_expression_t test
      pp_expression_t alternative


  and pp_expression formatter expression =
    match expression with
    | Access access_list ->
        pp_access_list formatter access_list

    | Await expression ->
        Format.fprintf
          formatter
          "await %a"
          pp_expression_t expression

    | BinaryOperator { BinaryOperator.left; operator; right } ->
        Format.fprintf
          formatter
          "%a %a %a"
          pp_expression_t left
          BinaryOperator.pp_binary_operator operator
          pp_expression_t right

    | BooleanOperator { BooleanOperator.left; operator; right } ->
        Format.fprintf
          formatter
          "%a %a %a"
          pp_expression_t left
          BooleanOperator.pp_boolean_operator operator
          pp_expression_t right

    | Bytes string
    | String string
    | Format string ->
        Format.fprintf formatter "%S" string

    | ComparisonOperator { ComparisonOperator.left; right } ->
        Format.fprintf
          formatter
          "%a %a"
          pp_expression_t left
          pp_comparison_list right

    | Float float_value

    | Complex float_value -> Format.fprintf formatter "%f" float_value

    | Dictionary { Dictionary.entries; keywords } ->
        let pp_keywords format = function
          | Some keywords ->
              Format.fprintf format ", %a" pp_expression_t keywords
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
          pp_expression_t body

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
          pp_expression_t operand

    | Yield yield ->
        match yield with
        | Some yield ->
            Format.fprintf
              formatter
              "yield %a"
              pp_expression_t yield
        | None ->
            Format.fprintf formatter "%s" "yield"

  let pp = pp_expression_t
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
