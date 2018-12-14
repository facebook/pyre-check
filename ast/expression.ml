(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open Sexplib.Std

open Pyre


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
    type 'expression access =
      | Call of (('expression Argument.record) list) Node.t
      | Expression of 'expression
      | Identifier of Identifier.t
    [@@deriving compare, eq, sexp, show, hash]


    type 'expression record = ('expression access) list
    [@@deriving compare, eq, sexp, show, hash]
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
      operator: operator;
      right: 'expression;
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
    keywords: 'expression list;
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


module StringLiteral = struct
  type 'expression kind =
    | String
    | Bytes
    | Format of 'expression list


  and 'expression t = {
    value: string;
    kind: 'expression kind;
  }
  [@@deriving compare, eq, sexp, show, hash]


  let create ?(bytes = false) ?expressions value =
    let kind =
      if bytes then
        Bytes
      else
        match expressions with
        | Some expressions -> Format expressions
        | _ -> String
    in
    { value; kind }
end


type expression =
  | Access of t Record.Access.record
  | Await of t
  | BooleanOperator of t BooleanOperator.t
  | ComparisonOperator of t Record.ComparisonOperator.record
  | Complex of float
  | Dictionary of t Dictionary.t
  | DictionaryComprehension of ((t Dictionary.entry), t) Comprehension.t
  | Ellipses
  | False
  | Float of float
  | Generator of (t, t) Comprehension.t
  | Integer of int
  | Lambda of t Lambda.t
  | List of t list
  | ListComprehension of (t, t) Comprehension.t
  | Set of t list
  | SetComprehension of (t, t) Comprehension.t
  | Starred of t Starred.t
  | String of t StringLiteral.t
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


  module SerializableMap = SerializableMap.Make(struct
      type nonrec t = t
      let compare = compare
    end)


  include Hashable.Make(struct
      type nonrec t = t
      let compare = compare
      let hash = hash
      let hash_fold_t = hash_fold_t
      let sexp_of_t = sexp_of_t
      let t_of_sexp = t_of_sexp
    end)


  let create_from_identifiers identifiers =
    List.map identifiers ~f:(fun identifier -> Identifier identifier)


  let create_from_expression ({ Node.value; _ } as expression) =
    match value with
    | Access access -> access
    | _ -> [Expression expression]


  let create name =
    let identifier_names name =
      if String.equal name "..." then
        [name]
      else
        String.split ~on:'.' name
    in
    identifier_names name
    |> List.map ~f:Identifier.create
    |> create_from_identifiers


  let pp format access =
    let identifier (element: expression_t Record.Access.access): string =
      match element with
      | Identifier identifier ->
          Identifier.show identifier
      | Call _ ->
          Format.asprintf "(...)"
      | _ ->
          "?"
    in
    List.map access ~f:identifier
    |> String.concat ~sep:"."
    |> Format.fprintf format "%s"


  let show access =
    Format.asprintf "%a" pp access


  let expression ?location access =
    let location = Option.value location ~default:Location.Reference.any in
    Access access
    |> Node.create ~location


  let sanitized access =
    let sanitized element =
      match element with
      | Identifier identifier -> Identifier (Identifier.sanitized identifier)
      | _ -> element
    in
    List.map access ~f:sanitized


  let pp_sanitized format access =
    let identifier (element: expression_t Record.Access.access): string =
      match element with
      | Identifier identifier -> Identifier.show identifier
      | Call _ -> Format.asprintf "(...)"
      | _ -> "?"
    in
    sanitized access
    |> List.map ~f:identifier
    |> String.concat ~sep:"."
    |> Format.fprintf format "%s"


  let show_sanitized access =
    Format.asprintf "%a" pp_sanitized access


  let pp format access =
    Format.fprintf format "%s" (show access)


  let local_qualifier_pattern = Str.regexp "^\\$local_\\([a-zA-Z_0-9\\?]+\\)\\$"


  let delocalize access =
    match access with
    | (Identifier identifier) :: tail
      when Identifier.show identifier |> String.is_prefix ~prefix:"$local_" ->
        let qualifier =
          let name = Identifier.show identifier in
          if Str.string_match local_qualifier_pattern name 0 then
            Str.matched_group 1 name
            |> String.substr_replace_all ~pattern:"?" ~with_:"."
            |> create
          else
            begin
              Log.debug "Unable to extract qualifier from %s" name;
              []
            end
        in
        let identifier =
          Identifier.show_sanitized identifier
          |> Identifier.create
        in
        qualifier @ [Identifier identifier] @ tail
    | _ ->
        access


  let delocalize_qualified access =
    List.rev access
    |> (function
        | (Identifier identifier) :: tail -> (Identifier (Identifier.sanitized identifier)) :: tail
        | access -> access)
    |> List.rev


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


  let prefix access =
    match List.rev access with
    | _head :: prefix_reversed ->
        List.rev prefix_reversed
    | _ ->
        []


  let last access =
    List.last access


  let call ?(arguments = []) ~location ~name () =
    [Identifier (Identifier.create name); Call { Node.location; value = arguments }]

  type call = {
    callee: string;
    arguments: Argument.t list;
  }

  let name_and_arguments ~call =
    let is_identifier = function
      | Identifier _ ->
          true
      | _ ->
          false
    in
    match List.split_while ~f:is_identifier call with
    | identifiers, [Call { Node.value = arguments; _ }] ->
        Some { callee = show identifiers; arguments }
    | _ ->
        None


  let backup ~name =
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
        >>| fun name -> [Identifier (Identifier.create name)]
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

  let is_assert_function access =
    List.take_while access ~f:(function | Identifier _ -> true | _ -> false)
    |> show
    |> Core.Set.mem Recognized.assert_functions
end


let access =
  Access.create_from_expression


let rec delocalize ({ Node.value; _ } as expression) =
  let value =
    match value with
    | Access access ->
        let access =
          let delocalize_element = function
            | Access.Call ({ Node.value = arguments; _ } as call) ->
                let delocalize_argument ({ Argument.value; _ } as argument) =
                  { argument with Argument.value = delocalize value }
                in
                Access.Call { call with Node.value = List.map arguments ~f:delocalize_argument }
            | Access.Expression expression ->
                Access.Expression (delocalize expression)
            | element ->
                element
          in
          Access.delocalize access
          |> List.map ~f:delocalize_element
        in
        Access access
    | List elements ->
        List (List.map elements ~f:delocalize)
    | Tuple elements ->
        Tuple (List.map elements ~f:delocalize)
    | _ ->
        value
  in
  { expression with Node.value }


let delocalize_qualified ({ Node.value; _ } as expression) =
  let value =
    match value with
    | Access access -> Access (Access.delocalize_qualified access)
    | _ -> value
  in
  { expression with Node.value }



module ComparisonOperator = struct
  include Record.ComparisonOperator


  type t = expression_t Record.ComparisonOperator.record
  [@@deriving compare, eq, sexp, show, hash]


  let override { left = { Node.location; _ } as left; operator; right } =
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
      | IsNot -> None
      | LessThan -> Some "__lt__"
      | LessThanOrEquals -> Some "__le__"
      | NotEquals -> Some "__ne__"
      | In -> None
      | NotIn -> None
    in
    operator
    >>| fun name ->
    let arguments = [{ Argument.name = None; value = right }] in
    Access ((access left) @ Access.call ~arguments ~location ~name ())
    |> Node.create ~location
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


let negate ({ Node.location; value } as node) =
  match value with
  | UnaryOperator {
      UnaryOperator.operator = UnaryOperator.Not;
      operand;
    } ->
      operand
  | ComparisonOperator {
      ComparisonOperator.operator = ComparisonOperator.IsNot;
      left;
      right;
    } ->
      {
        Node.location;
        value = ComparisonOperator {
            ComparisonOperator.operator = ComparisonOperator.Is;
            left;
            right;
          };
      }
  | ComparisonOperator {
      ComparisonOperator.operator = ComparisonOperator.Is;
      left;
      right;
    } ->
      {
        Node.location;
        value = ComparisonOperator {
            ComparisonOperator.operator = ComparisonOperator.IsNot;
            left;
            right;
          };
      }
  | _ ->
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
    | (UnaryOperator {
        UnaryOperator.operator = UnaryOperator.Not;
        operand = { Node.value; _ };
      } as unary) ->
        begin
          match value with
          | ComparisonOperator {
              ComparisonOperator.left;
              operator;
              right;
            } ->
              ComparisonOperator {
                ComparisonOperator.left;
                operator = ComparisonOperator.inverse operator;
                right;
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
              unary
        end
    | _ ->
        value
  in
  { Node.location; value = normalized }


let exists_in_list ~expression_list target_string =
  let rec matches expected actual =
    match expected, delocalize_qualified actual with
    | (expected :: expected_tail),
      { Node.location; value = Access ((Access.Identifier identifier) :: identifiers) }
      when Identifier.show identifier = expected ->
        if List.is_empty expected_tail && List.is_empty identifiers then
          true
        else
          matches expected_tail { Node.location; value = Access identifiers }
    | _ ->
        false
  in
  List.exists ~f:(matches (String.split ~on:'.' target_string)) expression_list


module PrettyPrinter = struct
  let rec pp_expression_t formatter expression_t =
    match expression_t with
    | { Node.value = expression; _ } ->
        Format.fprintf formatter "%a" pp_expression expression


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


  and pp_access_list formatter access_list =
    match access_list with
    | [] -> ()
    | access :: [] -> Format.fprintf formatter "%a" pp_access access
    | access
      :: Access.Identifier identifier
      :: Access.Call { Node.value = arguments; _ }
      :: access_list
      when (Identifier.equal identifier (Identifier.create "__getitem__")) ->
        Format.fprintf
          formatter
          (if List.is_empty access_list then "%a[%a]%a" else "%a[%a].%a")
          pp_access access
          pp_argument_list arguments
          pp_access_list access_list
    | access :: (((Access.Call _) :: _) as access_list) ->
        Format.fprintf
          formatter
          "%a%a"
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

  and pp_keywords formatter keywords =
    match keywords with
    | [] -> ()
    | keyword :: [] -> Format.fprintf formatter ", %a" pp_expression_t keyword
    | keyword :: keywords ->
        Format.fprintf
          formatter
          ", %a%a"
          pp_expression_t keyword
          pp_keywords keywords

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
          "*%a"
          pp_expression_t expression
    | Starred.Twice expression ->
        Format.fprintf
          formatter
          "**%a"
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

    | BooleanOperator { BooleanOperator.left; operator; right } ->
        Format.fprintf
          formatter
          "%a %a %a"
          pp_expression_t left
          BooleanOperator.pp_boolean_operator operator
          pp_expression_t right

    | String { StringLiteral.value; kind } ->
        let bytes =
          match kind with
          | StringLiteral.Bytes -> "b"
          | _ -> ""
        in
        begin
          match kind with
          | StringLiteral.Format expressions ->
              Format.fprintf formatter "%s\"%s\"(%a)" bytes value pp_expression_list expressions
          | _ ->
              Format.fprintf formatter "%s\"%s\"" bytes value
        end
    | ComparisonOperator { ComparisonOperator.left; operator; right } ->
        Format.fprintf
          formatter
          "%a %a %a"
          pp_expression_t left
          ComparisonOperator.pp_comparison_operator operator
          pp_expression_t right

    | Ellipses ->
        Format.fprintf formatter "..."

    | Float float_value

    | Complex float_value -> Format.fprintf formatter "%f" float_value

    | Dictionary { Dictionary.entries; keywords } ->
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
            Format.fprintf formatter "%a" pp_expression_t yield
        | None ->
            Format.fprintf formatter "None"

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
