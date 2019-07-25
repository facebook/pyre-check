(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Sexplib.Std
open Pyre

module BooleanOperator = struct
  type operator =
    | And
    | Or
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  type 'expression t = {
    left: 'expression;
    operator: operator;
    right: 'expression;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  let pp_boolean_operator formatter operator =
    Format.fprintf
      formatter
      "%s"
      ( match operator with
      | And -> "and"
      | Or -> "or" )


  let inverse = function
    | And -> Or
    | Or -> And
end

module Record = struct
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
    [@@deriving compare, eq, sexp, show, hash, to_yojson]

    type 'expression record = {
      left: 'expression;
      operator: operator;
      right: 'expression;
    }
    [@@deriving compare, eq, sexp, show, hash, to_yojson]

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
        ( match operator with
        | Equals -> "="
        | GreaterThan -> ">"
        | GreaterThanOrEquals -> ">="
        | In -> "in"
        | Is -> "is"
        | IsNot -> "is not"
        | LessThan -> "<"
        | LessThanOrEquals -> "<="
        | NotEquals -> "!="
        | NotIn -> "not in" )
  end

  module UnaryOperator = struct
    type operator =
      | Invert
      | Negative
      | Not
      | Positive
    [@@deriving compare, eq, sexp, show, hash, to_yojson]

    type 'expression record = {
      operator: operator;
      operand: 'expression;
    }
    [@@deriving compare, eq, sexp, show, hash, to_yojson]

    let pp_unary_operator formatter operator =
      Format.fprintf
        formatter
        "%s"
        ( match operator with
        | Invert -> "~"
        | Negative -> "-"
        | Not -> "not"
        | Positive -> "+" )
  end
end

module Name = struct
  module Attribute = struct
    type 'expression t = {
      base: 'expression;
      attribute: Identifier.t;
      special: bool;
    }
    [@@deriving compare, eq, sexp, show, hash, to_yojson]
  end

  type 'expression t =
    | Attribute of 'expression Attribute.t
    | Identifier of Identifier.t
  [@@deriving compare, eq, sexp, show, hash, to_yojson]
end

module Call = struct
  module Argument = struct
    type 'expression t = {
      name: Identifier.t Node.t option;
      value: 'expression;
    }
    [@@deriving compare, eq, sexp, show, hash, to_yojson]
  end

  type 'expression t = {
    callee: 'expression;
    arguments: 'expression Argument.t list;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]
end

module Lambda = struct
  type 'expression t = {
    parameters: 'expression Parameter.t list;
    body: 'expression;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]
end

module Ternary = struct
  type 'expression t = {
    target: 'expression;
    test: 'expression;
    alternative: 'expression;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]
end

module Dictionary = struct
  type 'expression entry = {
    key: 'expression;
    value: 'expression;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  type 'expression t = {
    entries: 'expression entry list;
    keywords: 'expression list;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]
end

module Comprehension = struct
  type 'expression generator = {
    target: 'expression;
    iterator: 'expression;
    conditions: 'expression list;
    async: bool;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  type ('element, 'expression) t = {
    element: 'element;
    generators: 'expression generator list;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]
end

module Starred = struct
  type 'expression t =
    | Once of 'expression
    | Twice of 'expression
  [@@deriving compare, eq, sexp, show, hash, to_yojson]
end

module StringLiteral = struct
  module Substring = struct
    type kind =
      | Literal
      | Format
    [@@deriving compare, eq, sexp, show, hash, to_yojson]

    type t = {
      value: string;
      kind: kind;
    }
    [@@deriving compare, eq, sexp, show, hash, to_yojson]

    let is_all_literal =
      List.for_all ~f:(fun { Node.value = { kind; _ }; _ } -> equal_kind kind Literal)
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
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

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


  let create_mixed pieces =
    (* Default to literal string so subsequent pre-processing logic can be simplier. *)
    match pieces with
    | [] -> { value = ""; kind = String }
    | [{ Node.value = { Substring.kind = Literal; value }; _ }] -> { value; kind = String }
    | _ ->
        let value =
          pieces
          |> List.map ~f:(fun { Node.value = { Substring.value; _ }; _ } -> value)
          |> String.concat ~sep:""
        in
        if Substring.is_all_literal pieces then
          { value; kind = String }
        else
          { value; kind = Mixed pieces }
end

type expression =
  | Await of t
  | BooleanOperator of t BooleanOperator.t
  | Call of t Call.t
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

let _ = show (* shadowed below *)

type expression_t = t [@@deriving compare, eq, sexp, show, hash, to_yojson]

module ComparisonOperator = struct
  include Record.ComparisonOperator

  type t = expression_t Record.ComparisonOperator.record [@@deriving compare, eq, sexp, show, hash]

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
    Call
      {
        callee =
          {
            Node.location;
            value = Name (Name.Attribute { base = left; attribute = name; special = true });
          };
        arguments;
      }
    |> Node.create ~location
end

module UnaryOperator = struct
  include Record.UnaryOperator

  type t = expression_t Record.UnaryOperator.record [@@deriving compare, eq, sexp, show, hash]

  let override { operator; operand = { Node.location; _ } as operand } =
    ( match operator with
    | Invert -> Some "__invert__"
    | Negative -> Some "__neg__"
    | Not -> None
    | Positive -> Some "__pos__" )
    >>| fun name ->
    Call
      {
        callee =
          {
            Node.location;
            value = Name (Name.Attribute { base = operand; attribute = name; special = true });
          };
        arguments = [];
      }
    |> Node.create ~location
end

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
          ComparisonOperator
            { ComparisonOperator.operator = ComparisonOperator.IsNot; left; right };
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
        BooleanOperator
          { BooleanOperator.operator; left = normalize left; right = normalize right }
    | UnaryOperator { UnaryOperator.operator = UnaryOperator.Not; operand = { Node.value; _ } } as
      unary -> (
      match value with
      | ComparisonOperator { ComparisonOperator.left; operator; right } ->
          ComparisonOperator
            { ComparisonOperator.left; operator = ComparisonOperator.inverse operator; right }
      | False -> True
      | True -> False
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
      | _ -> unary )
    | _ -> value
  in
  { Node.location; value = normalized }


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
  create_name_from_reference ~location reference
  |> (fun name -> Name name)
  |> Node.create ~location


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
      | None -> None )
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
           (Name.show pp name))


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
      let sanitize_argument ({ Call.Argument.name; _ } as argument) =
        let name =
          match name with
          | Some { Node.value; location } ->
              Some { Node.value = Identifier.sanitized value; location }
          | None -> None
        in
        { argument with Call.Argument.name }
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
    | Name (Name.Identifier identifier) when identifier |> String.is_prefix ~prefix:"$local_" ->
        let sanitized = Identifier.sanitized identifier in
        let local_qualifier_pattern = Str.regexp "^\\$local_\\([a-zA-Z_0-9\\?]+\\)\\$" in
        if Str.string_match local_qualifier_pattern identifier 0 then
          let qualifier =
            Str.matched_group 1 identifier
            |> String.substr_replace_all ~pattern:"?" ~with_:"."
            |> create_name ~location
            |> fun name -> Name name |> Node.create ~location
          in
          Name (Name.Attribute { base = qualifier; attribute = sanitized; special = false })
        else (
          Log.debug "Unable to extract qualifier from %s" identifier;
          Name (Name.Identifier sanitized) )
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
    { Call.callee = { Node.location = { Location.stop = callee_end; path; _ }; _ }; arguments }
  =
  match List.rev arguments with
  | [] ->
      {
        Location.path;
        start = callee_end;
        stop = { callee_end with column = callee_end.Location.column + 2 };
      }
  | { Call.Argument.value = { Node.location = { Location.stop; _ }; _ }; _ } :: _ ->
      { Location.path; start = callee_end; stop = { stop with column = stop.Location.column + 1 } }


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
        Format.fprintf formatter "%a,%a" pp_argument argument pp_argument_list argument_list


  and pp_dictionary_entry formatter { Dictionary.key; value } =
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


  and pp_generator formatter { Comprehension.target; iterator; conditions; async } =
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
      | _ -> Format.fprintf formatter "%a(%a)" pp_expression_t callee pp_argument_list arguments )
    | String { StringLiteral.value; kind } -> (
        let bytes =
          match kind with
          | StringLiteral.Bytes -> "b"
          | _ -> ""
        in
        match kind with
        | StringLiteral.Format expressions ->
            Format.fprintf formatter "%s\"%s\"(%a)" bytes value pp_expression_list expressions
        | _ -> Format.fprintf formatter "%s\"%s\"" bytes value )
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
    | Ellipsis -> Format.fprintf formatter "..."
    | Float float_value
    | Complex float_value ->
        Format.fprintf formatter "%f" float_value
    | Dictionary { Dictionary.entries; keywords } ->
        Format.fprintf formatter "Dictionary { %a%a }" pp_dictionary entries pp_keywords keywords
    | DictionaryComprehension { Comprehension.element; generators } ->
        Format.fprintf
          formatter
          "Dictionary comprehension { %a: %a }"
          pp_dictionary_entry
          element
          pp_generators
          generators
    | False -> Format.fprintf formatter "%s" "False"
    | Generator generator -> Format.fprintf formatter "%a" pp_basic_comprehension generator
    | Integer integer -> Format.fprintf formatter "%d" integer
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
    | True -> Format.fprintf formatter "%s" "True"
    | Tuple tuple -> Format.fprintf formatter "(%a)" pp_expression_list tuple
    | UnaryOperator { UnaryOperator.operator; operand } ->
        Format.fprintf
          formatter
          "%a %a"
          UnaryOperator.pp_unary_operator
          operator
          pp_expression_t
          operand
    | Yield yield -> (
      match yield with
      | Some yield -> Format.fprintf formatter "%a" pp_expression_t yield
      | None -> Format.fprintf formatter "None" )


  let pp = pp_expression_t
end

let pp formatter expression = Format.fprintf formatter "%a" PrettyPrinter.pp expression

let show expression = Format.asprintf "%a" pp expression

let rec show_sanitized { Node.location; value } =
  match value with
  | Name (Name.Identifier identifier) -> Identifier.sanitized identifier
  | Name (Name.Attribute { base; attribute; _ }) ->
      Format.asprintf "%s.%s" (show_sanitized base) (Identifier.sanitized attribute)
  | Call { callee; arguments } ->
      let arguments =
        let show_argument { Call.Argument.name; value } =
          let value = show_sanitized value in
          match name with
          | None -> value
          | Some { Node.value = name; _ } -> Format.sprintf "%s = %s" name value
        in
        arguments |> List.map ~f:show_argument |> String.concat ~sep:", "
      in
      Format.asprintf "%s(%s)" (show_sanitized callee) arguments
  | Tuple items ->
      List.map items ~f:show_sanitized
      |> String.concat ~sep:", "
      |> fun tuple_body -> Format.sprintf "(%s)" tuple_body
  | _ -> show { Node.location; value }


let pp_sanitized formatter expression = show_sanitized expression |> Format.fprintf formatter "%s"

let pp_expression_list formatter expression_list =
  Format.fprintf formatter "%a" PrettyPrinter.pp_expression_list expression_list


let pp_expression_argument_list formatter expression_argument_list =
  Format.fprintf formatter "%a" PrettyPrinter.pp_argument_list expression_argument_list


let pp_expression_parameter_list formatter expression_parameter_list =
  Format.fprintf formatter "%a" PrettyPrinter.pp_parameter_list expression_parameter_list
