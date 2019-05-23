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
      | Identifier of Identifier.t
    [@@deriving compare, eq, sexp, show, hash]


    type 'expression record = ('expression access) list
    [@@deriving compare, eq, sexp, show, hash]


    type 'expression general_access_record =
      | SimpleAccess of 'expression record
      | ExpressionAccess of { expression: 'expression; access: 'expression record }
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


module Name = struct
  module Attribute = struct
    type 'expression t = {
      base: 'expression;
      attribute: Identifier.t;
    }
    [@@deriving compare, eq, sexp, show, hash]
  end

  type 'expression t =
    | Attribute of 'expression Attribute.t
    | Identifier of Identifier.t
  [@@deriving compare, eq, sexp, show, hash]
end


module Call = struct
  module Argument = struct
    type 'expression t = {
      name: (Identifier.t Node.t) option;
      value: 'expression;
    }
    [@@deriving compare, eq, sexp, show, hash]
  end

  type 'expression t = {
    callee: 'expression;
    arguments: ('expression Argument.t) list;
  }
  [@@deriving compare, eq, sexp, show, hash]
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
  module Substring = struct
    type kind =
      | Literal
      | Format
    [@@deriving compare, eq, sexp, show, hash]

    type t = { value: string; kind: kind }
    [@@deriving compare, eq, sexp, show, hash]

    let is_all_literal = List.for_all
        ~f:(fun { kind; _ } -> equal_kind kind Literal)
  end

  type 'expression kind =
    | String
    | Bytes
    | Format of 'expression list
    | Mixed of Substring.t list


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

  let create_mixed pieces =
    (* Default to literal string so subsequent pre-processing logic can be simplier. *)
    match pieces with
    | [] ->
        { value = ""; kind = String }
    | [{ Substring.kind = Literal; value }] ->
        { value; kind = String }
    | _ ->
        let value =
          pieces
          |> List.map ~f:(fun { Substring.value; _ } -> value)
          |> String.concat ~sep:""
        in
        if Substring.is_all_literal pieces then
          { value; kind = String }
        else
          { value; kind = Mixed pieces }
end


type expression =
  | Access of t Record.Access.general_access_record
  | Await of t
  | BooleanOperator of t BooleanOperator.t
  | Call of t Call.t
  | ComparisonOperator of t Record.ComparisonOperator.record
  | Complex of float
  | Dictionary of t Dictionary.t
  | DictionaryComprehension of ((t Dictionary.entry), t) Comprehension.t
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


and t = expression Node.t
[@@deriving compare, eq, sexp, show, hash]


let _ = show  (* shadowed below *)


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
  [@@deriving compare, eq, sexp, hash]

  type general_access = expression_t Record.Access.general_access_record
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


  let create name =
    let identifier_names name =
      if String.equal name "..." then
        [name]
      else
        String.split ~on:'.' name
    in
    identifier_names name
    |> create_from_identifiers


  let pp format access =
    let identifier (element: expression_t Record.Access.access): string =
      match element with
      | Identifier identifier ->
          identifier
      | Call _ ->
          Format.asprintf "(...)"
    in
    List.map access ~f:identifier
    |> String.concat ~sep:"."
    |> Format.fprintf format "%s"


  let show access =
    Format.asprintf "%a" pp access


  let expression ?(location = Location.Reference.any) access =
    Access (SimpleAccess access)
    |> Node.create ~location


  let sanitized access =
    let sanitized element =
      match element with
      | Identifier identifier ->
          Identifier (Identifier.sanitized identifier)
      | Call arguments ->
          let sanitize_argument ({ Argument.name; _ } as argument)=
            {
              argument with
              Argument.name = name >>| Node.map ~f:Identifier.sanitized;
            }
          in
          Call (Node.map arguments ~f:(List.map ~f:sanitize_argument))
    in
    List.map access ~f:sanitized


  let equal_sanitized left_access right_access =
    equal (sanitized left_access) (sanitized right_access)


  let pp_sanitized format access =
    let identifier (element: expression_t Record.Access.access): string =
      match element with
      | Identifier identifier -> identifier
      | Call _ -> Format.asprintf "(...)"
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
      when identifier |> String.is_prefix ~prefix:"$local_" ->
        let qualifier =
          let name = identifier in
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
          Identifier.sanitized identifier

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
    [Identifier name; Call { Node.location; value = arguments }]

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
          match name with
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
    | _ ->
        None


  let combine { Node.location; value } access =
    match value with
    | Access (SimpleAccess head) ->
        SimpleAccess (head @ access)
    | Access (ExpressionAccess { expression; access = head }) ->
        ExpressionAccess { expression; access = head @ access }
    | _ ->
        ExpressionAccess { expression = { Node.location; value }; access }


  let redirect ~arguments ~location ~name =
    match name, arguments with
    | [Identifier name], [{ Argument.value; _ }] ->
        begin
          match name with
          | "abs" -> Some "__abs__"
          | "repr" -> Some "__repr__"
          | "str" -> Some "__str__"
          | _ -> None
        end
        >>| fun name ->
        combine value (call ~arguments:[] ~location ~name ())
    | _ -> None


  let is_assert_function access =
    List.take_while access ~f:(function | Identifier _ -> true | _ -> false)
    |> show
    |> Core.Set.mem Recognized.assert_functions
end


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
    let arguments = [{ Call.Argument.name = None; value = right }] in
    Call {
      callee = { Node.location; value = Name (Name.Attribute { base = left; attribute = name })};
      arguments;
    }
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
    >>| (fun name ->
      Call {
        callee = {
          Node.location;
          value = Name (Name.Attribute { base = operand; attribute = name });
        };
        arguments = [];
      }
      |> Node.create ~location
    )
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


let rec convert { Node.location; value } =
  (* Can't use `Visit` module due to circularity :( *)
  let rec split expression =
    let convert_argument { Call.Argument.name; value } =
      { Argument.name; value = convert value }
    in
    let convert_generator { Comprehension.target; iterator; conditions; async } =
      {
        Comprehension.target = convert target;
        iterator = convert iterator;
        conditions = List.map ~f:convert conditions;
        async;
      }
    in
    let convert_entry { Dictionary.key; value } =
      { Dictionary.key = convert key; value = convert value }
    in
    let convert_parameter { Node.value = { Parameter.value; annotation; name }; location } =
      let value =
        {
          Parameter.name;
          value = value >>| convert;
          annotation = annotation >>| convert;
        }
      in
      { Node.location; value }
    in
    match expression with
    | Access (SimpleAccess access) ->
        None, List.rev access
    | Access (ExpressionAccess { expression; access }) ->
        Some (convert expression |> Node.value), List.rev access
    | Name (Name.Identifier identifier) ->
        None, [Access.Identifier identifier]
    | Name (
        Name.Attribute {
          base = { Node.value = Name (Name.Identifier base); _ };
          attribute;
        }
      ) ->
        None, [Access.Identifier attribute; Access.Identifier base]
    | Name (Name.Attribute { base; attribute }) ->
        let base_expression, access = split (Node.value base) in
        base_expression, (Access.Identifier attribute) :: access
    | Call { callee = { Node.value = Name (Name.Identifier callee); location }; arguments } ->
        let arguments =
          { Node.location; value = List.map ~f:convert_argument arguments }
        in
        None, [Call arguments; Access.Identifier callee]
    | Call { callee; arguments } ->
        let base_expression, access = split (Node.value callee) in
        let arguments =
          { Node.location = callee.Node.location; value = List.map ~f:convert_argument arguments }
        in
        base_expression, (Call arguments) :: access
    | Await expression ->
        Await (convert expression) |> Option.some, []
    | BooleanOperator { BooleanOperator.left; right; operator } ->
        BooleanOperator {
          BooleanOperator.left = convert left;
          right = convert right;
          operator
        } |> Option.some,
        []
    | ComparisonOperator { ComparisonOperator.left; right; operator } ->
        ComparisonOperator {
          ComparisonOperator.left = convert left;
          right = convert right;
          operator
        } |> Option.some,
        []
    | Dictionary { Dictionary.entries; keywords } ->
        Dictionary {
          Dictionary.entries = List.map ~f:convert_entry entries;
          keywords = List.map ~f:convert keywords;
        } |> Option.some,
        []
    | DictionaryComprehension { Comprehension.element; generators } ->
        DictionaryComprehension {
          Comprehension.element = convert_entry element;
          generators = List.map ~f:convert_generator generators
        } |> Option.some,
        []
    | Generator { Comprehension.element; generators } ->
        Generator {
          Comprehension.element = convert element;
          generators = List.map ~f:convert_generator generators;
        } |> Option.some,
        []
    | Lambda { Lambda.parameters; body } ->
        Lambda {
          Lambda.parameters = List.map ~f:convert_parameter parameters;
          body = convert body;
        } |> Option.some,
        []
    | List elements ->
        List (List.map ~f:convert elements) |> Option.some, []
    | ListComprehension { Comprehension.element; generators } ->
        ListComprehension {
          Comprehension.element = convert element;
          generators = List.map ~f:convert_generator generators;
        } |> Option.some,
        []
    | Set elements ->
        Set (List.map ~f:convert elements) |> Option.some, []
    | SetComprehension { Comprehension.element; generators } ->
        SetComprehension {
          Comprehension.element = convert element;
          generators = List.map ~f:convert_generator generators;
        } |> Option.some,
        []
    | Starred (Starred.Once expression) ->
        Starred (Starred.Once (convert expression)) |> Option.some, []
    | Starred (Starred.Twice expression) ->
        Starred (Starred.Twice (convert expression)) |> Option.some, []
    | String { StringLiteral.value; kind } ->
        let kind =
          match kind with
          | StringLiteral.Format expressions ->
              StringLiteral.Format (List.map expressions ~f:convert)
          | _ ->
              kind
        in
        String { StringLiteral.value; kind } |> Option.some, []
    | Ternary { Ternary.target; test; alternative } ->
        Ternary {
          Ternary.target = convert target;
          test = convert test;
          alternative = convert alternative;
        } |> Option.some,
        []
    | Tuple elements ->
        Tuple (List.map ~f:convert elements) |> Option.some, []
    | UnaryOperator { UnaryOperator.operand; operator } ->
        UnaryOperator {
          UnaryOperator.operand = convert operand;
          operator;
        } |> Option.some,
        []
    | Yield expression ->
        Yield (expression >>| convert) |> Option.some, []
    | _ ->
        Some expression, []
  in
  let value =
    match split value with
    | Some expression, [] ->
        expression
    | Some expression, flattened ->
        Access (
          ExpressionAccess {
            expression = Node.create ~location expression;
            access = List.rev flattened
          })
    | None, flattened ->
        Access (SimpleAccess (List.rev flattened))
  in
  { Node.location; value }


let rec convert_to_new ({ Node.location; value } as expression) =
  let rec convert expression access =
    let convert_arguments { Node.value = arguments; _ } =
      let convert { Argument.name; value } =
        { Call.Argument.name; value = convert_to_new value }
      in
      List.map ~f:convert arguments
    in
    let convert_generator { Comprehension.target; iterator; conditions; async } =
      {
        Comprehension.target = convert_to_new target;
        iterator = convert_to_new iterator;
        conditions = List.map ~f:convert_to_new conditions;
        async;
      }
    in
    let convert_entry { Dictionary.key; value } =
      { Dictionary.key = convert_to_new key; value = convert_to_new value }
    in
    let convert_parameter { Node.value = { Parameter.value; annotation; name }; location } =
      let value =
        {
          Parameter.name;
          value = value >>| convert_to_new;
          annotation = annotation >>| convert_to_new;
        }
      in
      { Node.location; value }
    in
    match expression, access with
    | Some { Node.value = expression; location },
      Access.Identifier identifier :: [] ->
        Name (Name.Attribute {
            base = convert_to_new { Node.location; value = expression };
            attribute = identifier;
          })
        |> Node.create ~location
    | Some { Node.value = expression; location }, Call arguments :: [] ->
        Call {
          callee = convert_to_new { Node.location; value = expression };
          arguments = convert_arguments arguments;
        }
        |> Node.create ~location
    | None, Identifier identifier :: [] ->
        Name (Name.Identifier identifier)
        |> Node.create ~location
    | None, Identifier identifier :: [Identifier base] ->
        Name (Name.Attribute {
            base = (Name (Name.Identifier base)) |> Node.create ~location;
            attribute = identifier;
          })
        |> Node.create ~location
    | None, Call arguments :: [Identifier base] ->
        Call {
          callee = convert_to_new { Node.location; value = Name (Name.Identifier base) };
          arguments = convert_arguments arguments;
        }
        |> Node.create ~location
    | _, Identifier identifier :: access ->
        Name (Name.Attribute {
            base = convert expression access;
            attribute = identifier;
          })
        |> Node.create ~location
    | _, Call arguments :: access ->
          Call {
            callee = convert expression access;
            arguments = convert_arguments arguments;
          }
        |> Node.create ~location
    | Some { Node.value = Await expression; location }, [] ->
        Await (convert_to_new expression)
        |> Node.create ~location
    | Some { Node.value = BooleanOperator { BooleanOperator.left; right; operator }; location },
      [] ->
        BooleanOperator {
          BooleanOperator.left = convert_to_new left;
          right = convert_to_new right;
          operator
        } |> Node.create ~location
    | Some { Node.value = Call { callee; arguments }; location }, [] ->
        Call {
          callee = convert_to_new callee;
          arguments;
        } |> Node.create ~location
    | Some {
        Node.value = ComparisonOperator { ComparisonOperator.left; right; operator };
        location;
      },
      [] ->
        ComparisonOperator {
          ComparisonOperator.left = convert_to_new left;
          right = convert_to_new right;
          operator
        } |> Node.create ~location
    | Some { Node.value = Dictionary { Dictionary.entries; keywords }; location }, [] ->
        Dictionary {
          Dictionary.entries = List.map ~f:convert_entry entries;
          keywords = List.map ~f:convert_to_new keywords;
        } |> Node.create ~location
    | Some { Node.value = DictionaryComprehension { Comprehension.element; generators }; location },
      [] ->
        DictionaryComprehension {
          Comprehension.element = convert_entry element;
          generators = List.map ~f:convert_generator generators
        } |> Node.create ~location
    | Some { Node.value = Generator { Comprehension.element; generators }; location }, [] ->
        Generator {
          Comprehension.element = convert_to_new element;
          generators = List.map ~f:convert_generator generators;
        } |> Node.create ~location
    | Some { Node.value = Lambda { Lambda.parameters; body }; location }, [] ->
        Lambda {
          Lambda.parameters = List.map ~f:convert_parameter parameters;
          body = convert_to_new body;
        } |> Node.create ~location
    | Some { Node.value = List elements; location }, [] ->
        List (List.map ~f:convert_to_new elements)
        |> Node.create ~location
    | Some { Node.value = ListComprehension { Comprehension.element; generators }; location },
      [] ->
        ListComprehension {
          Comprehension.element = convert_to_new element;
          generators = List.map ~f:convert_generator generators;
        } |> Node.create ~location
    | Some { Node.value = Name (Name.Attribute { base; attribute }); location }, [] ->
        Name (Name.Attribute {
          base = convert_to_new base;
          attribute;
        }) |> Node.create ~location
    | Some { Node.value = Set elements; location }, [] ->
        Set (List.map ~f:convert_to_new elements)
        |> Node.create ~location
    | Some { Node.value = SetComprehension { Comprehension.element; generators }; location }, [] ->
        SetComprehension {
          Comprehension.element = convert_to_new element;
          generators = List.map ~f:convert_generator generators;
        } |> Node.create ~location
    | Some { Node.value = Starred (Starred.Once expression); location }, [] ->
        Starred (Starred.Once (convert_to_new expression))
        |> Node.create ~location
    | Some { Node.value = Starred (Starred.Twice expression); location }, [] ->
        Starred (Starred.Twice (convert_to_new expression))
        |> Node.create ~location
    | Some { Node.value = String { StringLiteral.value; kind }; location }, [] ->
        let kind =
          match kind with
          | StringLiteral.Format expressions ->
              StringLiteral.Format (List.map expressions ~f:convert_to_new)
          | _ ->
              kind
        in
        String { StringLiteral.value; kind }
        |> Node.create ~location
    | Some { Node.value = Ternary { Ternary.target; test; alternative }; location }, [] ->
        Ternary {
          Ternary.target = convert_to_new target;
          test = convert_to_new test;
          alternative = convert_to_new alternative;
        } |> Node.create ~location
    | Some { Node.value = Tuple elements; location }, [] ->
        Tuple (List.map ~f:convert_to_new elements)
        |> Node.create ~location
    | Some { Node.value = UnaryOperator { UnaryOperator.operand; operator }; location }, [] ->
        UnaryOperator {
          UnaryOperator.operand = convert_to_new operand;
          operator;
        } |> Node.create ~location
    | Some { Node.value = Yield expression; location }, [] ->
        Yield (expression >>| convert_to_new)
        |> Node.create ~location
    | Some expression, _ ->
        expression
    | _ ->
        Name (Name.Identifier "")
        |> Node.create ~location
  in
  match value with
  | Access (SimpleAccess access) ->
      convert None (List.rev access)
  | Access (ExpressionAccess { expression; access }) ->
      convert (Some expression) (List.rev access)
  | _ ->
      convert (Some expression) []


let create_name_from_identifiers identifiers =
  let rec create = function
    | [] ->
        failwith "Access must have non-zero identifiers."
    | [{ Node.location; value = identifier }] ->
        Name (Name.Identifier identifier)
        |> Node.create ~location
    | { Node.location; value = identifier } :: rest ->
        Name (
          Name.Attribute {
            base = create rest;
            attribute = identifier;
          })
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
  identifier_names name
  |> List.map ~f:(Node.create ~location)
  |> create_name_from_identifiers


let name_to_identifiers name =
  let rec collect sofar name =
    match sofar, name with
    | Some sofar, Name (Name.Identifier identifier) ->
        Some (identifier :: sofar)
    | Some sofar, Name (Name.Attribute { base; attribute }) ->
        collect (Some (attribute :: sofar)) (Node.value base)
    | _ ->
        None
  in
  collect (Some []) (Name name)


let is_simple_name name =
  Option.is_some (name_to_identifiers name)


let rec has_identifier_base expression =
  match Node.value expression with
  | Call { callee; _ } ->
      has_identifier_base callee
  | Name (Name.Attribute { base; _ }) ->
      has_identifier_base base
  | Name (Name.Identifier _ ) ->
      true
  | _ ->
      false


let rec sanitized ({ Node.value; location } as expression) =
  match value with
  | Name (Name.Identifier identifier) ->
      Name (Name.Identifier (Identifier.sanitized identifier))
      |> Node.create ~location
  | Name (Name.Attribute { base; attribute }) ->
      Name (Name.Attribute { base = sanitized base; attribute = Identifier.sanitized attribute })
      |> Node.create ~location
  | Call { callee; arguments } ->
      let sanitize_argument ({ Call.Argument.name; _ } as argument) =
        let name =
          match name with
          | Some { Node.value; location } ->
              Some { Node.value = Identifier.sanitized value; location }
          | None ->
              None
        in
        { argument with Call.Argument.name }
      in
      Call { callee = sanitized callee; arguments = List.map ~f:sanitize_argument arguments }
      |> Node.create ~location
  | _ ->
      expression


let rec delocalize ({ Node.value; location } as expression) =
  let value =
    let delocalize_element = function
      | Access.Call ({ Node.value = arguments; _ } as call) ->
          let delocalize_argument ({ Argument.value; _ } as argument) =
            { argument with Argument.value = delocalize value }
          in
          Access.Call { call with Node.value = List.map arguments ~f:delocalize_argument }
      | element ->
          element
    in
    match value with
    | Access (SimpleAccess access) ->
        let access =
          Access.delocalize access
          |> List.map ~f:delocalize_element
        in
        Access (SimpleAccess access)
    | Access (ExpressionAccess { expression; access }) ->
        Access
          (ExpressionAccess {
              expression = delocalize expression;
              access = List.map access ~f:delocalize_element;
            })
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
            |> fun name -> Name name
            |> Node.create ~location
          in
          Name (Name.Attribute { base = qualifier; attribute = sanitized })
        else
          begin
            Log.debug "Unable to extract qualifier from %s" identifier;
            Name (Name.Identifier sanitized)
          end
    | Name (Name.Identifier identifier) ->
        Name (Name.Identifier identifier)
    | Name (Name.Attribute { base; attribute }) ->
        Name (Name.Attribute { base = delocalize base; attribute })
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
    | Access (SimpleAccess access) ->
        Access (SimpleAccess (Access.delocalize_qualified access))
    | _ ->
        value
  in
  { expression with Node.value }


let exists_in_list ?(match_prefix=false) ~expression_list target_string =
  let rec matches expected actual =
    match expected, delocalize_qualified actual with
    | (expected :: expected_tail),
      {
        Node.location;
        value = Access (SimpleAccess ((Access.Identifier identifier) :: identifiers));
      }
      when String.equal identifier expected ->
        if List.is_empty expected_tail &&
           (match_prefix || List.is_empty identifiers)
        then
          true
        else
          matches expected_tail { Node.location; value = Access (SimpleAccess identifiers) }
    | _ ->
        false
  in
  List.map ~f:convert expression_list
  |> List.exists ~f:(matches (String.split ~on:'.' target_string))


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
    | Access.Identifier identifier ->
        Format.fprintf formatter "%s" @@ identifier


  and pp_access_list formatter access_list =
    match access_list with
    | [] -> ()
    | access :: [] -> Format.fprintf formatter "%a" pp_access access
    | access
      :: Access.Identifier identifier
      :: Access.Call { Node.value = arguments; _ }
      :: access_list
      when (Identifier.equal identifier "__getitem__") ->
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
          (Node.value name)
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
    let identifier = name in
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
    | Access (SimpleAccess access_list) ->
        pp_access_list formatter access_list

    | Access (ExpressionAccess { expression; access = access_list }) ->
        Format.fprintf
          formatter
          "%a.%a"
          pp_expression (Node.value expression)
          pp_access_list access_list

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

    | Call { Call.callee; arguments } ->
        begin
          match Node.value callee with
          | Name (Name.Attribute { base; attribute = "__getitem__" }) ->
              Format.fprintf
                formatter
                "%a[%a]"
                pp_expression_t base
                pp_argument_list
                  (List.map
                    ~f:(fun { Call.Argument.name; value } -> { Argument.name; value })
                    arguments)
          | _ ->
              Format.fprintf
                formatter
                "%a(%a)"
                pp_expression_t callee
                pp_argument_list
                  (List.map
                    ~f:(fun { Call.Argument.name; value } -> { Argument.name; value })
                    arguments)
        end

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

    | Ellipsis ->
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

    | Name (Name.Identifier name) ->
        Format.fprintf formatter "%s" name

    | Name (Name.Attribute { base; attribute }) ->
        Format.fprintf
          formatter
          "%a.%s"
          pp_expression (Node.value base)
          attribute

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


let rec show_sanitized { Node.location; value } =
  match value with
  | Access (SimpleAccess access) ->
      Access.show_sanitized access
  | Access (ExpressionAccess { expression; access = [] }) ->
      Format.asprintf "%a" pp expression
  | Access (ExpressionAccess { expression; access }) ->
      Format.asprintf "%a.%s" pp expression (Access.show_sanitized access)
  | Name (Name.Identifier identifier) ->
      Identifier.sanitized identifier
  | Name (Name.Attribute { base; attribute }) ->
      Format.asprintf "%s.%s" (show_sanitized base) (Identifier.sanitized attribute)
  | Call { callee; _ } ->
      Format.asprintf "%s.(...)" (show_sanitized callee)
  | _ ->
      show { Node.location; value }


let pp_sanitized formatter expression =
  show_sanitized expression
  |> Format.fprintf formatter "%s"


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
