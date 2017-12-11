(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core


module Define = struct
  type 'statement t = {
    name: Expression.access;
    parameters: (Expression.t Parameter.t) list;
    body: 'statement list;
    decorators: Expression.t list;
    docstring: string option;
    return_annotation: Expression.t option;
    async: bool;
    parent: Expression.access option; (* The class owning the method. *)
  }
  [@@deriving compare, eq, sexp, show]
end


module Class = struct
  type 'statement t = {
    name: Expression.access;
    bases: (Expression.t Argument.t) list;
    body: 'statement list;
    decorators: Expression.t list;
    docstring: string option;
  }
  [@@deriving compare, eq, sexp, show]
end


module For = struct
  type 'statement t = {
    target: Expression.t;
    iterator: Expression.t;
    body: 'statement list;
    orelse: 'statement list;
    async: bool;
  }
  [@@deriving compare, eq, sexp, show]
end


module While = struct
  type 'statement t = {
    test: Expression.t;
    body: 'statement list;
    orelse: 'statement list;
  }
  [@@deriving compare, eq, sexp, show]
end


module If = struct
  type 'statement t = {
    test: Expression.t;
    body: 'statement list;
    orelse: 'statement list;
  }
  [@@deriving compare, eq, sexp, show]
end


module With = struct
  type 'statement t = {
    items: (Expression.t * Expression.t option) list;
    body: 'statement list;
    async: bool;
  }
  [@@deriving compare, eq, sexp, show]
end


module Try = struct
  type 'statement handler = {
    kind: Expression.t option;
    name: Identifier.t option;
    handler_body: 'statement list;
  }
  [@@deriving compare, eq, sexp, show]


  type 'statement t = {
    body: 'statement list;
    handlers: 'statement handler list;
    orelse: 'statement list;
    finally: 'statement list;
  }
  [@@deriving compare, eq, sexp, show]
end


module Assert = struct
  type t = {
    test: Expression.t;
    message: Expression.t option;
  }
  [@@deriving compare, eq, sexp, show]
end


module Import = struct
  type import = {
    name: Expression.access;
    alias: Expression.access option;
  }
  [@@deriving compare, eq, sexp, show]


  type t = {
    from: Expression.access option;
    imports: import list;
  }
  [@@deriving compare, eq, sexp, show]
end


module Assign = struct
  type t = {
    target: Expression.t;
    annotation: Expression.t option;
    value: Expression.t option;
    compound: Expression.BinaryOperator.operator option;
    parent: Expression.access option;
  }
  [@@deriving compare, eq, sexp, show]


  let is_static_field_initialization { parent; _ } =
    Option.is_some parent
end

module Stub = struct
  type 'statement t =
    | Assign of Assign.t
    | Class of 'statement Class.t
    | Define of 'statement Define.t
  [@@deriving compare, eq, sexp, show]
end


type statement =
  | Assign of Assign.t
  | Assert of Assert.t
  | Break
  | Class of t Class.t
  | Continue
  | Define of t Define.t
  | Delete of Expression.t
  | Expression of Expression.t
  | For of t For.t
  | Global of Identifier.t list
  | If of t If.t
  | Import of Import.t
  | Nonlocal of Identifier.t list
  | Pass
  | Raise of Expression.t option
  | Return of Expression.t option
  | Stub of t Stub.t
  | Try of t Try.t
  | With of t With.t
  | While of t While.t
  | Yield of Expression.t
  | YieldFrom of Expression.t


and t = statement Node.t
[@@deriving compare, eq, sexp, show]


type define = t Define.t
[@@deriving compare, eq, sexp, show]


let assume ({ Node.location; _ } as test) =
  {
    Node.location;
    value = Assert { Assert.test; message = None };
  }


(* Naive assumptions *)
let terminates body =
  let find_terminator = function
    | { Node.value = Return _; _ }
    | { Node.value = Raise _; _ }
    | { Node.value = Continue; _ } -> true
    | _ -> false
  in
  Option.is_some (List.find ~f:find_terminator body)


module PrettyPrinter = struct
  let pp_decorators formatter =
    function
    | [] -> ()
    | decorators ->
        Format.fprintf
          formatter
          "@[<v>@@(%a)@;@]"
          Expression.pp_expression_list decorators


  let pp_access_list_option formatter =
    function
    | None -> ()
    | Some access_list ->
        Format.fprintf
          formatter
          "@[%a.@]"
          Expression.pp_expression_access_list access_list


  let pp_access_list formatter =
    function
    | [] -> ()
    | access_list ->
        Format.fprintf
          formatter
          "@[%a@]"
          Expression.pp_expression_access_list access_list


  let pp_list formatter pp sep list =
    let rec pp' formatter =
      function
      | [] -> ()
      | x :: [] -> Format.fprintf formatter "%a" pp x
      | x :: xs -> Format.fprintf formatter ("%a"^^sep^^"%a") pp x pp' xs
    in
    pp' formatter list


  let pp_option formatter option pp =
    Option.value_map option ~default:() ~f:(Format.fprintf formatter "%a" pp)


  let pp_option_with_prefix formatter (prefix,option) pp =
    Option.value_map
      option
      ~default:()
      ~f:(Format.fprintf formatter (prefix^^"%a") pp)


  let pp_expression_option formatter (prefix,option) =
    pp_option_with_prefix formatter (prefix,option) Expression.pp


  let pp_binary_operator_option formatter option =
    pp_option formatter option Expression.BinaryOperator.pp_binary_operator


  let pp_async formatter =
    function
    | true -> Format.fprintf formatter "async@;"
    | false -> ()


  let rec pp_statement_node formatter { Node.value = statement ; _ } =
    Format.fprintf formatter "%a" pp_statement statement


  and pp_statement_list formatter =
    function
    | [] -> ()
    | statement :: [] -> Format.fprintf formatter "%a" pp_statement_node statement
    | statement :: statement_list ->
        Format.fprintf
          formatter "%a@;%a"
          pp_statement_node statement
          pp_statement_list statement_list


  and pp_assign formatter { Assign.target; annotation; value; compound; parent } =
    Format.fprintf
      formatter
      "%a%a %a= %a%a"
      pp_access_list_option parent
      Expression.pp target
      pp_binary_operator_option compound
      pp_expression_option ("", value)
      pp_expression_option (" # ", annotation)


  and pp_class formatter { Class.name; bases; body; decorators; _ } =
    Format.fprintf
      formatter
      "%a@[<v 2>class %a(%a):@;@[<v>%a@]@;@]"
      pp_decorators decorators
      pp_access_list name
      Expression.pp_expression_argument_list bases
      pp_statement_list body


  and pp_define
      formatter
      { Define.name; parameters; body; decorators; return_annotation; async; parent; _ } =
    let return_annotation =
      match return_annotation with
      | Some annotation -> Format.asprintf " -> %a" Expression.pp annotation
      | _ -> ""
    in
    Format.fprintf
      formatter
      "%a@[<v 2>%adef %a%a(%a)%s:@;%a@]@."
      pp_decorators decorators
      pp_async async
      pp_access_list_option parent
      pp_access_list name
      Expression.pp_expression_parameter_list parameters
      return_annotation
      pp_statement_list body


  and pp_statement formatter statement =
    match statement with
    | Assign assign ->
        Format.fprintf
          formatter
          "%a"
          pp_assign assign

    | Assert { Assert.test; Assert.message } ->
        Format.fprintf
          formatter
          "assert %a, %a"
          Expression.pp test
          pp_expression_option ("", message)

    | Break ->
        Format.fprintf formatter "break"

    | Class definition ->
        Format.fprintf formatter "%a" pp_class definition

    | Continue ->
        Format.fprintf formatter "continue"

    | Define define ->
        Format.fprintf formatter "%a" pp_define define

    | Delete expression ->
        Format.fprintf formatter "del %a" Expression.pp expression

    | Expression expression ->
        Expression.pp formatter expression

    | For { For.target; iterator; body; orelse; async } ->
        Format.fprintf
          formatter
          "@[<v 2>%afor %a in %a:@;%a@]%a"
          pp_async async
          Expression.pp target
          Expression.pp iterator
          pp_statement_list body
          pp_statement_list orelse

    | Global global_list ->
        pp_list formatter Identifier.pp "," global_list

    | If { If.test; body; orelse } ->
        Format.fprintf
          formatter
          "@[<v>@[<v 2>if %a:@;%a@]@;@[<v 2>else:@;%a@]@]"
          Expression.pp test
          pp_statement_list body
          pp_statement_list orelse

    | Import { Import.from; imports } ->
        let pp_from formatter access_list =
          pp_option_with_prefix formatter ("from ", access_list) pp_access_list
        in
        let pp_import formatter { Import.name; alias } =
          let pp_alias_option formatter access_list =
            pp_option_with_prefix formatter ("as ", access_list) pp_access_list
          in
          Format.fprintf
            formatter
            "%a%a"
            pp_access_list name
            pp_alias_option alias
        in
        let pp_imports formatter import_list =
          pp_list formatter pp_import ", " import_list
        in
        Format.fprintf
          formatter
          "@[<v>%a import %a@]"
          pp_from from
          pp_imports imports

    | Nonlocal nonlocal_list ->
        pp_list formatter Identifier.pp "," nonlocal_list

    | Pass ->
        Format.fprintf formatter "%s" "pass"

    | Raise expression ->
        Format.fprintf
          formatter
          "raise %a"
          pp_expression_option ("", expression)

    | Return expression ->
        Format.fprintf
          formatter
          "return %a"
          pp_expression_option ("", expression)

    | Stub (Stub.Assign assign) ->
        Format.fprintf
          formatter
          "%a"
          pp_assign assign

    | Stub (Stub.Class definition) ->
        Format.fprintf formatter "%a" pp_class definition

    | Stub (Stub.Define define) ->
        Format.fprintf formatter "%a" pp_define define

    | Try { Try.body; handlers; orelse; finally } ->
        let pp_try_block formatter body =
          Format.fprintf
            formatter
            "@[<v 2>try:@;%a@]"
            pp_statement_list body
        in
        let pp_except_block formatter handlers =
          let pp_as formatter name =
            pp_option_with_prefix formatter (" as ", name) Identifier.pp
          in
          let pp_handler formatter {Try.kind; Try.name; Try.handler_body } =
            Format.fprintf
              formatter
              "@[<v 2>except%a%a:@;%a@]"
              pp_expression_option (" ", kind)
              pp_as name
              pp_statement_list handler_body
          in
          let pp_handler_list formatter handler_list =
            pp_list formatter pp_handler "@;" handler_list
          in
          Format.fprintf
            formatter
            "%a"
            pp_handler_list handlers
        in
        let pp_else_block formatter =
          function
          | [] -> ()
          | orelse ->
              Format.fprintf
                formatter
                "@[<v 2>else:@;%a@]"
                pp_statement_list orelse
        in
        let pp_finally_block formatter =
          function
          | [] -> ()
          | finally ->
              Format.fprintf
                formatter
                "@[<v 2>finally:@;@[<v>%a@]@]"
                pp_statement_list finally
        in
        Format.fprintf
          formatter
          "@[<v>%a@;%a@;%a@;%a@]"
          pp_try_block body
          pp_except_block handlers
          pp_else_block orelse
          pp_finally_block finally

    | With { With.items; body; async } ->
        let pp_item formatter (expression, expression_option) =
          Format.fprintf
            formatter
            "%a%a"
            Expression.pp expression
            pp_expression_option (" as ", expression_option)
        in
        let rec pp_item_list formatter =
          function
          | [] -> ()
          | item :: [] -> Format.fprintf formatter "%a" pp_item item
          | item :: item_list ->
              Format.fprintf formatter "%a,%a" pp_item item pp_item_list item_list
        in
        Format.fprintf
          formatter
          "@[<v 2>%a with %a:@;%a@]"
          pp_async async
          pp_item_list items
          pp_statement_list body

    | While { While.test; body; orelse } ->
        Format.fprintf
          formatter
          "@[<v 2>while %a:@;%a@]@[<v>%a@]"
          Expression.pp test
          pp_statement_list body
          pp_statement_list orelse

    | Yield expression -> Format.fprintf formatter "%a" Expression.pp expression
    | YieldFrom expression -> Format.fprintf formatter "%a" Expression.pp expression


  let pp = pp_statement_node
end


let pp formatter statement =
  Format.fprintf
    formatter
    "%a"
    PrettyPrinter.pp statement

let show statement = Format.asprintf "%a" pp statement

let extract_docstring statements =
  (* See PEP 257 for Docstring formatting. The main idea is that we want to get the shortest
   * indentation from line 2 onwards as the indentation of the docstring. *)
  let unindent docstring =
    let indentation line =
      let line_without_indentation = String.lstrip line in
      (String.length line) - (String.length line_without_indentation) in
    match String.split ~on:'\n' docstring with
    | [] -> docstring
    | first :: rest ->
        let indentations = List.map ~f:indentation rest in
        let difference = List.fold ~init:Int.max_value ~f:Int.min indentations in
        let rest = List.map ~f:(fun s -> String.drop_prefix s difference) rest in
        String.concat ~sep:"\n" (first::rest)
  in
  match statements with
  | { Node.value = Expression { Node.value = Expression.String s; _ }; _ } :: _ -> Some (unindent s)
  | _ -> None
