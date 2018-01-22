(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Pyre


module Record = struct
  module Define = struct
    type 'statement record = {
      name: Expression.Access.t;
      parameters: (Expression.t Parameter.t) list;
      body: 'statement list;
      decorators: Expression.t list;
      docstring: string option;
      return_annotation: Expression.t option;
      async: bool;
      generated: bool;
      parent: Expression.Access.t option; (* The class owning the method. *)
    }
    [@@deriving compare, eq, sexp, show]
  end

  module Class = struct
    type 'statement record = {
      name: Expression.Access.t;
      bases: (Expression.t Argument.t) list;
      body: 'statement list;
      decorators: Expression.t list;
      docstring: string option;
    }
    [@@deriving compare, eq, sexp, show]
  end
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
    name: Expression.Access.t;
    alias: Expression.Access.t option;
  }
  [@@deriving compare, eq, sexp, show]


  type t = {
    from: Expression.Access.t option;
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
    parent: Expression.Access.t option;
  }
  [@@deriving compare, eq, sexp, show]


  let is_static_attribute_initialization { parent; _ } =
    Option.is_some parent
end

module Stub = struct
  type 'statement t =
    | Assign of Assign.t
    | Class of 'statement Record.Class.record
    | Define of 'statement Record.Define.record
  [@@deriving compare, eq, sexp, show]
end


type statement =
  | Assign of Assign.t
  | Assert of Assert.t
  | Break
  | Class of t Record.Class.record
  | Continue
  | Define of t Record.Define.record
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


type statement_node = t
[@@deriving compare, eq, sexp, show]


module Define = struct
  include Record.Define


  type t = statement_node Record.Define.record
  [@@deriving compare, eq, sexp, show]


  let create_toplevel statements =
    {
      name = Expression.Access.create "$toplevel";
      parameters = [];
      body = statements;
      decorators = [];
      docstring = None;
      return_annotation = None;
      async = false;
      generated = false;
      parent = None;
    }


  let is_method { name; parent; _ } =
    Option.is_some parent && List.length name = 1


  let has_decorator { decorators; _ } decorator =
    let open Expression in
    let rec is_decorator expected actual =
      match expected, actual with
      | (expected_decorator :: expected_decorators),
        { Node.location; value = Access ((Access.Identifier identifier) :: identifiers) }
        when Identifier.show identifier = expected_decorator ->
          if List.is_empty expected_decorators && List.is_empty identifiers then
            true
          else
            is_decorator expected_decorators { Node.location; value = Access identifiers }
      | _ ->
          false
    in
    List.exists ~f:(is_decorator (String.split ~on:'.' decorator)) decorators


  let is_abstract_method define =
    has_decorator define "abstractmethod" ||
    has_decorator define "abc.abstractmethod" ||
    has_decorator define "abstractproperty" ||
    has_decorator define "abc.abstractproperty"


  let is_overloaded_method define =
    has_decorator define "overload" ||
    has_decorator define "typing.overload"


  let is_static_method define =
    has_decorator define "staticmethod"


  let is_class_method define =
    has_decorator define "classmethod"


  let is_constructor { name; parent; _ } =
    let string_name = Expression.Access.show name in
    match parent with
    | None -> false
    | Some parent ->
        Expression.Access.show parent = string_name ||
        string_name = "__init__"


  let is_generated_constructor { generated; _ } = generated


  let is_untyped { return_annotation; _ } =
    Option.is_none return_annotation


  let create_generated_constructor { Record.Class.name; docstring; _ } =
    {
      name;
      parameters = [Parameter.create ~name:(Identifier.create "self") ()];
      body = [Node.create Pass];
      decorators = [];
      return_annotation = None;
      async = false;
      generated = true;
      parent = Some name;
      docstring;
    }

  let contains_call { body; _ } name =
    let matches = function
      | {
        Node.value = Expression {
            Node.value = Expression.Access [
                Expression.Access.Call {
                  Node.value = {
                    Expression.Call.name = {
                      Node.value = Expression.Access access;
                      _;
                    };
                    _;
                  };
                  _;
                };
              ];
            _;
          };
        _;
      } when Expression.Access.show access = name ->
          true
      | _ ->
          false
    in
    List.exists ~f:matches body


  let dump define =
    contains_call define "pyre_dump"


  let dump_cfg define =
    contains_call define "pyre_dump_cfg"


  let implicit_attribute_assigns { body; _ } =
    let attribute_assign map { Node.location; value } =
      match value with
      | Assign ({
          Assign.target = ({
              Node.value = Expression.Access
                  ((Expression.Access.Identifier self) :: ([_] as access));
              _;
            } as target);
          _;
        } as assign) when Identifier.show self = "self" ->
          let assign =
            Node.create
              ~location
              {
                assign with
                Assign.target = {
                  target with
                  Node.value = Expression.Access access;
                };
                value = None;
              }
          in
          let update = function
            | Some data -> Some data
            | None -> Some assign
          in
          Map.change ~f:update map access
      | _ ->
          map
    in
    List.fold ~init:Expression.Access.Map.empty ~f:attribute_assign body


  let property_attribute_assign ~location ({ name; return_annotation; _ } as define) =
    let property_annotations =
      [
        "abc.abstractproperty";
        "libfb.py.decorators.lazy_property";
        "property";
        "util.etc.lazy_property";
      ]
    in
    if List.exists ~f:(has_decorator define) property_annotations then
      Some
        (Node.create
           ~location
           {
             Assign.target = Node.create ~location (Expression.Access name);
             annotation = return_annotation;
             value = None;
             compound = None;
             parent = None;
           })
    else
      None


  let strip define =
    { define with body = [] }
end


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


  and pp_class formatter { Record.Class.name; bases; body; decorators; _ } =
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


module Class = struct
  include Record.Class


  type t = statement_node Record.Class.record
  [@@deriving compare, eq, sexp, show]


  let constructor { Record.Class.body; _ } =
    let constructor = function
      | { Node.value = Define define; _ } when Define.is_constructor define ->
          Some define
      | _ ->
          None
    in
    List.find_map ~f:constructor body


  let attribute_assigns ({ Record.Class.body; _ } as definition) =
    let implicit_attribute_assigns =
      constructor definition
      >>| Define.implicit_attribute_assigns
      |> Option.value ~default:Expression.Access.Map.empty
    in
    let property_assigns =
      let property_assigns map = function
        | { Node.location; value = Define define } ->
            (Define.property_attribute_assign ~location define
             >>= fun ({ Node.value = { Assign.target; _ }; _ } as assign) ->
             match target with
             | { Node.value = Expression.Access ([_] as access); _ } ->
                 Some (Map.add ~key:access ~data:assign map)
             | _ ->
                 None)
            |> Option.value ~default:map
        | _ ->
            map
      in
      List.fold ~init:Expression.Access.Map.empty ~f:property_assigns body
    in
    let explicit_attribute_assigns =
      let attribute_assigns map { Node.location; value } =
        match value with
        | Assign ({
            Assign.target = { Node.value = Expression.Access ([_] as access); _ };
            _;
          } as assign)
        | Stub
            (Stub.Assign
               ({
                 Assign.target = { Node.value = Expression.Access ([_] as access); _ };
                 _;
               } as assign)) ->
            Map.add ~key:access ~data:(Node.create ~location assign) map
        | _ ->
            map
      in
      List.fold ~init:Expression.Access.Map.empty ~f:attribute_assigns body
    in
    (* Explicit declarations override implicit ones. *)
    let merge ~key:_ = function
      | `Both (_, right) ->
          Some right
      | `Left value
      | `Right value ->
          Some value
    in
    Map.merge ~f:merge implicit_attribute_assigns explicit_attribute_assigns
    |> Map.merge ~f:merge property_assigns


  let strip ({ Record.Class.body; _ } as class_define ) =
    let strip_define statement =
      match Node.value statement with
      | Define define when not (Define.is_constructor define) ->
          { statement with Node.value = Define (Define.strip define) }
      | _ ->
          statement
    in
    { class_define with Record.Class.body = List.map ~f:strip_define body }


  let update
      { Record.Class.body = stub; _ }
      ~definition:({ Record.Class.body; _ } as definition) =
    let updated, undefined =
      let update (updated, undefined) statement =
        match statement with
        | { Node.location; value = Assign ({ Assign.target; _ } as assign)} ->
            begin
              let is_stub = function
                | { Node.value = Stub (Stub.Assign { Assign.target = stub_target; _ }); _ }
                | { Node.value = Assign { Assign.target = stub_target; _; }; _; }
                  when Expression.equal target stub_target ->
                    true
                | _ ->
                    false
              in
              match List.find ~f:is_stub stub with
              | Some { Node.value = Stub (Stub.Assign { Assign.annotation; _ }); _ } ->
                  let updated_assign =
                    {
                      Node.location;
                      value = Assign { assign with Assign.annotation }
                    }
                  in
                  updated_assign :: updated,
                  (List.filter ~f:(fun statement -> not (is_stub statement)) undefined)
              | _ ->
                  statement :: updated, undefined
            end
        | { Node.location; value = Define ({ Record.Define.name; parameters; _ } as define)} ->
            begin
              let is_stub = function
                | {
                  Node.value = Stub (Stub.Define {
                      Record.Define.name = stub_name;
                      parameters = stub_parameters;
                      _;
                    });
                  _;
                }
                | {
                  Node.value = Define {
                      Record.Define.name = stub_name;
                      parameters = stub_parameters;
                      _;
                    };
                  _;
                } when Expression.Access.equal name stub_name &&
                       List.length parameters = List.length stub_parameters ->
                    true
                | _ ->
                    false
              in
              match List.find ~f:is_stub stub with
              | Some {
                  Node.value = Stub (Stub.Define { Define.parameters; return_annotation; _ });
                  _;
                } ->
                  let updated_define =
                    {
                      Node.location;
                      value = Define { define with Define.parameters; return_annotation }
                    }
                  in
                  updated_define :: updated,
                  (List.filter ~f:(fun statement -> not (is_stub statement)) undefined)
              | _ ->
                  statement :: updated, undefined
            end
        | _ ->
            statement :: updated, undefined
      in
      List.fold ~init:([], stub) ~f:update body
    in
    { definition with Record.Class.body = undefined @ updated }
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
