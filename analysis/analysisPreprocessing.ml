(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression
open Pyre
open PyreParser
open Statement


let expand_string_annotations source =
  let module Transform = Transform.MakeStatementTransformer(struct
      type t = unit

      let statement_postorder _ ({ Node.location; value } as statement) =
        let transform ({ Define.parameters; return_annotation; _ } as define) =
          let access = function
            | { Node.location; value = String string } ->
                let parsed =
                  try
                    let buffer = Lexing.from_string (string ^ "\n") in
                    let state = Lexer.State.initial () in
                    match ParserGenerator.parse (Lexer.read state) buffer with
                    | [{ Node.value = Expression { Node.value = Access access; _ } ; _ }] ->
                        Some access
                    | _ ->
                        raise ParserGenerator.Error
                  with
                  | ParserGenerator.Error
                  | Failure _ ->
                      begin
                        Log.debug
                          "Invalid string annotation `%s` at %a"
                          string
                          Location.pp
                          location;
                        None
                      end
                in
                parsed >>| (fun parsed -> { Node.location; value = Access parsed })
            | expression ->
                Some expression
          in
          let parameter ({ Node.value = ({ Parameter.annotation; _ } as parameter); _ } as node) =
            {
              node with
              Node.value = { parameter with Parameter.annotation = annotation >>= access };
            }
          in
          {
            define with
            Define.parameters = List.map ~f:parameter parameters;
            return_annotation = return_annotation >>= access;
          }
        in
        let statement =
          match value with
          | Define define ->
              Define (transform define)
              |> Node.create ~location
          | Stub (Stub.Define define) ->
              Stub (Stub.Define (transform define))
              |> Node.create ~location
          | _ ->
              statement
        in
        (), [statement]
    end)
  in
  Transform.transform () source
  |> snd


let expand_format_string source =
  let module Transform = Transform.Make(struct
      include Transform.Identity
      type t = unit

      let expression_postorder _ expression =
        match expression with
        | { Node.location; value = FormatString { FormatString.value; _ } } ->
            let rec get_matches regexp input_string start_position =
              try
                let match_start_position = Str.search_forward regexp input_string start_position in
                let match_string = Str.matched_string input_string in
                match_string :: get_matches regexp input_string (match_start_position + 1)
              with Not_found -> []
            in
            let parse input_string =
              try
                let buffer =
                  (String.length input_string) - 1
                  |> String.slice input_string 1
                  |> fun processed_input -> Lexing.from_string (processed_input ^ "\n")
                in
                let state = Lexer.State.initial () in
                match ParserGenerator.parse (Lexer.read state) buffer with
                | [{ Node.value = Expression expression; _ }] ->
                    [expression]
                | _ ->
                    raise ParserGenerator.Error
              with
              | ParserGenerator.Error
              | Failure _ ->
                  begin
                    Log.debug
                      "Pyre could not parse format string `%s` at %a"
                      input_string
                      Location.pp
                      location;
                    []
                  end
            in
            let expression_list =
              get_matches (Str.regexp "{[^{^}]*}") value 0
              |> List.concat_map ~f:parse
            in
            {
              Node.location;
              value = FormatString {
                  FormatString.value;
                  expression_list;
                };
            }
        | _ ->
            expression
    end)
  in
  Transform.transform () source
  |> snd


type alias = {
  access: Access.t;
  is_forward_reference: bool;
}


type scope = {
  qualifier: Access.t;
  aliases: alias Access.Map.t;
  immutables: Access.Set.t;
  locals: Access.Set.t;
  use_forward_references: bool;
  skip: Location.Set.t;
}


let qualify ({ Source.qualifier; statements; _ } as source) =
  let rec qualify_statements ?(qualify_assigns = false) ~scope statements =
    let scope =
      let rec explore_scope ~scope statements =
        let global_alias access = { access; is_forward_reference = true } in
        let explore_scope
            ({ qualifier; aliases; immutables; skip; _ } as scope)
            { Node.location; value } =
          match value with
          | Assign { Assign.target; annotation = Some annotation; _ }
          | Stub (Stub.Assign { Assign.target; annotation = Some annotation; _ })
            when Expression.show annotation = "_SpecialForm" ->
              let name = Expression.access target in
              {
                scope with
                aliases = Map.set aliases ~key:name ~data:(global_alias (qualifier @ name));
                skip = Set.add skip location;
              }
          | Assign {
              Assign.target;
              value = Some { Node.value = Access (typing :: type_alias :: _); _ };
              _;
            }
            when Access.show [typing] = "TypeAlias" ||
                 Access.show [typing; type_alias] = "typing.TypeAlias" ||
                 Access.show [typing] = "TypeVar" ||
                 Access.show [typing; type_alias] = "typing.TypeVar" ->
              let name = Expression.access target in
              {
                scope with
                aliases = Map.set aliases ~key:name ~data:(global_alias (qualifier @ name));
                skip = Set.add skip location;
              }
          | Class { Class.name; _ }
          | Stub (Stub.Class { Class.name; _ }) ->
              {
                scope with
                aliases = Map.set aliases ~key:name ~data:(global_alias (qualifier @ name));
              }
          | Define { Define.name; _ }
          | Stub (Stub.Define { Define.name; _ }) ->
              {
                scope with
                aliases = Map.set aliases ~key:name ~data:(global_alias (qualifier @ name));
              }
          | If { If.body; orelse; _ } ->
              let scope = explore_scope ~scope body in
              explore_scope ~scope orelse
          | For { For.body; orelse; _ } ->
              let scope = explore_scope ~scope body in
              explore_scope ~scope orelse
          | Global identifiers ->
              let immutables =
                let register_global immutables identifier =
                  Set.add immutables [Access.Identifier identifier]
                in
                List.fold identifiers ~init:immutables ~f:register_global
              in
              { scope with immutables }
          | Try { Try.body; handlers; orelse; finally } ->
              let scope = explore_scope ~scope body in
              let scope =
                let explore_handler scope { Try.handler_body; _ } =
                  explore_scope ~scope handler_body
                in
                List.fold handlers ~init:scope ~f:explore_handler
              in
              let scope = explore_scope ~scope orelse in
              explore_scope ~scope finally
          | With { With.body; _ } ->
              explore_scope ~scope body
          | While { While.body; orelse; _ } ->
              let scope = explore_scope ~scope body in
              explore_scope ~scope orelse
          | _ ->
              scope
        in
        List.fold statements ~init:scope ~f:explore_scope
      in
      explore_scope ~scope statements
    in
    let scope, reversed_statements =
      let qualify (scope, statements) statement =
        let scope, statement = qualify_statement ~qualify_assign:qualify_assigns ~scope statement in
        scope, statement :: statements
      in
      List.fold statements ~init:(scope, []) ~f:qualify
    in
    scope, List.rev reversed_statements

  and prefix_identifier ~scope:({ aliases; immutables; _ } as scope) ~prefix name =
    let stars, name =
      let name = Identifier.show name in
      if String.is_prefix name ~prefix:"**" then
        "**", String.drop_prefix name 2
      else if String.is_prefix name ~prefix:"*" then
        "*", String.drop_prefix name 1
      else
        "", name
    in
    let renamed =
      Format.asprintf "$%s_%s" prefix name
      |> Identifier.create
    in
    let name = Identifier.create name in
    let access = [Access.Identifier name] in
    {
      scope with
      aliases =
        Map.set
          aliases
          ~key:access
          ~data:{ access = [Access.Identifier renamed]; is_forward_reference = false };
      immutables = Set.add immutables access;
    },
    stars,
    renamed

  and qualify_parameters ~scope parameters =
    (* Rename parameters to prevent aliasing. *)
    let rename_parameter
        (scope, reversed_parameters)
        ({ Node.value = { Parameter.name; value; annotation }; _ } as parameter) =
      let scope, stars, renamed = prefix_identifier ~scope ~prefix:"parameter" name in
      scope,
      {
        parameter with
        Node.value = {
          Parameter.name = Identifier.map renamed ~f:(fun identifier -> stars ^ identifier);
          value = value >>| qualify_expression ~scope;
          annotation = annotation >>| qualify_expression ~scope;
        };
      } :: reversed_parameters
    in
    let scope, parameters =
      List.fold
        parameters
        ~init:({ scope with locals = Access.Set.empty }, [])
        ~f:rename_parameter
    in
    scope, List.rev parameters

  and qualify_statement
      ~qualify_assign
      ~scope:({ qualifier; aliases; skip; _ } as scope)
      ({ Node.location; value } as statement) =
    let scope, value =
      let local_alias access = { access; is_forward_reference = false } in

      let qualify_assign { Assign.target; annotation; value; parent } =
        let target_scope, target =
          if not (Set.mem skip location) then
            let rec qualify_target ~scope:({ aliases; immutables; locals; _ } as scope) target =
              let scope, value =
                match Node.value target with
                | Tuple elements ->
                    let scope, reversed_elements =
                      let qualify_tuple_target (scope, reversed_elements) element =
                        let scope, element = qualify_target ~scope element in
                        scope, element :: reversed_elements
                      in
                      List.fold elements ~init:(scope, []) ~f:qualify_tuple_target
                    in
                    scope, Tuple (List.rev reversed_elements)
                | Access ([_] as access) when qualify_assign ->
                    (* Qualify field assignments in class body. *)
                    scope, Access (qualifier @ (qualify_access ~scope access))
                | Access ([Access.Identifier name] as access) ->
                    (* Incrementally number local variables to avoid shadowing. *)
                    let scope =
                      let qualified =
                        Identifier.show name
                        |> String.is_prefix ~prefix:"$"
                      in
                      if not qualified &&
                         not (Set.mem locals access) &&
                         not (Set.mem immutables access) then
                        let alias =
                          let aliases =
                            Map.data aliases
                            |> List.map ~f:(fun { access; _ } -> access)
                            |> Access.Set.of_list
                          in
                          let rec new_alias ~count =
                            let alias =
                              Identifier.show name
                              |> Format.asprintf "$local_%d_%s" count
                              |> Identifier.create
                              |> fun identifier -> [Access.Identifier identifier]
                            in
                            if not (Set.mem aliases alias) then
                              alias
                            else
                              new_alias ~count:(count + 1)
                          in
                          new_alias ~count:0
                        in
                        {
                          scope with
                          aliases = Map.set aliases ~key:access ~data:(local_alias alias);
                          locals = Set.add locals access
                        }
                      else
                        scope
                    in
                    scope, Access (qualify_access ~scope access)
                | Access access ->
                    let access =
                      let qualified = qualify_access ~scope access in
                      if qualify_assign then qualifier @ qualified else qualified
                    in
                    scope, Access access
                | target ->
                    scope, target
              in
              scope, { target with Node.value }
            in
            qualify_target ~scope target
          else
            scope, target
        in
        target_scope,
        {
          Assign.target;
          annotation = annotation >>| qualify_expression ~scope;
          value = value >>| qualify_expression ~scope;
          parent = parent >>| fun access -> qualify_access ~scope access;
        }
      in
      let qualify_class ({ Class.name; bases; body; decorators; _ } as definition) =
        let qualify_base ({ Argument.value; _ } as argument) =
          { argument with Argument.value = qualify_expression ~scope value }
        in
        {
          definition with
          Class.name = qualify_access ~scope name;
          bases = List.map bases ~f:qualify_base;
          body =
            qualify_statements
              ~qualify_assigns:true
              ~scope:{ scope with qualifier = qualifier @ name }
              body
            |> snd;
          decorators = List.map decorators ~f:(qualify_expression ~scope);
        }
      in
      let qualify_define
          ({
            Define.name;
            parameters;
            body;
            decorators;
            return_annotation;
            parent;
            _;
          } as define) =
        let renamed_scope, parameters = qualify_parameters ~scope parameters in
        {
          define with
          Define.name = qualify_access ~scope name;
          parameters;
          body =
            qualify_statements
              ~scope:{ renamed_scope with qualifier = qualifier @ name }
              body
            |> snd;
          decorators =
            List.map
              decorators
              ~f:(qualify_expression
                    ~scope:{ scope with use_forward_references = Option.is_none parent });
          return_annotation = return_annotation >>| qualify_expression ~scope;
          parent = parent >>| fun access -> qualify_access ~scope access;
        }
      in

      let join_scopes left right =
        let merge ~key:_ = function
          | `Both (left, _) -> Some left
          | `Left left -> Some left
          | `Right right -> Some right
        in
        {
          left with
          aliases = Map.merge left.aliases right.aliases ~f:merge;
          locals = Set.union left.locals right.locals;
        }
      in

      match value with
      | Assign assign ->
          let scope, assign = qualify_assign assign in
          scope, Assign assign
      | Assert { Assert.test; message } ->
          scope,
          Assert {
            Assert.test = qualify_expression ~scope test;
            message;
          }
      | Class definition ->
          scope,
          Class (qualify_class definition)
      | Define define ->
          scope,
          Define (qualify_define define)
      | Delete expression ->
          scope,
          Delete (qualify_expression ~scope expression)
      | Expression expression ->
          scope,
          Expression (qualify_expression ~scope expression)
      | For ({ For.target; iterator; body; orelse; _ } as block) ->
          let renamed_scope, target = qualify_target ~scope target in
          let body_scope, body = qualify_statements ~scope:renamed_scope body in
          let orelse_scope, orelse = qualify_statements ~scope:renamed_scope orelse in
          join_scopes body_scope orelse_scope,
          For {
            block with
            For.target;
            iterator = qualify_expression ~scope iterator;
            body;
            orelse;
          }
      | Global identifiers ->
          scope,
          Global identifiers
      | If { If.test; body; orelse } ->
          let body_scope, body = qualify_statements ~scope body in
          let orelse_scope, orelse = qualify_statements ~scope orelse in
          join_scopes body_scope orelse_scope,
          If { If.test = qualify_expression ~scope test; body; orelse }
      | Import { Import.from = Some from; imports }
        when Access.show from <> "builtins" ->
          let import aliases { Import.name; alias } =
            let from = Source.expand_relative_import source ~from in
            match alias with
            | Some alias ->
                (* Add `alias -> from.name`. *)
                Map.set aliases ~key:alias ~data:(local_alias (from @ name))
            | None ->
                (* Add `name -> from.name`. *)
                Map.set aliases ~key:name ~data:(local_alias (from @ name))
          in
          { scope with aliases = List.fold imports ~init:aliases ~f:import },
          value
      | Import { Import.from = None; imports } ->
          let import aliases { Import.name; alias } =
            match alias with
            | Some alias ->
                (* Add `alias -> from.name`. *)
                Map.set aliases ~key:alias ~data:(local_alias name)
            | None ->
                aliases
          in
          { scope with aliases = List.fold imports ~init:aliases ~f:import },
          value
      | Nonlocal identifiers  ->
          scope,
          Nonlocal identifiers
      | Raise expression ->
          scope,
          Raise (expression >>| qualify_expression ~scope)
      | Return expression ->
          scope,
          Return (expression >>| qualify_expression ~scope)
      | Stub (Stub.Assign assign) ->
          let scope, assign = qualify_assign assign in
          scope, Stub (Stub.Assign assign)
      | Stub (Stub.Class definition) ->
          scope,
          Stub (Stub.Class (qualify_class definition))
      | Stub (Stub.Define define) ->
          scope,
          Stub (Stub.Define (qualify_define define))
      | Try { Try.body; handlers; orelse; finally } ->
          let body_scope, body = qualify_statements ~scope body in
          let handler_scopes, handlers =
            let qualify_handler { Try.kind; name; handler_body } =
              let scope, handler_body = qualify_statements ~scope handler_body in
              scope, { Try.kind = kind >>| qualify_expression ~scope; name; handler_body }
            in
            List.map handlers ~f:qualify_handler
            |> List.unzip
          in
          let orelse_scope, orelse = qualify_statements ~scope:body_scope orelse in
          let finally_scope, finally = qualify_statements ~scope finally in
          let scope =
            List.fold handler_scopes ~init:body_scope ~f:join_scopes
            |> join_scopes orelse_scope
            |> join_scopes finally_scope
          in
          scope,
          Try { Try.body; handlers; orelse; finally }
      | With ({ With.items; body; _ } as block) ->
          let scope, items =
            let qualify_item (scope, reversed_items) (name, alias) =
              let scope, item =
                let renamed_scope, alias =
                  match alias with
                  | Some alias ->
                      let scope, alias = qualify_target ~scope alias in
                      scope, Some alias
                  | _ ->
                      scope, alias
                in
                renamed_scope,
                (qualify_expression ~scope name, alias)
              in
              scope, item :: reversed_items
            in
            let scope, reversed_items = List.fold items ~init:(scope, []) ~f:qualify_item in
            scope, List.rev reversed_items
          in
          let scope, body = qualify_statements ~scope body in
          scope,
          With { block with With.items; body }
      | While { While.test; body; orelse } ->
          let body_scope, body = qualify_statements ~scope body in
          let orelse_scope, orelse = qualify_statements ~scope orelse in
          join_scopes body_scope orelse_scope,
          While { While.test = qualify_expression ~scope test; body; orelse }
      | Statement.Yield expression ->
          scope,
          Statement.Yield (qualify_expression ~scope expression)
      | Statement.YieldFrom expression ->
          scope,
          Statement.YieldFrom (qualify_expression ~scope expression)
      | Break | Continue | Import _ | Pass ->
          scope,
          value
    in
    scope, { statement with Node.value }

  and qualify_expression ~scope ({ Node.value; _ } as expression) =
    let value =
      let qualify_entry ~scope { Dictionary.key; value } =
        {
          Dictionary.key = qualify_expression ~scope key;
          value = qualify_expression ~scope value;
        }
      in
      let qualify_generators ~scope generators =
        let qualify_generator
            (scope, reversed_generators)
            ({ Comprehension.target; iterator; conditions; _ } as generator) =
          let renamed_scope, target = qualify_target ~scope target in
          renamed_scope,
          {
            generator with
            Comprehension.target;
            iterator = qualify_expression ~scope iterator;
            conditions = List.map conditions ~f:(qualify_expression ~scope:renamed_scope);
          } :: reversed_generators
        in
        let scope, reversed_generators =
          List.fold
            generators
            ~init:(scope, [])
            ~f:qualify_generator
        in
        scope, List.rev reversed_generators
      in
      match value with
      | Access access ->
          Access (qualify_access ~scope access)
      | Await expression ->
          Await (qualify_expression ~scope expression)
      | BooleanOperator { BooleanOperator.left; operator; right } ->
          BooleanOperator {
            BooleanOperator.left = qualify_expression ~scope left;
            operator;
            right = qualify_expression ~scope right;
          }
      | ComparisonOperator { ComparisonOperator.left; right } ->
          let qualify_operand (operator, operand) =
            operator, qualify_expression ~scope operand
          in
          ComparisonOperator {
            ComparisonOperator.left = qualify_expression ~scope left;
            right = List.map right ~f:qualify_operand;
          }
      | Dictionary { Dictionary.entries; keywords } ->
          Dictionary {
            Dictionary.entries = List.map entries ~f:(qualify_entry ~scope);
            keywords = keywords >>| qualify_expression ~scope
          }
      | DictionaryComprehension { Comprehension.element; generators } ->
          let scope, generators = qualify_generators ~scope generators in
          DictionaryComprehension {
            Comprehension.element = qualify_entry ~scope element;
            generators;
          }
      | FormatString { FormatString.value; expression_list } ->
          FormatString {
            FormatString.value;
            expression_list = List.map expression_list ~f:(qualify_expression ~scope)
          }
      | Generator { Comprehension.element; generators } ->
          let scope, generators = qualify_generators ~scope generators in
          Generator {
            Comprehension.element = qualify_expression ~scope element;
            generators;
          }
      | Lambda { Lambda.parameters; body } ->
          let scope, parameters = qualify_parameters ~scope parameters in
          Lambda {
            Lambda.parameters;
            body = qualify_expression ~scope body;
          }
      | List elements ->
          List (List.map elements ~f:(qualify_expression ~scope))
      | ListComprehension { Comprehension.element; generators } ->
          let scope, generators = qualify_generators ~scope generators in
          ListComprehension {
            Comprehension.element = qualify_expression ~scope element;
            generators;
          }
      | Set elements ->
          Set (List.map elements ~f:(qualify_expression ~scope))
      | SetComprehension { Comprehension.element; generators } ->
          let scope, generators = qualify_generators ~scope generators in
          SetComprehension {
            Comprehension.element = qualify_expression ~scope element;
            generators;
          }
      | Starred (Starred.Once expression) ->
          Starred (Starred.Once (qualify_expression ~scope expression))
      | Starred (Starred.Twice expression) ->
          Starred (Starred.Twice (qualify_expression ~scope expression))
      | Ternary { Ternary.target; test; alternative } ->
          Ternary {
            Ternary.target = qualify_expression ~scope target;
            test = qualify_expression ~scope test;
            alternative = qualify_expression ~scope alternative;
          }
      | Tuple elements ->
          Tuple (List.map elements ~f:(qualify_expression ~scope))
      | UnaryOperator { UnaryOperator.operator; operand } ->
          UnaryOperator {
            UnaryOperator.operator;
            operand = qualify_expression ~scope operand;
          }
      | Yield (Some expression) ->
          Yield (Some (qualify_expression ~scope expression))
      | Yield None ->
          Yield None
      | Bytes _ | Complex _ | False | Float _ | Integer _ | String _ | True ->
          value
    in
    { expression with Node.value }

  and qualify_target ~scope target =
    let rec renamed_scope scope target =
      match target with
      | { Node.value = Tuple elements; _ } ->
          List.fold elements ~init:scope ~f:renamed_scope
      | { Node.value = Access [Access.Identifier name]; _ } ->
          let scope, _, _ = prefix_identifier ~scope ~prefix:"target" name in
          scope
      | _ ->
          scope
    in
    let scope = renamed_scope scope target in
    scope, qualify_expression ~scope target

  and qualify_access ~scope:({ aliases; use_forward_references; _ } as scope) access =
    match access with
    | head :: tail ->
        let head =
          match Map.find aliases [head] with
          | Some { access; is_forward_reference = true } when use_forward_references -> access
          | Some { access; is_forward_reference = false } -> access
          | _ -> [head]
        in
        let qualify_element = function
          | Access.Call ({ Node.value = arguments ; _ } as call) ->
              let qualify_argument { Argument.name; value } =
                let name =
                  let rename identifier =
                    Identifier.show identifier
                    |> Format.asprintf "$parameter_%s"
                    |> Identifier.create
                  in
                  name
                  >>| Node.map ~f:rename
                in
                { Argument.name; value = qualify_expression ~scope value }
              in
              Access.Call { call with Node.value = List.map arguments ~f:qualify_argument }
          | Access.Expression expression ->
              Access.Expression (qualify_expression ~scope expression)
          | element ->
              element
        in
        List.map (head @ tail) ~f:qualify_element
    | _ ->
        access
  in

  let scope =
    {
      qualifier;
      aliases = Access.Map.empty;
      locals = Access.Set.empty;
      immutables = Access.Set.empty;
      use_forward_references = true;
      skip = Location.Set.empty;
    }
  in
  { source with Source.statements = qualify_statements ~scope statements |> snd }


let replace_version_specific_code source =
  let module Transform = Transform.MakeStatementTransformer(struct
      include Transform.Identity
      type t = unit

      let statement_postorder _ ({ Node.location; value } as statement) =
        match value with
        | If { If.test; body; orelse } ->
            (* Normalizes a comparison of a < b, a <= b, b >= a or b > a to Some (a, b). *)
            let extract_single_comparison { Node.value; _ } =
              match value with
              | Expression.ComparisonOperator {
                  Expression.ComparisonOperator.left;
                  right = [
                    operator,
                    right
                  ];
                } ->
                  begin
                    match operator with
                    | Expression.ComparisonOperator.LessThan
                    | Expression.ComparisonOperator.LessThanOrEquals ->
                        Some (left, right)

                    | Expression.ComparisonOperator.GreaterThan
                    | Expression.ComparisonOperator.GreaterThanOrEquals ->
                        Some (right, left)

                    | _ ->
                        None
                  end
              | _ -> None
            in
            begin
              match extract_single_comparison test with
              | Some (left, { Node.value = Expression.Tuple ({ Node.value = major; _ } :: _); _ })
                when Expression.show left = "sys.version_info" && major = Expression.Integer 3 ->
                  (),
                  if List.is_empty orelse then
                    [Node.create ~location Statement.Pass]
                  else
                    orelse
              | Some ({ Node.value = Expression.Tuple ({ Node.value = major; _ } :: _); _ }, right)
                when Expression.show right = "sys.version_info" && major = Expression.Integer 3 ->
                  (),
                  if List.is_empty body then
                    [Node.create ~location Statement.Pass]
                  else
                    body
              | _ ->
                  (), [statement]
            end
        | _ ->
            (), [statement]
    end)
  in
  Transform.transform () source
  |> snd


let expand_type_checking_imports source =
  let module Transform = Transform.MakeStatementTransformer(struct
      include Transform.Identity
      type t = unit

      let statement_postorder _ ({ Node.value; _ } as statement) =
        let is_type_checking { Node.value; _ } =
          match value with
          | Access [Access.Identifier typing; Access.Identifier type_checking]
            when Identifier.show typing = "typing" &&
                 Identifier.show type_checking = "TYPE_CHECKING" ->
              true
          | Access [Access.Identifier type_checking]
            when Identifier.show type_checking = "TYPE_CHECKING" ->
              true
          | _ ->
              false
        in
        match value with
        | If { If.test; body; _ } when is_type_checking test ->
            (), body
        | _ ->
            (), [statement]
    end)
  in
  Transform.transform () source
  |> snd


let return_access = Access.create "$return"


let expand_returns source =
  let module ExpandingTransform = Transform.MakeStatementTransformer(struct
      include Transform.Identity
      type t = unit

      let statement_postorder state statement =
        match statement with
        (* Expand returns to make them more amenable for analyses. E.g:
           `return x` -> `$return = x; return $return` *)
        | { Node.location; value = Return (Some value) } ->
            let target = { Node.location; value = Access return_access } in
            state,
            [
              {
                Node.location;
                value = Assign {
                    Assign.target;
                    annotation = None;
                    value = Some value;
                    parent = None;
                  };
              };
              { Node.location; value = Return (Some target) };
            ]

        (* Insert implicit return statements at the end of function bodies. *)
        | { Node.location; value = Define define } ->
            let define =
              let has_yield =
                let module Visit = Visit.Make(struct
                    type t = bool

                    let expression sofar _ =
                      sofar

                    let statement sofar = function
                      | { Node.value = Statement.Yield _; _ } -> true
                      | { Node.value = Statement.YieldFrom _; _ } -> true
                      | _ -> sofar
                  end)
                in
                Visit.visit false (Source.create define.Define.body)
              in
              let has_return_in_finally =
                match List.last define.Define.body with
                | Some { Node.value = Try { Try.finally; _ }; _ } ->
                    begin
                      match List.last finally with
                      | Some { Node.value = Return _; _ } ->
                          true
                      | _ ->
                          false
                    end
                | _ ->
                    false
              in
              let loops_forever =
                match List.last define.Define.body with
                | Some { Node.value = While { While.test = { Node.value = True; _ }; _ }; _ } ->
                    true
                | _ ->
                    false
              in
              if has_yield || has_return_in_finally || loops_forever then
                define
              else
                match List.last define.Define.body with
                | Some { Node.value = Return _; _ } ->
                    define
                | Some statement ->
                    {
                      define with
                      Define.body = define.Define.body @ [{
                          Node.location = statement.Node.location;
                          value = Return None;
                        }];
                    }
                | _ ->
                    define
            in
            state,
            [{ Node.location; value = Define define }]
        | _ ->
            state, [statement]
    end)
  in
  ExpandingTransform.transform () source
  |> snd


let expand_ternary_assign source =
  let module ExpandingTransform = Transform.MakeStatementTransformer(struct
      type t = unit

      let statement_postorder state statement =
        match statement with
        | {
          Node.location;
          value = Assign ({
              Assign.value =
                Some { Node.value = Ternary { Ternary.target; test; alternative }; _ };
              _;
            } as assign)
        } ->
            state,
            [
              {
                Node.location;
                value = If {
                    If.test;
                    body = [
                      {
                        Node.location;
                        value = Assign { assign with Assign.value = Some target };
                      }
                    ];
                    orelse = [
                      {
                        Node.location;
                        value = Assign { assign with Assign.value = Some alternative };
                      }
                    ];
                  };
              };
            ]
        | _ ->
            state, [statement]
    end)
  in
  ExpandingTransform.transform () source
  |> snd


let expand_named_tuples ({ Source.qualifier; statements; _ } as source) =
  let tuple_attributes ~parent ~expression =
    match expression with
    | {
      Node.location;
      value =
        Access [
          Access.Identifier module_name;
          Access.Identifier named_tuple;
          Access.Call { Node.value = arguments; _ };
        ];
    } when (Identifier.show module_name = "typing" &&
            Identifier.show named_tuple = "NamedTuple") ||
           (Identifier.show module_name = "collections" &&
            Identifier.show named_tuple = "namedtuple") ->
        let attributes =
          match arguments with
          | [_; { Argument.value = { Node.location; value = String serialized }; _ }] ->
              let attribute name =
                Access (parent @ Access.create name)
                |> Node.create ~location
              in
              String.split serialized ~on:' '
              |> List.map ~f:attribute
          | [_; { Argument.value = { Node.value = List arguments; _ }; _ }] ->
              let rec accessify ({ Node.value; _ } as expression) =
                let value =
                  match value with
                  | String name -> Access (parent @ Access.create name)
                  | Tuple [name; annotation] -> Tuple [accessify name; annotation]
                  | _ -> value
                in
                { expression with Node.value }
              in
              List.map arguments ~f:accessify
          | _ ->
              []
        in
        let attribute ({ Node.location; value } as expression) =
          let target, annotation =
            match value with
            | Tuple [target; annotation] ->
                target, annotation
            | _ ->
                expression, { Node.location; value = Access (Access.create "typing.Any")}
          in
          Assign {
            Assign.target;
            annotation = Some annotation;
            value = None;
            parent = Some parent;
          }
          |> Node.create ~location
        in
        let attributes = List.map attributes ~f:attribute in
        if List.is_empty attributes then
          Some [Node.create ~location Pass]
        else
          Some attributes
    | _ ->
        None
  in
  let tuple_base ~location =
    {
      Argument.name = None;
      value = Node.create ~location (Access (Access.create "typing.NamedTuple"));
    }
  in
  let expand_named_tuples ({ Node.location; value } as statement) =
    let value =
      match value with
      | Assign {
          Assign.target = {
            Node.value = Access name;
            _;
          };
          value = Some expression;
          _;
        } ->
          let name = Access.delocalize name ~qualifier in
          tuple_attributes ~parent:name ~expression
          >>| (fun body ->
              Class {
                Class.name;
                bases = [tuple_base ~location];
                body;
                decorators = [];
                docstring = None;
              })
          |> Option.value ~default:value
      | Class ({ Class.name; bases; body; _; } as original) ->
          let extract_named_tuples (bases, attributes_sofar) ({ Argument.value; _ } as base) =
            tuple_attributes ~parent:name ~expression:value
            >>| (fun attributes -> (tuple_base ~location) :: bases, attributes_sofar @ attributes)
            |> Option.value ~default:(base :: bases, attributes_sofar)
          in
          let reversed_bases, attributes = List.fold bases ~init:([], []) ~f:extract_named_tuples in
          Class {
            original with
            Class.bases = List.rev reversed_bases;
            body = attributes @ body;
          }
      | _ ->
          value
    in
    { statement with Node.value }
  in
  { source with Source.statements = List.map ~f:expand_named_tuples statements }


let defines ?(include_stubs = false) ({ Source.qualifier; statements; _ } as source) =
  let toplevel =
    Node.create_with_default_location (Statement.Define.create_toplevel ~qualifier ~statements)
  in
  let module Collector = Visit.StatementCollector(struct
      type t = Define.t Node.t
      let keep_recursing =
        function
        | { Node.value = Define _; _ }
        | { Node.value = Stub (Stub.Define _); _ }  ->
            Transform.Stop
        | _ ->
            Transform.Recurse


      let predicate = function
        | { Node.location; value = Define define } ->
            Some ({ Node.location; Node.value = define })
        | { Node.location; value = Stub (Stub.Define define) } when include_stubs ->
            Some ({ Node.location; Node.value = define })
        | _ ->
            None
    end)
  in
  toplevel :: (Collector.collect source)


let classes source =
  let module Collector = Visit.StatementCollector(struct
      type t = Statement.Class.t Node.t
      let keep_recursing _ = Transform.Recurse

      let predicate = function
        | { Node.location; value = Class class_define } ->
            Some ({ Node.location; Node.value = class_define })
        | _ ->
            None
    end)
  in
  Collector.collect source


let dequalify_map source =
  let module ImportDequalifier = Transform.MakeStatementTransformer(struct
      include Transform.Identity
      type t = Access.t Access.Map.t

      let statement_postorder map ({ Node.value; _ } as statement) =
        match value with
        | Import { Import.from = None; imports } ->
            let add_import map { Import.name; alias } =
              match alias with
              | Some alias ->
                  (* Add `name -> alias`. *)
                  Map.set map ~key:(List.rev name) ~data:alias
              | None ->
                  map
            in
            List.fold_left imports ~f:add_import ~init:map,
            [statement]
        | Import { Import.from = Some from; imports } ->
            let add_import map { Import.name; alias } =
              match alias with
              | Some alias ->
                  (* Add `alias -> from.name`. *)
                  Map.set map ~key:(List.rev (from @ name)) ~data:alias
              | None ->
                  (* Add `name -> from.name`. *)
                  Map.set map ~key:(List.rev (from @ name)) ~data:name
            in
            List.fold_left imports ~f:add_import ~init:map,
            [statement]
        | _ ->
            map, [statement]
    end)
  in
  (* Note that map keys are reversed accesses because it makes life much easier in dequalify *)
  let map = Map.set ~key:(List.rev source.Source.qualifier) ~data:[] Access.Map.empty in
  ImportDequalifier.transform map source
  |> fst


let preprocess source =
  source
  |> expand_string_annotations
  |> expand_format_string
  |> replace_version_specific_code
  |> expand_type_checking_imports
  |> qualify
  |> expand_returns
  |> expand_ternary_assign
  |> expand_named_tuples
