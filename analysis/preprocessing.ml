(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression
open Pyre
open PyreParser
open Statement


exception MissingWildcardImport


let convert source =
  let module Transform = Transform.Make(struct
      include Transform.Identity
      type t = unit

      let transform_expression_children _ _ = false

      let expression _ expression =
        Expression.convert expression
    end)
  in
  Transform.transform () source
  |> Transform.source


let expand_relative_imports ({ Source.handle; qualifier; _ } as source) =
  let module Transform = Transform.MakeStatementTransformer(struct
      type t = Reference.t

      let statement qualifier { Node.location; value } =
        let value =
          match value with
          | Import { Import.from = Some from; imports }
            when not (String.equal (Reference.show from) "builtins") &&
                 not (String.equal (Reference.show from) "future.builtins") ->
              Import {
                Import.from = Some (Source.expand_relative_import ~handle ~qualifier ~from);
                imports;
              }
          | _ ->
              value
        in
        qualifier, [{ Node.location; value }]
    end)
  in
  Transform.transform qualifier source
  |> Transform.source


let expand_string_annotations ({ Source.handle; _ } as source) =
  let module Transform = Transform.Make(struct
      type t = unit

      let transform_expression_children _ _ = true

      let transform_children _ _ = true

      let rec transform_string_annotation_expression handle =
        let rec transform_expression
            ({
              Node.location = {
                Location.start = { Location.line = start_line; column = start_column};
                _;
              } as location;
              value;
            } as expression) =
          let value =
            let ends_with_literal = function
              | Name.Identifier "Literal" -> true
              | Name.Attribute { attribute = "Literal"; _ } -> true
              | _ -> false
            in
            match value with
            | Name (Name.Attribute { base; attribute }) ->
                Name (Name.Attribute { base = transform_expression base; attribute })
            | Call {
                callee = {
                  Node.value = Name (
                      Name.Attribute {
                        base = { Node.value = Name base; _ };
                        attribute = "__getitem__";
                      });
                  _;
                };
                _;
              } when ends_with_literal base ->
                (* Don't transform arguments in Literals. This will hit any generic type named
                   Literal, but otherwise `from ... import Literal` wouldn't work as this has
                   to be before qualification. *)
                value
            | Call { callee; arguments } ->
                let transform_argument ({ Call.Argument.value; _ } as argument) =
                  { argument with Call.Argument.value = transform_expression value }
                in
                Call {
                  callee = transform_expression callee;
                  arguments = List.map ~f:transform_argument arguments;
                }
            | String { StringLiteral.value; _ } ->
                begin
                  try
                    let parsed =
                      (* Start at column + 1 since parsing begins after
                         the opening quote of the string literal. *)
                      Parser.parse
                        ~start_line
                        ~start_column:(start_column + 1)
                        [value ^ "\n"]
                        ~handle
                    in
                    match parsed with
                    | [{ Node.value = Expression { Node.value = (Name _ as expression); _ }; _ }]
                    | [{ Node.value = Expression { Node.value = (Call _ as expression); _ }; _ }] ->
                        expression
                    | _ ->
                        failwith "Invalid annotation"
                  with
                  | Parser.Error _
                  | Failure _ ->
                      Log.debug
                        "Invalid string annotation `%s` at %a"
                        value
                        Location.Reference.pp
                        location;
                      Name (Name.Identifier "$unparsed_annotation")
                end
            | Tuple elements ->
                Tuple (List.map elements ~f:transform_expression)
            | _ ->
                value
          in
          { expression with Node.value }
        in
        transform_expression

      let statement _ ({ Node.value; _ } as statement) =
        let transform_assign ~assign:({ Assign.annotation; _ } as assign) =
          {
            assign with
            Assign.annotation = annotation >>| transform_string_annotation_expression handle
          }
        in
        let transform_define
            ~define:({ Define.signature = { parameters; return_annotation; _ }; _ } as define) =
          let parameter ({ Node.value = ({ Parameter.annotation; _ } as parameter); _ } as node) =
            {
              node with
              Node.value = {
                parameter with
                Parameter.annotation = annotation >>| transform_string_annotation_expression handle;
              };
            }
          in
          let signature =
            {
              define.signature with
              parameters = List.map parameters ~f:parameter;
              return_annotation =
                return_annotation >>| transform_string_annotation_expression handle;
            }
          in
          {
            define with signature
          }
        in
        let transform_class ~class_statement:({ Class.bases; _ } as class_statement) =
          let transform_base ({ Expression.Call.Argument.value; _ } as base) =
            let value =
              match value with
              | { Node.value = Expression.String _; _ } -> value
              | _ -> transform_string_annotation_expression handle value
            in
            { base with value }
          in
          { class_statement with bases = List.map bases ~f:transform_base }
        in
        let statement =
          let value =
            match value with
            | Assign assign -> Assign (transform_assign ~assign)
            | Define define -> Define (transform_define ~define)
            | Class class_statement -> Class (transform_class ~class_statement)
            | _ -> value
          in
          { statement with Node.value }
        in
        (), [statement]

      let expression _ expression =
        let transform_arguments = function
          | [
            ({
              Call.Argument.name = None;
              value = ({ Node.value = String _; _ } as value)
            } as type_argument);
            value_argument;
          ] ->
              let annotation = transform_string_annotation_expression handle value in
              [{ type_argument with value = annotation }; value_argument]
          | arguments -> arguments
        in
        let value =
          match Node.value expression with
          | Call {
              callee = ({ Node.value = Name (Name.Identifier "cast"); _ } as callee);
              arguments;
            } ->
              Call { callee; arguments = transform_arguments arguments }
          | Call {
              callee = ({
                  Node.value = Name (Name.Attribute {
                      base = { Node.value = Name (Name.Identifier "typing"); _ };
                      attribute = "cast"
                    });
                  _;
                } as callee);
              arguments;
            } ->
              Call { callee; arguments = transform_arguments arguments }
          | value -> value
        in
        { expression with Node.value }
    end)
  in
  Transform.transform () source
  |> Transform.source


let expand_format_string ({ Source.handle; _ } as source) =
  let module Transform = Transform.Make(struct
      include Transform.Identity
      type t = unit

      type state =
        | Literal
        | Expression of int * string

      let expression _ expression =
        match expression with
        | {
          Node.location = ({ Location.start = { Location.line; column }; _ } as location);
          value = String { StringLiteral.value; kind = StringLiteral.Mixed substrings; _ };
        } ->
            let gather_fstring_expressions substrings =
              let gather_expressions_in_substring
                  (current_position, expressions)
                  { StringLiteral.Substring.kind; value } =
                let value_length = String.length value in
                let rec expand_fstring input_string start_position state: ('a list) =
                  if start_position = value_length then
                    []
                  else
                    let token = String.get input_string start_position in
                    let expressions, next_state =
                      match token, state with
                      | '{', Literal ->
                          [], Expression (start_position, "")
                      | '{', Expression (_, "") ->
                          [], Literal
                      | '}', Literal ->
                          [], Literal
                      (* NOTE: this does not account for nested expressions in
                          e.g. format specifiers. *)
                      | '}', Expression (c, string) ->
                          [(column + current_position + c, string)], Literal
                      (* Ignore leading whitespace in expressions. *)
                      | (' ' | '\t'), (Expression (_, "") as expression) ->
                          [], expression
                      | _, Literal ->
                          [], Literal
                      | _, Expression (c, string) ->
                          [], Expression (c, string ^ (Char.to_string token))
                    in
                    let next_expressions =
                      expand_fstring input_string (start_position + 1) next_state
                    in
                    expressions @ next_expressions
                in
                let next_position = current_position + value_length in
                match kind with
                | StringLiteral.Substring.Literal ->
                    next_position, expressions
                | StringLiteral.Substring.Format ->
                    let fstring_expressions = expand_fstring value 0 Literal in
                    next_position, (List.rev fstring_expressions) @ expressions
              in
              List.fold substrings ~init:(0, []) ~f:gather_expressions_in_substring
              |> snd
              |> List.rev
            in
            let parse (start_column, input_string) =
              try
                let string = input_string ^ "\n" in
                match Parser.parse [string ^ "\n"] ~start_line:line ~start_column ~handle with
                | [{ Node.value = Expression expression; _ }] ->
                    [expression]
                | _ ->
                    failwith "Not an expression"
              with
              | Parser.Error _
              | Failure _ ->
                  begin
                    Log.debug
                      "Pyre could not parse format string `%s` at %a"
                      input_string
                      Location.Reference.pp
                      location;
                    []
                  end
            in
            let expressions =
              substrings
              |> gather_fstring_expressions
              |> List.concat_map ~f:parse
            in
            { Node.location; value = String { StringLiteral.kind = Format expressions; value } }
        | _ ->
            expression
    end)
  in
  Transform.transform () source
  |> Transform.source


type alias = {
  name: Reference.t;
  qualifier: Reference.t;
  is_forward_reference: bool;
}


type scope = {
  qualifier: Reference.t;
  aliases: alias Reference.Map.t;
  immutables: Reference.Set.t;
  locals: Reference.Set.t;
  use_forward_references: bool;
  is_top_level: bool;
  skip: Location.Reference.Set.t;
}

let qualify_local_identifier name ~qualifier =
  let qualifier =
    Reference.show qualifier
    |> String.substr_replace_all ~pattern:"." ~with_:"?"
  in
  name
  |> Format.asprintf "$local_%s$%s" qualifier
  |> fun identifier -> Name.Identifier identifier


let qualify ({ Source.handle; qualifier = source_qualifier; statements; _ } as source) =
  let prefix_identifier ~scope:({ aliases; immutables; _ } as scope) ~prefix name =
    let stars, name =
      if String.is_prefix name ~prefix:"**" then
        "**", String.drop_prefix name 2
      else if String.is_prefix name ~prefix:"*" then
        "*", String.drop_prefix name 1
      else
        "", name
    in
    let renamed =
      Format.asprintf "$%s$%s" prefix name
    in
    let reference = Reference.create name in
    {
      scope with
      aliases =
        Map.set
          aliases
          ~key:reference
          ~data:{
            name = Reference.create renamed;
            qualifier = source_qualifier;
            is_forward_reference = false;
          };
      immutables = Set.add immutables reference;
    },
    stars,
    renamed
  in
  let rec explore_scope ~scope statements =
    let global_alias ~qualifier ~name =
      {
        name = Reference.combine qualifier name;
        qualifier;
        is_forward_reference = true;
      }
    in
    let explore_scope
        ({ qualifier; aliases; immutables; skip; _ } as scope)
        { Node.location; value } =
      match value with
      | Assign {
          Assign.target = { Node.value = Name name; _ };
          annotation = Some annotation;
          _;
        }
        when String.equal (Expression.show annotation) "_SpecialForm" ->
          let name = Reference.from_name name in
          {
            scope with
            aliases = Map.set aliases ~key:name ~data:(global_alias ~qualifier ~name);
            skip = Set.add skip location;
          }
      | Class { Class.name; _ } ->
          {
            scope with
            aliases = Map.set aliases ~key:name ~data:(global_alias ~qualifier ~name);
          }
      | Define { Define.signature = { name; _ }; _ } ->
          {
            scope with
            aliases = Map.set aliases ~key:name ~data:(global_alias ~qualifier ~name);
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
              Set.add immutables (Reference.create identifier)
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
  let rec qualify_parameters ~scope parameters =
    (* Rename parameters to prevent aliasing. *)
    let parameters =
      let qualify_annotation { Node.location; value = { Parameter.annotation; _ } as parameter } =
        {
          Node.location;
          value = {
            parameter with
            Parameter.annotation = annotation >>| qualify_expression ~qualify_strings:true ~scope;
          };
        }
      in
      List.map parameters ~f:qualify_annotation
    in
    let rename_parameter
        (scope, reversed_parameters)
        ({ Node.value = { Parameter.name; value; annotation }; _ } as parameter) =
      let scope, stars, renamed = prefix_identifier ~scope ~prefix:"parameter" name in
      scope,
      {
        parameter with
        Node.value = {
          Parameter.name = stars ^ renamed;
          value = value >>| qualify_expression ~qualify_strings:false ~scope;
          annotation;
        };
      } :: reversed_parameters
    in
    let scope, parameters =
      List.fold
        parameters
        ~init:({ scope with locals = Reference.Set.empty }, [])
        ~f:rename_parameter
    in
    scope, List.rev parameters

  and qualify_statements ?(qualify_assigns = false) ~scope statements =
    let scope = explore_scope ~scope statements in
    let scope, reversed_statements =
      let qualify (scope, statements) statement =
        let scope, statement = qualify_statement ~qualify_assign:qualify_assigns ~scope statement in
        scope, statement :: statements
      in
      List.fold statements ~init:(scope, []) ~f:qualify
    in
    scope, List.rev reversed_statements

  and qualify_statement
      ~qualify_assign
      ~scope:({ qualifier; aliases; skip; is_top_level; _ } as scope)
      ({ Node.location; value } as statement) =
    let scope, value =
      let local_alias ~qualifier ~name = { name; qualifier; is_forward_reference = false } in

      let qualify_assign { Assign.target; annotation; value; parent } =
        let value =
          match value with
          | { Node.value = String _; _ } ->
              (* String literal assignments might be type aliases. *)
              qualify_expression ~qualify_strings:is_top_level value ~scope
          | {
            Node.value =
              Call {
                callee = {
                  Node.value = Name (Name.Attribute { attribute = "__getitem__"; _ });
                  _;
                };
                _;
              };
            _;
          } ->
              qualify_expression ~qualify_strings:is_top_level value ~scope
          | _ ->
              qualify_expression ~qualify_strings:false value ~scope
        in
        let target_scope, target =
          if not (Set.mem skip location) then
            let rec qualify_target ~scope:({ aliases; immutables; locals; _ } as scope) target =
              let scope, value =
                let qualify_targets scope elements =
                  let qualify_element (scope, reversed_elements) element =
                    let scope, element = qualify_target ~scope element in
                    scope, element :: reversed_elements
                  in
                  let scope, reversed_elements =
                    List.fold elements ~init:(scope, []) ~f:qualify_element
                  in
                  scope, List.rev reversed_elements
                in
                match Node.value target with
                | Tuple elements ->
                    let scope, elements = qualify_targets scope elements in
                    scope, Tuple elements
                | List elements ->
                    let scope, elements = qualify_targets scope elements in
                    scope, List elements
                | Name (Name.Identifier name) when qualify_assign ->
                    let sanitized = Identifier.sanitized name in
                    let qualified =
                      let qualifier =
                        Node.create
                          ~location:(Node.location target)
                          (Name (Reference.name qualifier))
                      in
                      Name.Attribute { base = qualifier; attribute = sanitized }
                    in
                    let scope =
                      let aliases =
                        let update = function
                          | Some alias -> alias
                          | None -> local_alias ~qualifier ~name:(Reference.from_name qualified)
                        in
                        Map.update aliases (Reference.create name) ~f:update
                      in
                      { scope with aliases }
                    in
                    scope, Name qualified
                | Starred (Starred.Once name) ->
                    let scope, name = qualify_target ~scope name in
                    scope, Starred (Starred.Once name)
                | Name (Name.Identifier name) ->
                    (* Incrementally number local variables to avoid shadowing. *)
                    let scope =
                      let qualified = String.is_prefix name ~prefix:"$" in
                      let reference = Reference.create name in
                      if not qualified &&
                         not (Set.mem locals reference) &&
                         not (Set.mem immutables reference) then
                        let alias =
                          qualify_local_identifier name ~qualifier
                          |> Reference.from_name
                        in
                        {
                          scope with
                          aliases =
                            Map.set
                              aliases
                              ~key:reference
                              ~data:(local_alias ~qualifier ~name:alias);
                          locals = Set.add locals reference;
                        }
                      else
                        scope
                    in
                    scope,
                    qualify_name ~qualify_strings:false ~scope (Name (Name.Identifier name))
                | Name name ->
                    let name =
                      let qualified =
                        match qualify_name ~qualify_strings:false ~scope (Name name) with
                        | Name (Name.Identifier name) ->
                            Name (Name.Identifier (Identifier.sanitized name))
                        | qualified ->
                            qualified
                      in
                      if qualify_assign then
                        let rec combine qualifier = function
                          | Name (Name.Identifier identifier) ->
                              Name (
                                Name.Attribute {
                                  base = Node.create ~location:(Node.location target) qualifier;
                                  attribute = identifier;
                                })
                          | Name (Name.Attribute { base; attribute }) ->
                              Name (
                                Name.Attribute {
                                  base =
                                    Node.create
                                      ~location:(Node.location base)
                                      (combine qualifier (Node.value base));
                                  attribute;
                                })
                          | _ -> failwith "Impossible."
                        in
                        combine (Name (Reference.name qualifier)) qualified
                      else
                        qualified
                    in
                    scope, name
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
          annotation = annotation >>| qualify_expression ~qualify_strings:true ~scope;
          (* Assignments can be type aliases. *)
          value;
          parent = parent >>| fun parent -> qualify_reference ~scope parent;
        }
      in
      let qualify_define
          ({ qualifier; _ } as scope)
          ({
            Define.signature = {
              name;
              parameters;
              decorators;
              return_annotation;
              parent;
              _
            };
            body;
            _;
          } as define) =
        let scope = { scope with is_top_level = false } in
        let return_annotation =
          return_annotation
          >>| qualify_expression ~qualify_strings:true ~scope
        in
        let parent =
          parent
          >>| fun parent -> qualify_reference ~scope parent
        in
        let decorators =
          List.map
            decorators
            ~f:(qualify_expression
                  ~qualify_strings:false
                  ~scope:{ scope with use_forward_references = true })
        in
        let scope, parameters = qualify_parameters ~scope parameters in
        let qualifier = Reference.combine qualifier name in
        let _, body = qualify_statements ~scope:{ scope with qualifier } body in
        let signature =
          {
            define.signature with
            name =
              qualify_reference ~suppress_synthetics:true ~scope name;
            parameters;
            decorators;
            return_annotation;
            parent;
          }
        in
        { define with signature; body }
      in
      let qualify_class ({ Class.name; bases; body; decorators; _ } as definition) =
        let scope = { scope with is_top_level = false } in
        let qualify_base ({ Expression.Call.Argument.value; _ } as argument) =
          {
            argument with
            Expression.Call.Argument.value = qualify_expression ~qualify_strings:false ~scope value
          }
        in
        let decorators =
          List.map
            decorators
            ~f:(qualify_expression ~qualify_strings:false ~scope)
        in
        let body =
          let qualifier = Reference.combine qualifier name in
          let original_scope = { scope with qualifier } in
          let scope = explore_scope body ~scope:original_scope in
          let qualify (scope, statements) ({ Node.location; value } as statement) =
            let scope, statement =
              match value with
              | Define ({
                  signature = { name; parameters; return_annotation; decorators; _ }
                ; _ } as define) ->
                  let define = qualify_define original_scope define in
                  let _, parameters = qualify_parameters ~scope parameters in
                  let return_annotation =
                    return_annotation
                    >>| qualify_expression ~scope ~qualify_strings:true
                  in
                  let qualify_decorator ({ Node.value; _ } as decorator) =
                    match value with
                    | Name (Name.Identifier ("staticmethod" | "classmethod" | "property"))
                    | Name (Name.Attribute { attribute = ("getter" | "setter" | "deleter"); _ }) ->
                        decorator
                    | _ ->
                        (* TODO (T41755857): Decorator qualification logic
                           should be slightly more involved than this. *)
                        qualify_expression ~qualify_strings:false ~scope decorator
                  in
                  let decorators = List.map decorators ~f:qualify_decorator in
                  let signature =
                    {
                      define.signature with
                      name = qualify_reference ~scope name;
                      parameters;
                      decorators;
                      return_annotation;
                    }
                  in
                  scope, {
                    Node.location;
                    value = Define { define with signature };
                  }
              | _ ->
                  qualify_statement statement ~qualify_assign:true ~scope
            in
            scope, statement :: statements
          in
          List.fold body ~init:(scope, []) ~f:qualify
          |> snd
          |> List.rev
        in
        {
          definition with
          (* Ignore aliases, imports, etc. when declaring a class name. *)
          Class.name = Reference.combine scope.qualifier name;
          bases = List.map bases ~f:qualify_base;
          body;
          decorators;
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
      | Assert { Assert.test; message; origin } ->
          scope,
          Assert {
            Assert.test = qualify_expression ~qualify_strings:false ~scope test;
            message;
            origin;
          }
      | Class ({ name; _ } as definition) ->
          let scope = {
            scope with
            aliases =
              Map.set
                aliases
                ~key:name
                ~data:(local_alias ~qualifier ~name:(Reference.combine qualifier name));
          }
          in
          scope,
          Class (qualify_class definition)
      | Define define ->
          scope,
          Define (qualify_define scope define)
      | Delete expression ->
          scope,
          Delete (qualify_expression ~qualify_strings:false ~scope expression)
      | Expression expression ->
          scope,
          Expression (qualify_expression ~qualify_strings:false ~scope expression)
      | For ({ For.target; iterator; body; orelse; _ } as block) ->
          let renamed_scope, target = qualify_target ~scope target in
          let body_scope, body = qualify_statements ~scope:renamed_scope body in
          let orelse_scope, orelse = qualify_statements ~scope:renamed_scope orelse in
          join_scopes body_scope orelse_scope,
          For {
            block with
            For.target;
            iterator = qualify_expression ~qualify_strings:false ~scope iterator;
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
          If { If.test = qualify_expression ~qualify_strings:false ~scope test; body; orelse }
      | Import { Import.from = Some from; imports }
        when not (String.equal (Reference.show from) "builtins") ->
          let import aliases { Import.name; alias } =
            match alias with
            | Some alias ->
                (* Add `alias -> from.name`. *)
                Map.set aliases
                  ~key:alias
                  ~data:(local_alias ~qualifier ~name:(Reference.combine from name))
            | None ->
                (* Add `name -> from.name`. *)
                Map.set aliases
                  ~key:name
                  ~data:(local_alias ~qualifier ~name:(Reference.combine from name))
          in
          { scope with aliases = List.fold imports ~init:aliases ~f:import },
          value
      | Import { Import.from = None; imports } ->
          let import aliases { Import.name; alias } =
            match alias with
            | Some alias ->
                (* Add `alias -> from.name`. *)
                Map.set aliases ~key:alias ~data:(local_alias ~qualifier ~name)
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
          Raise (expression >>| qualify_expression ~qualify_strings:false ~scope)
      | Return ({ Return.expression; _ } as return) ->
          scope,
          Return {
            return with
            Return.expression = expression >>| qualify_expression ~qualify_strings:false ~scope;
          }
      | Try { Try.body; handlers; orelse; finally } ->
          let body_scope, body = qualify_statements ~scope body in
          let handler_scopes, handlers =
            let qualify_handler { Try.kind; name; handler_body } =
              let renamed_scope, name =
                match name with
                | Some name ->
                    let scope, _, renamed = prefix_identifier ~scope ~prefix:"target" name in
                    scope, Some renamed
                | _ ->
                    scope, name
              in
              let kind = kind >>| qualify_expression ~qualify_strings:false ~scope in
              let scope, handler_body = qualify_statements ~scope:renamed_scope handler_body in
              scope, { Try.kind; name; handler_body }
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
                (qualify_expression ~qualify_strings:false ~scope name, alias)
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
          While { While.test = qualify_expression ~qualify_strings:false ~scope test; body; orelse }
      | Statement.Yield expression ->
          scope,
          Statement.Yield (qualify_expression ~qualify_strings:false ~scope expression)
      | Statement.YieldFrom expression ->
          scope,
          Statement.YieldFrom (qualify_expression ~qualify_strings:false ~scope expression)
      | Break | Continue | Import _ | Pass ->
          scope,
          value
    in
    scope, { statement with Node.value }

  and qualify_target ~scope target =
    let rec renamed_scope ({ locals; _ } as scope) target =
      match target with
      | { Node.value = Tuple elements; _ } ->
          List.fold elements ~init:scope ~f:renamed_scope
      | { Node.value = Name (Name.Identifier name); _ } ->
          if Set.mem locals (Reference.from_name (Name.Identifier name)) then
            scope
          else
            let scope, _, _ = prefix_identifier ~scope ~prefix:"target" name in
            scope
      | _ ->
          scope
    in
    let scope = renamed_scope scope target in
    scope, qualify_expression ~qualify_strings:false ~scope target

  and qualify_reference
      ?(suppress_synthetics = false)
      ~scope:{ aliases; use_forward_references; _ }
      reference =
    match Reference.as_list reference with
    | [] ->
        Reference.empty
    | head :: tail ->
        match Map.find aliases (Reference.create head) with
        | Some { name; is_forward_reference; qualifier }
          when (not is_forward_reference) || use_forward_references ->
            if Reference.show name |> String.is_prefix ~prefix:"$" &&
               suppress_synthetics then
              Reference.combine qualifier reference
            else
              Reference.combine name (Reference.create_from_list tail)
        | _ ->
            reference

  and qualify_name
      ?(suppress_synthetics = false)
      ~qualify_strings
      ~scope:({ aliases; use_forward_references; _ } as scope) = function
    | Name (Name.Identifier identifier) ->
        begin
          match Map.find aliases (Reference.create identifier) with
          | Some { name; is_forward_reference; qualifier }
            when (not is_forward_reference) || use_forward_references ->
              if Reference.show name |> String.is_prefix ~prefix:"$" &&
                 suppress_synthetics then
                Name (
                  Name.Attribute {
                    base = Reference.expression qualifier;
                    attribute = identifier
                  })
              else
                Node.value (Reference.expression name)
          | _ ->
              Name (Name.Identifier identifier)
        end
    | Name (Name.Attribute { base; attribute }) ->
        Name (
          Name.Attribute {
            base = qualify_expression ~qualify_strings ~scope base;
            attribute;
          })
    | expression ->
        expression

  and qualify_expression ~qualify_strings ~scope ({ Node.location; value } as expression) =
    let value =
      let qualify_entry ~qualify_strings ~scope { Dictionary.key; value } =
        {
          Dictionary.key = qualify_expression ~qualify_strings ~scope key;
          value = qualify_expression ~qualify_strings ~scope value;
        }
      in
      let qualify_generators ~qualify_strings ~scope generators =
        let qualify_generator
            (scope, reversed_generators)
            ({ Comprehension.target; iterator; conditions; _ } as generator) =
          let renamed_scope, target = qualify_target ~scope target in
          renamed_scope,
          {
            generator with
            Comprehension.target;
            iterator = qualify_expression ~qualify_strings ~scope iterator;
            conditions =
              List.map
                conditions
                ~f:(qualify_expression ~qualify_strings ~scope:renamed_scope);
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
      | Access expression ->
          (* Deprecated. *)
          Access expression
      | Await expression ->
          Await (qualify_expression ~qualify_strings ~scope expression)
      | BooleanOperator { BooleanOperator.left; operator; right } ->
          BooleanOperator {
            BooleanOperator.left = qualify_expression ~qualify_strings ~scope left;
            operator;
            right = qualify_expression ~qualify_strings ~scope right;
          }
      | Call { callee; arguments } ->
          let callee = qualify_expression ~qualify_strings ~scope callee in
          let qualify_argument { Call.Argument.name; value } =
            let qualify_strings =
              match Node.value callee with
              | Name (
                  Name.Attribute {
                    base = { Node.value = Name (Name.Identifier "typing"); _ };
                    attribute = "TypeVar";
                  }
                ) ->
                  true
              | Name (
                  Name.Attribute {
                    base =
                      {
                        Node.value = Name (
                            Name.Attribute
                              {
                                base = {
                                  Node.value = Name (Name.Identifier "typing_extensions");
                                  _ };
                                attribute = "Literal";
                              }
                          );
                        _
                      };
                    attribute = "__getitem__";
                  }
                ) ->
                  false
              | _ ->
                  qualify_strings
            in
            let name =
              let rename identifier = "$parameter$" ^ identifier in
              name
              >>| Node.map ~f:rename
            in
            { Call.Argument.name; value = qualify_expression ~qualify_strings ~scope value }
          in
          Call { callee; arguments = List.map ~f:qualify_argument arguments }
      | ComparisonOperator { ComparisonOperator.left; operator; right } ->
          ComparisonOperator {
            ComparisonOperator.left = qualify_expression ~qualify_strings ~scope left;
            operator;
            right = qualify_expression ~qualify_strings ~scope right;
          }
      | Dictionary { Dictionary.entries; keywords } ->
          Dictionary {
            Dictionary.entries = List.map entries ~f:(qualify_entry ~qualify_strings ~scope);
            keywords = List.map keywords ~f:(qualify_expression ~qualify_strings ~scope);
          }
      | DictionaryComprehension { Comprehension.element; generators } ->
          let scope, generators = qualify_generators ~qualify_strings ~scope generators in
          DictionaryComprehension {
            Comprehension.element = qualify_entry ~qualify_strings ~scope element;
            generators;
          }
      | Generator { Comprehension.element; generators } ->
          let scope, generators = qualify_generators ~qualify_strings ~scope generators in
          Generator {
            Comprehension.element = qualify_expression ~qualify_strings ~scope element;
            generators;
          }
      | Lambda { Lambda.parameters; body } ->
          let scope, parameters = qualify_parameters ~scope parameters in
          Lambda {
            Lambda.parameters;
            body = qualify_expression ~qualify_strings ~scope body;
          }
      | List elements ->
          List (List.map elements ~f:(qualify_expression ~qualify_strings ~scope))
      | ListComprehension { Comprehension.element; generators } ->
          let scope, generators = qualify_generators ~qualify_strings ~scope generators in
          ListComprehension {
            Comprehension.element = qualify_expression ~qualify_strings ~scope element;
            generators;
          }
      | Name _ ->
          qualify_name ~qualify_strings ~scope value
      | Set elements ->
          Set (List.map elements ~f:(qualify_expression ~qualify_strings ~scope))
      | SetComprehension { Comprehension.element; generators } ->
          let scope, generators = qualify_generators ~qualify_strings ~scope generators in
          SetComprehension {
            Comprehension.element = qualify_expression ~qualify_strings ~scope element;
            generators;
          }
      | Starred (Starred.Once expression) ->
          Starred (Starred.Once (qualify_expression ~qualify_strings ~scope expression))
      | Starred (Starred.Twice expression) ->
          Starred (Starred.Twice (qualify_expression ~qualify_strings ~scope expression))
      | String { StringLiteral.value; kind } ->
          begin
            let kind =
              match kind with
              | StringLiteral.Format expressions ->
                  StringLiteral.Format
                    (List.map expressions ~f:(qualify_expression ~qualify_strings ~scope))
              | _ ->
                  kind
            in
            if qualify_strings then
              try
                match Parser.parse [value ^ "\n"] ~handle with
                | [{ Node.value = Expression expression; _ }] ->
                    qualify_expression ~qualify_strings ~scope expression
                    |> Expression.show
                    |> fun value -> String { StringLiteral.value; kind }
                | _ ->
                    failwith "Not an expression"
              with
              | Parser.Error _
              | Failure _ ->
                  begin
                    Log.debug
                      "Invalid string annotation `%s` at %a"
                      value
                      Location.Reference.pp
                      location;
                    String { StringLiteral.value; kind }
                  end
            else
              String { StringLiteral.value; kind }
          end
      | Ternary { Ternary.target; test; alternative } ->
          Ternary {
            Ternary.target = qualify_expression ~qualify_strings ~scope target;
            test = qualify_expression ~qualify_strings ~scope test;
            alternative = qualify_expression ~qualify_strings ~scope alternative;
          }
      | Tuple elements ->
          Tuple (List.map elements ~f:(qualify_expression ~qualify_strings ~scope))
      | UnaryOperator { UnaryOperator.operator; operand } ->
          UnaryOperator {
            UnaryOperator.operator;
            operand = qualify_expression ~qualify_strings ~scope operand;
          }
      | Yield (Some expression) ->
          Yield (Some (qualify_expression ~qualify_strings ~scope expression))
      | Yield None ->
          Yield None
      | Complex _ | Ellipsis | False | Float _ | Integer _ | True ->
          value
    in
    { expression with Node.value }
  in

  let scope =
    {
      qualifier = source_qualifier;
      aliases = Reference.Map.empty;
      locals = Reference.Set.empty;
      immutables = Reference.Set.empty;
      use_forward_references = true;
      is_top_level = true;
      skip = Location.Reference.Set.empty;
    }
  in
  { source with Source.statements = qualify_statements ~scope statements |> snd }


let replace_version_specific_code source =
  let module Transform = Transform.MakeStatementTransformer(struct
      include Transform.Identity
      type t = unit

      type operator =
        | Equality of Expression.t * Expression.t
        | Comparison of Expression.t * Expression.t
        | Neither

      let statement _ ({ Node.location; value } as statement) =
        match value with
        | If { If.test; body; orelse } ->
            (* Normalizes a comparison of a < b, a <= b, b >= a or b > a to Some (a, b). *)
            let extract_single_comparison { Node.value; _ } =
              match value with
              | Expression.ComparisonOperator {
                  Expression.ComparisonOperator.left;
                  operator;
                  right;
                } ->
                  begin
                    match operator with
                    | Expression.ComparisonOperator.LessThan
                    | Expression.ComparisonOperator.LessThanOrEquals ->
                        Comparison (left, right)

                    | Expression.ComparisonOperator.GreaterThan
                    | Expression.ComparisonOperator.GreaterThanOrEquals ->
                        Comparison (right, left)

                    | Expression.ComparisonOperator.Equals ->
                        Equality (left, right)
                    | _ ->
                        Neither
                  end
              | _ ->
                  Neither
            in
            let add_pass_statement ~location body =
              if List.is_empty body then
                [Node.create ~location Statement.Pass]
              else
                body
            in
            begin
              match extract_single_comparison test with
              | Comparison
                  (left,
                   {
                     Node.value = Expression.Tuple ({ Node.value = Expression.Integer 3; _ } :: _);
                     _;
                   })
                when String.equal (Expression.show left) "sys.version_info" ->
                  (), add_pass_statement ~location orelse
              | Comparison (left, { Node.value = Expression.Integer 3; _ })
                when String.equal (Expression.show left) "sys.version_info[0]" ->
                  (), add_pass_statement ~location orelse
              | Comparison
                  ({ Node.value = Expression.Tuple ({ Node.value = major; _ } :: _); _ }, right)
                when String.equal (Expression.show right) "sys.version_info" &&
                     Expression.equal_expression major (Expression.Integer 3) ->
                  (), add_pass_statement ~location body
              | Comparison ({ Node.value = Expression.Integer 3; _ }, right)
                when String.equal (Expression.show right) "sys.version_info[0]" ->
                  (), add_pass_statement ~location body
              | Equality (left, right)
                when String.is_prefix ~prefix:"sys.version_info" (Expression.show left) ||
                     String.is_prefix ~prefix:"sys.version_info" (Expression.show right) ->
                  (* Never pin our stubs to a python version. *)
                  (), add_pass_statement ~location orelse
              | _ ->
                  (), [statement]
            end
        | _ ->
            (), [statement]
    end)
  in
  Transform.transform () source
  |> Transform.source


let replace_platform_specific_code source =
  let module Transform = Transform.MakeStatementTransformer(struct
      include Transform.Identity
      type t = unit

      let statement _ ({ Node.location; value } as statement) =
        match value with
        | If { If.test = { Node.value = test; _ }; body; orelse } ->
            begin
              let statements =
                let statements =
                  let open Expression in
                  let matches_removed_platform left right =
                    let is_platform expression =
                      String.equal (Expression.show expression) "sys.platform"
                    in
                    let is_win32 { Node.value; _ } =
                      match value with
                      | String { StringLiteral.value = "win32"; _ } ->
                          true
                      | _ ->
                          false
                    in
                    (is_platform left && is_win32 right) or (is_platform right && is_win32 left)
                  in
                  match test with
                  | ComparisonOperator {
                      ComparisonOperator.left;
                      operator;
                      right;
                    } when matches_removed_platform left right ->
                      begin
                        match operator with
                        | ComparisonOperator.Equals
                        | Is ->
                            orelse
                        | NotEquals
                        | IsNot ->
                            body
                        | _ ->
                            [statement]
                      end
                  | _ ->
                      [statement]
                in
                if not (List.is_empty statements) then
                  statements
                else
                  [Node.create ~location Statement.Pass]
              in
              (), statements
            end
        | _ ->
            (), [statement]
    end)
  in
  Transform.transform () source
  |> Transform.source


let expand_type_checking_imports source =
  let module Transform = Transform.MakeStatementTransformer(struct
      include Transform.Identity
      type t = unit

      let statement _ ({ Node.value; _ } as statement) =
        let is_type_checking { Node.value; _ } =
          match value with
          | Name (
              Name.Attribute {
                base = { Node.value = Name (Name.Identifier "typing"); _ };
                attribute = "TYPE_CHECKING";
              })
          | Name (Name.Identifier "TYPE_CHECKING") ->
              true
          | _ ->
              false
        in
        match value with
        | If { If.test; body; _ } when is_type_checking test ->
            (), body
        | If {
            If.test =
              {
                Node.value = UnaryOperator { UnaryOperator.operator = Not; operand };
                _;
              };
            orelse;
            _;
          } when is_type_checking operand ->
            (), orelse
        | _ ->
            (), [statement]
    end)
  in
  Transform.transform () source
  |> Transform.source


let expand_wildcard_imports ~force source =
  let module Transform = Transform.MakeStatementTransformer(struct
      include Transform.Identity
      type t = unit

      let statement state ({ Node.value; _ } as statement) =
        match value with
        | Import { Import.from = Some from; imports }
          when List.exists imports
              ~f:(fun { Import.name; _ } -> String.equal (Reference.show name) "*") ->
            let expanded_import =
              match Ast.SharedMemory.Modules.get_exports ~qualifier:from with
              | Some exports ->
                  exports
                  |> List.map ~f:(fun name -> { Import.name; alias = None })
                  |> (fun expanded -> Import { Import.from = Some from; imports = expanded })
                  |> (fun value -> { statement with Node.value })
              | None ->
                  if force then
                    statement
                  else
                    raise MissingWildcardImport
            in
            state, [expanded_import]
        | _ ->
            state, [statement]
    end)
  in
  Transform.transform () source
  |> Transform.source


let expand_implicit_returns source =
  let module ExpandingTransform = Transform.MakeStatementTransformer(struct
      include Transform.Identity
      type t = unit

      let statement state statement =
        match statement with
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
                          value = Return { Return.expression = None; is_implicit = true };
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
  |> ExpandingTransform.source


let defines
    ?(include_stubs = false)
    ?(include_nested = false)
    ?(extract_into_toplevel = false)
    ({ Source.qualifier; statements; _ } as source) =

  let module Collector = Visit.StatementCollector(struct
      type t = Define.t Node.t

      let visit_children = function
        | { Node.value = Define _; _ } -> include_nested
        | _ -> true

      let predicate = function
        | { Node.location; value = Define define } when Define.is_stub define ->
            if include_stubs then
              Some ({ Node.location; Node.value = define })
            else
              None
        | { Node.location; value = Define define } ->
            Some ({ Node.location; Node.value = define })
        | _ ->
            None
    end)
  in
  let defines = (Collector.collect source) in
  if extract_into_toplevel then
    let toplevel =
      Node.create_with_default_location
        (Statement.Define.create_toplevel ~qualifier:(Some qualifier) ~statements)
    in
    toplevel :: defines
  else
    defines


let classes source =
  let module Collector = Visit.StatementCollector(struct
      type t = Statement.Class.t Node.t

      let visit_children _ =
        true

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
      type t = Reference.t Reference.Map.t

      let statement map ({ Node.value; _ } as statement) =
        match value with
        | Import { Import.from = None; imports } ->
            let add_import map { Import.name; alias } =
              match alias with
              | Some alias ->
                  (* Add `name -> alias`. *)
                  Map.set map ~key:name ~data:alias
              | None ->
                  map
            in
            List.fold_left imports ~f:add_import ~init:map,
            [statement]
        | Import { Import.from = Some from; imports } ->
            let add_import map { Import.name; alias } =
              match alias with
              | Some alias ->
                  (* Add `from.name -> alias`. *)
                  Map.set map ~key:(Reference.combine from name) ~data:alias
              | None ->
                  (* Add `from.name -> name`. *)
                  Map.set map ~key:(Reference.combine from name) ~data:name
            in
            List.fold_left imports ~f:add_import ~init:map,
            [statement]
        | _ ->
            map, [statement]
    end)
  in
  let map = Map.set ~key:source.Source.qualifier ~data:Reference.empty Reference.Map.empty in
  ImportDequalifier.transform map source
  |> fun { ImportDequalifier.state; _ } -> state


let replace_mypy_extensions_stub ({ Source.handle; statements; _ } as source) =
  if String.is_suffix (File.Handle.show handle) ~suffix:"mypy_extensions.pyi" then
    let typed_dictionary_stub ~location =
      let node value = Node.create ~location value in
      Assign {
        target = node (Name (Name.Identifier "TypedDict"));
        annotation = Some (
            node (
              Name (
                Name.Attribute {
                  base = { Node.value = Name (Name.Identifier "typing"); location };
                  attribute = "_SpecialForm"
                })
            )
          );
        value = node Ellipsis;
        parent = None;
      } |> node
    in
    let replace_typed_dictionary_define = function
      | {
        Node.location;
        value = Define { signature = { name; _ }; _ }
      } when String.equal (Reference.show name) "TypedDict" ->
          typed_dictionary_stub ~location
      | statement ->
          statement
    in
    { source with statements = List.map ~f:replace_typed_dictionary_define statements }
  else
    source


let expand_typed_dictionary_declarations ({ Source.statements; qualifier; _ } as source) =
  let expand_typed_dictionaries ({ Node.location; value } as statement) =
    let expanded_declaration =
      let typed_dictionary_declaration_assignment ~name ~fields ~target ~parent ~total =
        let arguments =
          let fields =
            let tuple (key, value) = Node.create (Expression.Tuple [key; value]) ~location in
            List.map fields ~f:tuple
          in
          let total =
            Node.create (if total then Expression.True else Expression.False) ~location
          in
          [{
            Call.Argument.name = None;
            value = Node.create (Expression.Tuple (name :: total :: fields)) ~location;
          }]
        in
        let name =
          Call {
            callee = {
              Node.location;
              value = Name (
                  Name.Attribute {
                    base = {
                      Node.location;
                      value = Name (
                          Name.Attribute {
                            base = {
                              Node.location;
                              value = Name (Name.Identifier "mypy_extensions");
                            };
                            attribute = "TypedDict";
                          }
                        );
                    };
                    attribute = "__getitem__";
                  });
            };
            arguments;
          }
          |> Node.create ~location
        in
        let annotation =
          Call {
            callee = {
              Node.location;
              value = Name (
                  Name.Attribute {
                    base = {
                      Node.location;
                      value = Name (
                          Name.Attribute {
                            base = {
                              Node.location;
                              value = Name (Name.Identifier "typing");
                            };
                            attribute = "Type";
                          }
                        );
                    };
                    attribute = "__getitem__";
                  });
            };
            arguments = [{ Call.Argument.name = None; value = name }];
          }
          |> Node.create ~location
          |> Option.some
        in
        Assign {
          target;
          annotation;
          value = name;
          parent;
        };
      in
      let extract_totality arguments =
        let is_total ~total = String.equal (Identifier.sanitized total) "total" in
        List.find_map arguments ~f:(function
            | {
              Expression.Call.Argument.name = Some { value = total; _ };
              value = { Node.value = Expression.True; _ }
            } when is_total ~total ->
                Some true
            | {
              Expression.Call.Argument.name = Some { value = total; _ };
              value = { Node.value = Expression.False; _ }
            } when is_total ~total ->
                Some false
            | _ ->
                None)
        |> Option.value ~default:true
      in
      match value with
      | Assign {
          target;
          value = {
            Node.value =
              Call {
                callee = {
                  Node.value = Name (
                      Name.Attribute {
                        base = {
                          Node.value = Name (Name.Identifier "mypy_extensions");
                          _;
                        };
                        attribute = "TypedDict";
                      }
                    );
                  _;
                };
                arguments =
                  { Call.Argument.name = None; value = name } ::
                  {
                    Call.Argument.name = None;
                    value = { Node.value = Dictionary { Dictionary.entries; _ }; _ };
                    _;
                  } ::
                  argument_tail;
              };
            _;
          };
          parent;
          _;
        } ->
          typed_dictionary_declaration_assignment
            ~name
            ~fields:(List.map entries ~f:(fun { Dictionary.key; value } -> key, value))
            ~target
            ~parent
            ~total:(extract_totality argument_tail)
      | Class {
          name = class_name;
          bases =
            {
              Expression.Call.Argument.name = None;
              value = {
                Node.value =
                  Name (
                    Name.Attribute {
                      base = { Node.value = Name (Name.Identifier "mypy_extensions"); _ };
                      attribute = "TypedDict"
                    });
                _;
              };
            } :: bases_tail;
          body;
          decorators = _;
          docstring = _;
        } ->
          let string_literal identifier =
            Expression.String { value = identifier; kind = StringLiteral.String }
            |> Node.create ~location
          in
          let fields =
            let extract = function
              | {
                Node.value =
                  Assign {
                    target = { Node.value = Name name; _ };
                    annotation = Some annotation;
                    value = { Node.value = Ellipsis; _ };
                    parent = _;
                  };
                _;
              } ->
                  Reference.drop_prefix ~prefix:class_name (Reference.from_name name)
                  |> Reference.single
                  >>| (fun name -> string_literal name, annotation)
              | _ ->
                  None
            in
            List.filter_map body ~f:extract
          in
          let declaration class_name =
            let qualified =
              let qualifier =
                Reference.show qualifier
                |> String.substr_replace_all ~pattern:"." ~with_:"?"
              in
              class_name
              |> Format.asprintf "$local_%s$%s" qualifier
              |> fun identifier -> Name (Name.Identifier identifier)
            in
            typed_dictionary_declaration_assignment
              ~name:(string_literal class_name)
              ~fields
              ~target:(Node.create qualified ~location)
              ~parent:None
              ~total:(extract_totality bases_tail)
          in
          Reference.drop_prefix ~prefix:qualifier class_name
          |> Reference.single
          >>| declaration
          |> Option.value ~default:value
      | _ ->
          value
    in
    { statement with Node.value = expanded_declaration }
  in
  { source with Source.statements = List.map ~f:expand_typed_dictionaries statements }


let preprocess_steps ~force source =
  source
  |> expand_relative_imports
  |> expand_string_annotations
  |> expand_format_string
  |> replace_platform_specific_code
  |> replace_version_specific_code
  |> expand_type_checking_imports
  |> expand_wildcard_imports ~force
  |> qualify
  |> expand_implicit_returns
  |> replace_mypy_extensions_stub
  |> expand_typed_dictionary_declarations


let preprocess source =
  preprocess_steps ~force:true source


let try_preprocess source =
  match preprocess_steps ~force:false source with
  | source ->
      Some source
  | exception MissingWildcardImport ->
      None
