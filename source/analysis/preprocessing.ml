(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Expression
open Pyre
open PyreParser
open Statement

let expand_relative_imports
    ({ Source.source_path = { SourcePath.qualifier; _ } as source_path; _ } as source)
  =
  let module Transform = Transform.MakeStatementTransformer (struct
    type t = Reference.t

    let statement qualifier { Node.location; value } =
      let value =
        match value with
        | Statement.Import { Import.from = Some from; imports }
          when (not (String.equal (Reference.show (Node.value from)) "builtins"))
               && not (String.equal (Reference.show (Node.value from)) "future.builtins") ->
            Statement.Import
              { Import.from = Some (SourcePath.expand_relative_import source_path ~from); imports }
        | _ -> value
      in
      qualifier, [{ Node.location; value }]
  end)
  in
  Transform.transform qualifier source |> Transform.source


let transform_string_annotation_expression ~relative =
  let rec transform_expression
      ( {
          Node.location =
            { Location.start = { Location.line = start_line; column = start_column }; _ } as
            location;
          value;
        } as expression )
    =
    let transform_argument ({ Call.Argument.value; _ } as argument) =
      { argument with Call.Argument.value = transform_expression value }
    in
    let value =
      match value with
      | Expression.Name (Name.Attribute ({ base; _ } as name)) ->
          Expression.Name (Name.Attribute { name with base = transform_expression base })
      | Call
          {
            callee =
              { Node.value = Name (Name.Attribute { base; attribute = "__getitem__"; _ }); _ } as
              callee;
            arguments;
          } -> (
          match base with
          | {
           Node.value =
             Expression.Name
               (Name.Attribute
                 {
                   base =
                     {
                       Node.value =
                         Expression.Name
                           (Name.Identifier "typing" | Name.Identifier "typing_extensions");
                       _;
                     };
                   attribute = "Literal";
                   _;
                 });
           _;
          } ->
              (* Don't transform arguments in Literals. *)
              value
          | _ -> Call { callee; arguments = List.map ~f:transform_argument arguments } )
      | Expression.Call { callee; arguments = variable_name :: remaining_arguments }
        when name_is ~name:"typing.TypeVar" callee
             || name_is ~name:"$local_typing$TypeVar" callee
             || name_is ~name:"typing_extensions.IntVar" callee ->
          Expression.Call
            {
              callee;
              arguments = variable_name :: List.map ~f:transform_argument remaining_arguments;
            }
      | String { StringLiteral.value = string_value; _ } -> (
          try
            let parsed =
              (* Start at column + 1 since parsing begins after the opening quote of the string
                 literal. *)
              Parser.parse
                ~start_line
                ~start_column:(start_column + 1)
                [string_value ^ "\n"]
                ~relative
            in
            match parsed with
            | [{ Node.value = Expression { Node.value = Name _ as expression; _ }; _ }]
            | [{ Node.value = Expression { Node.value = Call _ as expression; _ }; _ }] ->
                expression
            | _ -> failwith "Invalid annotation"
          with
          | Parser.Error _
          | Failure _ ->
              Log.debug
                "Invalid string annotation `%s` at %s:%a"
                string_value
                relative
                Location.pp
                location;
              (* TODO(T76231928): replace this silent ignore with something typeCheck.ml can use *)
              value )
      | Tuple elements -> Tuple (List.map elements ~f:transform_expression)
      | _ -> value
    in
    { expression with Node.value }
  in
  transform_expression


let transform_annotations ~transform_annotation_expression source =
  let module Transform = Transform.Make (struct
    type t = unit

    let transform_expression_children _ _ = true

    let transform_children _ _ = true

    let statement _ ({ Node.value; _ } as statement) =
      let transform_assign ~assign:({ Assign.annotation; _ } as assign) =
        { assign with Assign.annotation = annotation >>| transform_annotation_expression }
      in
      let transform_define ({ Define.signature = { parameters; return_annotation; _ }; _ } as define)
        =
        let parameter ({ Node.value = { Parameter.annotation; _ } as parameter; _ } as node) =
          {
            node with
            Node.value =
              {
                parameter with
                Parameter.annotation = annotation >>| transform_annotation_expression;
              };
          }
        in
        let signature =
          {
            define.signature with
            parameters = List.map parameters ~f:parameter;
            return_annotation = return_annotation >>| transform_annotation_expression;
          }
        in
        { define with signature }
      in
      let transform_class ~class_statement:({ Class.bases; _ } as class_statement) =
        let transform_base ({ Call.Argument.value; _ } as base) =
          let value = transform_annotation_expression value in
          { base with value }
        in
        { class_statement with bases = List.map bases ~f:transform_base }
      in
      let statement =
        let value =
          match value with
          | Statement.Assign assign -> Statement.Assign (transform_assign ~assign)
          | Define define -> Define (transform_define define)
          | Class class_statement -> Class (transform_class ~class_statement)
          | _ -> value
        in
        { statement with Node.value }
      in
      (), [statement]


    let expression _ expression =
      let transform_arguments = function
        | [
            ( { Call.Argument.name = None; value = { Node.value = String _; _ } as value } as
            type_argument );
            value_argument;
          ] ->
            let annotation = transform_annotation_expression value in
            [{ type_argument with value = annotation }; value_argument]
        | arguments -> arguments
      in
      let value =
        match Node.value expression with
        | Expression.Call { callee; arguments }
          when name_is ~name:"pyre_extensions.safe_cast" callee
               || name_is ~name:"typing.cast" callee
               || name_is ~name:"cast" callee
               || name_is ~name:"safe_cast" callee ->
            Expression.Call { callee; arguments = transform_arguments arguments }
        | value -> value
      in
      { expression with Node.value }
  end)
  in
  Transform.transform () source |> Transform.source


let expand_string_annotations ({ Source.source_path = { SourcePath.relative; _ }; _ } as source) =
  transform_annotations
    ~transform_annotation_expression:(transform_string_annotation_expression ~relative)
    source


let expand_strings_in_annotation_expression =
  transform_string_annotation_expression ~relative:"$some_path"


let expand_format_string ({ Source.source_path = { SourcePath.relative; _ }; _ } as source) =
  let module Transform = Transform.Make (struct
    include Transform.Identity

    type t = unit

    type state =
      | Literal
      | Expression of int * int * string

    let expression _ expression =
      match expression with
      | {
       Node.location;
       value = Expression.String { StringLiteral.value; kind = StringLiteral.Mixed substrings; _ };
      } ->
          let gather_fstring_expressions substrings =
            let gather_expressions_in_substring
                expressions
                {
                  Node.value = { Substring.kind; value };
                  location = { Location.start = { Location.line; column; _ }; _ };
                }
              =
              let value_length = String.length value in
              let rec expand_fstring input_string ~line_offset ~column_offset ~index state : 'a list
                =
                if index = value_length then
                  []
                else
                  let token = input_string.[index] in
                  let expressions, next_state =
                    match token, state with
                    | '{', Literal -> [], Expression (line_offset, column_offset + 1, "")
                    | '{', Expression (_, _, "") -> [], Literal
                    | '}', Literal -> [], Literal
                    (* NOTE: this does not account for nested expressions in e.g. format specifiers. *)
                    | '}', Expression (fstring_start_line, fstring_start_column, string) ->
                        [(fstring_start_line, fstring_start_column), string], Literal
                    (* Ignore leading whitespace in expressions. *)
                    | (' ' | '\t'), (Expression (_, _, "") as expression) -> [], expression
                    | _, Literal -> [], Literal
                    | _, Expression (line, column, string) ->
                        [], Expression (line, column, string ^ Char.to_string token)
                  in
                  let line_offset, column_offset =
                    match token with
                    | '\n' -> line_offset + 1, 0
                    | _ -> line_offset, column_offset + 1
                  in
                  let next_expressions =
                    expand_fstring
                      input_string
                      ~line_offset
                      ~column_offset
                      ~index:(index + 1)
                      next_state
                  in
                  expressions @ next_expressions
              in
              match kind with
              | Substring.Literal -> expressions
              | Substring.Format ->
                  let fstring_expressions =
                    expand_fstring value ~line_offset:line ~column_offset:column ~index:0 Literal
                  in
                  List.rev fstring_expressions @ expressions
            in
            List.fold substrings ~init:[] ~f:gather_expressions_in_substring |> List.rev
          in
          let parse ((start_line, start_column), input_string) =
            try
              let string = input_string ^ "\n" in
              match Parser.parse [string ^ "\n"] ~start_line ~start_column ~relative with
              | [{ Node.value = Expression expression; _ }] -> [expression]
              | _ -> failwith "Not an expression"
            with
            | Parser.Error _
            | Failure _ ->
                Log.debug
                  "Pyre could not parse format string `%s` at %s:%a"
                  input_string
                  relative
                  Location.pp
                  location;
                []
          in
          let expressions = substrings |> gather_fstring_expressions |> List.concat_map ~f:parse in
          {
            Node.location;
            value = Expression.String { StringLiteral.kind = Format expressions; value };
          }
      | _ -> expression
  end)
  in
  Transform.transform () source |> Transform.source


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
  skip: Location.Set.t;
  is_in_function: bool;
  is_in_class: bool;
}

let qualify_local_identifier ~qualifier name =
  let qualifier = Reference.show qualifier |> String.substr_replace_all ~pattern:"." ~with_:"?" in
  Format.asprintf "$local_%s$%s" qualifier name


let qualify
    ( {
        Source.source_path = { SourcePath.relative; qualifier = source_qualifier; _ };
        statements;
        _;
      } as source )
  =
  let is_qualified = String.is_prefix ~prefix:"$" in
  let qualify_if_needed ~qualifier name =
    if Reference.is_strict_prefix ~prefix:qualifier name then
      name
    else
      Reference.combine qualifier name
  in
  let prefix_identifier ~scope:({ aliases; immutables; _ } as scope) ~prefix name =
    let stars, name = Identifier.split_star name in
    let renamed = Format.asprintf "$%s$%s" prefix name in
    let reference = Reference.create name in
    ( {
        scope with
        aliases =
          Map.set
            aliases
            ~key:reference
            ~data:
              {
                name = Reference.create renamed;
                qualifier = source_qualifier;
                is_forward_reference = false;
              };
        immutables = Set.add immutables reference;
      },
      stars,
      renamed )
  in
  let rec explore_scope ~scope statements =
    let global_alias ~qualifier ~name =
      { name = Reference.combine qualifier name; qualifier; is_forward_reference = true }
    in
    let explore_scope
        ({ qualifier; aliases; immutables; skip; is_in_function; _ } as scope)
        { Node.location; value }
      =
      match value with
      | Statement.Assign
          { Assign.target = { Node.value = Name name; _ }; annotation = Some annotation; _ }
        when String.equal (Expression.show annotation) "_SpecialForm" ->
          let name = name_to_reference_exn name in
          {
            scope with
            aliases = Map.set aliases ~key:name ~data:(global_alias ~qualifier ~name);
            skip = Set.add skip location;
          }
      | Class { Class.name = { Node.value = name; _ }; _ } ->
          { scope with aliases = Map.set aliases ~key:name ~data:(global_alias ~qualifier ~name) }
      | Define { Define.signature = { name = { Node.value = name; _ }; _ }; _ } when is_in_function
        ->
          qualify_function_name ~scope name |> fst
      | Define { Define.signature = { name = { Node.value = name; _ }; _ }; _ }
        when not is_in_function ->
          { scope with aliases = Map.set aliases ~key:name ~data:(global_alias ~qualifier ~name) }
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
            let explore_handler scope { Try.Handler.body; _ } = explore_scope ~scope body in
            List.fold handlers ~init:scope ~f:explore_handler
          in
          let scope = explore_scope ~scope orelse in
          explore_scope ~scope finally
      | With { With.body; _ } -> explore_scope ~scope body
      | While { While.body; orelse; _ } ->
          let scope = explore_scope ~scope body in
          explore_scope ~scope orelse
      | _ -> scope
    in
    List.fold statements ~init:scope ~f:explore_scope
  and qualify_function_name
      ~scope:({ aliases; locals; immutables; is_in_function; is_in_class; qualifier; _ } as scope)
      name
    =
    if is_in_function then
      match Reference.as_list name with
      | [simple_name] when (not (is_qualified simple_name)) && not (Set.mem immutables name) ->
          let alias = qualify_local_identifier simple_name ~qualifier |> Reference.create in
          ( {
              scope with
              aliases =
                Map.set
                  aliases
                  ~key:name
                  ~data:{ name = alias; qualifier; is_forward_reference = false };
              locals = Set.add locals name;
            },
            alias )
      | _ -> scope, qualify_reference ~scope name
    else
      let scope =
        if is_in_class then
          scope
        else
          {
            scope with
            aliases =
              Map.set
                aliases
                ~key:name
                ~data:
                  {
                    name = Reference.combine qualifier name;
                    qualifier;
                    is_forward_reference = false;
                  };
          }
      in
      scope, qualify_reference ~suppress_synthetics:true ~scope name
  and qualify_parameters ~scope parameters =
    (* Rename parameters to prevent aliasing. *)
    let parameters =
      let qualify_annotation { Node.location; value = { Parameter.annotation; _ } as parameter } =
        {
          Node.location;
          value =
            {
              parameter with
              Parameter.annotation = annotation >>| qualify_expression ~qualify_strings:true ~scope;
            };
        }
      in
      List.map parameters ~f:qualify_annotation
    in
    let rename_parameter
        (scope, reversed_parameters)
        ({ Node.value = { Parameter.name; value; annotation }; _ } as parameter)
      =
      if not (is_qualified (snd (Identifier.split_star name))) then
        let scope, stars, renamed = prefix_identifier ~scope ~prefix:"parameter" name in
        ( scope,
          {
            parameter with
            Node.value =
              {
                Parameter.name = stars ^ renamed;
                value = value >>| qualify_expression ~qualify_strings:false ~scope;
                annotation;
              };
          }
          :: reversed_parameters )
      else
        scope, parameter :: reversed_parameters
    in
    let scope, parameters =
      List.fold
        parameters
        ~init:({ scope with locals = Reference.Set.empty }, [])
        ~f:rename_parameter
    in
    scope, List.rev parameters
  and qualify_statements ~scope statements =
    let scope = explore_scope ~scope statements in
    let scope, reversed_statements =
      let qualify (scope, statements) statement =
        let scope, statement = qualify_statement ~qualify_assign:false ~scope statement in
        scope, statement :: statements
      in
      List.fold statements ~init:(scope, []) ~f:qualify
    in
    scope, List.rev reversed_statements
  and qualify_statement
      ~qualify_assign
      ~scope:({ qualifier; aliases; skip; is_top_level; _ } as scope)
      ({ Node.location; value } as statement)
    =
    let scope, value =
      let local_alias ~qualifier ~name = { name; qualifier; is_forward_reference = false } in
      let qualify_assign { Assign.target; annotation; value; parent } =
        let qualify_value ~is_potential_alias ~scope = function
          | { Node.value = Expression.String _; _ } ->
              (* String literal assignments might be type aliases. *)
              qualify_expression ~qualify_strings:is_potential_alias value ~scope
          | {
              Node.value =
                Call
                  {
                    callee =
                      { Node.value = Name (Name.Attribute { attribute = "__getitem__"; _ }); _ };
                    _;
                  };
              _;
            } ->
              qualify_expression ~qualify_strings:is_potential_alias value ~scope
          | _ -> qualify_expression ~qualify_strings:false value ~scope
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
                let location = Node.location target in
                match Node.value target with
                | Expression.Tuple elements ->
                    let scope, elements = qualify_targets scope elements in
                    scope, Expression.Tuple elements
                | List elements ->
                    let scope, elements = qualify_targets scope elements in
                    scope, List elements
                | Name (Name.Identifier name) when qualify_assign ->
                    let sanitized = Identifier.sanitized name in
                    let qualified =
                      let qualifier =
                        Node.create
                          ~location
                          (Expression.Name (create_name_from_reference ~location qualifier))
                      in
                      Name.Attribute { base = qualifier; attribute = sanitized; special = false }
                    in
                    let scope =
                      let aliases =
                        let update = function
                          | Some alias -> alias
                          | None -> local_alias ~qualifier ~name:(name_to_reference_exn qualified)
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
                      let reference = Reference.create name in
                      if
                        (not (is_qualified name))
                        && (not (Set.mem locals reference))
                        && not (Set.mem immutables reference)
                      then
                        let alias = qualify_local_identifier name ~qualifier |> Reference.create in
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
                    ( scope,
                      qualify_name
                        ~qualify_strings:false
                        ~location
                        ~scope
                        (Expression.Name (Name.Identifier name)) )
                | Name name ->
                    let name =
                      if qualify_assign then
                        qualify_if_needed ~qualifier (name_to_reference_exn name)
                        |> create_name_from_reference ~location
                        |> fun name -> Expression.Name name
                      else
                        match qualify_name ~qualify_strings:false ~location ~scope (Name name) with
                        | Name (Name.Identifier name) ->
                            Expression.Name (Name.Identifier (Identifier.sanitized name))
                        | qualified -> qualified
                    in
                    scope, name
                | target -> scope, target
              in
              scope, { target with Node.value }
            in
            qualify_target ~scope target
          else
            scope, target
        in
        let qualified_annotation = annotation >>| qualify_expression ~qualify_strings:true ~scope in
        let qualified_value =
          let is_potential_alias =
            match qualified_annotation >>| Expression.show with
            | Some "typing_extensions.TypeAlias"
            | None ->
                is_top_level
            | _ -> false
          in
          qualify_value ~scope:target_scope ~is_potential_alias value
        in
        ( target_scope,
          {
            Assign.target;
            annotation = qualified_annotation;
            value = qualified_value;
            parent = (parent >>| fun parent -> qualify_reference ~scope parent);
          } )
      in
      let qualify_define
          ({ qualifier; _ } as original_scope)
          ( {
              Define.signature =
                {
                  name = { Node.value = name; location = name_location };
                  parameters;
                  decorators;
                  return_annotation;
                  parent;
                  nesting_define;
                  _;
                };
              body;
              _;
            } as define )
        =
        let scope = { original_scope with is_top_level = false } in
        let return_annotation =
          return_annotation >>| qualify_expression ~qualify_strings:true ~scope
        in
        let parent = parent >>| fun parent -> qualify_reference ~scope parent in
        let nesting_define =
          nesting_define >>| fun nesting_define -> qualify_reference ~scope nesting_define
        in
        let decorators =
          List.map
            decorators
            ~f:
              (qualify_decorator
                 ~qualify_strings:false
                 ~scope:{ scope with use_forward_references = true })
        in
        (* Take care to qualify the function name before parameters, as parameters shadow it. *)
        let scope, _ = qualify_function_name ~scope name in
        let scope, parameters = qualify_parameters ~scope parameters in
        let qualifier = qualify_if_needed ~qualifier name in
        let _, body =
          qualify_statements ~scope:{ scope with qualifier; is_in_function = true } body
        in
        let original_scope_with_alias, name = qualify_function_name ~scope:original_scope name in
        let signature =
          {
            define.signature with
            name = { Node.value = name; location = name_location };
            parameters;
            decorators;
            return_annotation;
            parent;
            nesting_define;
          }
        in
        original_scope_with_alias, { define with signature; body }
      in
      let qualify_class
          ( { Class.name = { Node.value = name; location }; bases; body; decorators; _ } as
          definition )
        =
        let scope = { scope with is_top_level = false } in
        let qualify_base ({ Call.Argument.value; _ } as argument) =
          {
            argument with
            Call.Argument.value = qualify_expression ~qualify_strings:true ~scope value;
          }
        in
        let decorators = List.map decorators ~f:(qualify_decorator ~qualify_strings:false ~scope) in
        let body =
          let qualifier = qualify_if_needed ~qualifier name in
          let original_scope =
            { scope with qualifier; is_in_function = false; is_in_class = true }
          in
          let scope = explore_scope body ~scope:original_scope in
          let qualify (scope, statements) ({ Node.location; value } as statement) =
            let scope, statement =
              match value with
              | Statement.Define
                  ( {
                      signature =
                        {
                          name = { Node.value = name; location = name_location };
                          parameters;
                          return_annotation;
                          decorators;
                          _;
                        };
                      _;
                    } as define ) ->
                  let _, define = qualify_define original_scope define in
                  let _, parameters = qualify_parameters ~scope parameters in
                  let return_annotation =
                    return_annotation >>| qualify_expression ~scope ~qualify_strings:true
                  in
                  let qualify_decorator
                      ({ Decorator.name = { Node.value = name; _ }; _ } as decorator)
                    =
                    match name |> Reference.as_list |> List.rev with
                    | ["staticmethod"]
                    | ["classmethod"]
                    | ["property"]
                    | "getter" :: _
                    | "setter" :: _
                    | "deleter" :: _ ->
                        decorator
                    | _ ->
                        (* TODO (T41755857): Decorator qualification logic should be slightly more
                           involved than this. *)
                        qualify_decorator ~qualify_strings:false ~scope decorator
                  in
                  let decorators = List.map decorators ~f:qualify_decorator in
                  let signature =
                    {
                      define.signature with
                      name =
                        { Node.value = qualify_reference ~scope name; location = name_location };
                      parameters;
                      decorators;
                      return_annotation;
                    }
                  in
                  scope, { Node.location; value = Statement.Define { define with signature } }
              | _ -> qualify_statement statement ~qualify_assign:true ~scope
            in
            scope, statement :: statements
          in
          List.fold body ~init:(scope, []) ~f:qualify |> snd |> List.rev
        in
        {
          definition with
          (* Ignore aliases, imports, etc. when declaring a class name. *)
          Class.name = { Node.location; value = qualify_if_needed ~qualifier:scope.qualifier name };
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
      | Statement.Assign assign ->
          let scope, assign = qualify_assign assign in
          scope, Statement.Assign assign
      | Assert { Assert.test; message; origin } ->
          ( scope,
            Assert
              {
                Assert.test = qualify_expression ~qualify_strings:false ~scope test;
                message;
                origin;
              } )
      | Class ({ name = { Node.value = name; _ }; _ } as definition) ->
          let scope =
            {
              scope with
              aliases =
                Map.set
                  aliases
                  ~key:name
                  ~data:(local_alias ~qualifier ~name:(Reference.combine qualifier name));
            }
          in
          scope, Class (qualify_class definition)
      | Define define ->
          let scope, define = qualify_define scope define in
          scope, Define define
      | Delete expression ->
          scope, Delete (qualify_expression ~qualify_strings:false ~scope expression)
      | Expression expression ->
          scope, Expression (qualify_expression ~qualify_strings:false ~scope expression)
      | For ({ For.target; iterator; body; orelse; _ } as block) ->
          let renamed_scope, target = qualify_target ~scope target in
          let body_scope, body = qualify_statements ~scope:renamed_scope body in
          let orelse_scope, orelse = qualify_statements ~scope:renamed_scope orelse in
          ( join_scopes body_scope orelse_scope,
            For
              {
                block with
                For.target;
                iterator = qualify_expression ~qualify_strings:false ~scope iterator;
                body;
                orelse;
              } )
      | Global identifiers -> scope, Global identifiers
      | If { If.test; body; orelse } ->
          let body_scope, body = qualify_statements ~scope body in
          let orelse_scope, orelse = qualify_statements ~scope orelse in
          ( join_scopes body_scope orelse_scope,
            If { If.test = qualify_expression ~qualify_strings:false ~scope test; body; orelse } )
      | Import { Import.from = Some { Node.value = from; _ }; imports }
        when not (String.equal (Reference.show from) "builtins") ->
          let import aliases { Import.name = { Node.value = name; _ }; alias } =
            match alias with
            | Some { Node.value = alias; _ } ->
                (* Add `alias -> from.name`. *)
                Map.set
                  aliases
                  ~key:(Reference.create alias)
                  ~data:(local_alias ~qualifier ~name:(Reference.combine from name))
            | None ->
                (* Add `name -> from.name`. *)
                Map.set
                  aliases
                  ~key:name
                  ~data:(local_alias ~qualifier ~name:(Reference.combine from name))
          in
          { scope with aliases = List.fold imports ~init:aliases ~f:import }, value
      | Import { Import.from = None; imports } ->
          let import aliases { Import.name = { Node.value = name; _ }; alias } =
            match alias with
            | Some { Node.value = alias; _ } ->
                (* Add `alias -> from.name`. *)
                Map.set aliases ~key:(Reference.create alias) ~data:(local_alias ~qualifier ~name)
            | None -> aliases
          in
          { scope with aliases = List.fold imports ~init:aliases ~f:import }, value
      | Nonlocal identifiers -> scope, Nonlocal identifiers
      | Raise { Raise.expression; from } ->
          ( scope,
            Raise
              {
                Raise.expression = expression >>| qualify_expression ~qualify_strings:false ~scope;
                from = from >>| qualify_expression ~qualify_strings:false ~scope;
              } )
      | Return ({ Return.expression; _ } as return) ->
          ( scope,
            Return
              {
                return with
                Return.expression = expression >>| qualify_expression ~qualify_strings:false ~scope;
              } )
      | Try { Try.body; handlers; orelse; finally } ->
          let body_scope, body = qualify_statements ~scope body in
          let handler_scopes, handlers =
            let qualify_handler { Try.Handler.kind; name; body } =
              let renamed_scope, name =
                match name with
                | Some name when not (is_qualified name) ->
                    let scope, _, renamed = prefix_identifier ~scope ~prefix:"target" name in
                    scope, Some renamed
                | _ -> scope, name
              in
              let kind = kind >>| qualify_expression ~qualify_strings:false ~scope in
              let scope, body = qualify_statements ~scope:renamed_scope body in
              scope, { Try.Handler.kind; name; body }
            in
            List.map handlers ~f:qualify_handler |> List.unzip
          in
          let orelse_scope, orelse = qualify_statements ~scope:body_scope orelse in
          let finally_scope, finally = qualify_statements ~scope finally in
          let scope =
            List.fold handler_scopes ~init:body_scope ~f:join_scopes
            |> join_scopes orelse_scope
            |> join_scopes finally_scope
          in
          scope, Try { Try.body; handlers; orelse; finally }
      | With ({ With.items; body; _ } as block) ->
          let scope, items =
            let qualify_item (scope, reversed_items) (name, alias) =
              let scope, item =
                let renamed_scope, alias =
                  match alias with
                  | Some alias ->
                      let scope, alias = qualify_target ~scope alias in
                      scope, Some alias
                  | _ -> scope, alias
                in
                renamed_scope, (qualify_expression ~qualify_strings:false ~scope name, alias)
              in
              scope, item :: reversed_items
            in
            let scope, reversed_items = List.fold items ~init:(scope, []) ~f:qualify_item in
            scope, List.rev reversed_items
          in
          let scope, body = qualify_statements ~scope body in
          scope, With { block with With.items; body }
      | While { While.test; body; orelse } ->
          let body_scope, body = qualify_statements ~scope body in
          let orelse_scope, orelse = qualify_statements ~scope orelse in
          ( join_scopes body_scope orelse_scope,
            While
              { While.test = qualify_expression ~qualify_strings:false ~scope test; body; orelse } )
      | Statement.Yield expression ->
          scope, Statement.Yield (qualify_expression ~qualify_strings:false ~scope expression)
      | Statement.YieldFrom expression ->
          scope, Statement.YieldFrom (qualify_expression ~qualify_strings:false ~scope expression)
      | Break
      | Continue
      | Import _
      | Pass ->
          scope, value
    in
    scope, { statement with Node.value }
  and qualify_target ?(in_comprehension = false) ~scope target =
    let rec renamed_scope ({ locals; _ } as scope) target =
      let has_local name = (not in_comprehension) && Set.mem locals (Reference.create name) in
      match target with
      | { Node.value = Expression.Tuple elements; _ } ->
          List.fold elements ~init:scope ~f:renamed_scope
      | { Node.value = Name (Name.Identifier name); _ } ->
          if has_local name || is_qualified name then
            scope
          else
            let scope, _, _ = prefix_identifier ~scope ~prefix:"target" name in
            scope
      | _ -> scope
    in
    let scope = renamed_scope scope target in
    scope, qualify_expression ~qualify_strings:false ~scope target
  and qualify_reference
      ?(suppress_synthetics = false)
      ~scope:{ aliases; use_forward_references; _ }
      reference
    =
    match Reference.as_list reference with
    | [] -> Reference.empty
    | head :: tail -> (
        match Map.find aliases (Reference.create head) with
        | Some { name; is_forward_reference; qualifier }
          when (not is_forward_reference) || use_forward_references ->
            if Reference.show name |> is_qualified && suppress_synthetics then
              Reference.combine qualifier reference
            else
              Reference.combine name (Reference.create_from_list tail)
        | _ -> reference )
  and qualify_name
      ?(suppress_synthetics = false)
      ~qualify_strings
      ~location
      ~scope:({ aliases; use_forward_references; _ } as scope)
    = function
    | Name (Name.Identifier identifier) -> (
        match Map.find aliases (Reference.create identifier) with
        | Some { name; is_forward_reference; qualifier }
          when (not is_forward_reference) || use_forward_references ->
            if Reference.show name |> is_qualified && suppress_synthetics then
              Name
                (Name.Attribute
                   {
                     base = from_reference ~location qualifier;
                     attribute = identifier;
                     special = false;
                   })
            else
              Node.value (from_reference ~location name)
        | _ -> Name (Name.Identifier identifier) )
    | Name
        (Name.Attribute
          { base = { Node.value = Name (Name.Identifier "builtins"); _ }; attribute; _ }) ->
        Name (Name.Identifier attribute)
    | Name (Name.Attribute ({ base; _ } as name)) ->
        Name (Name.Attribute { name with base = qualify_expression ~qualify_strings ~scope base })
    | expression -> expression
  and qualify_expression ~qualify_strings ~scope ({ Node.location; value } as expression) =
    let value =
      let qualify_entry ~qualify_strings ~scope { Dictionary.Entry.key; value } =
        {
          Dictionary.Entry.key = qualify_expression ~qualify_strings ~scope key;
          value = qualify_expression ~qualify_strings ~scope value;
        }
      in
      let qualify_generators ~qualify_strings ~scope generators =
        let qualify_generator
            (scope, reversed_generators)
            ({ Comprehension.Generator.target; iterator; conditions; _ } as generator)
          =
          let renamed_scope, target = qualify_target ~in_comprehension:true ~scope target in
          ( renamed_scope,
            {
              generator with
              Comprehension.Generator.target;
              iterator = qualify_expression ~qualify_strings ~scope iterator;
              conditions =
                List.map conditions ~f:(qualify_expression ~qualify_strings ~scope:renamed_scope);
            }
            :: reversed_generators )
        in
        let scope, reversed_generators =
          List.fold generators ~init:(scope, []) ~f:qualify_generator
        in
        scope, List.rev reversed_generators
      in
      match value with
      | Expression.Await expression ->
          Expression.Await (qualify_expression ~qualify_strings ~scope expression)
      | BooleanOperator { BooleanOperator.left; operator; right } ->
          BooleanOperator
            {
              BooleanOperator.left = qualify_expression ~qualify_strings ~scope left;
              operator;
              right = qualify_expression ~qualify_strings ~scope right;
            }
      | Call { callee; arguments } ->
          let callee = qualify_expression ~qualify_strings ~scope callee in
          let arguments =
            match arguments with
            | [type_argument; value_argument]
              when name_is ~name:"pyre_extensions.safe_cast" callee
                   || name_is ~name:"typing.cast" callee
                   || name_is ~name:"cast" callee
                   || name_is ~name:"safe_cast" callee ->
                [
                  qualify_argument ~qualify_strings:true ~scope type_argument;
                  qualify_argument ~qualify_strings ~scope value_argument;
                ]
            | variable_name :: remaining_arguments when name_is ~name:"typing.TypeVar" callee ->
                variable_name
                :: List.map ~f:(qualify_argument ~qualify_strings:true ~scope) remaining_arguments
            | arguments ->
                let qualify_strings =
                  if
                    name_is ~name:"typing_extensions.Literal.__getitem__" callee
                    || name_is ~name:"typing.Literal.__getitem__" callee
                  then
                    false
                  else
                    match callee with
                    | { value = Name (Name.Attribute { attribute = "__getitem__"; _ }); _ } ->
                        qualify_strings
                    | _ -> false
                in
                List.map ~f:(qualify_argument ~qualify_strings ~scope) arguments
          in
          Expression.Call { callee; arguments }
      | ComparisonOperator { ComparisonOperator.left; operator; right } ->
          ComparisonOperator
            {
              ComparisonOperator.left = qualify_expression ~qualify_strings ~scope left;
              operator;
              right = qualify_expression ~qualify_strings ~scope right;
            }
      | Dictionary { Dictionary.entries; keywords } ->
          Dictionary
            {
              Dictionary.entries = List.map entries ~f:(qualify_entry ~qualify_strings ~scope);
              keywords = List.map keywords ~f:(qualify_expression ~qualify_strings ~scope);
            }
      | DictionaryComprehension { Comprehension.element; generators } ->
          let scope, generators = qualify_generators ~qualify_strings ~scope generators in
          DictionaryComprehension
            { Comprehension.element = qualify_entry ~qualify_strings ~scope element; generators }
      | Generator { Comprehension.element; generators } ->
          let scope, generators = qualify_generators ~qualify_strings ~scope generators in
          Generator
            {
              Comprehension.element = qualify_expression ~qualify_strings ~scope element;
              generators;
            }
      | Lambda { Lambda.parameters; body } ->
          let scope, parameters = qualify_parameters ~scope parameters in
          Lambda { Lambda.parameters; body = qualify_expression ~qualify_strings ~scope body }
      | List elements -> List (List.map elements ~f:(qualify_expression ~qualify_strings ~scope))
      | ListComprehension { Comprehension.element; generators } ->
          let scope, generators = qualify_generators ~qualify_strings ~scope generators in
          ListComprehension
            {
              Comprehension.element = qualify_expression ~qualify_strings ~scope element;
              generators;
            }
      | Name _ -> qualify_name ~qualify_strings ~location ~scope value
      | Set elements -> Set (List.map elements ~f:(qualify_expression ~qualify_strings ~scope))
      | SetComprehension { Comprehension.element; generators } ->
          let scope, generators = qualify_generators ~qualify_strings ~scope generators in
          SetComprehension
            {
              Comprehension.element = qualify_expression ~qualify_strings ~scope element;
              generators;
            }
      | Starred (Starred.Once expression) ->
          Starred (Starred.Once (qualify_expression ~qualify_strings ~scope expression))
      | Starred (Starred.Twice expression) ->
          Starred (Starred.Twice (qualify_expression ~qualify_strings ~scope expression))
      | String { StringLiteral.value; kind } ->
          let kind =
            match kind with
            | StringLiteral.Format expressions ->
                StringLiteral.Format
                  (List.map expressions ~f:(qualify_expression ~qualify_strings ~scope))
            | _ -> kind
          in
          if qualify_strings then (
            try
              match Parser.parse [value ^ "\n"] ~relative with
              | [{ Node.value = Expression expression; _ }] ->
                  qualify_expression ~qualify_strings ~scope expression
                  |> Expression.show
                  |> fun value -> Expression.String { StringLiteral.value; kind }
              | _ -> failwith "Not an expression"
            with
            | Parser.Error _
            | Failure _ ->
                Log.debug
                  "Invalid string annotation `%s` at %s:%a"
                  value
                  relative
                  Location.pp
                  location;
                String { StringLiteral.value; kind } )
          else
            String { StringLiteral.value; kind }
      | Ternary { Ternary.target; test; alternative } ->
          Ternary
            {
              Ternary.target = qualify_expression ~qualify_strings ~scope target;
              test = qualify_expression ~qualify_strings ~scope test;
              alternative = qualify_expression ~qualify_strings ~scope alternative;
            }
      | Tuple elements -> Tuple (List.map elements ~f:(qualify_expression ~qualify_strings ~scope))
      | WalrusOperator { target; value } ->
          WalrusOperator
            {
              target = qualify_expression ~qualify_strings ~scope target;
              value = qualify_expression ~qualify_strings ~scope value;
            }
      | UnaryOperator { UnaryOperator.operator; operand } ->
          UnaryOperator
            { UnaryOperator.operator; operand = qualify_expression ~qualify_strings ~scope operand }
      | Yield (Some expression) ->
          Yield (Some (qualify_expression ~qualify_strings ~scope expression))
      | Yield None -> Yield None
      | Complex _
      | Ellipsis
      | False
      | Float _
      | Integer _
      | True ->
          value
    in
    { expression with Node.value }
  and qualify_decorator ~qualify_strings ~scope { Decorator.name; arguments } =
    {
      Decorator.name = Node.map name ~f:(qualify_reference ~scope);
      arguments = arguments >>| List.map ~f:(qualify_argument ~qualify_strings ~scope);
    }
  and qualify_argument { Call.Argument.name; value } ~qualify_strings ~scope =
    let name =
      let rename identifier =
        let parameter_prefix = "$parameter$" in
        if String.is_prefix identifier ~prefix:parameter_prefix then
          identifier
        else
          parameter_prefix ^ identifier
      in
      name >>| Node.map ~f:rename
    in
    { Call.Argument.name; value = qualify_expression ~qualify_strings ~scope value }
  in
  let scope =
    {
      qualifier = source_qualifier;
      aliases = Reference.Map.empty;
      locals = Reference.Set.empty;
      immutables = Reference.Set.empty;
      use_forward_references = true;
      is_top_level = true;
      skip = Location.Set.empty;
      is_in_function = false;
      is_in_class = false;
    }
  in
  { source with Source.statements = qualify_statements ~scope statements |> snd }


let replace_version_specific_code ~major_version ~minor_version ~micro_version source =
  let module Transform = Transform.MakeStatementTransformer (struct
    include Transform.Identity

    type t = unit

    module Comparison = struct
      type t =
        | LessThan
        | LessThanOrEquals
        | GreaterThan
        | GreaterThanOrEquals
        | Equals
        | NotEquals
      [@@deriving sexp]

      let inverse = function
        | LessThan -> GreaterThan
        | LessThanOrEquals -> GreaterThanOrEquals
        | GreaterThan -> LessThan
        | GreaterThanOrEquals -> LessThanOrEquals
        | Equals -> Equals
        | NotEquals -> NotEquals


      let evaluate ~operator compare_result =
        match operator with
        | LessThan -> compare_result < 0
        | LessThanOrEquals -> compare_result <= 0
        | GreaterThan -> compare_result > 0
        | GreaterThanOrEquals -> compare_result >= 0
        | Equals -> compare_result = 0
        | NotEquals -> compare_result <> 0
    end

    let evaluate_one_version ~operator actual_version given_version =
      [%compare: int] actual_version given_version |> Comparison.evaluate ~operator


    let evaluate_two_versions ~operator actual_versions given_versions =
      [%compare: int * int] actual_versions given_versions |> Comparison.evaluate ~operator


    let evaluate_three_versions ~operator actual_versions given_versions =
      [%compare: int * int * int] actual_versions given_versions |> Comparison.evaluate ~operator


    let statement _ ({ Node.location; value } as statement) =
      match value with
      | Statement.If { If.test; body; orelse } -> (
          let extract_comparison { Node.value; _ } =
            match value with
            | Expression.ComparisonOperator { ComparisonOperator.left; operator; right } -> (
                match operator with
                | ComparisonOperator.LessThan -> Some (Comparison.LessThan, left, right)
                | ComparisonOperator.LessThanOrEquals ->
                    Some (Comparison.LessThanOrEquals, left, right)
                | ComparisonOperator.GreaterThan -> Some (Comparison.GreaterThan, left, right)
                | ComparisonOperator.GreaterThanOrEquals ->
                    Some (Comparison.GreaterThanOrEquals, left, right)
                | ComparisonOperator.Equals -> Some (Comparison.Equals, left, right)
                | ComparisonOperator.NotEquals -> Some (Comparison.NotEquals, left, right)
                | _ -> None )
            | _ -> None
          in

          let add_pass_statement ~location body =
            if List.is_empty body then
              [Node.create ~location Statement.Pass]
            else
              body
          in
          let do_replace condition =
            if condition then
              (), add_pass_statement ~location body
            else
              (), add_pass_statement ~location orelse
          in

          let is_system_version_expression = function
            | {
                Node.value =
                  Expression.Name
                    (Name.Attribute
                      {
                        base = { Node.value = Expression.Name (Name.Identifier "sys"); _ };
                        attribute = "version_info";
                        _;
                      });
                _;
              } ->
                true
            | _ -> false
          in
          let is_system_version_attribute_access_expression ~attribute = function
            | {
                Node.value =
                  Expression.Name
                    (Name.Attribute
                      {
                        base =
                          {
                            Node.value =
                              Expression.Name
                                (Name.Attribute
                                  {
                                    base =
                                      { Node.value = Expression.Name (Name.Identifier "sys"); _ };
                                    attribute = "version_info";
                                    _;
                                  });
                            _;
                          };
                        attribute = version_attribute;
                        _;
                      });
                _;
              }
              when String.equal attribute version_attribute ->
                true
            | _ -> false
          in
          let is_system_version_tuple_access_expression ?index = function
            | {
                Node.value =
                  Expression.Call
                    {
                      Call.callee =
                        {
                          Node.value =
                            Expression.Name
                              (Name.Attribute
                                {
                                  base =
                                    {
                                      Node.value =
                                        Expression.Name
                                          (Name.Attribute
                                            {
                                              base =
                                                {
                                                  Node.value =
                                                    Expression.Name (Name.Identifier "sys");
                                                  _;
                                                };
                                              attribute = "version_info";
                                              _;
                                            });
                                      _;
                                    };
                                  attribute = "__getitem__";
                                  special = true;
                                });
                          _;
                        };
                      arguments = [argument];
                    };
                _;
              } -> (
                match index, argument with
                | None, _ -> true
                | ( Some expected_index,
                    { Call.Argument.value = { Node.value = Expression.Integer actual_index; _ }; _ }
                  )
                  when Int.equal expected_index actual_index ->
                    true
                | _ -> false )
            | _ -> false
          in

          match extract_comparison test with
          | Some
              ( operator,
                left,
                {
                  Node.value =
                    Expression.Tuple
                      ({ Node.value = Expression.Integer given_major_version; _ }
                      :: { Node.value = Expression.Integer given_minor_version; _ }
                         :: { Node.value = Expression.Integer given_micro_version; _ } :: _);
                  _;
                } )
            when is_system_version_expression left ->
              evaluate_three_versions
                ~operator
                (major_version, minor_version, micro_version)
                (given_major_version, given_minor_version, given_micro_version)
              |> do_replace
          | Some
              ( operator,
                left,
                {
                  Node.value =
                    Expression.Tuple
                      ({ Node.value = Expression.Integer given_major_version; _ }
                      :: { Node.value = Expression.Integer given_minor_version; _ } :: _);
                  _;
                } )
            when is_system_version_expression left ->
              evaluate_two_versions
                ~operator
                (major_version, minor_version)
                (given_major_version, given_minor_version)
              |> do_replace
          | Some
              ( operator,
                left,
                {
                  Node.value =
                    Expression.Tuple
                      ({ Node.value = Expression.Integer given_major_version; _ } :: _);
                  _;
                } )
            when is_system_version_expression left ->
              evaluate_one_version ~operator major_version given_major_version |> do_replace
          | Some (operator, left, { Node.value = Expression.Integer given_major_version; _ })
            when is_system_version_tuple_access_expression ~index:0 left ->
              evaluate_one_version ~operator major_version given_major_version |> do_replace
          | Some (operator, left, { Node.value = Expression.Integer given_minor_version; _ })
            when is_system_version_tuple_access_expression ~index:1 left ->
              evaluate_one_version ~operator minor_version given_minor_version |> do_replace
          | Some (operator, left, { Node.value = Expression.Integer given_micro_version; _ })
            when is_system_version_tuple_access_expression ~index:2 left ->
              evaluate_one_version ~operator micro_version given_micro_version |> do_replace
          | Some (operator, left, { Node.value = Expression.Integer given_major_version; _ })
            when is_system_version_attribute_access_expression ~attribute:"major" left ->
              evaluate_one_version ~operator major_version given_major_version |> do_replace
          | Some (operator, left, { Node.value = Expression.Integer given_minor_version; _ })
            when is_system_version_attribute_access_expression ~attribute:"minor" left ->
              evaluate_one_version ~operator minor_version given_minor_version |> do_replace
          | Some (operator, left, { Node.value = Expression.Integer given_micro_version; _ })
            when is_system_version_attribute_access_expression ~attribute:"micro" left ->
              evaluate_one_version ~operator micro_version given_micro_version |> do_replace
          | Some
              ( operator,
                {
                  Node.value =
                    Expression.Tuple
                      ({ Node.value = Expression.Integer given_major_version; _ }
                      :: { Node.value = Expression.Integer given_minor_version; _ }
                         :: { Node.value = Expression.Integer given_micro_version; _ } :: _);
                  _;
                },
                right )
            when is_system_version_expression right ->
              evaluate_three_versions
                ~operator:(Comparison.inverse operator)
                (major_version, minor_version, micro_version)
                (given_major_version, given_minor_version, given_micro_version)
              |> do_replace
          | Some
              ( operator,
                {
                  Node.value =
                    Expression.Tuple
                      ({ Node.value = Expression.Integer given_major_version; _ }
                      :: { Node.value = Expression.Integer given_minor_version; _ } :: _);
                  _;
                },
                right )
            when is_system_version_expression right ->
              evaluate_two_versions
                ~operator:(Comparison.inverse operator)
                (major_version, minor_version)
                (given_major_version, given_minor_version)
              |> do_replace
          | Some
              ( operator,
                {
                  Node.value =
                    Expression.Tuple
                      ({ Node.value = Expression.Integer given_major_version; _ } :: _);
                  _;
                },
                right )
            when is_system_version_expression right ->
              evaluate_one_version
                ~operator:(Comparison.inverse operator)
                major_version
                given_major_version
              |> do_replace
          | Some (operator, { Node.value = Expression.Integer given_major_version; _ }, right)
            when is_system_version_tuple_access_expression ~index:0 right ->
              evaluate_one_version
                ~operator:(Comparison.inverse operator)
                major_version
                given_major_version
              |> do_replace
          | Some (operator, { Node.value = Expression.Integer given_minor_version; _ }, right)
            when is_system_version_tuple_access_expression ~index:1 right ->
              evaluate_one_version
                ~operator:(Comparison.inverse operator)
                minor_version
                given_minor_version
              |> do_replace
          | Some (operator, { Node.value = Expression.Integer given_micro_version; _ }, right)
            when is_system_version_tuple_access_expression ~index:2 right ->
              evaluate_one_version
                ~operator:(Comparison.inverse operator)
                micro_version
                given_micro_version
              |> do_replace
          | Some (operator, { Node.value = Expression.Integer given_major_version; _ }, right)
            when is_system_version_attribute_access_expression ~attribute:"major" right ->
              evaluate_one_version
                ~operator:(Comparison.inverse operator)
                major_version
                given_major_version
              |> do_replace
          | Some (operator, { Node.value = Expression.Integer given_minor_version; _ }, right)
            when is_system_version_attribute_access_expression ~attribute:"minor" right ->
              evaluate_one_version
                ~operator:(Comparison.inverse operator)
                minor_version
                given_minor_version
              |> do_replace
          | Some (operator, { Node.value = Expression.Integer given_micro_version; _ }, right)
            when is_system_version_attribute_access_expression ~attribute:"micro" right ->
              evaluate_one_version
                ~operator:(Comparison.inverse operator)
                micro_version
                given_micro_version
              |> do_replace
          | _ -> (), [statement] )
      | _ -> (), [statement]
  end)
  in
  Transform.transform () source |> Transform.source


let replace_platform_specific_code source =
  let module Transform = Transform.MakeStatementTransformer (struct
    include Transform.Identity

    type t = unit

    let statement _ ({ Node.location; value } as statement) =
      match value with
      | Statement.If { If.test = { Node.value = test; _ }; body; orelse } ->
          let statements =
            let statements =
              let open Expression in
              let matches_removed_platform left right =
                let is_platform expression =
                  String.equal (Expression.show expression) "sys.platform"
                in
                let is_win32 { Node.value; _ } =
                  match value with
                  | String { StringLiteral.value = "win32"; _ } -> true
                  | _ -> false
                in
                (is_platform left && is_win32 right) or (is_platform right && is_win32 left)
              in
              match test with
              | ComparisonOperator { ComparisonOperator.left; operator; right }
                when matches_removed_platform left right -> (
                  match operator with
                  | ComparisonOperator.Equals
                  | Is ->
                      orelse
                  | NotEquals
                  | IsNot ->
                      body
                  | _ -> [statement] )
              | _ -> [statement]
            in
            if not (List.is_empty statements) then
              statements
            else
              [Node.create ~location Statement.Pass]
          in
          (), statements
      | _ -> (), [statement]
  end)
  in
  Transform.transform () source |> Transform.source


let expand_type_checking_imports source =
  let module Transform = Transform.MakeStatementTransformer (struct
    include Transform.Identity

    type t = unit

    let statement _ ({ Node.value; _ } as statement) =
      let is_type_checking { Node.value; _ } =
        match value with
        | Expression.Name
            (Name.Attribute
              {
                base = { Node.value = Name (Name.Identifier "typing"); _ };
                attribute = "TYPE_CHECKING";
                _;
              })
        | Name (Name.Identifier "TYPE_CHECKING") ->
            true
        | _ -> false
      in
      match value with
      | Statement.If { If.test; body; _ } when is_type_checking test -> (), body
      | If
          {
            If.test = { Node.value = UnaryOperator { UnaryOperator.operator = Not; operand }; _ };
            orelse;
            _;
          }
        when is_type_checking operand ->
          (), orelse
      | _ -> (), [statement]
  end)
  in
  Transform.transform () source |> Transform.source


let expand_implicit_returns source =
  let module ExpandingTransform = Transform.MakeStatementTransformer (struct
    include Transform.Identity

    type t = unit

    let statement state statement =
      match statement with
      (* Insert implicit return statements at the end of function bodies. *)
      | { Node.value = Statement.Define define; _ } when Define.is_stub define -> state, [statement]
      | { Node.location; value = Define define } ->
          let define =
            let has_yield =
              let module Visit = Visit.Make (struct
                type t = bool

                let expression sofar _ = sofar

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
              | Some { Node.value = Try { Try.finally; _ }; _ } -> (
                  match List.last finally with
                  | Some { Node.value = Return _; _ } -> true
                  | _ -> false )
              | _ -> false
            in
            let loops_forever =
              match List.last define.Define.body with
              | Some { Node.value = While { While.test = { Node.value = True; _ }; _ }; _ } -> true
              | _ -> false
            in
            if has_yield || has_return_in_finally || loops_forever then
              define
            else
              match List.last define.Define.body with
              | Some { Node.value = Return _; _ } -> define
              | Some statement ->
                  let rec get_last statement =
                    let get_last_in_block last_statement statement_block =
                      match last_statement, List.rev statement_block with
                      | None, last_statement_in_block :: _ -> get_last last_statement_in_block
                      | _ -> last_statement
                    in
                    match Node.value statement with
                    | Statement.For { body; orelse; _ } ->
                        List.fold ~init:None ~f:get_last_in_block [orelse; body]
                    | If { body; orelse; _ } ->
                        List.fold ~init:None ~f:get_last_in_block [orelse; body]
                    | Try { body; handlers; orelse; finally } ->
                        let last_handler_body =
                          match List.rev handlers with
                          | { Try.Handler.body; _ } :: _ -> body
                          | _ -> []
                        in
                        List.fold
                          ~init:None
                          ~f:get_last_in_block
                          [finally; orelse; last_handler_body; body]
                    | While { body; orelse; _ } ->
                        List.fold ~init:None ~f:get_last_in_block [orelse; body]
                    | _ -> Some statement
                  in
                  let last_statement = get_last statement |> Option.value ~default:statement in
                  {
                    define with
                    Define.body =
                      define.Define.body
                      @ [
                          {
                            Node.location = last_statement.Node.location;
                            value = Return { Return.expression = None; is_implicit = true };
                          };
                        ];
                  }
              | _ -> define
          in
          state, [{ Node.location; value = Define define }]
      | _ -> state, [statement]
  end)
  in
  ExpandingTransform.transform () source |> ExpandingTransform.source


let defines
    ?(include_stubs = false)
    ?(include_nested = false)
    ?(include_toplevels = false)
    ?(include_methods = true)
    source
  =
  let module Collector = Visit.StatementCollector (struct
    type t = Define.t Node.t

    let visit_children = function
      | { Node.value = Statement.Define _; _ } -> include_nested
      | { Node.value = Class _; _ } -> include_methods
      | _ -> false


    let predicate = function
      | { Node.location; value = Statement.Class class_; _ } when include_toplevels ->
          Class.toplevel_define class_ |> Node.create ~location |> Option.some
      | { Node.location; value = Define define } when Define.is_stub define ->
          if include_stubs then
            Some { Node.location; Node.value = define }
          else
            None
      | { Node.location; value = Define define } -> Some { Node.location; Node.value = define }
      | _ -> None
  end)
  in
  let defines = Collector.collect source in
  if include_toplevels then
    let toplevel = Source.top_level_define_node source in
    toplevel :: defines
  else
    defines


let count_defines source =
  let module Visitor = Visit.MakeStatementVisitor (struct
    type t = int

    let visit_children = function
      | { Node.value = Statement.Define _; _ }
      | { Node.value = Class _; _ } ->
          true
      | _ -> false


    let statement _ count = function
      | { Node.value = Statement.Class _; _ } ->
          (* +1 for $classtoplevel *)
          count + 1
      | { Node.value = Define _; _ } -> count + 1
      | _ -> count
  end)
  in
  let count = Visitor.visit 0 source in
  (* +1 for $toplevel *)
  count + 1


let classes source =
  let module Collector = Visit.StatementCollector (struct
    type t = Class.t Node.t

    let visit_children _ = true

    let predicate = function
      | { Node.location; value = Statement.Class class_define } ->
          Some { Node.location; Node.value = class_define }
      | _ -> None
  end)
  in
  Collector.collect source


let dequalify_map ({ Source.source_path = { SourcePath.qualifier; _ }; _ } as source) =
  let module ImportDequalifier = Transform.MakeStatementTransformer (struct
    include Transform.Identity

    type t = Reference.t Reference.Map.t

    let statement map ({ Node.value; _ } as statement) =
      match value with
      | Statement.Import { Import.from = None; imports } ->
          let add_import map { Import.name = { Node.value = name; _ }; alias } =
            match alias with
            | Some { Node.value = alias; _ } ->
                (* Add `name -> alias`. *)
                Map.set map ~key:name ~data:(Reference.create alias)
            | None -> map
          in
          List.fold_left imports ~f:add_import ~init:map, [statement]
      | Import { Import.from = Some { Node.value = from; _ }; imports } ->
          let add_import map { Import.name = { Node.value = name; _ }; alias } =
            match alias with
            | Some { Node.value = alias; _ } ->
                (* Add `from.name -> alias`. *)
                Map.set map ~key:(Reference.combine from name) ~data:(Reference.create alias)
            | None ->
                (* Add `from.name -> name`. *)
                Map.set map ~key:(Reference.combine from name) ~data:name
          in
          List.fold_left imports ~f:add_import ~init:map, [statement]
      | _ -> map, [statement]
  end)
  in
  let map = Map.set ~key:qualifier ~data:Reference.empty Reference.Map.empty in
  ImportDequalifier.transform map source |> fun { ImportDequalifier.state; _ } -> state


let is_lazy_import { Node.value; _ } =
  match value with
  | Expression.Name name -> (
      match name_to_reference name with
      | Some reference
        when String.Set.mem Recognized.lazy_import_functions (Reference.show reference) ->
          true
      | _ -> false )
  | _ -> false


let replace_lazy_import ?(is_lazy_import = is_lazy_import) source =
  let module LazyImportTransformer = Transform.MakeStatementTransformer (struct
    type t = unit

    let statement _ ({ Node.value; location } as statement) =
      match value with
      | Statement.Assign
          {
            Assign.target =
              {
                Node.value = Expression.Name (Name.Identifier identifier);
                location = identifier_location;
              };
            value =
              {
                Node.value =
                  Expression.Call
                    {
                      callee;
                      arguments =
                        [
                          {
                            Call.Argument.value =
                              {
                                Node.value =
                                  Expression.String { StringLiteral.kind = String; value = literal };
                                location = literal_location;
                              };
                            _;
                          };
                        ];
                    };
                _;
              };
            _;
          }
        when is_lazy_import callee ->
          ( (),
            [
              Statement.Import
                {
                  from = None;
                  imports =
                    [
                      {
                        Import.name =
                          Reference.create literal |> Node.create ~location:literal_location;
                        alias =
                          Some
                            ( Identifier.sanitized identifier
                            |> Node.create ~location:identifier_location );
                      };
                    ];
                }
              |> Node.create ~location;
            ] )
      | Statement.Assign
          {
            Assign.target =
              {
                Node.value = Expression.Name (Name.Identifier identifier);
                location = identifier_location;
              };
            value =
              {
                Node.value =
                  Expression.Call
                    {
                      callee;
                      arguments =
                        [
                          {
                            Call.Argument.value =
                              {
                                Node.value =
                                  Expression.String
                                    { StringLiteral.kind = String; value = from_literal };
                                location = from_literal_location;
                              };
                            _;
                          };
                          {
                            Call.Argument.value =
                              {
                                Node.value =
                                  Expression.String
                                    { StringLiteral.kind = String; value = import_literal };
                                location = import_literal_location;
                              };
                            _;
                          };
                        ];
                    };
                _;
              };
            _;
          }
        when is_lazy_import callee ->
          ( (),
            [
              Statement.Import
                {
                  from =
                    Some
                      (Reference.create from_literal |> Node.create ~location:from_literal_location);
                  imports =
                    [
                      {
                        Import.name =
                          Reference.create import_literal
                          |> Node.create ~location:import_literal_location;
                        alias =
                          Some
                            ( Identifier.sanitized identifier
                            |> Node.create ~location:identifier_location );
                      };
                    ];
                }
              |> Node.create ~location;
            ] )
      | _ -> (), [statement]
  end)
  in
  LazyImportTransformer.transform () source |> LazyImportTransformer.source


let replace_mypy_extensions_stub
    ({ Source.source_path = { SourcePath.relative; _ }; statements; _ } as source)
  =
  if String.is_suffix relative ~suffix:"mypy_extensions.pyi" then
    let typed_dictionary_stub ~location =
      let node value = Node.create ~location value in
      Statement.Assign
        {
          target = node (Expression.Name (Name.Identifier "TypedDict"));
          annotation =
            Some
              (node
                 (Expression.Name
                    (Name.Attribute
                       {
                         base = { Node.value = Name (Name.Identifier "typing"); location };
                         attribute = "_SpecialForm";
                         special = false;
                       })));
          value = node Expression.Ellipsis;
          parent = None;
        }
      |> node
    in
    let replace_typed_dictionary_define = function
      | { Node.location; value = Statement.Define { signature = { name; _ }; _ } }
        when String.equal (Reference.show (Node.value name)) "TypedDict" ->
          typed_dictionary_stub ~location
      | statement -> statement
    in
    { source with statements = List.map ~f:replace_typed_dictionary_define statements }
  else
    source


let expand_typed_dictionary_declarations
    ({ Source.statements; source_path = { SourcePath.qualifier; _ }; _ } as source)
  =
  let expand_typed_dictionaries ({ Node.location; value } as statement) =
    let expanded_declaration =
      let string_literal identifier =
        Expression.String { value = identifier; kind = StringLiteral.String }
        |> Node.create ~location
      in
      let extract_string_literal literal_expression =
        match Node.value literal_expression with
        | Expression.String { StringLiteral.value; kind = StringLiteral.String } -> Some value
        | _ -> None
      in
      let typed_dictionary_class_declaration ~name ~fields ~total =
        match name with
        | { Node.value = Expression.String { value = class_name; kind = StringLiteral.String }; _ }
          ->
            let class_reference = Reference.create class_name in
            let assignments =
              let assignment (key, value) =
                match Node.value key with
                | Expression.String
                    { StringLiteral.value = attribute_name; kind = StringLiteral.String } ->
                    Some
                      ( Statement.Assign
                          {
                            target =
                              Expression.Name
                                (Name.Attribute
                                   {
                                     base = from_reference ~location class_reference;
                                     attribute = attribute_name;
                                     special = false;
                                   })
                              |> Node.create ~location;
                            annotation = Some value;
                            value = Node.create ~location Expression.Ellipsis;
                            parent = Some class_reference;
                          }
                      |> Node.create ~location )
                | _ -> None
              in
              match List.filter_map fields ~f:assignment with
              | [] -> [Node.create ~location Statement.Pass]
              | assignments -> assignments
            in

            (* Note: Add a placeholder to indicate the totality of the class. Not using
               `total=False` because that gives an error saying `False` is not a valid literal type.
               Not using `total=Literal[False]` because that requires importing `Literal`. *)
            let non_total_base =
              {
                Call.Argument.name = None;
                value =
                  Node.create
                    ~location
                    (Expression.Name
                       (create_name
                          ~location
                          (* Note: Cannot use `Type.TypedDictionary.class_name` because Type is not
                             available here. *)
                          "NonTotalTypedDictionary"));
              }
            in
            Some
              (Statement.Class
                 {
                   name = Node.create ~location class_reference;
                   bases =
                     ( [
                         {
                           Call.Argument.name = None;
                           value =
                             Node.create
                               ~location
                               (Expression.Name (create_name ~location "TypedDictionary"));
                         };
                       ]
                     @
                     if total then
                       []
                     else
                       [non_total_base] );
                   decorators = [];
                   body = assignments;
                   top_level_unbound_names = [];
                 })
        | _ -> None
      in
      let extract_totality_from_base base =
        let is_total ~total = String.equal (Identifier.sanitized total) "total" in
        match base with
        | {
         Call.Argument.name = Some { value = total; _ };
         value = { Node.value = Expression.True; _ };
        }
          when is_total ~total ->
            Some true
        | {
         Call.Argument.name = Some { value = total; _ };
         value = { Node.value = Expression.False; _ };
        }
          when is_total ~total ->
            Some false
        | _ -> None
      in
      let extract_totality arguments =
        List.find_map arguments ~f:extract_totality_from_base |> Option.value ~default:true
      in
      match value with
      | Statement.Assign
          {
            value =
              {
                Node.value =
                  Call
                    {
                      callee =
                        {
                          Node.value =
                            Name
                              (Name.Attribute
                                {
                                  base =
                                    {
                                      Node.value =
                                        Name
                                          (Name.Identifier
                                            ("mypy_extensions" | "typing_extensions" | "typing"));
                                      _;
                                    };
                                  attribute = "TypedDict";
                                  _;
                                });
                          _;
                        };
                      arguments =
                        { Call.Argument.name = None; value = name }
                        :: {
                             Call.Argument.name = None;
                             value = { Node.value = Dictionary { Dictionary.entries; _ }; _ };
                             _;
                           }
                           :: argument_tail;
                    };
                _;
              };
            _;
          } ->
          extract_string_literal name
          >>= (fun name ->
                typed_dictionary_class_declaration
                  ~name:(string_literal (Reference.show (Reference.create ~prefix:qualifier name)))
                  ~fields:(List.map entries ~f:(fun { Dictionary.Entry.key; value } -> key, value))
                  ~total:(extract_totality argument_tail))
          |> Option.value ~default:value
      | Class
          {
            name = { Node.value = class_name; _ };
            bases =
              {
                Call.Argument.name = None;
                value =
                  {
                    Node.value =
                      Name
                        (Name.Attribute
                          {
                            base =
                              {
                                Node.value =
                                  Name
                                    (Name.Identifier
                                      ("mypy_extensions" | "typing_extensions" | "typing"));
                                _;
                              };
                            attribute = "TypedDict";
                            _;
                          });
                    _;
                  };
              }
              :: bases_tail;
            body;
            decorators = _;
            top_level_unbound_names = _;
          } ->
          let fields =
            let extract = function
              | {
                  Node.value =
                    Statement.Assign
                      { target = { Node.value = Name name; _ }; annotation = Some annotation; _ };
                  _;
                } ->
                  Reference.drop_prefix ~prefix:class_name (name_to_reference_exn name)
                  |> Reference.single
                  >>| fun name -> string_literal name, annotation
              | _ -> None
            in
            List.filter_map body ~f:extract
          in
          let declaration class_name =
            (* Note: We create the class anew because we don't want to keep any methods. *)
            let class_declaration =
              typed_dictionary_class_declaration
                ~name:(string_literal class_name)
                ~fields
                ~total:(extract_totality bases_tail)
            in
            class_declaration
          in
          declaration (Reference.show class_name) |> Option.value ~default:value
      | Class ({ bases; _ } as class_definition) ->
          let replace_totality base =
            match extract_totality_from_base base with
            | Some true -> None
            | Some false ->
                Some
                  {
                    Call.Argument.name = None;
                    value =
                      Expression.Name (create_name ~location "NonTotalTypedDictionary")
                      |> Node.create ~location;
                  }
            | None -> Some base
          in
          Class { class_definition with bases = List.filter_map bases ~f:replace_totality }
      | _ -> value
    in
    { statement with Node.value = expanded_declaration }
  in
  { source with Source.statements = List.map ~f:expand_typed_dictionaries statements }


let expand_named_tuples ({ Source.statements; _ } as source) =
  let rec expand_named_tuples ({ Node.location; value } as statement) =
    let extract_attributes expression =
      match expression with
      | {
          Node.location;
          value =
            Expression.Call
              {
                callee =
                  {
                    Node.value =
                      Name
                        (Name.Attribute
                          {
                            base = { Node.value = Name (Name.Identifier "typing"); _ };
                            attribute = "NamedTuple";
                            _;
                          });
                    _;
                  };
                arguments;
              };
        }
      | {
          Node.location;
          value =
            Call
              {
                callee =
                  {
                    Node.value =
                      Name
                        (Name.Attribute
                          {
                            base = { Node.value = Name (Name.Identifier "collections"); _ };
                            attribute = "namedtuple";
                            _;
                          });
                    _;
                  };
                arguments;
              };
        } ->
          let any_annotation =
            Expression.Name (create_name ~location "typing.Any") |> Node.create ~location
          in
          let attributes =
            match arguments with
            | [
             _;
             {
               Call.Argument.value = { value = String { StringLiteral.value = serialized; _ }; _ };
               _;
             };
            ] ->
                String.split serialized ~on:' '
                |> List.map ~f:(fun name -> name, any_annotation, None)
            | [_; { Call.Argument.value = { Node.value = List arguments; _ }; _ }]
            | [_; { Call.Argument.value = { Node.value = Tuple arguments; _ }; _ }] ->
                let get_name ({ Node.value; _ } as expression) =
                  match value with
                  | Expression.String { StringLiteral.value = name; _ } ->
                      name, any_annotation, None
                  | Tuple [{ Node.value = String { StringLiteral.value = name; _ }; _ }; annotation]
                    ->
                      name, annotation, None
                  | _ -> Expression.show expression, any_annotation, None
                in
                List.map arguments ~f:get_name
            | _ :: arguments ->
                List.filter_map arguments ~f:(fun argument ->
                    match argument with
                    | { Call.Argument.name = Some { Node.value = name; _ }; value } ->
                        Some (Identifier.sanitized name, value, None)
                    | _ -> None)
            | _ -> []
          in
          Some attributes
      | _ -> None
    in
    let fields_attribute ~parent ~location attributes =
      let node = Node.create ~location in
      let value =
        attributes
        |> List.map ~f:(fun (name, _, _) -> Expression.String (StringLiteral.create name) |> node)
        |> (fun parameters -> Expression.Tuple parameters)
        |> node
      in
      let annotation =
        let create_name name =
          Node.create ~location (Expression.Name (create_name ~location name))
        in
        let p = get_item_call "typing.Tuple" ~location in
        let q =
          match List.length attributes with
          | 0 -> [Node.create ~location (Expression.Tuple [])]
          | l -> List.init l ~f:(fun _ -> create_name "str")
        in
        q |> p |> Node.create ~location
      in
      Statement.Assign
        {
          Assign.target = Reference.create ~prefix:parent "_fields" |> from_reference ~location;
          annotation = Some annotation;
          value;
          parent = Some parent;
        }
      |> Node.create ~location
    in
    let tuple_attributes ~parent ~location attributes =
      let attribute_statements =
        let attribute { Node.value = name, annotation, _value; location } =
          let target = Reference.create ~prefix:parent name |> from_reference ~location in
          let annotation =
            let location = Node.location annotation in
            {
              Node.location;
              value =
                Expression.Call
                  {
                    callee =
                      {
                        Node.location;
                        value =
                          Expression.Name
                            (Attribute
                               {
                                 base =
                                   Reference.create "typing.Final"
                                   |> Ast.Expression.from_reference ~location;
                                 attribute = "__getitem__";
                                 special = true;
                               });
                      };
                    arguments = [{ name = None; value = annotation }];
                  };
            }
          in
          Statement.Assign
            {
              Assign.target;
              annotation = Some annotation;
              value = Node.create Expression.Ellipsis ~location;
              parent = Some parent;
            }
          |> Node.create ~location
        in
        List.map attributes ~f:attribute
      in
      let fields_attribute =
        List.map attributes ~f:Node.value |> fields_attribute ~parent ~location
      in
      fields_attribute :: attribute_statements
    in
    let tuple_constructors ~parent ~location attributes =
      let parameters =
        let to_parameter (name, annotation, value) =
          let value =
            match value with
            | Some { Node.value = Expression.Ellipsis; _ } -> None
            | _ -> value
          in
          Parameter.create ?value ~location ~annotation ~name:("$parameter$" ^ name) ()
        in
        List.map attributes ~f:to_parameter
      in
      let constructor ~is_new =
        let name, return_annotation, self_parameter =
          if is_new then
            ( "__new__",
              Node.create
                ~location
                (Expression.Name
                   (Name.Attribute
                      {
                        base = { Node.value = Name (Name.Identifier "typing"); location };
                        attribute = "NamedTuple";
                        special = false;
                      })),
              Parameter.create ~location ~name:"$parameter$cls" () )
          else
            ( "__init__",
              Node.create ~location (Expression.Name (Name.Identifier "None")),
              Parameter.create ~location ~name:"$parameter$self" () )
        in
        let assignments =
          if is_new || List.is_empty parameters then
            [
              Node.create
                ~location
                (Statement.Expression (Node.create ~location Expression.Ellipsis));
            ]
          else
            let to_assignment { Node.value = { Parameter.name; _ }; _ } =
              Node.create
                (Statement.Assign
                   {
                     target =
                       Node.create
                         ~location
                         (Expression.Name
                            (Attribute
                               {
                                 base =
                                   Node.create
                                     (Expression.Name (Identifier "$parameter$self"))
                                     ~location;
                                 attribute = name |> Identifier.sanitized;
                                 special = false;
                               }));
                     annotation = None;
                     value = Node.create (Expression.Name (Identifier name)) ~location;
                     parent = None;
                   })
                ~location
            in
            List.map parameters ~f:to_assignment
            @ [{ Node.location; value = Return { Return.expression = None; is_implicit = true } }]
        in
        Statement.Define
          {
            signature =
              {
                name = Node.create ~location (Reference.create ~prefix:parent name);
                parameters = self_parameter :: parameters;
                decorators = [];
                return_annotation = Some return_annotation;
                async = false;
                generator = false;
                parent = Some parent;
                nesting_define = None;
              };
            captures = [];
            unbound_names = [];
            body = assignments;
          }
        |> Node.create ~location
      in
      [constructor ~is_new:true; constructor ~is_new:false]
    in
    let tuple_base ~location =
      {
        Call.Argument.name = None;
        value = { Node.location; value = Name (create_name ~location "typing.NamedTuple") };
      }
    in
    let value =
      match value with
      | Statement.Assign
          {
            Assign.target = { Node.value = Name name; location = target_location };
            value = expression;
            _;
          } -> (
          let name = name_to_reference name >>| Reference.delocalize in
          match extract_attributes expression, name with
          | Some attributes, Some name
          (* TODO (T42893621): properly handle the excluded case *)
            when not (Reference.is_prefix ~prefix:(Reference.create "$parameter$cls") name) ->
              let constructors = tuple_constructors ~parent:name ~location attributes in
              let attributes =
                List.map attributes ~f:(Node.create ~location)
                |> tuple_attributes ~parent:name ~location
              in
              Statement.Class
                {
                  Class.name = Node.create ~location:target_location name;
                  bases = [tuple_base ~location];
                  body = constructors @ attributes;
                  decorators = [];
                  top_level_unbound_names = [];
                }
          | _ -> value )
      | Class ({ Class.name = { Node.value = name; _ }; bases; body; _ } as original) ->
          let is_named_tuple_primitive = function
            | {
                Call.Argument.value =
                  {
                    Node.value =
                      Name
                        (Name.Attribute
                          {
                            base = { Node.value = Name (Name.Identifier "typing"); _ };
                            attribute = "NamedTuple";
                            _;
                          });
                    _;
                  };
                _;
              } ->
                true
            | _ -> false
          in
          if List.exists ~f:is_named_tuple_primitive bases then
            let extract_assign = function
              | {
                  Node.value =
                    Statement.Assign
                      { Assign.target = { Node.value = Name target; _ }; value; annotation; _ };
                  location;
                } ->
                  let last =
                    match target with
                    | Name.Identifier identifier -> identifier
                    | Name.Attribute { attribute; _ } -> attribute
                  in
                  let annotation =
                    let any =
                      Expression.Name (create_name ~location "typing.Any") |> Node.create ~location
                    in
                    Option.value annotation ~default:any
                  in
                  Either.First (Node.create ~location (last, annotation, Some value))
              | statement -> Either.Second statement
            in
            let attributes, other = List.partition_map body ~f:extract_assign in
            let constructors =
              List.map attributes ~f:Node.value |> tuple_constructors ~parent:name ~location
            in
            let tuple_attributes = tuple_attributes ~parent:name ~location attributes in
            Class { original with Class.body = constructors @ tuple_attributes @ other }
          else
            let extract_named_tuples (bases, attributes_sofar) ({ Call.Argument.value; _ } as base) =
              match extract_attributes value with
              | Some attributes ->
                  let constructors =
                    let has_name generated_name = function
                      | {
                          Node.value =
                            Statement.Define { Define.signature = { Define.Signature.name; _ }; _ };
                          _;
                        } ->
                          String.equal (Reference.last (Node.value name)) generated_name
                      | _ -> false
                    in
                    if
                      List.exists body ~f:(has_name "__new__")
                      || List.exists body ~f:(has_name "__init__")
                    then
                      []
                    else
                      tuple_constructors ~parent:name ~location attributes
                  in
                  let attributes =
                    List.map attributes ~f:(Node.create ~location)
                    |> tuple_attributes ~parent:name ~location
                  in
                  tuple_base ~location :: bases, attributes_sofar @ constructors @ attributes
              | None -> base :: bases, attributes_sofar
            in
            let reversed_bases, attributes =
              List.fold bases ~init:([], []) ~f:extract_named_tuples
            in
            Class
              {
                original with
                Class.bases = List.rev reversed_bases;
                body = attributes @ List.map ~f:expand_named_tuples body;
              }
      | Define ({ Define.body; _ } as define) ->
          Define { define with Define.body = List.map ~f:expand_named_tuples body }
      | _ -> value
    in
    { statement with Node.value }
  in
  { source with Source.statements = List.map ~f:expand_named_tuples statements }


let expand_new_types ({ Source.statements; source_path = { SourcePath.qualifier; _ }; _ } as source)
  =
  let expand_new_type ({ Node.location; value } as statement) =
    let value =
      match value with
      | Statement.Assign
          {
            Assign.value =
              {
                Node.value =
                  Call
                    {
                      callee =
                        {
                          Node.value =
                            Name
                              (Name.Attribute
                                {
                                  base = { Node.value = Name (Name.Identifier "typing"); _ };
                                  attribute = "NewType";
                                  _;
                                });
                          _;
                        };
                      arguments =
                        [
                          {
                            Call.Argument.value =
                              {
                                Node.value = String { StringLiteral.value = name; _ };
                                location = name_location;
                              };
                            _;
                          };
                          ( {
                              (* TODO (T44209017): Error on invalid annotation expression *)
                              Call.Argument.value = base;
                              _;
                            } as base_argument );
                        ];
                    };
                _;
              };
            _;
          } ->
          let name = Reference.create ~prefix:qualifier name in
          let constructor =
            Statement.Define
              {
                signature =
                  {
                    name = Node.create ~location (Reference.create ~prefix:name "__init__");
                    parameters =
                      [
                        Parameter.create ~location ~name:"self" ();
                        Parameter.create ~location ~annotation:base ~name:"input" ();
                      ];
                    decorators = [];
                    return_annotation =
                      Some (Node.create ~location (Expression.Name (Name.Identifier "None")));
                    async = false;
                    generator = false;
                    parent = Some name;
                    nesting_define = None;
                  };
                captures = [];
                unbound_names = [];
                body = [Node.create Statement.Pass ~location];
              }
            |> Node.create ~location
          in
          Statement.Class
            {
              Class.name = Node.create ~location:name_location name;
              bases = [base_argument];
              body = [constructor];
              decorators = [];
              top_level_unbound_names = [];
            }
      | _ -> value
    in
    { statement with Node.value }
  in
  { source with Source.statements = List.map statements ~f:expand_new_type }


let expand_sqlalchemy_declarative_base ({ Source.statements; _ } as source) =
  let expand_declarative_base_instance ({ Node.location; value } as statement) =
    let expanded_declaration =
      let declarative_base_class_declaration ~base_module class_name_reference =
        let metaclass =
          {
            Call.Argument.name = Some (Node.create ~location "metaclass");
            value =
              Node.create
                ~location
                (Expression.Name
                   (create_name
                      ~location
                      (Format.asprintf "%s.ext.declarative.DeclarativeMeta" base_module)));
          }
        in
        Statement.Class
          {
            name = Node.create ~location class_name_reference;
            bases = [metaclass];
            decorators = [];
            body = [Node.create ~location Statement.Pass];
            top_level_unbound_names = [];
          }
      in
      match value with
      | Statement.Assign
          {
            target = { Node.value = Name name; _ };
            value = { Node.value = Call { callee = { Node.value = Name function_name; _ }; _ }; _ };
            _;
          } -> (
          match
            ( name_to_reference function_name >>| Reference.show,
              name_to_reference name >>| Reference.delocalize )
          with
          | Some "sqlalchemy.ext.declarative.declarative_base", Some class_name_reference ->
              declarative_base_class_declaration ~base_module:"sqlalchemy" class_name_reference
          | Some "sqlalchemy_1_4.ext.declarative.declarative_base", Some class_name_reference ->
              declarative_base_class_declaration ~base_module:"sqlalchemy_1_4" class_name_reference
          | _ -> value )
      | _ -> value
    in
    { statement with Node.value = expanded_declaration }
  in
  { source with Source.statements = List.map ~f:expand_declarative_base_instance statements }


let populate_nesting_defines ({ Source.statements; _ } as source) =
  let open Statement in
  let rec transform_statement ~nesting_define statement =
    match statement with
    | {
     Node.location;
     value =
       Define
         {
           Define.signature = { Define.Signature.name = { Node.value = name; _ }; _ } as signature;
           captures;
           unbound_names;
           body;
         };
    } ->
        let signature = { signature with Define.Signature.nesting_define } in
        let body = transform_statements ~nesting_define:(Some name) body in
        { Node.location; value = Define { signature; captures; unbound_names; body } }
    | { Node.location; value = Class class_ } ->
        let body = transform_statements ~nesting_define:None class_.body in
        { Node.location; value = Class { class_ with body } }
    | { Node.location; value = For for_ } ->
        let body = transform_statements ~nesting_define for_.body in
        let orelse = transform_statements ~nesting_define for_.orelse in
        { Node.location; value = For { for_ with body; orelse } }
    | { Node.location; value = If if_ } ->
        let body = transform_statements ~nesting_define if_.body in
        let orelse = transform_statements ~nesting_define if_.orelse in
        { Node.location; value = If { if_ with body; orelse } }
    | { Node.location; value = Try { Try.body; orelse; finally; handlers } } ->
        let body = transform_statements ~nesting_define body in
        let orelse = transform_statements ~nesting_define orelse in
        let finally = transform_statements ~nesting_define finally in
        let handlers =
          List.map handlers ~f:(fun ({ Try.Handler.body; _ } as handler) ->
              let body = transform_statements ~nesting_define body in
              { handler with body })
        in
        { Node.location; value = Try { Try.body; orelse; finally; handlers } }
    | { Node.location; value = With with_ } ->
        let body = transform_statements ~nesting_define with_.body in
        { Node.location; value = With { with_ with body } }
    | { Node.location; value = While while_ } ->
        let body = transform_statements ~nesting_define while_.body in
        let orelse = transform_statements ~nesting_define while_.orelse in
        { Node.location; value = While { while_ with body; orelse } }
    | statement -> statement
  and transform_statements ~nesting_define statements =
    List.map statements ~f:(transform_statement ~nesting_define)
  in
  { source with Source.statements = transform_statements ~nesting_define:None statements }


module NameAccessSet = Set.Make (Define.NameAccess)
module CaptureSet = Set.Make (Define.Capture)

module AccessCollector = struct
  let rec from_expression collected { Node.value; location = expression_location } =
    let open Expression in
    let from_entry collected { Dictionary.Entry.key; value } =
      let collected = from_expression collected key in
      from_expression collected value
    in
    match value with
    (* Lambdas are speical -- they bind their own names, which we want to exclude *)
    | Lambda { Lambda.parameters; body } ->
        let collected =
          let from_parameter collected { Node.value = { Parameter.value; _ }; _ } =
            Option.value_map value ~f:(from_expression collected) ~default:collected
          in
          List.fold parameters ~init:collected ~f:from_parameter
        in
        let bound_names =
          List.map parameters ~f:(fun { Node.value = { Parameter.name; _ }; _ } ->
              Identifier.split_star name |> snd)
          |> Identifier.Set.of_list
        in
        let names_in_body = from_expression NameAccessSet.empty body in
        let unbound_names_in_body =
          Set.filter names_in_body ~f:(fun { Define.NameAccess.name; _ } ->
              not (Identifier.Set.mem bound_names name))
        in
        Set.union unbound_names_in_body collected
    | Name (Name.Identifier identifier) ->
        (* For simple names, add them to the result *)
        Set.add collected { Define.NameAccess.name = identifier; location = expression_location }
    | Name (Name.Attribute { Name.Attribute.base; _ }) ->
        (* For attribute access, only count the base *)
        from_expression collected base
    (* The rest is boilerplates to make sure that expressions are visited recursively *)
    | Await await -> from_expression collected await
    | BooleanOperator { BooleanOperator.left; right; _ }
    | ComparisonOperator { ComparisonOperator.left; right; _ } ->
        let collected = from_expression collected left in
        from_expression collected right
    | Call { Call.callee; arguments } ->
        let collected = from_expression collected callee in
        List.fold arguments ~init:collected ~f:(fun collected { Call.Argument.value; _ } ->
            from_expression collected value)
    | Dictionary { Dictionary.entries; keywords } ->
        let collected = List.fold entries ~init:collected ~f:from_entry in
        List.fold keywords ~init:collected ~f:from_expression
    | DictionaryComprehension comprehension -> from_comprehension from_entry collected comprehension
    | Generator comprehension
    | ListComprehension comprehension
    | SetComprehension comprehension ->
        from_comprehension from_expression collected comprehension
    | List expressions
    | Set expressions
    | Tuple expressions
    | String { kind = StringLiteral.Format expressions; _ } ->
        List.fold expressions ~init:collected ~f:from_expression
    | Starred (Starred.Once expression)
    | Starred (Starred.Twice expression) ->
        from_expression collected expression
    | Ternary { Ternary.target; test; alternative } ->
        let collected = from_expression collected target in
        let collected = from_expression collected test in
        from_expression collected alternative
    | UnaryOperator { UnaryOperator.operand; _ } -> from_expression collected operand
    | WalrusOperator { WalrusOperator.target; value } ->
        let collected = from_expression collected target in
        from_expression collected value
    | Yield yield -> Option.value_map yield ~default:collected ~f:(from_expression collected)
    | String _
    | Complex _
    | Ellipsis
    | False
    | Float _
    | Integer _
    | True ->
        collected


  (* Generators are as special as lambdas -- they bind their own names, which we want to exclude *)
  and from_comprehension :
        'a. (NameAccessSet.t -> 'a -> NameAccessSet.t) -> NameAccessSet.t -> 'a Comprehension.t ->
        NameAccessSet.t
    =
   fun from_element collected { Comprehension.element; generators } ->
    let bound_names =
      List.fold
        generators
        ~init:Identifier.Set.empty
        ~f:(fun sofar { Comprehension.Generator.target; _ } ->
          from_expression NameAccessSet.empty target
          |> Set.fold ~init:sofar ~f:(fun sofar { Define.NameAccess.name; _ } -> Set.add sofar name))
    in
    let names =
      from_element NameAccessSet.empty element
      |> fun init ->
      List.fold
        generators
        ~init
        ~f:(fun sofar { Comprehension.Generator.iterator; conditions; _ } ->
          let sofar = from_expression sofar iterator in
          List.fold conditions ~init:sofar ~f:from_expression)
    in
    let unbound_names =
      Set.filter names ~f:(fun { Define.NameAccess.name; _ } ->
          not (Identifier.Set.mem bound_names name))
    in
    Set.union unbound_names collected


  and from_statement collected { Node.value; location = statement_location } =
    let from_optional_expression collected =
      Option.value_map ~default:collected ~f:(from_expression collected)
    in
    (* Boilerplates to visit all statements that may contain accesses *)
    match value with
    | Statement.Assign { Assign.target; value; annotation; _ } ->
        let collected = from_expression collected target in
        let collected = from_optional_expression collected annotation in
        from_expression collected value
    | Assert { Assert.test; message; _ } ->
        let collected = from_expression collected test in
        Option.value_map message ~f:(from_expression collected) ~default:collected
    | Class { Class.bases; decorators; _ } ->
        let collected =
          List.fold bases ~init:collected ~f:(fun sofar { Call.Argument.value; _ } ->
              from_expression sofar value)
        in
        List.map decorators ~f:Decorator.to_expression
        |> List.fold ~init:collected ~f:from_expression
    | Define
        { Define.signature = { Define.Signature.decorators; parameters; return_annotation; _ }; _ }
      ->
        let collected =
          List.map decorators ~f:Decorator.to_expression
          |> List.fold ~init:collected ~f:from_expression
        in
        let collected =
          List.fold
            parameters
            ~init:collected
            ~f:(fun sofar { Node.value = { Parameter.annotation; value; _ }; _ } ->
              let sofar = from_optional_expression sofar annotation in
              from_optional_expression sofar value)
        in
        from_optional_expression collected return_annotation
    | Delete expression
    | Expression expression
    | Yield expression
    | YieldFrom expression ->
        from_expression collected expression
    | For { For.target; iterator; body; orelse; _ } ->
        let collected = from_expression collected target in
        let collected = from_expression collected iterator in
        let collected = from_statements collected body in
        from_statements collected orelse
    | If { If.test; body; orelse }
    | While { While.test; body; orelse } ->
        let collected = from_expression collected test in
        let collected = from_statements collected body in
        from_statements collected orelse
    | Raise { Raise.expression; from } ->
        let collected = from_optional_expression collected expression in
        from_optional_expression collected from
    | Return { Return.expression; _ } -> from_optional_expression collected expression
    | Try { Try.body; handlers; orelse; finally } ->
        let collected = from_statements collected body in
        let collected =
          List.fold handlers ~init:collected ~f:(fun collected { Try.Handler.kind; name; body } ->
              let collected =
                Option.value_map kind ~f:(from_expression collected) ~default:collected
              in
              let collected =
                Option.value_map
                  name
                  ~f:(fun name ->
                    Set.add collected { Define.NameAccess.name; location = statement_location })
                  ~default:collected
              in
              from_statements collected body)
        in
        let collected = from_statements collected orelse in
        from_statements collected finally
    | With { With.items; body; _ } ->
        let collected =
          List.fold items ~init:collected ~f:(fun collected (value, target) ->
              let collected = from_expression collected value in
              from_optional_expression collected target)
        in
        from_statements collected body
    | Break
    | Continue
    | Global _
    | Import _
    | Nonlocal _
    | Pass ->
        collected


  and from_statements init statements = List.fold statements ~init ~f:from_statement

  let from_define { Define.body; _ } = from_statements NameAccessSet.empty body

  let from_class { Class.body; _ } = from_statements NameAccessSet.empty body
end

let populate_captures ({ Source.statements; _ } as source) =
  let open Scope in
  let to_capture ~is_decorator ~scopes { Define.NameAccess.name; _ } =
    match ScopeStack.lookup scopes name with
    | None -> None
    | Some
        {
          Access.kind = access_kind;
          scope = { Scope.kind = scope_kind; _ };
          binding = { Binding.kind = binding_kind; name; location };
        } -> (
        match access_kind with
        | Access.Kind.CurrentScope when not is_decorator ->
            (* We don't care about bindings that can be found in the current scope *)
            None
        | _ -> (
            match scope_kind with
            | Scope.Kind.(Module | Lambda | Comprehension) ->
                (* We don't care about module-level and expression-level bindings *)
                None
            | Scope.Kind.Define ({ Define.Signature.parent; _ } as signature) -> (
                match binding_kind with
                | Binding.Kind.(ClassName | ImportName) ->
                    (* Judgement call: these bindings are (supposedly) not useful for type checking *)
                    None
                | Binding.Kind.(ParameterName { star = Some Star.Twice; annotation; _ }) ->
                    let dictionary_annotation value_annotation =
                      {
                        Node.location;
                        value =
                          Expression.Call
                            {
                              callee =
                                {
                                  Node.location;
                                  value =
                                    Name
                                      (Name.Attribute
                                         {
                                           base =
                                             {
                                               Node.location;
                                               value =
                                                 Name
                                                   (Name.Attribute
                                                      {
                                                        base =
                                                          {
                                                            Node.location;
                                                            value = Name (Name.Identifier "typing");
                                                          };
                                                        attribute = "Dict";
                                                        special = false;
                                                      });
                                             };
                                           attribute = "__getitem__";
                                           special = true;
                                         });
                                };
                              arguments =
                                [
                                  {
                                    Call.Argument.name = None;
                                    value =
                                      {
                                        Node.location;
                                        value =
                                          Expression.Tuple
                                            [
                                              {
                                                Node.location;
                                                value = Expression.Name (Name.Identifier "str");
                                              };
                                              value_annotation;
                                            ];
                                      };
                                  };
                                ];
                            };
                      }
                    in
                    let annotation =
                      match annotation with
                      | None ->
                          Some
                            (dictionary_annotation
                               {
                                 Node.location;
                                 value =
                                   Expression.Name
                                     (Name.Attribute
                                        {
                                          base =
                                            {
                                              Node.location;
                                              value = Name (Name.Identifier "typing");
                                            };
                                          attribute = "Any";
                                          special = false;
                                        });
                               })
                      | Some
                          ( {
                              Node.value =
                                Expression.Name (Name.Attribute { attribute = "kwargs"; _ });
                              _;
                            } as annotation ) ->
                          (* Heuristic: If the annotation is of the form `XXX.kwargs`, treat it as
                             ParamSpec annotation. *)
                          Some annotation
                      | Some value_annotation -> Some (dictionary_annotation value_annotation)
                    in
                    Some { Define.Capture.name; kind = Annotation annotation }
                | Binding.Kind.(ParameterName { star = Some Star.Once; annotation; _ }) ->
                    let tuple_annotation value_annotation =
                      {
                        Node.location;
                        value =
                          Expression.Call
                            {
                              callee =
                                {
                                  Node.location;
                                  value =
                                    Name
                                      (Name.Attribute
                                         {
                                           base =
                                             {
                                               Node.location;
                                               value =
                                                 Name
                                                   (Name.Attribute
                                                      {
                                                        base =
                                                          {
                                                            Node.location;
                                                            value = Name (Name.Identifier "typing");
                                                          };
                                                        attribute = "Tuple";
                                                        special = false;
                                                      });
                                             };
                                           attribute = "__getitem__";
                                           special = true;
                                         });
                                };
                              arguments =
                                [
                                  {
                                    Call.Argument.name = None;
                                    value =
                                      {
                                        Node.location;
                                        value =
                                          Expression.Tuple
                                            [
                                              value_annotation;
                                              { Node.location; value = Expression.Ellipsis };
                                            ];
                                      };
                                  };
                                ];
                            };
                      }
                    in
                    let annotation =
                      match annotation with
                      | None ->
                          Some
                            (tuple_annotation
                               {
                                 Node.location;
                                 value =
                                   Expression.Name
                                     (Name.Attribute
                                        {
                                          base =
                                            {
                                              Node.location;
                                              value = Name (Name.Identifier "typing");
                                            };
                                          attribute = "Any";
                                          special = false;
                                        });
                               })
                      | Some
                          ( {
                              Node.value = Expression.Name (Name.Attribute { attribute = "args"; _ });
                              _;
                            } as annotation ) ->
                          (* Heuristic: If the annotation is of the form `XXX.args`, treat it as
                             ParamSpec annotation. *)
                          Some annotation
                      | Some value_annotation -> Some (tuple_annotation value_annotation)
                    in
                    Some { Define.Capture.name; kind = Annotation annotation }
                | Binding.Kind.(ParameterName { star = None; index = 0; annotation })
                  when Option.is_some parent
                       && Option.is_none annotation
                       && not (Define.Signature.is_static_method signature) ->
                    let parent = Option.value_exn parent in
                    if
                      Define.Signature.is_class_method signature
                      || Define.Signature.is_class_property signature
                    then
                      Some { Define.Capture.name; kind = ClassSelf parent }
                    else
                      Some { Define.Capture.name; kind = Self parent }
                | Binding.Kind.(
                    ( AssignTarget annotation
                    | ExceptTarget annotation
                    | ParameterName { star = None; annotation; _ } )) ->
                    Some { Define.Capture.name; kind = Annotation annotation }
                | Binding.Kind.DefineName signature ->
                    Some { Define.Capture.name; kind = DefineSignature signature }
                | Binding.Kind.(ComprehensionTarget | ForTarget | WalrusTarget | WithTarget) ->
                    Some { Define.Capture.name; kind = Annotation None } ) ) )
  in
  let rec transform_statement ~scopes statement =
    match statement with
    (* Process each define *)
    | { Node.location; value = Statement.Define ({ body; _ } as define) } ->
        let accesses = AccessCollector.from_define define in
        let scopes = ScopeStack.extend scopes ~with_:(Scope.of_define_exn define) in
        let captures =
          let to_capture ~is_decorator ~scopes sofar name =
            match to_capture ~is_decorator ~scopes name with
            | None -> sofar
            | Some capture -> CaptureSet.add sofar capture
          in
          Set.fold ~init:CaptureSet.empty accesses ~f:(to_capture ~is_decorator:false ~scopes)
          |> CaptureSet.to_list
        in
        let body = transform_statements ~scopes body in
        { Node.location; value = Statement.Define { define with body; captures } }
    (* The rest is just boilerplates to make sure every nested define gets visited *)
    | { Node.location; value = Class class_ } ->
        let body = transform_statements ~scopes class_.body in
        { Node.location; value = Class { class_ with body } }
    | { Node.location; value = For for_ } ->
        let body = transform_statements ~scopes for_.body in
        let orelse = transform_statements ~scopes for_.orelse in
        { Node.location; value = For { for_ with body; orelse } }
    | { Node.location; value = If if_ } ->
        let body = transform_statements ~scopes if_.body in
        let orelse = transform_statements ~scopes if_.orelse in
        { Node.location; value = If { if_ with body; orelse } }
    | { Node.location; value = Try { Try.body; orelse; finally; handlers } } ->
        let body = transform_statements ~scopes body in
        let orelse = transform_statements ~scopes orelse in
        let finally = transform_statements ~scopes finally in
        let handlers =
          List.map handlers ~f:(fun ({ Try.Handler.body; _ } as handler) ->
              let body = transform_statements ~scopes body in
              { handler with body })
        in
        { Node.location; value = Try { Try.body; orelse; finally; handlers } }
    | { Node.location; value = With with_ } ->
        let body = transform_statements ~scopes with_.body in
        { Node.location; value = With { with_ with body } }
    | { Node.location; value = While while_ } ->
        let body = transform_statements ~scopes while_.body in
        let orelse = transform_statements ~scopes while_.orelse in
        { Node.location; value = While { while_ with body; orelse } }
    | statement -> statement
  and transform_statements ~scopes statements =
    List.map statements ~f:(transform_statement ~scopes)
  in
  let scopes = ScopeStack.create source in
  { source with Source.statements = transform_statements ~scopes statements }


let populate_unbound_names source =
  let open Scope in
  let to_unbound_name ~scopes ({ Define.NameAccess.name; _ } as access) =
    match ScopeStack.lookup scopes name with
    | Some _ -> None
    | None -> Option.some_if (not (Builtins.mem name)) access
  in
  let compute_unbound_names ~scopes accesses =
    let deduplicate_access access_set =
      (* Only keep one access for each name *)
      let accumulate_names sofar ({ Define.NameAccess.name; _ } as access) =
        match Map.add sofar ~key:name ~data:access with
        | `Ok sofar -> sofar
        | `Duplicate -> sofar
      in
      NameAccessSet.fold access_set ~init:Identifier.Map.empty ~f:accumulate_names
      |> Identifier.Map.data
      |> NameAccessSet.of_list
    in
    let to_unbound_name ~scopes sofar name =
      match to_unbound_name ~scopes name with
      | None -> sofar
      | Some name -> NameAccessSet.add sofar name
    in
    deduplicate_access accesses
    |> Set.fold ~init:NameAccessSet.empty ~f:(to_unbound_name ~scopes)
    |> NameAccessSet.to_list
  in
  let rec transform_statement ~scopes statement =
    match statement with
    (* Process each define *)
    | { Node.location; value = Statement.Define ({ body; _ } as define) } ->
        let scopes =
          if Define.is_toplevel define then
            scopes
          else
            ScopeStack.extend scopes ~with_:(Scope.of_define_exn define)
        in
        let unbound_names = AccessCollector.from_define define |> compute_unbound_names ~scopes in
        let body = transform_statements ~scopes body in
        { Node.location; value = Statement.Define { define with body; unbound_names } }
    | { Node.location; value = Class ({ Class.body; _ } as class_) } ->
        let top_level_unbound_names =
          let scopes =
            ScopeStack.extend scopes ~with_:(Scope.of_define_exn (Class.toplevel_define class_))
          in
          AccessCollector.from_class class_ |> compute_unbound_names ~scopes
        in
        (* Use parent scope here as classes do not open up new scopes for the methods defined in it. *)
        let body = transform_statements ~scopes body in
        { Node.location; value = Class { class_ with body; top_level_unbound_names } }
    (* The rest is just boilerplates to make sure every nested define gets visited *)
    | { Node.location; value = For for_ } ->
        let body = transform_statements ~scopes for_.body in
        let orelse = transform_statements ~scopes for_.orelse in
        { Node.location; value = For { for_ with body; orelse } }
    | { Node.location; value = If if_ } ->
        let body = transform_statements ~scopes if_.body in
        let orelse = transform_statements ~scopes if_.orelse in
        { Node.location; value = If { if_ with body; orelse } }
    | { Node.location; value = Try { Try.body; orelse; finally; handlers } } ->
        let body = transform_statements ~scopes body in
        let orelse = transform_statements ~scopes orelse in
        let finally = transform_statements ~scopes finally in
        let handlers =
          List.map handlers ~f:(fun ({ Try.Handler.body; _ } as handler) ->
              let body = transform_statements ~scopes body in
              { handler with body })
        in
        { Node.location; value = Try { Try.body; orelse; finally; handlers } }
    | { Node.location; value = With with_ } ->
        let body = transform_statements ~scopes with_.body in
        { Node.location; value = With { with_ with body } }
    | { Node.location; value = While while_ } ->
        let body = transform_statements ~scopes while_.body in
        let orelse = transform_statements ~scopes while_.orelse in
        { Node.location; value = While { while_ with body; orelse } }
    | statement -> statement
  and transform_statements ~scopes statements =
    List.map statements ~f:(transform_statement ~scopes)
  in
  let scopes = ScopeStack.create source in
  let top_level_unbound_names, statements =
    let top_level_statement =
      let { Node.value = define; location } = Source.top_level_define_node source in
      Node.create ~location (Statement.Define define)
    in
    match transform_statement ~scopes top_level_statement |> Node.value with
    | Statement.Define { Define.unbound_names; body; _ } -> unbound_names, body
    | _ -> failwith "Define should not be transformed into other kinds of statements"
  in
  { source with Source.top_level_unbound_names; statements }


let replace_union_shorthand source =
  let module Transform = Transform.Make (struct
    type t = unit

    let transform_expression_children _ _ = true

    let transform_children _ _ = true

    let transform_shorthand_union_expression =
      let rec transform_expression ({ Node.value; location } as expression) =
        let union_value arguments =
          Expression.Call
            {
              callee =
                {
                  Node.location;
                  value =
                    Name
                      (Name.Attribute
                         {
                           base =
                             {
                               Node.location;
                               value =
                                 Name
                                   (Name.Attribute
                                      {
                                        base =
                                          { Node.location; value = Name (Name.Identifier "typing") };
                                        attribute = "Union";
                                        special = false;
                                      });
                             };
                           attribute = "__getitem__";
                           special = true;
                         });
                };
              arguments;
            }
        in
        let value =
          match value with
          | Expression.Call
              {
                callee = { Node.value = Name (Name.Attribute { base; attribute = "__or__"; _ }); _ };
                arguments;
              } ->
              let base_argument = { Call.Argument.value = base; name = None } in
              let transform_argument ({ Call.Argument.value; _ } as argument) =
                { argument with Call.Argument.value = transform_expression value }
              in
              let to_expression_list sofar { Call.Argument.value; _ } =
                match Node.value value with
                | Expression.Call
                    {
                      callee =
                        {
                          Node.value =
                            Name
                              (Name.Attribute
                                {
                                  base =
                                    {
                                      value =
                                        Name
                                          (Name.Attribute
                                            {
                                              base =
                                                { Node.value = Name (Name.Identifier "typing"); _ };
                                              attribute = "Union";
                                              special = false;
                                            });
                                      _;
                                    };
                                  _;
                                });
                          _;
                        };
                      arguments =
                        [
                          {
                            Call.Argument.name = None;
                            value = { Node.value = Tuple argument_list; _ };
                          };
                        ];
                    } ->
                    List.concat [sofar; argument_list] |> List.rev
                | _ -> value :: sofar
              in
              let arguments =
                List.concat [[base_argument]; arguments]
                |> List.map ~f:transform_argument
                |> List.fold ~init:[] ~f:to_expression_list
                |> List.rev
                |> fun argument_list ->
                [
                  {
                    Call.Argument.value = { Node.value = Expression.Tuple argument_list; location };
                    name = None;
                  };
                ]
              in
              union_value arguments
          | Expression.Call
              {
                callee =
                  { value = Name (Name.Attribute { attribute = "__getitem__"; _ }); _ } as callee;
                arguments;
              } ->
              let arguments =
                List.map arguments ~f:(fun ({ Call.Argument.value; _ } as argument) ->
                    { argument with value = transform_expression value })
              in
              Expression.Call { callee; arguments }
          | Tuple arguments -> Tuple (List.map ~f:transform_expression arguments)
          | List arguments -> List (List.map ~f:transform_expression arguments)
          | _ -> value
        in
        { expression with Node.value }
      in
      transform_expression


    let statement _ ({ Node.value; _ } as statement) =
      let transform_assign ~assign:({ Assign.annotation; _ } as assign) =
        { assign with Assign.annotation = annotation >>| transform_shorthand_union_expression }
      in
      let transform_define
          ~define:({ Define.signature = { parameters; return_annotation; _ }; _ } as define)
        =
        let parameter ({ Node.value = { Parameter.annotation; _ } as parameter; _ } as node) =
          {
            node with
            Node.value =
              {
                parameter with
                Parameter.annotation = annotation >>| transform_shorthand_union_expression;
              };
          }
        in
        let signature =
          {
            define.signature with
            parameters = List.map parameters ~f:parameter;
            return_annotation = return_annotation >>| transform_shorthand_union_expression;
          }
        in
        { define with signature }
      in
      let statement =
        let value =
          match value with
          | Statement.Assign assign -> Statement.Assign (transform_assign ~assign)
          | Define define -> Define (transform_define ~define)
          | _ -> value
        in
        { statement with Node.value }
      in
      (), [statement]


    let expression _ expression =
      let transform_argument ({ Call.Argument.value; _ } as argument) =
        { argument with Call.Argument.value = transform_shorthand_union_expression value }
      in
      let value =
        match Node.value expression with
        | Expression.Call { callee; arguments }
          when name_is ~name:"isinstance" callee || name_is ~name:"issubclass" callee ->
            let arguments = List.map ~f:transform_argument arguments in
            Expression.Call { callee; arguments }
        | value -> value
      in
      { expression with Node.value }
  end)
  in
  Transform.transform () source |> Transform.source


let inline_six_metaclass ({ Source.statements; _ } as source) =
  let inline_six_metaclass ({ Node.location; value } as statement) =
    let expanded_declaration =
      let transform_class ~class_statement:({ Class.bases; decorators; _ } as class_statement) =
        let is_six_add_metaclass_decorator { Decorator.name; _ } =
          Identifier.equal (Node.value name |> Reference.show) "six.add_metaclass"
        in
        let six_add_metaclass_decorators, rest =
          List.partition_tf decorators ~f:is_six_add_metaclass_decorator
        in
        match six_add_metaclass_decorators with
        | [
         {
           Decorator.arguments =
             Some
               [
                 {
                   Call.Argument.name = None;
                   value = { Node.value = Expression.Name _; _ } as name_expression;
                 };
               ];
           _;
         };
        ] ->
            let metaclass =
              {
                Call.Argument.name = Some (Node.create ~location "metaclass");
                value = name_expression;
              }
            in
            { class_statement with bases = bases @ [metaclass]; decorators = rest }
        | _ -> class_statement
      in
      match value with
      | Statement.Class class_statement -> Statement.Class (transform_class ~class_statement)
      | _ -> value
    in
    { statement with Node.value = expanded_declaration }
  in
  { source with Source.statements = List.map ~f:inline_six_metaclass statements }


let rec expand_starred_variadic_in_annotation_expression ({ Node.value; _ } as expression) =
  let transform_argument ({ Call.Argument.value; _ } as argument) =
    { argument with Call.Argument.value = expand_starred_variadic_in_annotation_expression value }
  in
  let value =
    match value with
    | Call
        {
          callee =
            {
              Node.value =
                Name
                  (Name.Attribute
                    {
                      base =
                        {
                          Node.value =
                            Name
                              (Name.Attribute
                                {
                                  base =
                                    {
                                      Node.value =
                                        Name
                                          ( Name.Identifier "typing_extensions"
                                          | Name.Identifier "typing" );
                                      _;
                                    };
                                  attribute = "Literal";
                                  _;
                                });
                          _;
                        };
                      attribute = "__getitem__";
                      _;
                    });
              _;
            };
          _;
        } ->
        (* Don't transform arguments in Literals. *)
        value
    | Call
        {
          callee =
            { Node.value = Name (Name.Attribute { attribute = "__getitem__"; _ }); _ } as callee;
          arguments;
        } ->
        Call { callee; arguments = List.map ~f:transform_argument arguments }
    | Expression.Call { callee; arguments = variable_name :: remaining_arguments }
      when name_is ~name:"typing.TypeVar" callee
           || name_is ~name:"$local_typing$TypeVar" callee
           || name_is ~name:"typing_extensions.IntVar" callee ->
        Expression.Call
          {
            callee;
            arguments = variable_name :: List.map ~f:transform_argument remaining_arguments;
          }
    | Starred
        (Once
          {
            Node.value =
              ( Name (Name.Identifier _)
              | Call
                  {
                    callee =
                      { Node.value = Name (Name.Attribute { attribute = "__getitem__"; _ }); _ };
                    _;
                  } ) as starred_value;
            location;
          }) ->
        Expression.Call
          {
            callee =
              {
                Node.location;
                value =
                  Expression.Name
                    (Attribute
                       {
                         base =
                           {
                             Node.location;
                             value =
                               Name
                                 (Name.Attribute
                                    {
                                      base =
                                        {
                                          Node.location;
                                          value = Name (Identifier "pyre_extensions");
                                        };
                                      attribute = "Unpack";
                                      special = false;
                                    });
                           };
                         attribute = "__getitem__";
                         special = true;
                       });
              };
            arguments =
              [
                {
                  name = None;
                  value =
                    expand_starred_variadic_in_annotation_expression
                      { Node.location; value = starred_value };
                };
              ];
          }
    | Tuple elements ->
        Tuple (List.map elements ~f:expand_starred_variadic_in_annotation_expression)
    | List elements -> List (List.map elements ~f:expand_starred_variadic_in_annotation_expression)
    | _ -> value
  in
  { expression with Node.value }


let expand_starred_type_variable_tuple source =
  transform_annotations
    ~transform_annotation_expression:expand_starred_variadic_in_annotation_expression
    source


(* Special syntax added to support configerator. *)
let expand_import_python_calls ({ Source.source_path = { SourcePath.qualifier; _ }; _ } as source) =
  let module Transform = Transform.MakeStatementTransformer (struct
    type t = Reference.t

    let statement qualifier { Node.location; value } =
      let value =
        match value with
        | Statement.Expression
            {
              Node.value =
                Call
                  {
                    callee =
                      { Node.value = Name (Name.Identifier ("import_python" | "import_thrift")); _ };
                    arguments =
                      [
                        {
                          Call.Argument.value =
                            { Node.value = Expression.String { value = from_name; _ }; _ };
                          _;
                        };
                      ];
                  };
              location;
            } ->
            Statement.Import
              {
                Import.from = None;
                imports =
                  [
                    {
                      Import.alias = None;
                      name =
                        {
                          Node.value =
                            Reference.create
                              (String.substr_replace_all ~pattern:"/" ~with_:"." from_name);
                          location;
                        };
                    };
                  ];
              }
        | Statement.Expression
            {
              Node.value =
                Call
                  {
                    callee =
                      { Node.value = Name (Name.Identifier ("import_python" | "import_thrift")); _ };
                    arguments =
                      {
                        Call.Argument.value =
                          { Node.value = Expression.String { value = from_name; _ }; _ };
                        _;
                      }
                      :: imports;
                  };
              location;
            } ->
            let create_import from_name =
              let imports =
                List.filter_map imports ~f:(fun { Call.Argument.value; _ } ->
                    match Node.value value with
                    | Expression.String { value = name; _ } ->
                        Some
                          {
                            Import.alias = None;
                            name =
                              { Node.value = Reference.create name; location = Node.location value };
                          }
                    | _ -> None)
              in
              let formatted_from_name =
                String.substr_replace_all ~pattern:"/" ~with_:"." from_name
              in
              {
                Import.from = Some { Node.value = Reference.create formatted_from_name; location };
                imports;
              }
            in
            Statement.Import (create_import from_name)
        | _ -> value
      in
      qualifier, [{ Node.location; value }]
  end)
  in
  Transform.transform qualifier source |> Transform.source


let preprocess_phase0 source =
  source
  |> expand_relative_imports
  |> replace_platform_specific_code
  |> expand_type_checking_imports
  |> expand_format_string
  |> expand_implicit_returns
  |> expand_import_python_calls


let preprocess_phase1 source =
  source
  |> populate_unbound_names
  |> replace_union_shorthand
  |> expand_starred_type_variable_tuple
  |> qualify
  |> replace_lazy_import
  |> expand_string_annotations
  |> replace_mypy_extensions_stub
  |> expand_typed_dictionary_declarations
  |> expand_sqlalchemy_declarative_base
  |> expand_named_tuples
  |> expand_new_types
  |> inline_six_metaclass
  |> populate_nesting_defines
  |> populate_captures


let preprocess source = preprocess_phase0 source |> preprocess_phase1
