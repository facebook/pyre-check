(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module allows us to preprocess source code before it is analyzed. This allows us to cleanly
   desugar special constructs (e.g., functional Enum/TypedDict declarations into class-based
   declarations) without polluting the type-checking code with special cases.

   We break this into two phases:

   * preprocess_before_wildcards: transformations that don't depend on expanding wildcard imports
   (e.g., selecting platform-specific code)

   * preprocess_after_wildcards: transformations that depend on expanding wildcard imports (e.g.,
   expanding functional TypedDict declarations, since the `TypedDict` may be imported via
   wildcard) *)

open Core
open Ast
open Expression
open Pyre
open Statement

let expand_relative_imports
    ({ Source.module_path = { ModulePath.qualifier; _ } as module_path; _ } as source)
  =
  let module Transform = Transform.MakeStatementTransformer (struct
    type t = Reference.t

    let statement qualifier { Node.location; value } =
      let value =
        match value with
        | Statement.Import { Import.from = Some { Node.value = from; location }; imports }
          when (not (String.equal (Reference.show from) "builtins"))
               && not (String.equal (Reference.show from) "future.builtins") ->
            Statement.Import
              {
                Import.from =
                  Some (Node.create ~location (ModulePath.expand_relative_import module_path ~from));
                imports;
              }
        | _ -> value
      in
      qualifier, [{ Node.location; value }]
  end)
  in
  Transform.transform qualifier source |> Transform.source


let extract_reference_from_callee_name ~scopes value =
  let rec collect ~sofar = function
    | Name.Identifier name -> (
        let open Scope in
        match ScopeStack.lookup (Lazy.force scopes) name with
        | Some
            {
              Access.binding =
                {
                  Binding.kind =
                    Binding.Kind.(ImportName (Import.From { module_name; original_name }));
                  _;
                };
              _;
            } ->
            let original_name = Option.value original_name ~default:name in
            Some
              (List.append (Reference.as_list module_name) (original_name :: sofar)
              |> Reference.create_from_list)
        | Some
            {
              Access.binding =
                { Binding.kind = Binding.Kind.(ImportName (Import.Module { original_name })); _ };
              _;
            } ->
            let original_names =
              Option.value_map original_name ~f:Reference.as_list ~default:[name]
            in
            Some (List.append original_names sofar |> Reference.create_from_list)
        | _ -> Some (Reference.create_from_list (name :: sofar)))
    | Name.Attribute { base = { Node.value = Expression.Name base_name; _ }; attribute; _ } ->
        collect ~sofar:(attribute :: sofar) base_name
    | _ -> None
  in
  collect ~sofar:[] value


let create_callee_name_matcher ~scopes reference_should_match name =
  match extract_reference_from_callee_name ~scopes name with
  | Some reference when reference_should_match reference -> true
  | _ -> false


let create_callee_name_matcher_from_references ~qualifier ~scopes references_to_match =
  let reference_set =
    references_to_match
    |> List.map ~f:(Reference.drop_prefix ~prefix:qualifier)
    |> Reference.Set.of_list
  in
  create_callee_name_matcher ~scopes (Set.mem reference_set)


let is_type_variable_definition callee =
  name_is ~name:"typing.TypeVar" callee
  || name_is ~name:"$local_typing$TypeVar" callee
  || name_is ~name:"typing_extensions.IntVar" callee


let transform_string_annotation_expression_after_qualification ~preserve_original_location ~relative
  =
  let rec transform_expression
      {
        Node.location =
          { Location.start = { Location.line = start_line; column = start_column }; _ } as location;
        value;
      }
    =
    let transform_argument ({ Call.Argument.value; _ } as argument) =
      { argument with Call.Argument.value = transform_expression value }
    in
    let value =
      match value with
      | Expression.Name (Name.Attribute ({ base; _ } as name)) ->
          Expression.Name (Name.Attribute { name with base = transform_expression base })
      | Expression.Subscript { Subscript.base; index; origin } -> (
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
          | _ -> Expression.Subscript { base; index = transform_expression index; origin })
      | Expression.Call { callee; arguments = variable_name :: remaining_arguments; origin }
        when is_type_variable_definition callee ->
          Expression.Call
            {
              callee;
              arguments = variable_name :: List.map ~f:transform_argument remaining_arguments;
              origin;
            }
      | List elements -> List (List.map elements ~f:transform_expression)
      | Constant (Constant.String { StringLiteral.value = string_value; _ }) -> (
          (* Start at column + 1 since parsing begins after the opening quote of the string
             literal. *)
          match
            PyreMenhirParser.Parser.parse
              ~start_line
              ~start_column:(start_column + 1)
              [string_value ^ "\n"]
              ~relative
          with
          | Ok [{ Node.value = Expression ({ Node.value = Name _; _ } as expression); _ }]
          | Ok [{ Node.value = Expression ({ Node.value = Subscript _; _ } as expression); _ }]
          | Ok [{ Node.value = Expression ({ Node.value = BinaryOperator _; _ } as expression); _ }]
          | Ok [{ Node.value = Expression ({ Node.value = Call _; _ } as expression); _ }] ->
              if preserve_original_location then
                Transform.map_location expression ~transform_location:(fun _ -> location)
                |> Node.value
              else
                Node.value expression
          | Ok _
          | Error _ ->
              (* TODO(T76231928): replace this silent ignore with something typeCheck.ml can use *)
              value)
      | Tuple elements -> Tuple (List.map elements ~f:transform_expression)
      | _ -> value
    in
    { Node.value; location }
  in
  transform_expression


let transform_string_annotation_expression_before_qualification
    ~preserve_original_location
    ~qualifier
    ~scopes
    ~relative
  =
  let is_literal =
    create_callee_name_matcher_from_references
      ~qualifier
      ~scopes
      [Reference.create "typing.Literal"; Reference.create "typing_extensions.Literal"]
  in
  let is_type_variable_definition =
    create_callee_name_matcher_from_references
      ~qualifier
      ~scopes
      [Reference.create "typing.TypeVar"; Reference.create "typing_extensions.TypeVar"]
  in
  let rec transform_expression
      {
        Node.location =
          { Location.start = { Location.line = start_line; column = start_column }; _ } as location;
        value;
      }
    =
    let transform_argument ({ Call.Argument.value; _ } as argument) =
      { argument with Call.Argument.value = transform_expression value }
    in
    let value =
      match value with
      | Expression.Name (Name.Attribute ({ base; _ } as name)) ->
          Expression.Name (Name.Attribute { name with base = transform_expression base })
      | Expression.Subscript { Subscript.base; index; origin } -> (
          match base with
          | { Node.value = Expression.Name name; _ } when is_literal name ->
              (* Don't transform arguments in Literals. *)
              value
          | _ -> Expression.Subscript { base; index = transform_expression index; origin })
      | Expression.Call
          {
            callee = { Node.value = Expression.Name name; _ } as callee;
            arguments = variable_name :: remaining_arguments;
            origin;
          }
        when is_type_variable_definition name ->
          Expression.Call
            {
              callee;
              arguments = variable_name :: List.map ~f:transform_argument remaining_arguments;
              origin;
            }
      | List elements -> List (List.map elements ~f:transform_expression)
      | Constant (Constant.String { StringLiteral.value = string_value; _ }) -> (
          (* Start at column + 1 since parsing begins after the opening quote of the string
             literal. *)
          match
            PyreMenhirParser.Parser.parse
              ~start_line
              ~start_column:(start_column + 1)
              [string_value ^ "\n"]
              ~relative
          with
          | Ok [{ Node.value = Expression ({ Node.value = Name _; _ } as expression); _ }]
          | Ok [{ Node.value = Expression ({ Node.value = Subscript _; _ } as expression); _ }]
          | Ok [{ Node.value = Expression ({ Node.value = BinaryOperator _; _ } as expression); _ }]
          | Ok [{ Node.value = Expression ({ Node.value = Call _; _ } as expression); _ }] ->
              if preserve_original_location then
                Transform.map_location expression ~transform_location:(fun _ -> location)
                |> Node.value
              else
                Node.value expression
          | Ok _
          | Error _ ->
              (* TODO(T76231928): replace this silent ignore with something typeCheck.ml can use *)
              value)
      | Tuple elements -> Tuple (List.map elements ~f:transform_expression)
      | _ -> value
    in
    { Node.value; location }
  in
  transform_expression


let transform_annotations
    ~scopes
    ~transform_annotation_expression
    ({ Source.module_path = { ModulePath.qualifier; _ }; _ } as source)
  =
  let is_type_alias =
    create_callee_name_matcher_from_references
      ~qualifier
      ~scopes
      [Reference.create "typing.TypeAlias"; Reference.create "typing_extensions.TypeAlias"]
  in
  let is_type_variable_definition =
    create_callee_name_matcher_from_references
      ~qualifier
      ~scopes
      [Reference.create "typing.TypeVar"; Reference.create "typing_extensions.TypeVar"]
  in
  let is_cast =
    create_callee_name_matcher_from_references
      ~qualifier
      ~scopes
      [Reference.create "pyre_extensions.safe_cast"; Reference.create "typing.cast"]
  in
  let module Transform = Transform.Make (struct
    type t = unit

    let transform_expression_children _ _ = true

    let transform_children state _ = state, true

    let transform_assign ~assign:({ Assign.annotation; value = assign_value; _ } as assign) =
      match annotation with
      | Some { Node.value = Expression.Name name; _ } when is_type_alias name ->
          { assign with Assign.value = assign_value >>| transform_annotation_expression }
      | _ -> (
          match assign_value >>| Node.value with
          | Some (Expression.Call { callee = { Node.value = Expression.Name name; _ }; _ })
            when is_type_variable_definition name ->
              {
                assign with
                Assign.annotation = annotation >>| transform_annotation_expression;
                Assign.value = assign_value >>| transform_annotation_expression;
              }
          | _ -> { assign with Assign.annotation = annotation >>| transform_annotation_expression })


    let statement _ ({ Node.value; _ } as statement) =
      let transform_define
          ({ Define.signature = { parameters; return_annotation; _ }; _ } as define)
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
      let transform_class ~class_statement:({ Class.base_arguments; _ } as class_statement) =
        let transform_base ({ Call.Argument.value; name } as base) =
          let should_transform =
            match name with
            | Some { Node.value = name; _ } -> String.equal name "metaclass"
            | None -> true
          in
          let value = if should_transform then transform_annotation_expression value else value in
          { base with value }
        in
        { class_statement with base_arguments = List.map base_arguments ~f:transform_base }
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
            ({
               Call.Argument.name = None;
               value = { Node.value = Constant (Constant.String _); _ } as value;
             } as type_argument);
            value_argument;
          ] ->
            let annotation = transform_annotation_expression value in
            [{ type_argument with value = annotation }; value_argument]
        | arguments -> arguments
      in
      let value =
        match Node.value expression with
        | Expression.Call
            { callee = { Node.value = Expression.Name name; _ } as callee; arguments; origin }
          when is_cast name ->
            Expression.Call { callee; arguments = transform_arguments arguments; origin }
        | value -> value
      in
      { expression with Node.value }
  end)
  in
  Transform.transform () source |> Transform.source


let expand_string_annotations ~preserve_original_location ({ Source.module_path; _ } as source) =
  let scopes = lazy (Scope.ScopeStack.create source) in
  transform_annotations
    ~scopes
    ~transform_annotation_expression:
      (transform_string_annotation_expression_before_qualification
         ~preserve_original_location
         ~qualifier:(ModulePath.qualifier module_path)
         ~scopes
         ~relative:(ModulePath.relative module_path))
    source


let expand_strings_in_annotation_expression =
  transform_string_annotation_expression_after_qualification
    ~relative:"$path_placeholder_for_alias_string_annotations"


let get_qualified_local_identifier ~qualifier name =
  let qualifier = Reference.show qualifier |> String.substr_replace_all ~pattern:"." ~with_:"?" in
  Format.asprintf "$local_%s$%s" qualifier name


let get_unqualified_local_identifier name =
  match String.is_prefix name ~prefix:"$local_", String.rindex name '$' with
  | true, Some index when index > 0 ->
      let qualifier =
        String.sub name ~pos:7 ~len:(index - 7)
        |> String.substr_replace_all ~pattern:"?" ~with_:"."
        |> Reference.create
      in
      let name = String.drop_prefix name (index + 1) in
      Some (qualifier, name)
  | _ -> None


let get_qualified_parameter name =
  let parameter_prefix = "$parameter$" in
  if String.is_prefix name ~prefix:parameter_prefix then
    name
  else
    parameter_prefix ^ name


let get_unqualified_parameter name =
  if String.is_prefix name ~prefix:"$parameter$" then
    Some (String.drop_prefix name 11)
  else
    None


module Qualify = struct
  type alias = { name: Reference.t }

  type qualify_strings =
    | Qualify
    | OptionallyQualify
    | DoNotQualify

  type scope = {
    module_name: Reference.t;
    parent: NestingContext.t;
    aliases: alias String.Map.t;
    locals: String.Set.t;
  }

  let is_qualified = String.is_prefix ~prefix:"$"

  let qualify_if_needed ~qualifier name =
    if Reference.is_strict_prefix ~prefix:qualifier name then
      name
    else
      Reference.combine qualifier name


  let qualify_local_identifier_ignore_preexisting
      ~scope:({ module_name; parent; aliases; locals; _ } as scope)
      name
    =
    if is_qualified name then
      scope, name
    else
      let qualifier = NestingContext.to_qualifier ~module_name parent in
      let renamed = get_qualified_local_identifier name ~qualifier in
      ( {
          scope with
          aliases = Map.set aliases ~key:name ~data:{ name = Reference.create renamed };
          locals = Set.add locals name;
        },
        renamed )


  let prefix_identifier ~scope:({ aliases; locals; _ } as scope) ~prefix name =
    let stars, name = Identifier.split_star name in
    if is_qualified name then
      None
    else
      let renamed = Format.asprintf "$%s$%s" prefix name in
      Some
        ( {
            scope with
            aliases = Map.set aliases ~key:name ~data:{ name = Reference.create renamed };
            locals = Set.add locals name;
          },
          stars ^ renamed )


  let qualify_reference ~scope:{ aliases; _ } reference =
    match Reference.as_list reference with
    | [] -> Reference.empty
    | head :: tail -> (
        match Map.find aliases head with
        | Some { name; _ } -> Reference.combine name (Reference.create_from_list tail)
        | _ -> reference)


  let qualify_function_name ~scope:({ module_name; parent; aliases; _ } as scope) name =
    match parent with
    | NestingContext.Function _ -> (
        match Reference.as_list name with
        | [simple_name] ->
            let scope, alias = qualify_local_identifier_ignore_preexisting ~scope simple_name in
            scope, Reference.create alias
        | _ -> scope, qualify_reference ~scope name)
    | NestingContext.Class _
    | NestingContext.TopLevel ->
        let scope =
          let qualifier = NestingContext.to_qualifier ~module_name parent in
          let function_name = Reference.show name in
          {
            scope with
            aliases =
              Map.set aliases ~key:function_name ~data:{ name = Reference.combine qualifier name };
          }
        in
        scope, qualify_reference ~scope name


  let qualify_identifier_name ~location ~scope:{ aliases; _ } identifier =
    match Map.find aliases identifier with
    | Some { name; _ } ->
        create_name_from_reference
          ~location
          ~create_origin:(fun identifiers ->
            Some (Origin.create ~location (Origin.Qualification identifiers)))
          name
    | _ -> Name.Identifier identifier


  let rec qualify_comprehension_target ~scope target =
    let rec renamed_scope scope target =
      match target with
      | { Node.value = Expression.Tuple elements; _ } ->
          List.fold elements ~init:scope ~f:renamed_scope
      | { Node.value = Name (Name.Identifier name); _ } -> (
          match prefix_identifier ~scope ~prefix:"target" name with
          | Some (scope, _) -> scope
          | None -> scope)
      | _ -> scope
    in
    let scope = renamed_scope scope target in
    scope, qualify_expression ~qualify_strings:DoNotQualify ~scope target


  and qualify_attribute_name ~qualify_strings ~scope ({ Name.Attribute.base; attribute; _ } as name)
    =
    match base with
    | { Node.value = Name (Name.Identifier "builtins"); _ } -> Name.Identifier attribute
    | _ -> Name.Attribute { name with base = qualify_expression ~qualify_strings ~scope base }


  and qualify_name ~qualify_strings ~location ~scope = function
    | Name.Identifier identifier -> qualify_identifier_name ~location ~scope identifier
    | Name.Attribute attribute -> qualify_attribute_name ~qualify_strings ~scope attribute


  and qualify_type_params type_params ~scope =
    List.map type_params ~f:(fun { Node.value; location } ->
        match value with
        | Ast.Expression.TypeParam.TypeVar { name; bound } ->
            let bound =
              match bound with
              | Some bound -> Some (qualify_expression ~qualify_strings:Qualify ~scope bound)
              | None -> None
            in

            { Node.value = Ast.Expression.TypeParam.TypeVar { name; bound }; location }
        | result -> { Node.value = result; location })


  and qualify_expression ~qualify_strings ~scope ({ Node.location; value } as expression) =
    let value =
      let qualify_entry ~qualify_strings ~scope entry =
        let open Dictionary.Entry in
        match entry with
        | KeyValue { key; value } ->
            KeyValue
              {
                key = qualify_expression ~qualify_strings ~scope key;
                value = qualify_expression ~qualify_strings ~scope value;
              }
        | Splat s -> Splat (qualify_expression ~qualify_strings ~scope s)
      in
      let qualify_generators ~qualify_strings ~scope generators =
        let qualify_generator
            (scope, reversed_generators)
            ({ Comprehension.Generator.target; iterator; conditions; _ } as generator)
          =
          let renamed_scope, target = qualify_comprehension_target ~scope target in
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
      | Expression.Await { Await.operand; origin } ->
          Expression.Await
            { Await.operand = qualify_expression ~qualify_strings ~scope operand; origin }
      | BinaryOperator { BinaryOperator.left; operator; right; origin } ->
          BinaryOperator
            {
              BinaryOperator.left = qualify_expression ~qualify_strings ~scope left;
              operator;
              right = qualify_expression ~qualify_strings ~scope right;
              origin;
            }
      | BooleanOperator { BooleanOperator.left; operator; right; origin } ->
          BooleanOperator
            {
              BooleanOperator.left = qualify_expression ~qualify_strings ~scope left;
              operator;
              right = qualify_expression ~qualify_strings ~scope right;
              origin;
            }
      | Subscript { Subscript.base; index; origin } ->
          let qualified_base = qualify_expression ~qualify_strings ~scope base in
          let qualified_index =
            let qualify_strings =
              if
                name_is ~name:"typing_extensions.Literal" qualified_base
                || name_is ~name:"typing.Literal" qualified_base
              then
                DoNotQualify
              else
                qualify_strings
            in
            qualify_expression ~qualify_strings ~scope index
          in
          Subscript { Subscript.base = qualified_base; index = qualified_index; origin }
      | Call { callee; arguments; origin } ->
          let callee = qualify_expression ~qualify_strings ~scope callee in
          let arguments =
            match arguments with
            | [type_argument; value_argument]
              when name_is ~name:"pyre_extensions.safe_cast" callee
                   || name_is ~name:"typing.cast" callee
                   || name_is ~name:"cast" callee
                   || name_is ~name:"safe_cast" callee ->
                [
                  qualify_argument ~qualify_strings:Qualify ~scope type_argument;
                  qualify_argument ~qualify_strings ~scope value_argument;
                ]
            | [value_argument; type_argument] when name_is ~name:"typing.assert_type" callee ->
                [
                  qualify_argument ~qualify_strings ~scope value_argument;
                  qualify_argument ~qualify_strings:Qualify ~scope type_argument;
                ]
            | variable_name :: remaining_arguments when is_type_variable_definition callee ->
                variable_name
                :: List.map
                     ~f:(qualify_argument ~qualify_strings:Qualify ~scope)
                     remaining_arguments
            | arguments ->
                List.map ~f:(qualify_argument ~qualify_strings:DoNotQualify ~scope) arguments
          in
          Expression.Call { callee; arguments; origin }
      | ComparisonOperator { ComparisonOperator.left; operator; right; origin } ->
          ComparisonOperator
            {
              ComparisonOperator.left = qualify_expression ~qualify_strings ~scope left;
              operator;
              right = qualify_expression ~qualify_strings ~scope right;
              origin;
            }
      | Dictionary entries ->
          Dictionary (List.map entries ~f:(qualify_entry ~qualify_strings ~scope))
      | DictionaryComprehension { Comprehension.element = { key; value }; generators } ->
          let scope, generators = qualify_generators ~qualify_strings ~scope generators in
          DictionaryComprehension
            {
              Comprehension.element =
                {
                  key = qualify_expression ~qualify_strings ~scope key;
                  value = qualify_expression ~qualify_strings ~scope value;
                };
              generators;
            }
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
      | Name name -> Name (qualify_name ~qualify_strings ~location ~scope name)
      | Set elements -> Set (List.map elements ~f:(qualify_expression ~qualify_strings ~scope))
      | SetComprehension { Comprehension.element; generators } ->
          let scope, generators = qualify_generators ~qualify_strings ~scope generators in
          SetComprehension
            {
              Comprehension.element = qualify_expression ~qualify_strings ~scope element;
              generators;
            }
      | Slice { Slice.start; stop; step; origin } ->
          Slice
            {
              Slice.start = start >>| qualify_expression ~qualify_strings ~scope;
              stop = stop >>| qualify_expression ~qualify_strings ~scope;
              step = step >>| qualify_expression ~qualify_strings ~scope;
              origin;
            }
      | Starred (Starred.Once expression) ->
          Starred (Starred.Once (qualify_expression ~qualify_strings ~scope expression))
      | Starred (Starred.Twice expression) ->
          Starred (Starred.Twice (qualify_expression ~qualify_strings ~scope expression))
      | FormatString substrings ->
          let qualify_substring = function
            | Substring.Literal _ as substring -> substring
            | Substring.Format { value; format_spec } ->
                Substring.Format
                  {
                    value = qualify_expression ~qualify_strings ~scope value;
                    format_spec =
                      Option.map ~f:(qualify_expression ~qualify_strings ~scope) format_spec;
                  }
          in
          FormatString (List.map substrings ~f:qualify_substring)
      | Constant (Constant.String { StringLiteral.value; kind }) -> (
          let error_on_qualification_failure =
            match qualify_strings with
            | Qualify -> true
            | _ -> false
          in
          match qualify_strings with
          | Qualify
          | OptionallyQualify -> (
              match PyreMenhirParser.Parser.parse [value ^ "\n"] with
              | Ok [{ Node.value = Expression expression; _ }] ->
                  qualify_expression ~qualify_strings ~scope expression
                  |> Expression.show
                  |> fun value ->
                  Expression.Constant (Constant.String { StringLiteral.value; kind })
              | Ok _
              | Error _
                when error_on_qualification_failure ->
                  let { module_name; _ } = scope in
                  Log.debug
                    "Invalid string annotation `%s` at %a:%a"
                    value
                    Reference.pp
                    module_name
                    Location.pp
                    location;
                  Constant (Constant.String { StringLiteral.value; kind })
              | _ -> Constant (Constant.String { StringLiteral.value; kind }))
          | DoNotQualify -> Constant (Constant.String { StringLiteral.value; kind }))
      | Ternary { Ternary.target; test; alternative } ->
          Ternary
            {
              Ternary.target = qualify_expression ~qualify_strings ~scope target;
              test = qualify_expression ~qualify_strings ~scope test;
              alternative = qualify_expression ~qualify_strings ~scope alternative;
            }
      | Tuple elements -> Tuple (List.map elements ~f:(qualify_expression ~qualify_strings ~scope))
      | WalrusOperator { target; value; origin } ->
          WalrusOperator
            {
              target = qualify_expression ~qualify_strings ~scope target;
              value = qualify_expression ~qualify_strings ~scope value;
              origin;
            }
      | UnaryOperator { UnaryOperator.operator; operand; origin } ->
          UnaryOperator
            {
              UnaryOperator.operator;
              operand = qualify_expression ~qualify_strings ~scope operand;
              origin;
            }
      | Yield (Some expression) ->
          Yield (Some (qualify_expression ~qualify_strings ~scope expression))
      | Yield None -> Yield None
      | YieldFrom expression -> YieldFrom (qualify_expression ~qualify_strings ~scope expression)
      | Constant _ -> value
    in
    { expression with Node.value }


  and qualify_argument { Call.Argument.name; value } ~qualify_strings ~scope =
    let name = name >>| Node.map ~f:get_qualified_parameter in
    { Call.Argument.name; value = qualify_expression ~qualify_strings ~scope value }


  and qualify_parameters ~scope parameters =
    let initial_scope = scope in
    (* Rename parameters to prevent aliasing. *)
    let parameters =
      let qualify_annotation { Node.location; value = { Parameter.annotation; _ } as parameter } =
        {
          Node.location;
          value =
            {
              parameter with
              Parameter.annotation =
                annotation >>| qualify_expression ~qualify_strings:Qualify ~scope;
            };
        }
      in
      List.map parameters ~f:qualify_annotation
    in
    let rename_parameter
        (scope, reversed_parameters)
        ({ Node.value = { Parameter.name; value; annotation }; _ } as parameter)
      =
      match prefix_identifier ~scope ~prefix:"parameter" name with
      | Some (scope, renamed) ->
          ( scope,
            {
              parameter with
              Node.value =
                {
                  Parameter.name = renamed;
                  value =
                    value >>| qualify_expression ~qualify_strings:DoNotQualify ~scope:initial_scope;
                  annotation;
                };
            }
            :: reversed_parameters )
      | None -> scope, parameter :: reversed_parameters
    in
    let scope, parameters =
      List.fold parameters ~init:({ scope with locals = String.Set.empty }, []) ~f:rename_parameter
    in
    scope, List.rev parameters


  let rec qualify_pattern ~scope { Node.value; location } =
    let qualify_pattern = qualify_pattern ~scope in
    let qualify_expression = qualify_expression ~qualify_strings:DoNotQualify ~scope in
    let qualify_match_target name =
      match qualify_identifier_name ~location:Location.any ~scope name with
      | Name.Identifier name -> name
      | _ -> name
    in
    let value =
      match value with
      | Match.Pattern.MatchAs { pattern; name } ->
          Match.Pattern.MatchAs
            { pattern = pattern >>| qualify_pattern; name = qualify_match_target name }
      | MatchClass
          {
            class_name = { Node.value = class_name; location };
            patterns;
            keyword_attributes;
            keyword_patterns;
          } ->
          MatchClass
            {
              class_name =
                Node.create
                  ~location
                  (qualify_name ~qualify_strings:DoNotQualify ~scope ~location class_name);
              patterns = List.map patterns ~f:qualify_pattern;
              keyword_attributes;
              keyword_patterns = List.map keyword_patterns ~f:qualify_pattern;
            }
      | MatchMapping { keys; patterns; rest } ->
          MatchMapping
            {
              keys = List.map keys ~f:qualify_expression;
              patterns = List.map patterns ~f:qualify_pattern;
              rest = rest >>| qualify_match_target;
            }
      | MatchOr patterns -> MatchOr (List.map patterns ~f:qualify_pattern)
      | MatchSequence patterns -> MatchSequence (List.map patterns ~f:qualify_pattern)
      | MatchSingleton constant -> (
          let expression =
            qualify_expression { Node.value = Expression.Constant constant; location }
          in
          match expression.value with
          | Expression.Constant constant -> MatchSingleton constant
          | _ -> MatchValue expression)
      | MatchStar maybe_identifier -> MatchStar (maybe_identifier >>| qualify_match_target)
      | MatchValue expression -> MatchValue (qualify_expression expression)
      | _ -> value
    in
    { Node.value; location }


  let explore_scope ~scope statements =
    let rec explore_locals ({ locals; _ } as scope) { Node.value; _ } =
      match value with
      | Statement.If { If.body; orelse; _ } ->
          let scope = List.fold body ~init:scope ~f:explore_locals in
          List.fold orelse ~init:scope ~f:explore_locals
      | Statement.For { For.body; orelse; _ } ->
          let scope = List.fold body ~init:scope ~f:explore_locals in
          List.fold orelse ~init:scope ~f:explore_locals
      | Statement.Global identifiers ->
          let locals =
            let register_global locals identifier = Set.add locals identifier in
            List.fold identifiers ~init:locals ~f:register_global
          in
          { scope with locals }
      | Statement.Nonlocal identifiers ->
          let locals =
            let register_nonlocal locals identifier = Set.add locals identifier in
            List.fold identifiers ~init:locals ~f:register_nonlocal
          in
          { scope with locals }
      | Statement.Try { Try.body; handlers; orelse; finally; handles_exception_group = _ } ->
          let scope = List.fold body ~init:scope ~f:explore_locals in
          let scope =
            let explore_handler scope { Try.Handler.body; _ } =
              List.fold body ~init:scope ~f:explore_locals
            in
            List.fold handlers ~init:scope ~f:explore_handler
          in
          let scope = List.fold orelse ~init:scope ~f:explore_locals in
          List.fold finally ~init:scope ~f:explore_locals
      | Statement.With { With.body; _ } -> List.fold body ~init:scope ~f:explore_locals
      | Statement.While { While.body; orelse; _ } ->
          let scope = List.fold body ~init:scope ~f:explore_locals in
          List.fold orelse ~init:scope ~f:explore_locals
      | _ -> scope
    in
    let global_alias ~module_name ~parent ~name =
      let qualifier = NestingContext.to_qualifier ~module_name parent in
      { name = Reference.combine qualifier (Reference.create name) }
    in
    let local_alias ~name = { name } in
    let add_local_alias_to_scope ~scope:({ module_name; parent; aliases; locals } as scope) name =
      let renamed =
        if is_qualified name then
          name
        else
          let qualifier = NestingContext.to_qualifier ~module_name parent in
          get_qualified_local_identifier name ~qualifier
      in
      {
        scope with
        aliases = Map.set aliases ~key:name ~data:{ name = Reference.create renamed };
        locals = Set.add locals name;
      }
    in
    let add_class_attribute_alias_to_scope
        ~scope:({ module_name; parent; aliases; _ } as scope)
        name
      =
      let qualified =
        let qualifier = NestingContext.to_qualifier ~module_name parent in
        let sanitized = Identifier.sanitized name in
        Reference.create ~prefix:qualifier sanitized
      in
      { scope with aliases = Map.set aliases ~key:name ~data:(local_alias ~name:qualified) }
    in
    let explore_single_target ?(force_local = false) ~scope:({ parent; locals; _ } as scope) name =
      if NestingContext.is_class parent && not force_local then
        add_class_attribute_alias_to_scope ~scope name
      else if Set.mem locals name then
        scope
      else
        add_local_alias_to_scope ~scope name
    in
    let rec explore_target ?(force_local = false) ~scope = function
      | { Node.value = Expression.Tuple elements; _ }
      | { Node.value = Expression.List elements; _ } ->
          List.fold elements ~init:scope ~f:(fun scope target ->
              explore_target ~force_local ~scope target)
      | { Node.value = Expression.(Starred (Starred.Once element)); _ } ->
          explore_target ~force_local ~scope element
      | { Node.value = Name (Name.Identifier name); _ } ->
          explore_single_target ~force_local ~scope name
      | _ -> scope
    in
    let explore_expression scope expression =
      let fold_walrus_operator ~folder ~state:scope { WalrusOperator.target; value; _ } =
        let scope = explore_target ~scope target in
        let scope = Ast.Expression.Folder.fold ~folder ~state:scope target in
        let scope = Ast.Expression.Folder.fold ~folder ~state:scope value in
        scope
      in
      let folder =
        Ast.Expression.Folder.create_with_uniform_location_fold ~fold_walrus_operator ()
      in
      Ast.Expression.Folder.fold ~folder ~state:scope expression
    in
    let rec explore_statement ({ module_name; parent; aliases; _ } as scope) { Node.value; _ } =
      match value with
      | Statement.Assign
          {
            Assign.target = { Node.value = Expression.Name (Name.Identifier target_name); _ };
            annotation = Some { Node.value = Expression.Name (Name.Identifier "_SpecialForm"); _ };
            _;
          } ->
          {
            scope with
            aliases =
              Map.set
                aliases
                ~key:target_name
                ~data:(global_alias ~module_name ~parent ~name:target_name);
          }
      | Statement.Assign { Assign.target; value; _ } ->
          let scope =
            match value with
            | Some value -> explore_expression scope value
            | None -> scope
          in
          explore_target ~scope target
      | Statement.AugmentedAssign { AugmentedAssign.target; value; _ } ->
          let scope = explore_expression scope value in
          explore_target ~scope target
      | Statement.Class { Class.name; _ } ->
          let class_name = Reference.show name in
          {
            scope with
            aliases =
              Map.set
                aliases
                ~key:class_name
                ~data:(global_alias ~module_name ~parent ~name:class_name);
          }
      | Statement.Define { Define.signature = { name; _ }; _ } ->
          qualify_function_name ~scope name |> fst
      | Statement.Delete expressions -> List.fold expressions ~init:scope ~f:explore_expression
      | Statement.Expression expression -> explore_expression scope expression
      | Statement.If { If.test; body; orelse; _ } ->
          let scope = explore_expression scope test in
          let scope = List.fold body ~init:scope ~f:explore_statement in
          List.fold orelse ~init:scope ~f:explore_statement
      | Statement.Import { Import.from = Some { Node.value = from; _ }; imports = _ }
        when String.equal (Reference.show from) "builtins" ->
          scope
      | Statement.Import { Import.from = Some { Node.value = from; _ }; imports } ->
          let import aliases { Node.value = { Import.name; alias }; _ } =
            match alias with
            | Some alias ->
                (* Add `alias -> from.name`. *)
                Map.set aliases ~key:alias ~data:(local_alias ~name:(Reference.combine from name))
            | None ->
                (* Add `name -> from.name`. *)
                Map.set
                  aliases
                  ~key:(Reference.show name)
                  ~data:(local_alias ~name:(Reference.combine from name))
          in
          { scope with aliases = List.fold imports ~init:aliases ~f:import }
      | Statement.Import { Import.from = None; imports } ->
          let import aliases { Node.value = { Import.name; alias }; _ } =
            match alias with
            | Some alias ->
                (* Add `alias -> from.name`. *)
                Map.set aliases ~key:alias ~data:(local_alias ~name)
            | None -> aliases
          in
          { scope with aliases = List.fold imports ~init:aliases ~f:import }
      | Statement.Match { Match.subject; cases } ->
          let scope = explore_expression scope subject in
          let explore_pattern scope = function
            | Match.Pattern.MatchValue expression -> explore_expression scope expression
            | _ -> scope
          in
          let explore_case scope { Match.Case.pattern = { Node.value = pattern; _ }; guard; body } =
            let scope = explore_pattern scope pattern in
            let scope =
              match guard with
              | Some guard -> explore_expression scope guard
              | None -> scope
            in
            List.fold body ~init:scope ~f:explore_statement
          in
          List.fold cases ~init:scope ~f:explore_case
      | Statement.Raise { Raise.expression; from } ->
          let scope =
            match expression with
            | Some expression -> explore_expression scope expression
            | None -> scope
          in
          let scope =
            match from with
            | Some from -> explore_expression scope from
            | None -> scope
          in
          scope
      | Statement.Return { Return.expression = Some expression; _ } ->
          explore_expression scope expression
      | Statement.Return { Return.expression = None; _ } -> scope
      | Statement.For { For.target; body; orelse; iterator; _ } ->
          let scope = explore_expression scope iterator in
          let scope = explore_target ~force_local:true ~scope target in
          let scope = List.fold body ~init:scope ~f:explore_statement in
          List.fold orelse ~init:scope ~f:explore_statement
      | Statement.Try { Try.body; handlers; orelse; finally; handles_exception_group = _ } ->
          let scope = List.fold body ~init:scope ~f:explore_statement in
          let scope =
            let explore_handler scope { Try.Handler.name; body; _ } =
              let scope =
                match name with
                | Some { Node.value = name; _ } -> explore_single_target ~scope name
                | None -> scope
              in
              List.fold body ~init:scope ~f:explore_statement
            in
            List.fold handlers ~init:scope ~f:explore_handler
          in
          let scope = List.fold orelse ~init:scope ~f:explore_statement in
          List.fold finally ~init:scope ~f:explore_statement
      | Statement.With { With.items; body; _ } ->
          let explore_with_item scope (item, alias) =
            let scope = explore_expression scope item in
            match alias with
            | Some alias -> explore_target ~force_local:true ~scope alias
            | None -> scope
          in
          let scope = List.fold items ~init:scope ~f:explore_with_item in
          List.fold body ~init:scope ~f:explore_statement
      | Statement.While { While.test; body; orelse; _ } ->
          let scope = explore_expression scope test in
          let scope = List.fold body ~init:scope ~f:explore_statement in
          List.fold orelse ~init:scope ~f:explore_statement
      | Statement.TypeAlias { name; _ } -> explore_target ~scope name
      | Statement.Break
      | Statement.Continue
      | Statement.Pass
      | Statement.Assert _
      | Statement.Global _
      | Statement.Nonlocal _ ->
          scope
    in
    let scope = List.fold statements ~init:scope ~f:explore_locals in
    List.fold statements ~init:scope ~f:explore_statement


  let rec qualify_statements ~scope statements = List.map statements ~f:(qualify_statement ~scope)

  and qualify_statement ~scope ({ Node.value; _ } as statement) =
    let value =
      let qualify_assign_target
          ~scope:({ module_name; parent; aliases; _ } as scope)
          ~annotation
          target
        =
        let is_special_form_assignment =
          match target, annotation with
          | ( { Node.value = Expression.Name (Name.Identifier _); _ },
              Some { Node.value = Expression.Name (Name.Identifier "_SpecialForm"); _ } ) ->
              true
          | _ -> false
        in
        if is_special_form_assignment then
          target
        else
          let location = Node.location target in
          let is_class_toplevel = NestingContext.is_class parent in
          match Node.value target with
          | Expression.Name (Name.Identifier name) when is_class_toplevel -> (
              match Map.find aliases name with
              | None -> target
              | Some { name } ->
                  Node.create
                    ~location
                    (Expression.Name
                       (create_name_from_reference
                          ~location
                          ~create_origin:(fun identifiers ->
                            Some (Origin.create ~location (Origin.Qualification identifiers)))
                          name)))
          | Expression.Name
              (Attribute ({ Name.Attribute.origin = attribute_origin; _ } as attribute) as name) ->
              let value =
                if is_class_toplevel then
                  match name_to_reference name with
                  | Some reference ->
                      let qualifier = NestingContext.to_qualifier ~module_name parent in
                      qualify_if_needed ~qualifier reference
                      |> create_name_from_reference ~location ~create_origin:(fun identifiers ->
                             Some
                               (Origin.create
                                  ?base:attribute_origin
                                  ~location
                                  (Origin.Qualification identifiers)))
                      |> fun name -> Expression.Name name
                  | None ->
                      let { Name.Attribute.base; _ } = attribute in
                      let qualified_base =
                        qualify_expression ~qualify_strings:DoNotQualify ~scope base
                      in
                      Expression.Name (Name.Attribute { attribute with base = qualified_base })
                else
                  match qualify_attribute_name ~qualify_strings:DoNotQualify ~scope attribute with
                  | Name.Identifier name ->
                      Expression.Name (Name.Identifier (Identifier.sanitized name))
                  | qualified -> Name qualified
              in
              Node.create ~location value
          | Expression.Tuple _
          | Expression.List _
          | Expression.Starred _
          | Expression.Name (Name.Identifier _)
          | Expression.Subscript _ ->
              qualify_expression ~qualify_strings:DoNotQualify ~scope target
          | _ ->
              (* This case is allowed in the type signatures, but should be prevented by the parser,
                 because the python grammar has no additional valid target forms *)
              target
      in
      let qualify_assign_annotation ~scope annotation =
        qualify_expression ~qualify_strings:Qualify ~scope annotation
      in
      let qualify_assign_value ~scope:({ parent; _ } as scope) ~qualified_annotation value =
        let do_qualify_assign_value ~qualify_potential_alias_strings ~scope value =
          match value with
          | { Node.value = Expression.Constant (Constant.String _); _ } ->
              (* String literal assignments might be type aliases. *)
              qualify_expression ~qualify_strings:qualify_potential_alias_strings value ~scope
          | { Node.value = Subscript _; _ } ->
              qualify_expression ~qualify_strings:qualify_potential_alias_strings value ~scope
          | _ -> qualify_expression ~qualify_strings:DoNotQualify value ~scope
        in
        let qualify_potential_alias_strings =
          match qualified_annotation >>| Expression.show, parent with
          | Some "typing_extensions.TypeAlias", _ -> Qualify
          | None, NestingContext.TopLevel -> OptionallyQualify
          | _, _ -> DoNotQualify
        in
        do_qualify_assign_value ~qualify_potential_alias_strings ~scope value
      in
      let qualify_define
          scope
          ({
             Define.signature =
               { name; parameters; decorators; return_annotation; parent; legacy_parent; _ };
             body;
             _;
           } as define)
        =
        let return_annotation =
          return_annotation >>| qualify_expression ~qualify_strings:Qualify ~scope
        in
        let legacy_parent = legacy_parent >>| fun parent -> qualify_reference ~scope parent in
        let decorators =
          List.map decorators ~f:(qualify_expression ~qualify_strings:DoNotQualify ~scope)
        in
        (* Take care to qualify the function name before parameters, as parameters shadow it. *)
        let _, qualified_function_name = qualify_function_name ~scope name in
        let inner_scope =
          let parent = NestingContext.create_function ~parent (Reference.last name) in
          { scope with parent }
        in
        let inner_scope, parameters = qualify_parameters ~scope:inner_scope parameters in
        let body =
          let scope = explore_scope ~scope:inner_scope body in
          qualify_statements ~scope body
        in
        let signature =
          {
            define.signature with
            name = qualified_function_name;
            parameters;
            decorators;
            return_annotation;
            legacy_parent;
          }
        in
        { define with signature; body }
      in
      let qualify_class
          ({ Class.name; base_arguments; parent; body; decorators; type_params; _ } as definition)
        =
        let qualify_base ({ Call.Argument.value; _ } as argument) =
          {
            argument with
            Call.Argument.value = qualify_expression ~qualify_strings:Qualify ~scope value;
          }
        in
        let decorators =
          List.map decorators ~f:(qualify_expression ~qualify_strings:DoNotQualify ~scope)
        in
        let { module_name; _ } = scope in
        let body =
          let original_scope =
            { scope with parent = NestingContext.create_class ~parent (Reference.last name) }
          in
          let qualify (scope, sofar) ({ Node.value; location } as statement) =
            let new_scope = explore_scope ~scope [statement] in
            let result =
              match value with
              | Statement.Define
                  ({
                     signature = { name; parameters; return_annotation; decorators; type_params; _ };
                     _;
                   } as define) ->
                  let define = qualify_define original_scope define in
                  let _, parameters = qualify_parameters ~scope parameters in
                  let return_annotation =
                    return_annotation >>| qualify_expression ~scope ~qualify_strings:Qualify
                  in
                  let qualify_decorator decorator =
                    let is_reserved name =
                      match Reference.as_list name |> List.rev with
                      | ["staticmethod"]
                      | ["classmethod"]
                      | ["property"]
                      | "getter" :: _
                      | "setter" :: _
                      | "deleter" :: _ ->
                          true
                      | _ -> false
                    in
                    match Decorator.from_expression decorator with
                    | Some { Decorator.name = { Node.value = name; _ }; _ } when is_reserved name ->
                        decorator
                    | _ ->
                        (* TODO (T41755857): Decorator qualification logic should be slightly more
                           involved than this. *)
                        qualify_expression ~qualify_strings:DoNotQualify ~scope decorator
                  in
                  let decorators = List.map decorators ~f:qualify_decorator in
                  let _, name = qualify_function_name ~scope name in
                  let signature =
                    {
                      define.signature with
                      name;
                      parameters;
                      decorators;
                      type_params = qualify_type_params type_params ~scope;
                      return_annotation;
                    }
                  in
                  { Node.value = Statement.Define { define with signature }; location }
              | Statement.Assign { Assign.target; annotation; value; origin } ->
                  let qualified_annotation =
                    Option.map annotation ~f:(qualify_assign_annotation ~scope)
                  in
                  let value =
                    Option.map value ~f:(qualify_assign_value ~scope ~qualified_annotation)
                  in
                  let target = qualify_assign_target ~scope:new_scope ~annotation target in
                  {
                    Node.value =
                      Statement.Assign
                        { Assign.target; annotation = qualified_annotation; value; origin };
                    location;
                  }
              | AugmentedAssign { AugmentedAssign.target; operator; value } ->
                  let value = qualify_assign_value ~scope ~qualified_annotation:None value in
                  let target = qualify_assign_target ~scope:new_scope ~annotation:None target in
                  {
                    Node.value =
                      Statement.AugmentedAssign { AugmentedAssign.target; operator; value };
                    location;
                  }
              | _ -> qualify_statement statement ~scope:new_scope
            in
            new_scope, result :: sofar
          in
          List.fold body ~init:(original_scope, []) ~f:qualify |> snd |> List.rev
        in

        {
          definition with
          (* Ignore aliases, imports, etc. when declaring a class name. *)
          Class.name =
            qualify_if_needed ~qualifier:(NestingContext.to_qualifier ~module_name parent) name;
          base_arguments = List.map base_arguments ~f:qualify_base;
          body;
          type_params = qualify_type_params type_params ~scope;
          decorators;
        }
      in
      match value with
      | Statement.Assign { Assign.target; annotation; value; origin } ->
          let target = qualify_assign_target ~scope ~annotation target in
          let qualified_annotation = Option.map annotation ~f:(qualify_assign_annotation ~scope) in
          let qualified_value =
            Option.map value ~f:(qualify_assign_value ~scope ~qualified_annotation)
          in
          Statement.Assign
            { Assign.target; annotation = qualified_annotation; value = qualified_value; origin }
      | AugmentedAssign { AugmentedAssign.target; operator; value } ->
          let value = qualify_assign_value ~scope ~qualified_annotation:None value in
          let target = qualify_assign_target ~scope ~annotation:None target in
          Statement.AugmentedAssign { AugmentedAssign.target; operator; value }
      | Assert { Assert.test; message; origin } ->
          Assert
            {
              Assert.test = qualify_expression ~qualify_strings:DoNotQualify ~scope test;
              message =
                Option.map message ~f:(qualify_expression ~qualify_strings:DoNotQualify ~scope);
              origin;
            }
      | Class definition -> Class (qualify_class definition)
      | Define define ->
          let define = qualify_define scope define in
          Define define
      | Delete expressions ->
          Delete (List.map expressions ~f:(qualify_expression ~qualify_strings:DoNotQualify ~scope))
      | Expression expression ->
          Expression (qualify_expression ~qualify_strings:DoNotQualify ~scope expression)
      | For ({ For.target; iterator; body; orelse; _ } as block) ->
          let target = qualify_expression ~qualify_strings:DoNotQualify ~scope target in
          let body = qualify_statements ~scope body in
          let orelse = qualify_statements ~scope orelse in
          For
            {
              block with
              For.target;
              iterator = qualify_expression ~qualify_strings:DoNotQualify ~scope iterator;
              body;
              orelse;
            }
      | If { If.test; body; orelse } ->
          let body = qualify_statements ~scope body in
          let orelse = qualify_statements ~scope orelse in
          If
            { If.test = qualify_expression ~qualify_strings:DoNotQualify ~scope test; body; orelse }
      | Match { Match.subject; cases } ->
          let cases = List.map cases ~f:(qualify_match_case ~scope) in
          Match
            {
              Match.subject = qualify_expression ~qualify_strings:DoNotQualify ~scope subject;
              cases;
            }
      | Raise { Raise.expression; from } ->
          Raise
            {
              Raise.expression =
                expression >>| qualify_expression ~qualify_strings:DoNotQualify ~scope;
              from = from >>| qualify_expression ~qualify_strings:DoNotQualify ~scope;
            }
      | Return ({ Return.expression; _ } as return) ->
          Return
            {
              return with
              Return.expression =
                expression >>| qualify_expression ~qualify_strings:DoNotQualify ~scope;
            }
      | Try { Try.body; handlers; orelse; finally; handles_exception_group } ->
          let body = qualify_statements ~scope body in
          let handlers =
            let qualify_exception_name ~scope:{ aliases; _ } { Node.value; location } =
              let value =
                match Map.find aliases value with
                | None -> value
                | Some { name; _ } ->
                    (* FIXME(grievejia): This assumes that `name` is always qualified as a local
                       identifier. This may not be true when we are inside class toplevel. *)
                    Reference.show name
              in
              { Node.value; location }
            in
            let qualify_handler { Try.Handler.kind; name; body } =
              let name = Option.map name ~f:(qualify_exception_name ~scope) in
              let kind = kind >>| qualify_expression ~qualify_strings:DoNotQualify ~scope in
              let body = qualify_statements ~scope body in
              { Try.Handler.kind; name; body }
            in
            List.map handlers ~f:qualify_handler
          in
          let orelse = qualify_statements ~scope orelse in
          let finally = qualify_statements ~scope finally in
          Try { Try.body; handlers; orelse; finally; handles_exception_group }
      | With ({ With.items; body; _ } as block) ->
          let qualify_item ~scope (name, alias) =
            ( qualify_expression ~qualify_strings:DoNotQualify ~scope name,
              Option.map alias ~f:(qualify_expression ~qualify_strings:DoNotQualify ~scope) )
          in
          let items = List.map items ~f:(qualify_item ~scope) in
          let body = qualify_statements ~scope body in
          With { block with With.items; body }
      | While { While.test; body; orelse } ->
          let body = qualify_statements ~scope body in
          let orelse = qualify_statements ~scope orelse in
          While
            {
              While.test = qualify_expression ~qualify_strings:DoNotQualify ~scope test;
              body;
              orelse;
            }
      | TypeAlias { TypeAlias.name; type_params; value } ->
          let value = qualify_assign_value ~scope ~qualified_annotation:None value in
          let target = qualify_assign_target ~scope ~annotation:None name in
          Statement.TypeAlias
            { TypeAlias.name = target; type_params = qualify_type_params type_params ~scope; value }
      | Break
      | Continue
      | Import _
      | Global _
      | Nonlocal _
      | Pass ->
          value
    in
    { statement with Node.value }


  and qualify_match_case ~scope { Match.Case.pattern; guard; body } =
    let body = qualify_statements ~scope body in
    {
      Match.Case.pattern = qualify_pattern ~scope pattern;
      guard = guard >>| qualify_expression ~qualify_strings:DoNotQualify ~scope;
      body;
    }
end

(* Qualification is a way to differentiate names between files/functions/etc. It currently renames the names making them unique.
 * TODO(T47589601) Rewrite qualification more correctly using scope. *)
let qualify ({ Source.module_path = { ModulePath.qualifier; _ }; statements; _ } as source) =
  let scope =
    {
      Qualify.module_name = qualifier;
      parent = NestingContext.create_toplevel ();
      aliases = String.Map.empty;
      locals = String.Set.empty;
    }
  in
  let statements =
    let scope = Qualify.explore_scope ~scope statements in
    Qualify.qualify_statements ~scope statements
  in
  { source with Source.statements }


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
            | Expression.ComparisonOperator { ComparisonOperator.left; operator; right; origin = _ }
              -> (
                match operator with
                | ComparisonOperator.LessThan -> Some (Comparison.LessThan, left, right)
                | ComparisonOperator.LessThanOrEquals ->
                    Some (Comparison.LessThanOrEquals, left, right)
                | ComparisonOperator.GreaterThan -> Some (Comparison.GreaterThan, left, right)
                | ComparisonOperator.GreaterThanOrEquals ->
                    Some (Comparison.GreaterThanOrEquals, left, right)
                | ComparisonOperator.Equals -> Some (Comparison.Equals, left, right)
                | ComparisonOperator.NotEquals -> Some (Comparison.NotEquals, left, right)
                | _ -> None)
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
          let is_system_version_tuple_access_expression ?which_index = function
            | {
                Node.value =
                  Expression.Subscript
                    {
                      Subscript.base =
                        {
                          Node.value =
                            Expression.Name
                              (Name.Attribute
                                {
                                  base = { Node.value = Expression.Name (Name.Identifier "sys"); _ };
                                  attribute = "version_info";
                                  _;
                                });
                          _;
                        };
                      index;
                      origin = _;
                    };
                _;
              } -> (
                match which_index, index with
                | None, _ -> true
                | ( Some expected_index,
                    { Node.value = Expression.Constant (Constant.Integer actual_index); _ } )
                  when Int.equal expected_index actual_index ->
                    true
                | _ -> false)
            | _ -> false
          in

          match extract_comparison test with
          | Some
              ( operator,
                left,
                {
                  Node.value =
                    Expression.Tuple
                      ({
                         Node.value = Expression.Constant (Constant.Integer given_major_version);
                         _;
                       }
                      :: {
                           Node.value = Expression.Constant (Constant.Integer given_minor_version);
                           _;
                         }
                      :: {
                           Node.value = Expression.Constant (Constant.Integer given_micro_version);
                           _;
                         }
                      :: _);
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
                      ({
                         Node.value = Expression.Constant (Constant.Integer given_major_version);
                         _;
                       }
                      :: {
                           Node.value = Expression.Constant (Constant.Integer given_minor_version);
                           _;
                         }
                      :: _);
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
                      ({
                         Node.value = Expression.Constant (Constant.Integer given_major_version);
                         _;
                       }
                      :: _);
                  _;
                } )
            when is_system_version_expression left ->
              evaluate_one_version ~operator major_version given_major_version |> do_replace
          | Some
              ( operator,
                left,
                { Node.value = Expression.Constant (Constant.Integer given_major_version); _ } )
            when is_system_version_tuple_access_expression ~which_index:0 left ->
              evaluate_one_version ~operator major_version given_major_version |> do_replace
          | Some
              ( operator,
                left,
                { Node.value = Expression.Constant (Constant.Integer given_minor_version); _ } )
            when is_system_version_tuple_access_expression ~which_index:1 left ->
              evaluate_one_version ~operator minor_version given_minor_version |> do_replace
          | Some
              ( operator,
                left,
                { Node.value = Expression.Constant (Constant.Integer given_micro_version); _ } )
            when is_system_version_tuple_access_expression ~which_index:2 left ->
              evaluate_one_version ~operator micro_version given_micro_version |> do_replace
          | Some
              ( operator,
                left,
                { Node.value = Expression.Constant (Constant.Integer given_major_version); _ } )
            when is_system_version_attribute_access_expression ~attribute:"major" left ->
              evaluate_one_version ~operator major_version given_major_version |> do_replace
          | Some
              ( operator,
                left,
                { Node.value = Expression.Constant (Constant.Integer given_minor_version); _ } )
            when is_system_version_attribute_access_expression ~attribute:"minor" left ->
              evaluate_one_version ~operator minor_version given_minor_version |> do_replace
          | Some
              ( operator,
                left,
                { Node.value = Expression.Constant (Constant.Integer given_micro_version); _ } )
            when is_system_version_attribute_access_expression ~attribute:"micro" left ->
              evaluate_one_version ~operator micro_version given_micro_version |> do_replace
          | Some
              ( operator,
                {
                  Node.value =
                    Expression.Tuple
                      ({
                         Node.value = Expression.Constant (Constant.Integer given_major_version);
                         _;
                       }
                      :: {
                           Node.value = Expression.Constant (Constant.Integer given_minor_version);
                           _;
                         }
                      :: {
                           Node.value = Expression.Constant (Constant.Integer given_micro_version);
                           _;
                         }
                      :: _);
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
                      ({
                         Node.value = Expression.Constant (Constant.Integer given_major_version);
                         _;
                       }
                      :: {
                           Node.value = Expression.Constant (Constant.Integer given_minor_version);
                           _;
                         }
                      :: _);
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
                      ({
                         Node.value = Expression.Constant (Constant.Integer given_major_version);
                         _;
                       }
                      :: _);
                  _;
                },
                right )
            when is_system_version_expression right ->
              evaluate_one_version
                ~operator:(Comparison.inverse operator)
                major_version
                given_major_version
              |> do_replace
          | Some
              ( operator,
                { Node.value = Expression.Constant (Constant.Integer given_major_version); _ },
                right )
            when is_system_version_tuple_access_expression ~which_index:0 right ->
              evaluate_one_version
                ~operator:(Comparison.inverse operator)
                major_version
                given_major_version
              |> do_replace
          | Some
              ( operator,
                { Node.value = Expression.Constant (Constant.Integer given_minor_version); _ },
                right )
            when is_system_version_tuple_access_expression ~which_index:1 right ->
              evaluate_one_version
                ~operator:(Comparison.inverse operator)
                minor_version
                given_minor_version
              |> do_replace
          | Some
              ( operator,
                { Node.value = Expression.Constant (Constant.Integer given_micro_version); _ },
                right )
            when is_system_version_tuple_access_expression ~which_index:2 right ->
              evaluate_one_version
                ~operator:(Comparison.inverse operator)
                micro_version
                given_micro_version
              |> do_replace
          | Some
              ( operator,
                { Node.value = Expression.Constant (Constant.Integer given_major_version); _ },
                right )
            when is_system_version_attribute_access_expression ~attribute:"major" right ->
              evaluate_one_version
                ~operator:(Comparison.inverse operator)
                major_version
                given_major_version
              |> do_replace
          | Some
              ( operator,
                { Node.value = Expression.Constant (Constant.Integer given_minor_version); _ },
                right )
            when is_system_version_attribute_access_expression ~attribute:"minor" right ->
              evaluate_one_version
                ~operator:(Comparison.inverse operator)
                minor_version
                given_minor_version
              |> do_replace
          | Some
              ( operator,
                { Node.value = Expression.Constant (Constant.Integer given_micro_version); _ },
                right )
            when is_system_version_attribute_access_expression ~attribute:"micro" right ->
              evaluate_one_version
                ~operator:(Comparison.inverse operator)
                micro_version
                given_micro_version
              |> do_replace
          | _ -> (), [statement])
      | _ -> (), [statement]
  end)
  in
  Transform.transform () source |> Transform.source


let replace_platform_specific_code ~sys_platform source =
  let module Transform = Transform.MakeStatementTransformer (struct
    include Transform.Identity

    type t = unit

    let statement _ ({ Node.location; value } as statement) =
      match value with
      | Statement.If { If.test = { Node.value = test; _ }; body; orelse } ->
          let statements =
            let statements =
              let open Expression in
              let get_platform_string platform_check_name left right =
                let is_platform expression =
                  String.equal (Expression.show expression) platform_check_name
                in
                let get_platform_string { Node.value; _ } =
                  match value with
                  | Constant (Constant.String { StringLiteral.value = platform_string; _ }) ->
                      Some platform_string
                  | _ -> None
                in
                if is_platform left then
                  get_platform_string right
                else if is_platform right then
                  get_platform_string left
                else
                  None
              in
              match test with
              | ComparisonOperator
                  { ComparisonOperator.left; operator = Equals | Is; right; origin = _ } -> (
                  match get_platform_string "sys.platform" left right with
                  | Some platform_string ->
                      if String.equal sys_platform platform_string then body else orelse
                  | _ -> [statement])
              | ComparisonOperator
                  { ComparisonOperator.left; operator = NotEquals | IsNot; right; origin = _ } -> (
                  match get_platform_string "sys.platform" left right with
                  | Some platform_string ->
                      if String.equal sys_platform platform_string then orelse else body
                  | _ -> [statement])
              | Call { callee; arguments = [{ Call.Argument.value = expression; _ }]; origin = _ }
                -> (
                  match get_platform_string "sys.platform.startswith" callee expression with
                  | Some platform_string ->
                      if String.is_prefix ~prefix:platform_string sys_platform then
                        body
                      else
                        orelse
                  | _ -> [statement])
              | UnaryOperator
                  {
                    operator = UnaryOperator.Not;
                    operand =
                      {
                        Node.value =
                          Call
                            {
                              callee;
                              arguments = [{ Call.Argument.value = expression; _ }];
                              origin = _;
                            };
                        _;
                      };
                    origin = _;
                  } -> (
                  match get_platform_string "sys.platform.startswith" callee expression with
                  | Some platform_string ->
                      if String.is_prefix ~prefix:platform_string sys_platform then
                        orelse
                      else
                        body
                  | _ -> [statement])
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
      let is_type_checking_with_pyrefly { Node.value; _ } =
        match value with
        | Expression.Name
            (Name.Attribute
              {
                base = { Node.value = Name (Name.Identifier _); _ };
                attribute = "TYPE_CHECKING_WITH_PYREFLY";
                _;
              })
        | Name (Name.Identifier "TYPE_CHECKING_WITH_PYREFLY") ->
            true
        | _ -> false
      in
      match value with
      | Statement.If { If.test; body; _ } when is_type_checking test -> (), body
      | Statement.If { If.test; orelse; _ } when is_type_checking_with_pyrefly test -> (), orelse
      | If
          {
            If.test =
              {
                Node.value = UnaryOperator { UnaryOperator.operator = Not; operand; origin = _ };
                _;
              };
            orelse;
            _;
          }
        when is_type_checking operand ->
          (), orelse
      | If
          {
            If.test =
              {
                Node.value = UnaryOperator { UnaryOperator.operator = Not; operand; origin = _ };
                _;
              };
            body;
            _;
          }
        when is_type_checking_with_pyrefly operand ->
          (), body
      | If
          {
            If.test =
              {
                Node.value =
                  BooleanOperator { BooleanOperator.operator = Or; left; right; origin = _ };
                _;
              };
            body;
            _;
          }
        when is_type_checking left || is_type_checking right ->
          (), body
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

                let expression sofar = function
                  | { Node.value = Expression.Yield _; _ } -> true
                  | { Node.value = Expression.YieldFrom _; _ } -> true
                  | _ -> sofar


                let statement sofar = function
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
                  | _ -> false)
              | _ -> false
            in
            let loops_forever =
              match List.last define.Define.body with
              | Some
                  {
                    Node.value =
                      While { While.test = { Node.value = Constant Constant.True; _ }; _ };
                    _;
                  } ->
                  true
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
                    | Try { body; handlers; orelse; finally; handles_exception_group = _ } ->
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
    ({ Source.module_path = { ModulePath.qualifier; _ }; _ } as source)
  =
  let module Collector = Visit.StatementCollector (struct
    type t = Define.t Node.t

    let visit_children = function
      | { Node.value = Statement.Define _; _ } -> include_nested
      | { Node.value = Class _; _ } -> include_methods
      | _ -> true


    let predicate = function
      | { Node.location; value = Statement.Class class_; _ } when include_toplevels ->
          Class.toplevel_define ~qualifier class_ |> Node.create ~location |> Option.some
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

    let visit_children _ = true

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


let toplevel_assigns source =
  let module Collector = Visit.StatementCollector (struct
    type t = Assign.t Node.t

    let visit_children _ = false

    let predicate = function
      | { Node.location; value = Statement.Assign assign } ->
          Some { Node.location; Node.value = assign }
      | _ -> None
  end)
  in
  Collector.collect source


let toplevel_expand_tuple_assign = function
  | {
      Node.value =
        {
          Assign.target = { Node.value = Expression.Tuple targets; _ };
          value = Some { Node.value = Expression.Tuple values; _ };
          _;
        };
      location;
    }
    when Int.equal (List.length targets) (List.length values) ->
      List.rev_map2_exn targets values ~f:(fun target value ->
          {
            Node.value =
              {
                Assign.target;
                value = Some value;
                annotation = None;
                origin = Some (Origin.create ~location Origin.TopLevelTupleAssign);
              };
            Node.location;
          })
  | assign -> [assign]


let dequalify_map ({ Source.module_path = { ModulePath.qualifier; _ }; _ } as source) =
  let module ImportDequalifier = Transform.MakeStatementTransformer (struct
    include Transform.Identity

    type t = Reference.t Reference.Map.t

    let statement map ({ Node.value; _ } as statement) =
      match value with
      | Statement.Import { Import.from = None; imports } ->
          let add_import map { Node.value = { Import.name; alias }; _ } =
            match alias with
            | Some alias ->
                (* Add `name -> alias`. *)
                Map.set map ~key:name ~data:(Reference.create alias)
            | None -> map
          in
          List.fold_left imports ~f:add_import ~init:map, [statement]
      | Import { Import.from = Some { Node.value = from; _ }; imports } ->
          let add_import map { Node.value = { Import.name; alias }; _ } =
            match alias with
            | Some alias ->
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


let default_is_lazy_import reference =
  Set.mem Recognized.lazy_import_functions (Reference.show reference)


let replace_lazy_import ?(is_lazy_import = default_is_lazy_import) source =
  let scopes = lazy (Scope.ScopeStack.create source) in
  let is_callee_name_lazy_import = create_callee_name_matcher ~scopes is_lazy_import in
  let module LazyImportTransformer = Transform.MakeStatementTransformer (struct
    type t = unit

    let statement _ ({ Node.value; location } as statement) =
      match value with
      | Statement.Assign
          {
            Assign.target = { Node.value = Expression.Name (Name.Identifier identifier); _ };
            value =
              Some
                {
                  Node.value =
                    Expression.Call
                      {
                        callee = { Node.value = Expression.Name callee_name; _ };
                        arguments =
                          [
                            {
                              Call.Argument.value =
                                {
                                  Node.value =
                                    Expression.Constant
                                      (Constant.String
                                        { StringLiteral.kind = String; value = literal });
                                  _;
                                };
                              _;
                            };
                          ];
                        origin = _;
                      };
                  _;
                };
            _;
          }
        when is_callee_name_lazy_import callee_name ->
          ( (),
            [
              Statement.Import
                {
                  from = None;
                  imports =
                    [
                      {
                        Node.value =
                          { Import.name = Reference.create literal; alias = Some identifier };
                        location;
                      };
                    ];
                }
              |> Node.create ~location;
            ] )
      | Statement.Assign
          {
            Assign.target = { Node.value = Expression.Name (Name.Identifier identifier); _ };
            value =
              Some
                {
                  Node.value =
                    Expression.Call
                      {
                        callee = { Node.value = Expression.Name callee_name; _ };
                        arguments =
                          [
                            {
                              Call.Argument.value =
                                {
                                  Node.value =
                                    Expression.Constant
                                      (Constant.String
                                        { StringLiteral.kind = String; value = from_literal });
                                  _;
                                };
                              _;
                            };
                            {
                              Call.Argument.value =
                                {
                                  Node.value =
                                    Expression.Constant
                                      (Constant.String
                                        { StringLiteral.kind = String; value = import_literal });
                                  _;
                                };
                              _;
                            };
                          ];
                        origin = _;
                      };
                  _;
                };
            _;
          }
        when is_callee_name_lazy_import callee_name ->
          ( (),
            [
              Statement.Import
                {
                  from = Some (Node.create_with_default_location (Reference.create from_literal));
                  imports =
                    [
                      {
                        Node.value =
                          { Import.name = Reference.create import_literal; alias = Some identifier };
                        location;
                      };
                    ];
                }
              |> Node.create ~location;
            ] )
      | _ -> (), [statement]
  end)
  in
  LazyImportTransformer.transform () source |> LazyImportTransformer.source


let expand_typed_dictionary_declarations
    ({ Source.statements; module_path = { ModulePath.qualifier; _ }; _ } as source)
  =
  let scopes = lazy (Scope.ScopeStack.create source) in
  let is_typed_dictionatry =
    create_callee_name_matcher_from_references
      ~qualifier
      ~scopes
      [
        Reference.create "mypy_extensions.TypedDict";
        Reference.create "typing_extensions.TypedDict";
        Reference.create "typing.TypedDict";
      ]
  in
  let rec expand_typed_dictionaries ({ Node.location; value } as statement) =
    let expanded_declaration =
      let string_literal identifier =
        Expression.Constant (Constant.String { value = identifier; kind = StringLiteral.String })
        |> Node.create ~location
      in
      let extract_string_literal literal_expression =
        match Node.value literal_expression with
        | Expression.Constant (Constant.String { StringLiteral.value; kind = StringLiteral.String })
          ->
            Some value
        | _ -> None
      in
      let base_is_typed_dictionary = function
        | { Call.Argument.name = None; value = { Node.value = Expression.Name name; _ } }
          when is_typed_dictionatry name ->
            true
        | _ -> false
      in
      let bases_include_typed_dictionary bases = List.exists bases ~f:base_is_typed_dictionary in
      let extract_totality_from_base base =
        match base with
        | {
         Call.Argument.name = Some { value = "total"; _ };
         value = { Node.value = Expression.Constant Constant.True; _ };
        } ->
            Some true
        | {
         Call.Argument.name = Some { value = "total"; _ };
         value = { Node.value = Expression.Constant Constant.False; _ };
        } ->
            Some false
        | _ -> None
      in
      let extract_totality arguments =
        List.find_map arguments ~f:extract_totality_from_base |> Option.value ~default:true
      in
      let typed_dictionary_class_declaration ~name ~parent ~fields ~total ~bases =
        match name with
        | {
         Node.value =
           Expression.Constant (Constant.String { value = class_name; kind = StringLiteral.String });
         _;
        } ->
            let class_reference = Reference.create class_name in
            let assignments =
              let assignment (key, value) =
                match Node.value key with
                | Expression.Constant
                    (Constant.String
                      { StringLiteral.value = attribute_name; kind = StringLiteral.String }) ->
                    Some
                      (Statement.Assign
                         {
                           target =
                             Expression.Name (Name.Identifier attribute_name)
                             |> Node.create ~location;
                           annotation = Some value;
                           value =
                             Some (Node.create ~location (Expression.Constant Constant.Ellipsis));
                           origin = Some (Origin.create ~location Origin.TypedDictImplicitClass);
                         }
                      |> Node.create ~location)
                | _ -> None
              in
              match List.filter_map fields ~f:assignment with
              | [] -> [Node.create ~location Statement.Pass]
              | assignments -> assignments
            in
            (* Filter out TypedDict and total from base class list arguments *)
            let other_base_classes =
              List.filter bases ~f:(fun base ->
                  Option.is_none (extract_totality_from_base base)
                  && not (base_is_typed_dictionary base))
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
                       (Name.Identifier
                          (* Note: Cannot use `Type.TypedDictionary.class_name` because Type is not
                             available here. *)
                          "NonTotalTypedDictionary"));
              }
            in
            Some
              (Statement.Class
                 {
                   name = class_reference;
                   base_arguments =
                     ([
                        {
                          Call.Argument.name = None;
                          value =
                            Node.create
                              ~location
                              (Expression.Name (Name.Identifier "TypedDictionary"));
                        };
                      ]
                     @ other_base_classes
                     @
                     if total then
                       []
                     else
                       [non_total_base]);
                   decorators = [];
                   parent;
                   body = assignments;
                   top_level_unbound_names = [];
                   type_params = [];
                 })
        | _ -> None
      in
      match value with
      | Statement.Assign
          {
            value =
              Some
                {
                  Node.value =
                    Call
                      {
                        callee = { Node.value = Expression.Name callee_name; _ };
                        arguments =
                          { Call.Argument.name = None; value = name }
                          :: {
                               Call.Argument.name = None;
                               value = { Node.value = Dictionary entries; _ };
                               _;
                             }
                          :: argument_tail;
                        origin = _;
                      };
                  _;
                };
            _;
          }
        when is_typed_dictionatry callee_name ->
          extract_string_literal name
          >>= (fun name ->
                typed_dictionary_class_declaration
                  ~name:(string_literal name)
                  ~parent:(NestingContext.create_toplevel ())
                  ~fields:
                    (List.filter_map entries ~f:(fun entry ->
                         let open Dictionary.Entry in
                         match entry with
                         | KeyValue { key; value } -> Some (key, value)
                         | Splat _ -> None))
                  ~total:(extract_totality argument_tail))
                ~bases:[]
          |> Option.value ~default:value
      | Class
          {
            name = class_name;
            base_arguments = bases;
            parent;
            body;
            decorators = _;
            top_level_unbound_names = _;
            type_params = _;
          }
        when bases_include_typed_dictionary bases ->
          let fields =
            let extract = function
              | {
                  Node.value =
                    Statement.Assign
                      {
                        target = { Node.value = Name (Name.Identifier name); _ };
                        annotation = Some annotation;
                        _;
                      };
                  _;
                } ->
                  Some (string_literal name, annotation)
              | _ -> None
            in
            List.filter_map body ~f:extract
          in
          let declaration class_name =
            (* Note: We create the class anew because we don't want to keep any methods. *)
            let class_declaration =
              typed_dictionary_class_declaration
                ~name:(string_literal class_name)
                ~parent
                ~fields
                ~total:(extract_totality bases)
                ~bases
            in
            class_declaration
          in
          declaration (Reference.show class_name) |> Option.value ~default:value
      | Class ({ base_arguments; body; _ } as class_definition) ->
          let replace_totality base =
            match extract_totality_from_base base with
            | Some true -> None
            | Some false ->
                Some
                  {
                    Call.Argument.name = None;
                    value =
                      Expression.Name (Name.Identifier "NonTotalTypedDictionary")
                      |> Node.create ~location;
                  }
            | None -> Some base
          in
          Class
            {
              class_definition with
              base_arguments = List.filter_map base_arguments ~f:replace_totality;
              body = List.map body ~f:expand_typed_dictionaries;
            }
      | If { test; body; orelse } ->
          If
            {
              test;
              body = List.map body ~f:expand_typed_dictionaries;
              orelse = List.map orelse ~f:expand_typed_dictionaries;
            }
      | _ -> value
    in
    { statement with Node.value = expanded_declaration }
  in
  { source with Source.statements = List.map ~f:expand_typed_dictionaries statements }


let expand_named_tuples
    ({ Source.statements; module_path = { ModulePath.qualifier; _ }; _ } as source)
  =
  let scopes = lazy (Scope.ScopeStack.create source) in
  let is_named_tuple =
    create_callee_name_matcher_from_references
      ~qualifier
      ~scopes
      [Reference.create "typing.NamedTuple"; Reference.create "collections.namedtuple"]
  in
  let rec expand_named_tuples ~parent ({ Node.location; value } as statement) =
    let extract_attributes expression =
      match expression with
      | {
       Node.location;
       value =
         Expression.Call { callee = { Node.value = Name callee_name; _ }; arguments; origin = _ };
      }
        when is_named_tuple callee_name ->
          let should_rename_invalid_attributes =
            let extract_rename_argument argument =
              match argument with
              | {
               Call.Argument.name = Some { value = "rename"; _ };
               value = { Node.value = Expression.Constant Constant.True; _ };
              } ->
                  Some true
              | {
               Call.Argument.name = Some { value = "rename"; _ };
               value = { Node.value = Expression.Constant Constant.False; _ };
              } ->
                  Some false
              | _ -> None
            in
            List.find_map arguments ~f:extract_rename_argument |> Option.value ~default:false
          in
          let any_annotation =
            Expression.Name (create_name ~location ~create_origin:(fun _ -> None) "typing.Any")
            |> Node.create ~location
          in
          let attribute_default_values =
            let extract_default_values argument =
              match argument with
              | {
               Call.Argument.name = Some { value = "defaults"; _ };
               value = { Node.value = Expression.Tuple items; _ };
              } ->
                  Some items
              | _ -> None
            in
            List.find_map arguments ~f:extract_default_values |> Option.value ~default:[]
          in
          let attributes =
            match arguments with
            (* Example form: namedtuple('T', 'a, b, c') *)
            | _
              :: {
                   Call.Argument.value =
                     {
                       value = Constant (Constant.String { StringLiteral.value = serialized; _ });
                       _;
                     };
                   _;
                 }
              :: _ ->
                Str.split (Str.regexp "[, ]") serialized
                |> List.map ~f:(fun name -> name, any_annotation, None)
                |> List.filter ~f:(fun (name, _, _) -> not (String.is_empty name))
            (* Example forms:
             * namedtuple('T', ["a", "b", "c"]
             * namedtuple('T', [("a", int), ("b", int), ("c", int)])
             * namedtuple('T', ("a", "b", "c")
             * namedtuple('T', (("a", int), ("b", int), ("c", int))) *)
            | _ :: { Call.Argument.value = { Node.value = List arguments; _ }; _ } :: _
            | _ :: { Call.Argument.value = { Node.value = Tuple arguments; _ }; _ } :: _ ->
                let get_name ({ Node.value; _ } as expression) =
                  match value with
                  | Expression.Constant (Constant.String { StringLiteral.value = name; _ }) ->
                      name, any_annotation, None
                  | Tuple
                      [
                        {
                          Node.value = Constant (Constant.String { StringLiteral.value = name; _ });
                          _;
                        };
                        annotation;
                      ] ->
                      name, annotation, None
                  | _ -> Expression.show expression, any_annotation, None
                in
                List.map arguments ~f:get_name
            (* Example form: namedtuple(a=int, b=int, c=int) *)
            | _ :: arguments ->
                List.filter_map arguments ~f:(fun argument ->
                    match argument with
                    | { Call.Argument.name = Some { Node.value = "rename" | "defaults"; _ }; _ } ->
                        None
                    | { Call.Argument.name = Some { Node.value = name; _ }; value } ->
                        Some (name, value, None)
                    | _ -> None)
            | _ -> []
          in
          (* https://docs.python.org/3/library/collections.html#namedtuple-factory-function-for-tuples-with-named-fields
             if rename=True is set, invalid or duplicate field names are renamed to their index with
             an underscore prefix *)
          let rename_attributes attributes =
            Core.List.foldi
              ~f:(fun idx (attributes, seen) ((name, annotation, value) as attribute) ->
                if
                  should_rename_invalid_attributes
                  && (Set.mem seen name || not (Identifier.is_valid_identifier name))
                then
                  ("_" ^ string_of_int idx, annotation, value) :: attributes, seen
                else
                  attribute :: attributes, Set.add seen name)
              ~init:([], Identifier.Set.empty)
              attributes
            |> fst
          in
          let rec assign_attribute_defaults defaults attributes =
            match attributes, defaults with
            | _, []
            | [], _ ->
                attributes
            | (name, annotation, None) :: attributes, default :: defaults ->
                (name, annotation, Some default) :: assign_attribute_defaults defaults attributes
            | attribute :: attributes, defaults ->
                attribute :: assign_attribute_defaults defaults attributes
          in
          (* rename_attributes reverses the list of attributes, but that's OK since we want to
             assign default values starting from the end. After default values are assigned, we
             reverse the attributes so they are ordered correctly. *)
          Some
            (attributes
            |> rename_attributes
            |> assign_attribute_defaults (List.rev attribute_default_values)
            |> List.rev)
      | _ -> None
    in
    let fields_attribute ~location attributes =
      let node = Node.create ~location in
      let value =
        attributes
        |> List.map ~f:(fun (name, _, _) ->
               Expression.Constant (Constant.String (StringLiteral.create name)) |> node)
        |> (fun parameters -> Expression.Tuple parameters)
        |> node
      in
      let fields_annotation =
        let create_name name =
          Expression.Name (create_name ~location ~create_origin:(fun _ -> None) name) |> node
        in
        let tuple_members =
          match List.length attributes with
          | 0 -> [Expression.Tuple [] |> node]
          | l -> List.init l ~f:(fun _ -> create_name "str")
        in
        let tuple_annotation =
          subscript_for_annotation "typing.Tuple" ~location tuple_members |> node
        in
        subscript_for_annotation "typing.ClassVar" ~location [tuple_annotation] |> node
      in
      Statement.Assign
        {
          Assign.target = Node.create ~location (Expression.Name (Name.Identifier "_fields"));
          annotation = Some fields_annotation;
          value = Some value;
          origin = Some (Origin.create ~location Origin.NamedTupleImplicitFields);
        }
      |> Node.create ~location
    in
    let tuple_attributes ~location attributes =
      let attribute_statements =
        let attribute { Node.value = name, annotation, default_value; location } =
          let target = Node.create ~location (Expression.Name (Name.Identifier name)) in
          let annotation =
            let location = Node.location annotation in
            {
              Node.location;
              value =
                Expression.Subscript
                  {
                    base =
                      Reference.create "typing.Final"
                      |> Ast.Expression.from_reference ~location ~create_origin:(fun _ -> None);
                    index = annotation;
                    origin = None;
                  };
            }
          in
          Statement.Assign
            {
              Assign.target;
              annotation = Some annotation;
              value =
                (default_value
                >>| fun _ -> Node.create (Expression.Constant Constant.Ellipsis) ~location);
              origin = Some (Origin.create ~location Origin.NamedTupleImplicitFields);
            }
          |> Node.create ~location
        in
        List.map attributes ~f:attribute
      in
      let fields_attribute = List.map attributes ~f:Node.value |> fields_attribute ~location in
      fields_attribute :: attribute_statements
    in
    let tuple_constructors ~class_name ~parent ~location attributes =
      let parameters =
        let to_parameter (name, annotation, value) =
          let value =
            match value with
            | Some { Node.value = Expression.Constant Constant.Ellipsis; _ } -> None
            | _ -> value
          in
          Parameter.create ?value ~location ~annotation ~name ()
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
                        origin = None;
                      })),
              Parameter.create ~location ~name:"cls" () )
          else
            ( "__init__",
              Node.create ~location (Expression.Constant Constant.NoneLiteral),
              Parameter.create ~location ~name:"self" () )
        in
        let assignments =
          if is_new || List.is_empty parameters then
            [
              Node.create
                ~location
                (Statement.Expression
                   (Node.create ~location (Expression.Constant Constant.Ellipsis)));
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
                                 base = Node.create (Expression.Name (Identifier "self")) ~location;
                                 attribute = name;
                                 origin =
                                   Some
                                     (Origin.create
                                        ~location
                                        (Origin.NamedTupleConstructorAssignment name));
                               }));
                     annotation = None;
                     value = Some (Node.create (Expression.Name (Identifier name)) ~location);
                     origin = Some (Origin.create ~location Origin.NamedTupleImplicitFields);
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
                name = Reference.create name;
                parameters = self_parameter :: parameters;
                decorators = [];
                return_annotation = Some return_annotation;
                async = false;
                generator = false;
                parent;
                legacy_parent = Some class_name;
                type_params = [];
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
        value =
          {
            Node.location;
            value = Name (create_name ~location ~create_origin:(fun _ -> None) "typing.NamedTuple");
          };
      }
    in
    let value =
      match value with
      | Statement.Assign
          { Assign.target = { Node.value = Name (Name.Identifier name); _ }; value = expression; _ }
        -> (
          match Option.map expression ~f:extract_attributes |> Option.join with
          | Some attributes
          (* TODO (T42893621): properly handle the excluded case *)
            when not (String.is_prefix ~prefix:"cls" name) ->
              let constructors =
                tuple_constructors
                  ~class_name:(Reference.create name)
                  ~parent:(NestingContext.create_class ~parent name)
                  ~location
                  attributes
              in
              let attributes =
                List.map attributes ~f:(Node.create ~location) |> tuple_attributes ~location
              in
              Statement.Class
                {
                  Class.name = Reference.create name;
                  base_arguments = [tuple_base ~location];
                  parent;
                  body = constructors @ attributes;
                  decorators = [];
                  top_level_unbound_names = [];
                  type_params = [];
                }
          | _ -> value)
      | Class ({ Class.name; base_arguments; parent; body; _ } as original) ->
          (* TODO(yangdanny): move synthesizing _fields, __init__, __new__, etc out of
             preprocessing *)
          let is_named_tuple_primitive = function
            | { Call.Argument.value = { Node.value = Name callee_name; _ }; _ } ->
                is_named_tuple callee_name
            | _ -> false
          in
          if List.exists ~f:is_named_tuple_primitive base_arguments then
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
                      Expression.Name
                        (create_name ~location ~create_origin:(fun _ -> None) "typing.Any")
                      |> Node.create ~location
                    in
                    Option.value annotation ~default:any
                  in
                  Either.First (Node.create ~location (last, annotation, value))
              | statement -> Either.Second statement
            in
            let attributes, other = List.partition_map body ~f:extract_assign in
            let constructors =
              List.map attributes ~f:Node.value
              |> tuple_constructors
                   ~class_name:name
                   ~parent:(NestingContext.create_class ~parent (Reference.last name))
                   ~location
            in
            let tuple_attributes = tuple_attributes ~location attributes in
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
                          String.equal (Reference.last name) generated_name
                      | _ -> false
                    in
                    if
                      List.exists body ~f:(has_name "__new__")
                      || List.exists body ~f:(has_name "__init__")
                    then
                      []
                    else
                      tuple_constructors
                        ~class_name:name
                        ~parent:(NestingContext.create_class ~parent (Reference.last name))
                        ~location
                        attributes
                  in
                  let attributes =
                    List.map attributes ~f:(Node.create ~location) |> tuple_attributes ~location
                  in
                  tuple_base ~location :: bases, attributes_sofar @ constructors @ attributes
              | None -> base :: bases, attributes_sofar
            in
            let reversed_bases, attributes =
              List.fold base_arguments ~init:([], []) ~f:extract_named_tuples
            in
            Class
              {
                original with
                Class.base_arguments = List.rev reversed_bases;
                body =
                  attributes
                  @ List.map
                      ~f:
                        (expand_named_tuples
                           ~parent:(NestingContext.create_class ~parent (Reference.last name)))
                      body;
              }
      | Define ({ Define.signature = { Define.Signature.name; _ }; body; _ } as define) ->
          Define
            {
              define with
              Define.body =
                List.map
                  ~f:
                    (expand_named_tuples
                       ~parent:(NestingContext.create_function ~parent (Reference.last name)))
                  body;
            }
      | _ -> value
    in
    { statement with Node.value }
  in
  {
    source with
    Source.statements =
      List.map ~f:(expand_named_tuples ~parent:(NestingContext.create_toplevel ())) statements;
  }


let expand_new_types ({ Source.statements; module_path = { ModulePath.qualifier; _ }; _ } as source)
  =
  let scopes = lazy (Scope.ScopeStack.create source) in
  let is_newtype =
    create_callee_name_matcher_from_references
      ~qualifier
      ~scopes
      [Reference.create "typing.NewType"]
  in
  let create_class_for_newtype
      ~location
      ~base_argument:
        ({
           (* TODO (T44209017): Error on invalid annotation expression *)
           Call.Argument.value = base;
           _;
         } as base_argument)
      name
    =
    let class_parent = NestingContext.create_toplevel () in
    let constructor =
      Statement.Define
        {
          signature =
            {
              name = Reference.create "__init__";
              parameters =
                [
                  Parameter.create ~location ~name:"self" ();
                  Parameter.create ~location ~annotation:base ~name:"input" ();
                ];
              decorators = [];
              return_annotation =
                Some (Node.create ~location (Expression.Constant Constant.NoneLiteral));
              async = false;
              generator = false;
              parent = NestingContext.create_class ~parent:class_parent (Reference.last name);
              legacy_parent = Some name;
              type_params = [];
            };
          captures = [];
          unbound_names = [];
          body = [Node.create Statement.Pass ~location];
        }
      |> Node.create ~location
    in
    Statement.Class
      {
        Class.name;
        base_arguments = [base_argument];
        parent = class_parent;
        body = [constructor];
        decorators = [];
        top_level_unbound_names = [];
        type_params = [];
      }
  in
  let expand_new_type ({ Node.location; value } as statement) =
    let value =
      match value with
      | Statement.Assign
          {
            Assign.value =
              Some
                {
                  Node.value =
                    Call
                      {
                        callee = { Node.value = Expression.Name callee_name; _ };
                        arguments =
                          [
                            {
                              Call.Argument.value =
                                {
                                  Node.value =
                                    Constant (Constant.String { StringLiteral.value = name; _ });
                                  _;
                                };
                              _;
                            };
                            base_argument;
                          ];
                        origin = _;
                      };
                  _;
                };
            _;
          }
        when is_newtype callee_name ->
          create_class_for_newtype ~location ~base_argument (Reference.create name)
      | _ -> value
    in
    { statement with Node.value }
  in
  { source with Source.statements = List.map statements ~f:expand_new_type }


let expand_sqlalchemy_declarative_base
    ({ Source.statements; module_path = { ModulePath.qualifier; _ }; _ } as source)
  =
  let expand_declarative_base_instance ({ Node.location; value } as statement) =
    let expanded_declaration =
      let declarative_base_class_declaration class_name =
        let metaclass =
          {
            Call.Argument.name = Some (Node.create ~location "metaclass");
            value =
              Node.create
                ~location
                (Expression.Name
                   (create_name
                      ~location
                      ~create_origin:(fun _ -> None)
                      "sqlalchemy.ext.declarative.DeclarativeMeta"));
          }
        in
        Statement.Class
          {
            name = Reference.create class_name;
            base_arguments = [metaclass];
            decorators = [];
            parent = NestingContext.create_toplevel ();
            body = [Node.create ~location Statement.Pass];
            top_level_unbound_names = [];
            type_params = [];
          }
      in
      let scopes = lazy (Scope.ScopeStack.create source) in
      let is_declarative_base =
        create_callee_name_matcher_from_references
          ~qualifier
          ~scopes
          [Reference.create "sqlalchemy.ext.declarative.declarative_base"]
      in
      match value with
      | Statement.Assign
          {
            target = { Node.value = Name (Name.Identifier target); _ };
            value =
              Some { Node.value = Call { callee = { Node.value = Name callee_name; _ }; _ }; _ };
            _;
          }
        when is_declarative_base callee_name ->
          declarative_base_class_declaration target
      | _ -> value
    in
    { statement with Node.value = expanded_declaration }
  in
  { source with Source.statements = List.map ~f:expand_declarative_base_instance statements }


let type_param_names_set type_params =
  List.map type_params ~f:(fun { Node.value; _ } ->
      match value with
      | Ast.Expression.TypeParam.TypeVar { name; _ } -> name
      | Ast.Expression.TypeParam.TypeVarTuple name -> name
      | Ast.Expression.TypeParam.ParamSpec name -> name)
  |> Identifier.Set.of_list


let remove_bound_names type_param_names_set =
  Set.filter ~f:(fun { Define.NameAccess.name; _ } -> not (Set.mem type_param_names_set name))


module NameAccessSet = Set.Make (Define.NameAccess)
module CaptureSet = Set.Make (Define.Capture)

module AccessCollector = struct
  let rec from_expression collected { Node.value; location = expression_location } =
    let open Expression in
    let from_entry collected entry =
      let open Dictionary.Entry in
      match entry with
      | KeyValue { key; value } ->
          let collected = from_expression collected key in
          from_expression collected value
      | Splat s -> from_expression collected s
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
              not (Set.mem bound_names name))
        in
        Set.union unbound_names_in_body collected
    | Name (Name.Identifier identifier) ->
        (* For simple names, add them to the result *)
        Set.add collected { Define.NameAccess.name = identifier; location = expression_location }
    | Name (Name.Attribute { Name.Attribute.base; _ }) ->
        (* For attribute access, only count the base *)
        from_expression collected base
    (* The rest is boilerplates to make sure that expressions are visited recursively *)
    | Await { Await.operand; origin = _ } -> from_expression collected operand
    | BinaryOperator { BinaryOperator.left; right; _ }
    | BooleanOperator { BooleanOperator.left; right; _ }
    | ComparisonOperator { ComparisonOperator.left; right; _ } ->
        let collected = from_expression collected left in
        from_expression collected right
    | Slice { Slice.start; stop; step; origin = _ } ->
        let collected = Option.value_map start ~default:collected ~f:(from_expression collected) in
        let collected = Option.value_map stop ~default:collected ~f:(from_expression collected) in
        Option.value_map step ~default:collected ~f:(from_expression collected)
    | Subscript { Subscript.base; index; origin = _ } ->
        let collected = from_expression collected base in
        from_expression collected index
    | Call { Call.callee; arguments; origin = _ } ->
        let collected = from_expression collected callee in
        List.fold arguments ~init:collected ~f:(fun collected { Call.Argument.value; _ } ->
            from_expression collected value)
    | Dictionary entries -> List.fold entries ~init:collected ~f:from_entry
    | DictionaryComprehension comprehension ->
        from_comprehension
          (fun collected Dictionary.Entry.KeyValue.{ key; value } ->
            let collected = from_expression collected key in
            from_expression collected value)
          collected
          comprehension
    | Generator comprehension
    | ListComprehension comprehension
    | SetComprehension comprehension ->
        from_comprehension from_expression collected comprehension
    | List expressions
    | Set expressions
    | Tuple expressions ->
        List.fold expressions ~init:collected ~f:from_expression
    | FormatString substrings ->
        let from_substring sofar = function
          | Substring.Literal _ -> sofar
          | Substring.Format format ->
              let sofar = from_expression sofar format.value in
              Option.value_map format.format_spec ~default:sofar ~f:(from_expression sofar)
        in
        List.fold substrings ~init:collected ~f:from_substring
    | Starred (Starred.Once expression)
    | Starred (Starred.Twice expression) ->
        from_expression collected expression
    | Ternary { Ternary.target; test; alternative } ->
        let collected = from_expression collected target in
        let collected = from_expression collected test in
        from_expression collected alternative
    | UnaryOperator { UnaryOperator.operand; _ } -> from_expression collected operand
    | WalrusOperator { WalrusOperator.value; _ } -> from_expression collected value
    | Yield expression ->
        Option.value_map expression ~default:collected ~f:(from_expression collected)
    | YieldFrom expression -> from_expression collected expression
    | Constant _ -> collected


  (* Generators are as special as lambdas -- they bind their own names, which we want to exclude *)
  and from_comprehension :
        'a.
        (NameAccessSet.t -> 'a -> NameAccessSet.t) ->
        NameAccessSet.t ->
        'a Comprehension.t ->
        NameAccessSet.t
    =
   fun from_element collected { Comprehension.element; generators } ->
    let remove_bound_names ~bound_names =
      Set.filter ~f:(fun { Define.NameAccess.name; _ } -> not (Set.mem bound_names name))
    in
    let bound_names, collected =
      let from_generator
          (bound_names, accesses_sofar)
          { Comprehension.Generator.target; iterator; conditions; _ }
        =
        let iterator_accesses =
          from_expression NameAccessSet.empty iterator |> remove_bound_names ~bound_names
        in
        let bound_names =
          let add_bound_name bound_names { Define.NameAccess.name; _ } = Set.add bound_names name in
          from_expression NameAccessSet.empty target |> Set.fold ~init:bound_names ~f:add_bound_name
        in
        let condition_accesses =
          List.fold conditions ~init:NameAccessSet.empty ~f:from_expression
          |> remove_bound_names ~bound_names
        in
        ( bound_names,
          NameAccessSet.union_list [accesses_sofar; iterator_accesses; condition_accesses] )
      in
      List.fold generators ~init:(Identifier.Set.empty, collected) ~f:from_generator
    in
    let element_accesses =
      from_element NameAccessSet.empty element |> remove_bound_names ~bound_names
    in
    Set.union collected element_accesses


  and from_statement collected { Node.value; _ } =
    let from_optional_expression collected =
      Option.value_map ~default:collected ~f:(from_expression collected)
    in
    (* Boilerplates to visit all statements that may contain accesses *)
    match value with
    | Statement.Assign { Assign.target; value; annotation; _ } ->
        let collected = from_expression collected target in
        let collected = from_optional_expression collected annotation in
        from_optional_expression collected value
    | Statement.AugmentedAssign { AugmentedAssign.target; value; _ } ->
        let collected = from_expression collected target in
        from_expression collected value
    | Assert { Assert.test; message; _ } ->
        let collected = from_expression collected test in
        Option.value_map message ~f:(from_expression collected) ~default:collected
    | Class { Class.base_arguments; decorators; type_params; _ } ->
        let remove_bound_type_param_names =
          type_param_names_set type_params |> remove_bound_names
        in
        let collected =
          List.fold base_arguments ~init:collected ~f:(fun sofar { Call.Argument.value; _ } ->
              from_expression sofar value)
        in
        List.fold ~init:collected ~f:from_expression decorators |> remove_bound_type_param_names
    | Define
        {
          Define.signature =
            { Define.Signature.decorators; parameters; return_annotation; type_params; _ };
          _;
        } ->
        let remove_bound_type_param_names =
          type_param_names_set type_params |> remove_bound_names
        in
        let collected = List.fold ~init:collected ~f:from_expression decorators in
        let collected =
          List.fold
            parameters
            ~init:collected
            ~f:(fun sofar { Node.value = { Parameter.annotation; value; _ }; _ } ->
              let sofar = from_optional_expression sofar annotation in
              let sofar = sofar |> remove_bound_type_param_names in
              from_optional_expression sofar value)
        in
        let collected_from_return_annotataion =
          from_optional_expression collected return_annotation
        in
        let collected_from_return_annotataion =
          collected_from_return_annotataion |> remove_bound_type_param_names
        in
        let collected = Set.union collected_from_return_annotataion collected in
        collected
    | Delete expressions -> List.fold expressions ~init:collected ~f:from_expression
    | Expression expression -> from_expression collected expression
    | For { For.target; iterator; body; orelse; _ } ->
        let collected = from_expression collected target in
        let collected = from_expression collected iterator in
        let collected = from_statements collected body in
        from_statements collected orelse
    | Match { Match.subject; cases } ->
        let collected = from_expression collected subject in
        let from_case collected { Match.Case.pattern; guard; body } =
          let rec from_pattern collected { Node.value; _ } =
            match value with
            | Match.Pattern.MatchAs { pattern; _ } ->
                Option.value_map ~default:collected ~f:(from_pattern collected) pattern
            | MatchClass
                {
                  class_name = { Node.value = class_name; location };
                  patterns;
                  keyword_patterns;
                  _;
                } ->
                let collected =
                  from_expression collected (Node.create ~location (Expression.Name class_name))
                in
                let collected = List.fold ~f:from_pattern ~init:collected patterns in
                List.fold ~f:from_pattern ~init:collected keyword_patterns
            | MatchMapping { patterns; _ }
            | MatchOr patterns
            | MatchSequence patterns ->
                List.fold ~f:from_pattern ~init:collected patterns
            | MatchValue expression -> from_expression collected expression
            | MatchSingleton _
            | MatchStar _
            | MatchWildcard ->
                collected
          in
          let collected = from_pattern collected pattern in
          let collected = from_optional_expression collected guard in
          from_statements collected body
        in
        List.fold ~f:from_case ~init:collected cases
    | If { If.test; body; orelse }
    | While { While.test; body; orelse } ->
        let collected = from_expression collected test in
        let collected = from_statements collected body in
        from_statements collected orelse
    | Raise { Raise.expression; from } ->
        let collected = from_optional_expression collected expression in
        from_optional_expression collected from
    | Return { Return.expression; _ } -> from_optional_expression collected expression
    | Try { Try.body; handlers; orelse; finally; handles_exception_group = _ } ->
        let collected = from_statements collected body in
        let collected =
          List.fold handlers ~init:collected ~f:(fun collected { Try.Handler.kind; name; body } ->
              let collected =
                Option.value_map kind ~f:(from_expression collected) ~default:collected
              in
              let collected =
                Option.value_map
                  name
                  ~f:(fun { Node.value = name; location } ->
                    Set.add collected { Define.NameAccess.name; location })
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
    | TypeAlias { TypeAlias.name; value; type_params; _ } ->
        (* remove type prarams from the accesses *)
        let type_param_names_set = type_param_names_set type_params in
        let remove_bound_names = remove_bound_names type_param_names_set in
        let collected = from_expression collected name in
        from_optional_expression collected (Some value) |> remove_bound_names
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
            | Scope.Kind.Define ({ Define.Signature.legacy_parent; _ } as signature) -> (
                match binding_kind with
                | Binding.Kind.(ClassName | ImportName _) ->
                    (* Judgement call: these bindings are (supposedly) not useful for type
                       checking *)
                    None
                | Binding.Kind.(ParameterName { star = Some Star.Twice; annotation; _ }) ->
                    let dictionary_annotation value_annotation =
                      {
                        Node.location;
                        value =
                          Expression.Subscript
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
                                           origin = None;
                                         });
                                };
                              index =
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
                              origin = None;
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
                                          origin = None;
                                        });
                               })
                      | Some
                          ({
                             Node.value =
                               Expression.Name (Name.Attribute { attribute = "kwargs"; _ });
                             _;
                           } as annotation) ->
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
                          Expression.Subscript
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
                                           origin = None;
                                         });
                                };
                              index =
                                {
                                  Node.location;
                                  value =
                                    Expression.Tuple
                                      [
                                        value_annotation;
                                        {
                                          Node.location;
                                          value = Expression.Constant Constant.Ellipsis;
                                        };
                                      ];
                                };
                              origin = None;
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
                                          origin = None;
                                        });
                               })
                      | Some
                          ({
                             Node.value = Expression.Name (Name.Attribute { attribute = "args"; _ });
                             _;
                           } as annotation) ->
                          (* Heuristic: If the annotation is of the form `XXX.args`, treat it as
                             ParamSpec annotation. *)
                          Some annotation
                      | Some value_annotation -> Some (tuple_annotation value_annotation)
                    in
                    Some { Define.Capture.name; kind = Annotation annotation }
                | Binding.Kind.(ParameterName { star = None; index = 0; annotation })
                  when Option.is_some legacy_parent
                       && Option.is_none annotation
                       && not (Define.Signature.is_static_method signature) ->
                    let parent = Option.value_exn legacy_parent in
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
                | Binding.Kind.(
                    ComprehensionTarget | ForTarget | MatchTarget | WalrusTarget | WithTarget) ->
                    Some { Define.Capture.name; kind = Annotation None })))
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
            | Some capture -> Set.add sofar capture
          in
          Set.fold ~init:CaptureSet.empty accesses ~f:(to_capture ~is_decorator:false ~scopes)
          |> Set.to_list
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
    | {
     Node.location;
     value = Try { Try.body; orelse; finally; handlers; handles_exception_group };
    } ->
        let body = transform_statements ~scopes body in
        let orelse = transform_statements ~scopes orelse in
        let finally = transform_statements ~scopes finally in
        let handlers =
          List.map handlers ~f:(fun ({ Try.Handler.body; _ } as handler) ->
              let body = transform_statements ~scopes body in
              { handler with body })
        in
        {
          Node.location;
          value = Try { Try.body; orelse; finally; handlers; handles_exception_group };
        }
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


let populate_unbound_names ({ Source.module_path = { ModulePath.qualifier; _ }; _ } as source) =
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
      Set.fold access_set ~init:Identifier.Map.empty ~f:accumulate_names
      |> Map.data
      |> NameAccessSet.of_list
    in
    let to_unbound_name ~scopes sofar name =
      match to_unbound_name ~scopes name with
      | None -> sofar
      | Some name -> Set.add sofar name
    in
    deduplicate_access accesses
    |> Set.fold ~init:NameAccessSet.empty ~f:(to_unbound_name ~scopes)
    |> Set.to_list
  in
  let rec transform_statement ~scopes statement =
    match statement with
    (* Process each define *)
    | {
     Node.location;
     value =
       Statement.Define
         ({ Define.signature = { Define.Signature.type_params; _ }; body; _ } as define);
    } ->
        let scopes =
          if Define.is_toplevel define then
            scopes
          else
            ScopeStack.extend scopes ~with_:(Scope.of_define_exn define)
        in
        (* collect class type parameters and remove them from the access set *)
        (* let type_param_names_set = type_param_names_set type_params in *)
        let remove_bound_type_param_names =
          type_param_names_set type_params |> remove_bound_names
        in
        let unbound_names = AccessCollector.from_define define |> compute_unbound_names ~scopes in
        let unbound_names =
          Set.to_list (NameAccessSet.of_list unbound_names |> remove_bound_type_param_names)
        in
        let body = transform_statements ~scopes body in
        { Node.location; value = Statement.Define { define with body; unbound_names } }
    | { Node.location; value = Class ({ Class.body; type_params; _ } as class_) } ->
        let top_level_unbound_names =
          let scopes =
            ScopeStack.extend
              scopes
              ~with_:(Scope.of_define_exn (Class.toplevel_define ~qualifier class_))
          in
          AccessCollector.from_class class_ |> compute_unbound_names ~scopes
        in
        (* Use parent scope here as classes do not open up new scopes for the methods defined in
           it. *)
        let body = transform_statements ~scopes body in

        (* collect class type parameters and remove them from the access set *)
        let type_param_names_set = type_param_names_set type_params in
        let remove_bound_names = remove_bound_names type_param_names_set in
        let top_level_unbound_names =
          Set.to_list (NameAccessSet.of_list top_level_unbound_names |> remove_bound_names)
        in

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
    | {
     Node.location;
     value = Try { Try.body; orelse; finally; handlers; handles_exception_group };
    } ->
        let body = transform_statements ~scopes body in
        let orelse = transform_statements ~scopes orelse in
        let finally = transform_statements ~scopes finally in
        let handlers =
          List.map handlers ~f:(fun ({ Try.Handler.body; _ } as handler) ->
              let body = transform_statements ~scopes body in
              { handler with body })
        in
        {
          Node.location;
          value = Try { Try.body; orelse; finally; handlers; handles_exception_group };
        }
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


let replace_union_shorthand_in_annotation_expression =
  let rec transform_expression ({ Node.value; location } as expression) =
    let flatten_typing_unions sofar part_of_union_expression =
      match Node.value part_of_union_expression with
      | Expression.Subscript
          {
            base =
              {
                value =
                  Name
                    (Name.Attribute
                      {
                        base = { Node.value = Name (Name.Identifier "typing"); _ };
                        attribute = "Union";
                        origin = _;
                      });
                _;
              };
            index = { Node.value = Tuple index_expressions; _ };
            origin = _;
          } ->
          List.concat [sofar; index_expressions] |> List.rev
      | _ -> part_of_union_expression :: sofar
    in
    let value =
      match value with
      | Expression.BinaryOperator { operator = BinaryOperator.BitOr; left; right; origin } ->
          let indices =
            [left; right]
            (* Recursively transform them into `typing.Union[...]` form *)
            |> List.map ~f:transform_expression
            (* Flatten all of the inner `typing.Union`s (and use `rev` to preserve order) *)
            |> List.fold ~init:[] ~f:flatten_typing_unions
            |> List.rev
          in
          let index = { Node.value = Expression.Tuple indices; location } in
          let origin = Some (Origin.create ?base:origin ~location Origin.UnionShorthand) in
          Expression.Subscript
            {
              base =
                {
                  Node.location;
                  value =
                    Name
                      (Name.Attribute
                         {
                           base = { Node.location; value = Name (Name.Identifier "typing") };
                           attribute = "Union";
                           origin;
                         });
                };
              index;
              origin;
            }
      | Subscript { Subscript.base; index; origin } ->
          Subscript { base; index = transform_expression index; origin }
      | Tuple arguments -> Tuple (List.map ~f:transform_expression arguments)
      | List arguments -> List (List.map ~f:transform_expression arguments)
      | _ -> value
    in
    { expression with Node.value }
  in
  transform_expression


let replace_union_shorthand source =
  let module Transform = Transform.Make (struct
    type t = unit

    let transform_expression_children _ _ = true

    let transform_children state _ = state, true

    let statement _ ({ Node.value; _ } as statement) =
      let transform_assign ~assign:({ Assign.annotation; _ } as assign) =
        {
          assign with
          Assign.annotation = annotation >>| replace_union_shorthand_in_annotation_expression;
        }
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
                Parameter.annotation =
                  annotation >>| replace_union_shorthand_in_annotation_expression;
              };
          }
        in
        let signature =
          {
            define.signature with
            parameters = List.map parameters ~f:parameter;
            return_annotation =
              return_annotation >>| replace_union_shorthand_in_annotation_expression;
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
        {
          argument with
          Call.Argument.value = replace_union_shorthand_in_annotation_expression value;
        }
      in
      let value =
        match Node.value expression with
        | Expression.Call { callee; arguments = value_argument :: type_argument :: rest; origin }
        (* Note: Union shorthand expansion happens before qualification *)
          when name_is ~name:"assert_type" callee ->
            let arguments = value_argument :: transform_argument type_argument :: rest in
            Expression.Call { callee; arguments; origin }
        | Expression.Call { callee; arguments; origin }
          when name_is ~name:"isinstance" callee || name_is ~name:"issubclass" callee ->
            let arguments = List.map ~f:transform_argument arguments in
            Expression.Call { callee; arguments; origin }
        | Expression.Call { callee; arguments; origin } when name_is ~name:"TypeVar" callee ->
            let arguments =
              List.map
                ~f:(fun argument ->
                  match argument with
                  | { name = Some { Node.value = "bound"; _ }; _ } -> transform_argument argument
                  | _ -> argument)
                arguments
            in
            Expression.Call { callee; arguments; origin }
        | value -> value
      in
      { expression with Node.value }
  end)
  in
  Transform.transform () source |> Transform.source


let mangle_private_attributes source =
  let module PrivateAttributeTransformer = struct
    type class_data = {
      mangling_prefix: Identifier.t;
      metadata: Ast.Statement.Class.t;
    }

    type t = class_data list

    let mangle_reference class_name reference =
      let mangled_identifier =
        Identifier.mangle_private_name ~class_name (Reference.last reference) |> Reference.create
      in
      match Reference.prefix reference with
      | Some prefix -> Reference.combine prefix mangled_identifier
      | None -> mangled_identifier


    let should_mangle identifier =
      (* Ensure there are at least two leading underscores and at most one trailing underscore. *)
      let thrift_typing_import_special_case =
        (* TODO(T97954725): Remove special casing *)
        String.equal identifier "__T"
      in
      Identifier.is_private_name identifier && not thrift_typing_import_special_case


    let transform_expression_children _ _ = true

    let transform_children state statement =
      match state, Node.value statement with
      | _, Statement.Class ({ name; _ } as class_statement) ->
          let mangling_prefix =
            name
            |> Reference.last
            |> String.lstrip ~drop:(fun character -> Char.equal character '_')
          in
          let class_data =
            { mangling_prefix; metadata = { class_statement with Ast.Statement.Class.body = [] } }
          in
          class_data :: state, true
      | _, _ -> state, true


    (* TODO(T199670045): This transformation does not play well with `parent` field, which needs to
       be fixed. *)
    let statement state ({ Node.value; _ } as statement) =
      let state, statement =
        match state, value with
        | ( { metadata; _ } :: ({ mangling_prefix; _ } :: _ as tail),
            Statement.Class { name; body; _ } )
          when should_mangle (Reference.last name) ->
            ( tail,
              {
                statement with
                value =
                  Statement.Class
                    { metadata with name = mangle_reference mangling_prefix name; body };
              } )
        | { metadata; _ } :: tail, Statement.Class { body; _ } ->
            (* Ensure identifiers following class body are not mangled. *)
            tail, { statement with value = Statement.Class { metadata with body } }
        | ( { mangling_prefix; _ } :: _,
            Statement.Define
              ({
                 Define.signature = { Define.Signature.name; parent; legacy_parent; _ } as signature;
                 _;
               } as define) )
          when should_mangle (Reference.last name) ->
            let parent, legacy_parent =
              (* Update if the parent of this statement is a class that itself is private and will
                 be mangled. *)
              match state, parent with
              | ( _ :: { mangling_prefix = parent_prefix; _ } :: _,
                  NestingContext.Class { name; parent } )
                when should_mangle name ->
                  let mangled_parent =
                    NestingContext.create_class
                      ~parent
                      (Identifier.mangle_private_name ~class_name:parent_prefix name)
                  in
                  let mangled_legacy_parent =
                    Option.map legacy_parent ~f:(mangle_reference parent_prefix)
                  in
                  mangled_parent, mangled_legacy_parent
              | _ -> parent, legacy_parent
            in
            ( state,
              {
                statement with
                value =
                  Statement.Define
                    {
                      define with
                      signature =
                        {
                          signature with
                          name = mangle_reference mangling_prefix name;
                          parent;
                          legacy_parent;
                        };
                    };
              } )
        | _ -> state, statement
      in
      state, [statement]


    let expression state ({ Node.value; _ } as expression) =
      let open Expression in
      let transformed_expression =
        match state, value with
        | { mangling_prefix; _ } :: _, Name (Name.Identifier identifier)
          when should_mangle identifier ->
            Name
              (Name.Identifier
                 (Identifier.mangle_private_name ~class_name:mangling_prefix identifier))
        | { mangling_prefix; _ } :: _, Name (Name.Attribute ({ attribute; _ } as name))
          when should_mangle attribute ->
            Name
              (Name.Attribute
                 {
                   name with
                   attribute = Identifier.mangle_private_name ~class_name:mangling_prefix attribute;
                 })
        | _ -> value
      in
      { expression with Node.value = transformed_expression }
  end
  in
  let module Transform = Transform.Make (PrivateAttributeTransformer) in
  Transform.transform [] source |> Transform.source


let inline_six_metaclass ({ Source.statements; _ } as source) =
  let inline_six_metaclass ({ Node.location; value } as statement) =
    let expanded_declaration =
      let transform_class
          ~class_statement:({ Class.base_arguments; decorators; _ } as class_statement)
        =
        let is_six_add_metaclass_decorator expression =
          match Decorator.from_expression expression with
          | Some ({ Decorator.name = { Node.value = name; _ }; _ } as decorator)
            when Reference.equal name (Reference.create_from_list ["six"; "add_metaclass"]) ->
              Either.First decorator
          | _ -> Either.Second expression
        in
        let six_add_metaclass_decorators, rest =
          List.partition_map decorators ~f:is_six_add_metaclass_decorator
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
            {
              class_statement with
              base_arguments = base_arguments @ [metaclass];
              decorators = rest;
            }
        | _ -> class_statement
      in
      match value with
      | Statement.Class class_statement -> Statement.Class (transform_class ~class_statement)
      | _ -> value
    in
    { statement with Node.value = expanded_declaration }
  in
  { source with Source.statements = List.map ~f:inline_six_metaclass statements }


(* Special syntax added to support configerator. *)
let expand_import_python_calls ({ Source.module_path = { ModulePath.qualifier; _ }; _ } as source) =
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
                            {
                              Node.value =
                                Expression.Constant (Constant.String { value = from_name; _ });
                              _;
                            };
                          _;
                        };
                      ];
                    origin = _;
                  };
              location;
            } ->
            Statement.Import
              {
                Import.from = None;
                imports =
                  [
                    {
                      Node.value =
                        {
                          Import.alias = None;
                          name =
                            Reference.create
                              (String.substr_replace_all ~pattern:"/" ~with_:"." from_name);
                        };
                      location;
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
                          {
                            Node.value =
                              Expression.Constant (Constant.String { value = from_name; _ });
                            _;
                          };
                        _;
                      }
                      :: imports;
                    origin = _;
                  };
              _;
            } ->
            let create_import from_name =
              let imports =
                List.filter_map imports ~f:(fun { Call.Argument.value; _ } ->
                    match Node.value value with
                    | Expression.Constant (Constant.String { value = name; _ }) ->
                        Some
                          {
                            Node.value = { Import.alias = None; name = Reference.create name };
                            location = Node.location value;
                          }
                    | _ -> None)
              in
              let formatted_from_name =
                String.substr_replace_all ~pattern:"/" ~with_:"." from_name
              in
              {
                Import.from =
                  Some (Node.create_with_default_location (Reference.create formatted_from_name));
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


(* Pytorch uses `self.register_buffer("foo", tensor)` to implicitly create an attribute `foo`.

   Preprocess this into `self.foo: Tensor = tensor` to add the attribute and avoid spurious "missing
   attribute" errors. *)
let expand_pytorch_register_buffer
    ({ Source.module_path = { ModulePath.qualifier; _ }; _ } as source)
  =
  let scopes = lazy (Scope.ScopeStack.create source) in
  let is_register_buffer =
    create_callee_name_matcher_from_references
      ~qualifier
      ~scopes
      [Reference.create "self.register_buffer"]
  in
  let module TransformRegisterBuffer = Transform.MakeStatementTransformer (struct
    type t = unit

    let statement _ ({ Node.value; location = statement_location } as statement) =
      match value with
      | Statement.Expression
          {
            Node.value =
              Expression.Call
                {
                  callee = { Node.value = Expression.Name callee_name; _ };
                  arguments =
                    {
                      name = None;
                      value =
                        {
                          Node.value =
                            Constant (Constant.String { value = attribute_name; kind = String });
                          _;
                        };
                    }
                    :: { value = initial_value; _ }
                    :: ([] | [{ name = Some { Node.value = "persistent"; _ }; _ }]);
                  origin;
                };
            location;
          }
        when is_register_buffer callee_name ->
          let annotation =
            Node.create
              ~location
              (Expression.Name
                 (Name.Attribute
                    {
                      base = Node.create ~location (Expression.Name (Name.Identifier "torch"));
                      attribute = "Tensor";
                      origin = None;
                    }))
            |> Option.some_if (not (is_none initial_value))
          in
          ( (),
            [
              Statement.Assign
                {
                  target =
                    Node.create
                      ~location
                      (Expression.Name
                         (Name.Attribute
                            {
                              base =
                                Node.create ~location (Expression.Name (Name.Identifier "self"));
                              attribute = attribute_name;
                              origin;
                            }));
                  annotation;
                  value = Some initial_value;
                  origin = Some (Origin.create ~location Origin.PyTorchRegisterBuffer);
                }
              |> Node.create ~location:statement_location;
            ] )
      | _ -> (), [statement]
  end)
  in
  let module TransformConstructor = Transform.MakeStatementTransformer (struct
    type t = unit

    let statement _ statement =
      match statement with
      | { Node.value = Statement.Define define; _ } when Define.is_constructor define ->
          let transform_constructor constructor =
            Source.create [constructor]
            |> TransformRegisterBuffer.transform ()
            |> TransformRegisterBuffer.source
            |> Source.statements
          in
          (), transform_constructor statement
      | _ -> (), [statement]
  end)
  in
  TransformConstructor.transform () source |> TransformConstructor.source


(* Inline the KW_ONLY pseudo-field into field(kw_only) for all subsequent attributes. *)
let add_dataclass_keyword_only_specifiers
    ({ Source.module_path = { ModulePath.qualifier; _ }; _ } as source)
  =
  let scopes = lazy (Scope.ScopeStack.create source) in
  let is_dataclass =
    create_callee_name_matcher_from_references
      ~qualifier
      ~scopes
      [Reference.create "dataclasses.dataclass"]
  in
  let is_dataclass_keyword_only =
    create_callee_name_matcher_from_references
      ~qualifier
      ~scopes
      [Reference.create "dataclasses.KW_ONLY"]
  in
  let is_dataclass_field =
    create_callee_name_matcher_from_references
      ~qualifier
      ~scopes
      [Reference.create "dataclasses.field"]
  in
  let is_dataclass_decorator { Node.value; _ } =
    match value with
    | Expression.Name name
    | Expression.Call { callee = { Node.value = Expression.Name name; _ }; _ } ->
        is_dataclass name
    | _ -> false
  in
  let is_keyword_only_pseudo_field statement =
    match statement with
    | {
     Node.value =
       Statement.Assign
         { target = _; annotation = Some { Node.value = Expression.Name name; _ }; _ };
     _;
    } ->
        is_dataclass_keyword_only name
    | _ -> false
  in
  let is_not_keyword_only_pseudo_field statement = not (is_keyword_only_pseudo_field statement) in
  let is_keyword_only_argument = function
    | { Call.Argument.name = Some { value = "kw_only"; _ }; _ } -> true
    | _ -> false
  in
  let keyword_only_argument =
    {
      Call.Argument.name = Some ("kw_only" |> Node.create_with_default_location);
      value = Expression.Constant Constant.True |> Node.create_with_default_location;
    }
  in
  let set_keyword_only_field statement_node =
    match statement_node with
    | {
     Node.value = Statement.Assign { value = expression_node; target; annotation; origin };
     location;
    } ->
        (* TODO: T101298692 don't substitute ellipsis for missing RHS of assignment *)
        let expression_node =
          Option.value
            expression_node
            ~default:(Node.create ~location (Expression.Constant Ellipsis))
        in
        let attribute_value =
          match expression_node with
          | {
           value =
             Expression.Call
               { callee = { value = Expression.Name callee_name; _ } as callee; arguments; origin };
           _;
          }
            when is_dataclass_field callee_name ->
              let arguments =
                if List.exists arguments ~f:is_keyword_only_argument then
                  arguments
                else
                  keyword_only_argument :: arguments
              in
              let expression = Expression.Call { callee; arguments; origin } in
              { expression_node with value = expression }
          | { value; _ } ->
              let arguments =
                [
                  keyword_only_argument;
                  {
                    Call.Argument.name = Some (Node.create_with_default_location "default");
                    value = value |> Node.create_with_default_location;
                  };
                ]
              in
              let origin = Some (Origin.create ~location Origin.DataclassImplicitField) in
              let callee =
                Expression.Name
                  (Name.Attribute
                     {
                       base =
                         Expression.Name (Name.Identifier "dataclasses")
                         |> Node.create_with_default_location;
                       attribute = "field";
                       origin;
                     })
                |> Node.create_with_default_location
              in
              { expression_node with value = Expression.Call { callee; arguments; origin } }
        in
        {
          statement_node with
          value = Statement.Assign { target; annotation; value = Some attribute_value; origin };
        }
    | _ -> statement_node
  in
  let set_keyword_only_after_pseudo_field body =
    let before, after = List.split_while body ~f:is_not_keyword_only_pseudo_field in
    let after = List.filter after ~f:is_not_keyword_only_pseudo_field in
    before @ List.map after ~f:set_keyword_only_field
  in

  let module TransformDataclassKeywordOnlyAttributes = Transform.MakeStatementTransformer (struct
    type t = unit

    let statement _ ({ Node.value; location } as statement) =
      match value with
      | Statement.Class
          { name; base_arguments; parent; body; decorators; top_level_unbound_names; type_params }
        when List.exists decorators ~f:is_dataclass_decorator
             && List.exists body ~f:is_keyword_only_pseudo_field ->
          ( (),
            [
              {
                Node.value =
                  Statement.Class
                    {
                      name;
                      base_arguments;
                      parent;
                      body = set_keyword_only_after_pseudo_field body;
                      decorators;
                      top_level_unbound_names;
                      type_params;
                    };
                location;
              };
            ] )
      | _ -> (), [statement]
  end)
  in
  TransformDataclassKeywordOnlyAttributes.transform () source
  |> TransformDataclassKeywordOnlyAttributes.source


module SelfType = struct
  let is_synthetic_type_variable type_name =
    String.is_prefix ~prefix:"_Self_" type_name && String.is_suffix ~suffix:"__" type_name


  let self_variable_name class_reference =
    Format.asprintf "_Self_%s__" (Reference.as_list class_reference |> String.concat ~sep:"_")


  let is_self ~qualifier ~scopes =
    create_callee_name_matcher_from_references
      ~qualifier
      ~scopes
      [Reference.create "typing.Self"; Reference.create "typing_extensions.Self"]


  let replace_self_type_with ~qualifier ~scopes ~synthetic_type_variable =
    let map_name ~mapper:_ name =
      if is_self ~qualifier ~scopes name then
        create_name_from_reference
          ~location:Location.any
          ~create_origin:(fun _ -> None)
          synthetic_type_variable
      else
        name
    in
    Mapper.map ~mapper:(Mapper.create_transformer ~map_name ())


  (* Ideally, we would return the potentially-transformed expression along with an indication of
     whether it was changed. But our current `Visit.Transformer` or `Expression.Mapper` APIs aren't
     powerful enough to express such a use. So, we do a separate check to decide whether we want to
     replace Self types in a signature. *)
  let signature_uses_self_type
      ~qualifier
      ~scopes
      { Define.Signature.return_annotation; parameters; _ }
    =
    let expression_uses_self_type expression =
      let fold_name ~folder:_ ~state name = is_self ~qualifier ~scopes name || state in

      let folder = Folder.create_with_uniform_location_fold ~fold_name () in
      Folder.fold ~folder ~state:false expression
    in
    let parameter_uses_self_type =
      List.exists parameters ~f:(fun { Node.value = { annotation; _ }; _ } ->
          annotation >>| expression_uses_self_type |> Option.value ~default:false)
    in
    let return_annotation_uses_self_type =
      return_annotation >>| expression_uses_self_type |> Option.value ~default:false
    in
    parameter_uses_self_type || return_annotation_uses_self_type


  (* Return true if `self/cls` parameter can be bound using a synthesized TypeVar bound to the
     current class.

     If `self/cls` is unannotated, then we can bind it with `self: <Self_TypeVar>` or `cls:
     Type[Self_TypeVar]`.

     If it has `self: ReadOnly[Self]` or `cls: ReadOnly[Type[Self]]`, then it can still be bound by
     replacing the `typing.Self` with a synthesized `Self_TypeVar`.

     Otherwise, if we have a concrete annotation, such as `self: Foo`, then we cannot bind it using
     a synthesized TypeVar. *)
  let can_bind_self_parameter_using_self_type_variable ~qualifier ~scopes parameter =
    let is_readonly =
      create_callee_name_matcher_from_references
        ~qualifier
        ~scopes
        [
          Reference.create "typing._PyreReadOnly_";
          Reference.create "pyre_extensions.ReadOnly";
          Reference.create "pyre_extensions.PyreReadOnly";
        ]
    in
    let is_type =
      create_callee_name_matcher_from_references
        ~qualifier
        ~scopes
        [Reference.create "typing.Type"; Reference.create "typing_extensions.Type"]
    in
    let is_self = is_self ~qualifier ~scopes in
    match parameter with
    | { Node.value = { Parameter.annotation = None; _ }; _ } :: _ -> true
    | {
        Node.value =
          {
            Parameter.annotation =
              Some
                {
                  Node.value =
                    Subscript
                      {
                        base = { Node.value = Expression.Name base_name; _ };
                        index = { Node.value = Expression.Name index_name; _ };
                        origin = _;
                      };
                  _;
                };
            _;
          };
        _;
      }
      :: _ ->
        is_readonly base_name && is_self index_name
    | {
        Node.value =
          {
            Parameter.annotation =
              Some
                {
                  Node.value =
                    Subscript
                      {
                        base = { Node.value = Expression.Name base_name; _ };
                        index =
                          {
                            Node.value =
                              Subscript
                                {
                                  base = { Node.value = Expression.Name inner_base_name; _ };
                                  index = { Node.value = Expression.Name inner_index_name; _ };
                                  origin = _;
                                };
                            _;
                          };
                        origin = _;
                      };
                  _;
                };
            _;
          };
        _;
      }
      :: _ ->
        is_readonly base_name && is_type inner_base_name && is_self inner_index_name
    | _ -> false


  let replace_self_type_in_signature
      ~qualifier
      ~scopes
      ({ Define.Signature.parameters; return_annotation; parent; _ } as signature)
    =
    match
      ( parent,
        can_bind_self_parameter_using_self_type_variable ~qualifier ~scopes parameters,
        parameters,
        return_annotation )
    with
    | ( NestingContext.Class _,
        true,
        ({ Node.value = self_or_class_parameter_value; location } as self_or_class_parameter)
        :: rest_parameters,
        return_annotation )
      when signature_uses_self_type ~qualifier ~scopes signature ->
        let qualified_parent_name = NestingContext.to_qualifier ~module_name:qualifier parent in
        let self_type_variable_reference =
          self_variable_name qualified_parent_name |> Reference.create
        in
        let self_or_class_parameter =
          let annotation =
            let variable_annotation =
              from_reference ~location ~create_origin:(fun _ -> None) self_type_variable_reference
            in
            if Define.Signature.is_class_method signature then
              subscript_for_annotation ~location "typing.Type" [variable_annotation]
              |> Node.create ~location
            else
              variable_annotation
          in
          let annotation_with_possible_readonly_wrapper =
            match self_or_class_parameter_value with
            | { Parameter.annotation = Some _readonly_self_or_class_parameter; _ } ->
                (* The user wrote `self: PyreReadOnly[Self]` or `cls: PyreReadOnly[Type[Self]]`. So,
                   wrap the synthesized type in `PyreReadOnly`. *)
                subscript_for_annotation ~location "pyre_extensions.PyreReadOnly" [annotation]
                |> Node.create ~location
            | _ -> annotation
          in
          {
            self_or_class_parameter with
            value =
              {
                self_or_class_parameter_value with
                annotation = Some annotation_with_possible_readonly_wrapper;
              };
          }
        in
        let replace_self_type_in_parameter_annotation = function
          | { Node.value = { Parameter.annotation = Some annotation; _ } as parameter_value; _ } as
            parameter ->
              let annotation =
                replace_self_type_with
                  annotation
                  ~qualifier
                  ~scopes
                  ~synthetic_type_variable:self_type_variable_reference
              in
              { parameter with value = { parameter_value with annotation = Some annotation } }
          | parameter -> parameter
        in
        let rest_parameters =
          rest_parameters |> List.map ~f:replace_self_type_in_parameter_annotation
        in
        ( {
            signature with
            parameters = self_or_class_parameter :: rest_parameters;
            return_annotation =
              return_annotation
              >>| replace_self_type_with
                    ~qualifier
                    ~scopes
                    ~synthetic_type_variable:self_type_variable_reference;
          },
          parent )
        |> Option.some
    | _ -> None


  let make_type_variable_definition ~qualifier nesting_context =
    let location = Location.any in
    let qualified_class_name = NestingContext.to_qualifier ~module_name:qualifier nesting_context in
    let self_variable_name = self_variable_name qualified_class_name in
    Statement.Assign
      {
        target = Node.create ~location (Expression.Name (Name.Identifier self_variable_name));
        value =
          Some
            (Expression.Call
               {
                 callee =
                   Node.create
                     ~location
                     (Expression.Name
                        (Name.Attribute
                           {
                             base =
                               Node.create ~location (Expression.Name (Name.Identifier "typing"));
                             attribute = "TypeVar";
                             origin =
                               Some
                                 (Origin.create
                                    ~location
                                    (Origin.SelfImplicitTypeVar self_variable_name));
                           }));
                 arguments =
                   [
                     {
                       Call.Argument.name = None;
                       value =
                         Expression.Constant
                           (Constant.String
                              { StringLiteral.kind = String; value = self_variable_name })
                         |> Node.create_with_default_location;
                     };
                     {
                       Call.Argument.name = Some (Node.create_with_default_location "bound");
                       value =
                         from_reference
                           ~location:Location.any
                           ~create_origin:(fun attributes ->
                             Some
                               (Origin.create
                                  ~location
                                  (Origin.SelfImplicitTypeVarQualification
                                     (self_variable_name, attributes))))
                           (NestingContext.to_qualifier
                              ~module_name:Reference.empty
                              nesting_context);
                     };
                   ];
                 origin =
                   Some (Origin.create ~location (Origin.SelfImplicitTypeVar self_variable_name));
               }
            |> Node.create ~location);
        annotation = None;
        origin = Some (Origin.create ~location Origin.SelfImplicitTypeVarAssign);
      }
    |> Node.create_with_default_location


  module NestingContextSet = Set.Make (NestingContext)

  let expand_self_type ({ Source.module_path = { qualifier; _ }; _ } as source) =
    let scopes = lazy (Scope.ScopeStack.create source) in
    let module Transform = Transform.MakeStatementTransformer (struct
      type classes_with_self = NestingContextSet.t

      type t = classes_with_self

      let statement sofar { Node.location; value } =
        let classes_with_self, value =
          match value with
          | Statement.Define ({ signature; _ } as define) -> (
              match replace_self_type_in_signature ~qualifier ~scopes signature with
              | Some (signature, class_with_self) ->
                  Set.add sofar class_with_self, Statement.Define { define with signature }
              | None -> sofar, value)
          | _ -> sofar, value
        in
        classes_with_self, [{ Node.location; value }]
    end)
    in
    let { Transform.source; state = classes_with_self } =
      Transform.transform NestingContextSet.empty source
    in
    let type_variable_definitions =
      Set.to_list classes_with_self |> List.map ~f:(make_type_variable_definition ~qualifier)
    in
    { source with statements = type_variable_definitions @ Source.statements source }
end

(** Expand Enums declared using functional syntax to the class-based syntax. *)
let expand_enum_functional_syntax
    ({ Source.statements; module_path = { ModulePath.qualifier; _ }; _ } as source)
  =
  let scopes = lazy (Scope.ScopeStack.create source) in
  let is_enum =
    create_callee_name_matcher_from_references
      ~qualifier
      ~scopes
      [
        Reference.create "enum.Enum";
        Reference.create "enum.StrEnum";
        Reference.create "enum.IntEnum";
      ]
  in
  let expand_enum_functional_declaration
      needs_enum_import_so_far
      ({ Node.location; value } as statement)
    =
    let enum_class_declaration ~class_name base_class members =
      let assignments =
        let field_for_member = function
          | {
              Node.value =
                Expression.Constant
                  (Constant.String
                    { StringLiteral.value = attribute_name; kind = StringLiteral.String });
              location;
            } ->
              let target =
                Expression.Name (Name.Identifier attribute_name) |> Node.create ~location
              in
              let value =
                Expression.Call
                  {
                    callee =
                      Reference.create "enum.auto"
                      |> from_reference
                           ~create_origin:(fun attributes ->
                             Some
                               (Origin.create
                                  ~location
                                  (Origin.FunctionalEnumImplicitAuto attributes)))
                           ~location;
                    arguments = [];
                    origin =
                      Some
                        (Origin.create
                           ~location
                           (Origin.FunctionalEnumImplicitAuto [attribute_name]));
                  }
                |> Node.create ~location
              in
              Some
                (Statement.Assign
                   {
                     target;
                     annotation = None;
                     value = Some value;
                     origin = Some (Origin.create ~location Origin.FunctionalEnumImplicitAutoAssign);
                   }
                |> Node.create ~location)
          | _ -> None
        in
        match List.filter_map members ~f:field_for_member with
        | [] -> [Node.create ~location Statement.Pass]
        | assignments -> assignments
      in
      let base_arguments =
        [{ Call.Argument.name = None; value = Node.create ~location (Expression.Name base_class) }]
      in
      Statement.Class
        {
          name = class_name;
          base_arguments;
          decorators = [];
          parent = NestingContext.create_toplevel ();
          body = assignments;
          top_level_unbound_names = [];
          type_params = [];
        }
    in
    match value with
    | Statement.Assign
        {
          value =
            Some
              {
                Node.value =
                  Call
                    {
                      callee = { Node.value = Expression.Name callee_name; _ };
                      arguments =
                        [
                          {
                            Call.Argument.name = None;
                            value =
                              {
                                Node.value =
                                  Expression.Constant
                                    (Constant.String
                                      {
                                        StringLiteral.value = class_name;
                                        kind = StringLiteral.String;
                                      });
                                _;
                              };
                          };
                          {
                            Call.Argument.name = None;
                            value = { Node.value = List members | Tuple members; _ };
                            _;
                          };
                        ];
                      origin = _;
                    };
                _;
              };
          _;
        }
      when is_enum callee_name ->
        let expanded_declaration =
          enum_class_declaration ~class_name:(Reference.create class_name) callee_name members
        in
        true, { statement with Node.value = expanded_declaration }
    | _ -> needs_enum_import_so_far, statement
  in
  let needs_enum_import, statements =
    List.fold_map ~init:false ~f:expand_enum_functional_declaration statements
  in
  let statements =
    if needs_enum_import then
      let location = Location.any in
      let import_statement =
        Statement.Import
          {
            from = None;
            imports =
              [{ Import.name = Reference.create "enum"; alias = None } |> Node.create ~location];
          }
        |> Node.create ~location
      in
      import_statement :: statements
    else
      statements
  in
  { source with Source.statements }


let drop_nested_body { Node.value = { Define.body; _ } as define; location } =
  let new_define =
    let rec drop_nested_body_in_statement = function
      | Statement.Class definition -> Statement.Class { definition with body = [] }
      | Define { Define.signature; _ } ->
          Statement.Define { Define.signature; captures = []; unbound_names = []; body = [] }
      | For ({ For.body; orelse; _ } as for_statement) ->
          Statement.For
            {
              for_statement with
              body = drop_nested_body_in_statements body;
              orelse = drop_nested_body_in_statements orelse;
            }
      | Match ({ Match.cases; _ } as match_statement) ->
          Statement.Match
            {
              match_statement with
              cases =
                List.map cases ~f:(fun ({ Match.Case.body; _ } as case) ->
                    { case with Match.Case.body = drop_nested_body_in_statements body });
            }
      | If ({ If.body; orelse; _ } as if_statement) ->
          Statement.If
            {
              if_statement with
              body = drop_nested_body_in_statements body;
              orelse = drop_nested_body_in_statements orelse;
            }
      | While ({ While.body; orelse; _ } as while_statement) ->
          Statement.While
            {
              while_statement with
              body = drop_nested_body_in_statements body;
              orelse = drop_nested_body_in_statements orelse;
            }
      | Try { Try.body; handlers; orelse; finally; handles_exception_group } ->
          Statement.Try
            {
              Try.body = drop_nested_body_in_statements body;
              handlers =
                List.map handlers ~f:(fun ({ Try.Handler.body; _ } as handler) ->
                    { handler with Try.Handler.body = drop_nested_body_in_statements body });
              orelse = drop_nested_body_in_statements orelse;
              finally = drop_nested_body_in_statements finally;
              handles_exception_group;
            }
      | With ({ With.body; _ } as with_statement) ->
          Statement.With { with_statement with body = drop_nested_body_in_statements body }
      | _ as statement -> statement
    and drop_nested_body_in_statements statements =
      List.map statements ~f:(Node.map ~f:drop_nested_body_in_statement)
    in
    { define with Define.body = drop_nested_body_in_statements body }
  in
  { Node.value = new_define; location }


let preprocess_before_wildcards source =
  source
  |> expand_relative_imports
  |> expand_type_checking_imports
  |> expand_implicit_returns
  |> expand_import_python_calls


let preprocess_after_wildcards ~string_annotation_preserve_location source =
  source
  |> expand_new_types
  |> populate_unbound_names
  |> replace_union_shorthand
  |> mangle_private_attributes
  |> replace_lazy_import
  |> expand_string_annotations ~preserve_original_location:string_annotation_preserve_location
  |> expand_typed_dictionary_declarations
  |> expand_sqlalchemy_declarative_base
  |> expand_named_tuples
  |> inline_six_metaclass
  |> expand_pytorch_register_buffer
  |> add_dataclass_keyword_only_specifiers
  |> SelfType.expand_self_type
  |> expand_enum_functional_syntax
  |> qualify
  |> populate_captures


let preprocess_no_wildcards ~string_annotation_preserve_location source =
  preprocess_before_wildcards source
  |> preprocess_after_wildcards ~string_annotation_preserve_location
