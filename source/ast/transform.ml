(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module allows creating source-to-source transformers for expressions and statements.

   It uses module functors to make a transformer using a given statement/expression transformer
   module. It also has helpers to create simple transformers based on functions, without
   instantiating a whole transformer module. *)

open Core
open Pyre
open Expression
open Statement

module type Transformer = sig
  type t

  val transform_expression_children : t -> Expression.t -> bool

  val expression : t -> Expression.t -> Expression.t

  val transform_children : t -> Statement.t -> t * bool

  val statement : t -> Statement.t -> t * Statement.t list
end

module type StatementTransformer = sig
  type t

  val statement : t -> Statement.t -> t * Statement.t list
end

module Identity : sig
  val transform_expression_children : 't -> Expression.t -> bool

  val expression : 't -> Expression.t -> Expression.t

  val transform_children : 't -> Statement.t -> 't * bool

  val statement : 't -> Statement.t -> 't * Statement.t list
end = struct
  let transform_expression_children _ _ = true

  let expression _ expression = expression

  let transform_children state _ = state, true

  let statement state statement = state, [statement]
end

module Make (Transformer : Transformer) = struct
  type result = {
    state: Transformer.t;
    source: Source.t;
  }

  let source { source; _ } = source

  let transform state source =
    let state = ref state in
    let transform_list list ~f =
      let accumulate list element = f element :: list in
      List.fold_left list ~f:accumulate ~init:[] |> List.rev
    in
    let transform_argument { Call.Argument.name; value } ~transform_expression =
      { Call.Argument.name; value = transform_expression value }
    in
    let transform_type_param ({ Node.value; _ } as node) ~transform_expression =
      let new_value =
        match value with
        | TypeParam.TypeVar { TypeParam.name; bound = Some bound } ->
            TypeParam.TypeVar { TypeParam.name; bound = Some (transform_expression bound) }
        | _ -> value
      in
      { node with Node.value = new_value }
    in
    let transform_parameter
        ({ Node.value = { Parameter.name; value; annotation }; _ } as node)
        ~transform_expression
      =
      {
        node with
        Node.value =
          {
            Parameter.name;
            value = value >>| transform_expression;
            annotation = annotation >>| transform_expression;
          };
      }
    in
    let transform_generator
        { Comprehension.Generator.target; iterator; conditions; async }
        ~transform_expression
      =
      {
        Comprehension.Generator.target = transform_expression target;
        iterator = transform_expression iterator;
        conditions = transform_list conditions ~f:transform_expression;
        async;
      }
    in
    let transform_entry entry ~transform_expression =
      let open Dictionary.Entry in
      match entry with
      | KeyValue KeyValue.{ key; value } ->
          KeyValue KeyValue.{ key = transform_expression key; value = transform_expression value }
      | Splat s -> Splat (transform_expression s)
    in
    let transform_substring substring ~transform_expression =
      match substring with
      | Substring.Format { value; format_spec } ->
          Substring.Format
            {
              value = transform_expression value;
              format_spec = format_spec >>| transform_expression;
            }
      | Substring.Literal _ -> substring
    in
    let rec transform_expression expression =
      let transform_children value =
        let open Expression in
        match value with
        | Await { Await.operand; origin } ->
            Expression.Await { Await.operand = transform_expression operand; origin }
        | BinaryOperator { BinaryOperator.left; operator; right; origin } ->
            BinaryOperator
              {
                BinaryOperator.left = transform_expression left;
                operator;
                right = transform_expression right;
                origin;
              }
        | BooleanOperator { BooleanOperator.left; operator; right; origin } ->
            BooleanOperator
              {
                BooleanOperator.left = transform_expression left;
                operator;
                right = transform_expression right;
                origin;
              }
        | Call { Call.callee; arguments; origin } ->
            Call
              {
                Call.callee = transform_expression callee;
                arguments = transform_arguments arguments;
                origin;
              }
        | ComparisonOperator { ComparisonOperator.left; operator; right; origin } ->
            ComparisonOperator
              {
                ComparisonOperator.left = transform_expression left;
                operator;
                right = transform_expression right;
                origin;
              }
        | Constant _ -> value
        | Dictionary entries ->
            Dictionary (transform_list entries ~f:(transform_entry ~transform_expression))
        | DictionaryComprehension
            { Comprehension.element = Dictionary.Entry.KeyValue.{ key; value }; generators } ->
            DictionaryComprehension
              {
                Comprehension.element =
                  Dictionary.Entry.KeyValue.
                    { key = transform_expression key; value = transform_expression value };
                generators =
                  transform_list generators ~f:(transform_generator ~transform_expression);
              }
        | Generator { Comprehension.element; generators } ->
            Generator
              {
                Comprehension.element = transform_expression element;
                generators =
                  transform_list generators ~f:(transform_generator ~transform_expression);
              }
        | FormatString substrings ->
            FormatString (transform_list substrings ~f:(transform_substring ~transform_expression))
        | Lambda { Lambda.parameters; body } ->
            Lambda
              {
                Lambda.parameters =
                  transform_list parameters ~f:(transform_parameter ~transform_expression);
                body = transform_expression body;
              }
        | List elements -> List (transform_list elements ~f:transform_expression)
        | ListComprehension { Comprehension.element; generators } ->
            ListComprehension
              {
                Comprehension.element = transform_expression element;
                generators =
                  transform_list generators ~f:(transform_generator ~transform_expression);
              }
        | Name (Name.Identifier _) -> value
        | Name (Name.Attribute ({ Name.Attribute.base; _ } as name)) ->
            Name (Name.Attribute { name with Name.Attribute.base = transform_expression base })
        | Set elements -> Set (transform_list elements ~f:transform_expression)
        | SetComprehension { Comprehension.element; generators } ->
            SetComprehension
              {
                Comprehension.element = transform_expression element;
                generators =
                  transform_list generators ~f:(transform_generator ~transform_expression);
              }
        | Starred starred ->
            let starred =
              match starred with
              | Starred.Once expression -> Starred.Once (transform_expression expression)
              | Starred.Twice expression -> Starred.Twice (transform_expression expression)
            in
            Starred starred
        | Slice { Slice.start; stop; step; origin } ->
            Slice
              {
                Slice.start = start >>| transform_expression;
                stop = stop >>| transform_expression;
                step = step >>| transform_expression;
                origin;
              }
        | Subscript { Subscript.base; index; origin } ->
            Subscript
              {
                Subscript.base = transform_expression base;
                index = transform_expression index;
                origin;
              }
        | Ternary { Ternary.target; test; alternative } ->
            Ternary
              {
                Ternary.target = transform_expression target;
                test = transform_expression test;
                alternative = transform_expression alternative;
              }
        | Tuple elements -> Tuple (transform_list elements ~f:transform_expression)
        | UnaryOperator { UnaryOperator.operator; operand; origin } ->
            UnaryOperator { UnaryOperator.operator; operand = transform_expression operand; origin }
        | WalrusOperator { WalrusOperator.target; value; origin } ->
            WalrusOperator
              {
                WalrusOperator.target = transform_expression target;
                value = transform_expression value;
                origin;
              }
        | Expression.Yield expression -> Expression.Yield (expression >>| transform_expression)
        | Expression.YieldFrom expression ->
            Expression.YieldFrom (expression |> transform_expression)
      in
      let initial_state = !state in
      let expression =
        if Transformer.transform_expression_children !state expression then
          { expression with Node.value = transform_children (Node.value expression) }
        else
          expression
      in
      let expression = Transformer.expression !state expression in
      state := initial_state;
      expression
    and transform_arguments arguments =
      let transform_argument { Call.Argument.name; value } =
        { Call.Argument.name; value = transform_expression value }
      in
      transform_list arguments ~f:transform_argument
    in
    let rec transform_statement statement =
      let transform_children value =
        let open Statement in
        match value with
        | Assign { Assign.target; annotation; value; origin } ->
            Statement.Assign
              {
                Assign.target = transform_expression target;
                annotation = annotation >>| transform_expression;
                value = value >>| transform_expression;
                origin;
              }
        | Assert { Assert.test; message; origin } ->
            Assert
              {
                Assert.test = transform_expression test;
                message = message >>| transform_expression;
                origin;
              }
        | AugmentedAssign { AugmentedAssign.target; operator; value } ->
            Statement.AugmentedAssign
              {
                AugmentedAssign.target = transform_expression target;
                operator;
                value = transform_expression value;
              }
        | Break -> value
        | Class
            {
              Class.name;
              base_arguments;
              parent;
              body;
              decorators;
              top_level_unbound_names;
              type_params;
            } ->
            Class
              {
                Class.name;
                base_arguments =
                  transform_list base_arguments ~f:(transform_argument ~transform_expression);
                parent;
                body = transform_list body ~f:transform_statement |> List.concat;
                decorators = transform_list decorators ~f:transform_expression;
                top_level_unbound_names;
                type_params =
                  transform_list type_params ~f:(transform_type_param ~transform_expression);
              }
        | Continue -> value
        | Define { Define.signature; captures; unbound_names; body } ->
            let transform_signature
                {
                  Define.Signature.name;
                  parameters;
                  decorators;
                  return_annotation;
                  async;
                  parent;
                  legacy_parent;
                  generator;
                  type_params;
                }
              =
              {
                Define.Signature.name;
                parameters =
                  transform_list parameters ~f:(transform_parameter ~transform_expression);
                decorators = transform_list decorators ~f:transform_expression;
                return_annotation = return_annotation >>| transform_expression;
                async;
                parent;
                legacy_parent;
                generator;
                type_params =
                  transform_list type_params ~f:(transform_type_param ~transform_expression);
              }
            in
            let transform_capture { Define.Capture.name; kind } =
              let transform_kind = function
                | Define.Capture.Kind.Annotation annotation ->
                    let annotation = Option.map annotation ~f:transform_expression in
                    Define.Capture.Kind.Annotation annotation
                | Define.Capture.Kind.DefineSignature value ->
                    let value = transform_signature value in
                    Define.Capture.Kind.DefineSignature value
                | Define.Capture.Kind.(Self _ | ClassSelf _) as kind -> kind
              in
              { Define.Capture.name; kind = transform_kind kind }
            in
            Define
              {
                Define.signature = transform_signature signature;
                captures = List.map captures ~f:transform_capture;
                unbound_names;
                body = transform_list body ~f:transform_statement |> List.concat;
              }
        | Delete expressions -> Delete (List.map expressions ~f:transform_expression)
        | Expression expression -> Expression (transform_expression expression)
        | For { For.target; iterator; body; orelse; async } ->
            For
              {
                For.target = transform_expression target;
                iterator = transform_expression iterator;
                body = transform_list body ~f:transform_statement |> List.concat;
                orelse = transform_list orelse ~f:transform_statement |> List.concat;
                async;
              }
        | Global _ -> value
        | If { If.test; body; orelse } ->
            If
              {
                If.test = transform_expression test;
                body = transform_list body ~f:transform_statement |> List.concat;
                orelse = transform_list orelse ~f:transform_statement |> List.concat;
              }
        | Import _ -> value
        | Match { Match.subject; cases } ->
            let rec transform_pattern { Node.value; location } =
              let value =
                let open Match.Pattern in
                match value with
                | MatchAs { pattern; name } ->
                    Match.Pattern.MatchAs { pattern = pattern >>| transform_pattern; name }
                | MatchClass { class_name; patterns; keyword_attributes; keyword_patterns } ->
                    MatchClass
                      {
                        class_name;
                        patterns = transform_list patterns ~f:transform_pattern;
                        keyword_attributes;
                        keyword_patterns = transform_list keyword_patterns ~f:transform_pattern;
                      }
                | MatchMapping { keys; patterns; rest } ->
                    MatchMapping
                      {
                        keys = transform_list keys ~f:transform_expression;
                        patterns = transform_list patterns ~f:transform_pattern;
                        rest;
                      }
                | MatchOr patterns -> MatchOr (transform_list patterns ~f:transform_pattern)
                | MatchSequence patterns ->
                    MatchSequence (transform_list patterns ~f:transform_pattern)
                | MatchSingleton constant -> (
                    let ({ Node.value; _ } as expression) =
                      transform_expression { Node.value = Expression.Constant constant; location }
                    in
                    match value with
                    | Expression.Constant constant -> MatchSingleton constant
                    | _ -> MatchValue expression)
                | MatchStar maybe_identifier -> MatchStar maybe_identifier
                | MatchValue expression -> MatchValue expression
                | MatchWildcard -> MatchWildcard
              in
              { Node.value; location }
            in
            let transform_case { Match.Case.pattern; guard; body } =
              {
                Match.Case.pattern = transform_pattern pattern;
                guard = guard >>| transform_expression;
                body = transform_list body ~f:transform_statement |> List.concat;
              }
            in
            Match
              {
                Match.subject = transform_expression subject;
                cases = transform_list cases ~f:transform_case;
              }
        | Nonlocal _ -> value
        | Pass -> value
        | Raise { Raise.expression; from } ->
            Raise
              {
                Raise.expression = expression >>| transform_expression;
                from = from >>| transform_expression;
              }
        | Return ({ Return.expression; _ } as return) ->
            Return { return with Return.expression = expression >>| transform_expression }
        | Try { Try.body; handlers; orelse; finally; handles_exception_group } ->
            let transform_handler { Try.Handler.kind; name; body } =
              {
                Try.Handler.kind = kind >>| transform_expression;
                name;
                body = transform_list body ~f:transform_statement |> List.concat;
              }
            in
            (* Body needs to be evaluated first to update local scope *)
            let body = transform_list body ~f:transform_statement |> List.concat in
            let handlers = transform_list handlers ~f:transform_handler in
            let orelse = transform_list orelse ~f:transform_statement |> List.concat in
            let finally = transform_list finally ~f:transform_statement |> List.concat in
            Try { Try.body; handlers; orelse; finally; handles_exception_group }
        | TypeAlias { TypeAlias.name; type_params; value } ->
            TypeAlias
              {
                TypeAlias.name = transform_expression name;
                type_params =
                  transform_list type_params ~f:(transform_type_param ~transform_expression);
                value = transform_expression value;
              }
        | With { With.items; body; async } ->
            let transform_item (item, alias) =
              transform_expression item, alias >>| transform_expression
            in
            With
              {
                With.items = transform_list items ~f:transform_item;
                body = transform_list body ~f:transform_statement |> List.concat;
                async;
              }
        | While { While.test; body; orelse } ->
            While
              {
                While.test = transform_expression test;
                body = transform_list body ~f:transform_statement |> List.concat;
                orelse = transform_list orelse ~f:transform_statement |> List.concat;
              }
      in
      let statement =
        let parent_state, should_transform_children =
          Transformer.transform_children !state statement
        in
        if should_transform_children then (
          state := parent_state;
          { statement with Node.value = transform_children (Node.value statement) })
        else
          statement
      in
      let new_state, statements = Transformer.statement !state statement in
      state := new_state;
      statements
    in
    let statements =
      transform_list source.Source.statements ~f:transform_statement |> List.concat
    in
    { state = !state; source = { source with Source.statements } }
end

module MakeStatementTransformer (Transformer : StatementTransformer) = struct
  type result = {
    state: Transformer.t;
    source: Source.t;
  }

  let source { source; _ } = source

  let transform state source =
    let state = ref state in
    let open Statement in
    let rec transform_statement { Node.location; value } =
      let value =
        match value with
        | Assign _
        | Assert _
        | AugmentedAssign _
        | Break
        | Continue
        | Delete _
        | Expression _
        | Global _
        | Import _
        | Pass
        | Raise _
        | Return _
        | TypeAlias _
        | Nonlocal _ ->
            value
        | Class ({ Class.body; _ } as value) ->
            Class { value with Class.body = List.concat_map ~f:transform_statement body }
        | Define ({ Define.body; _ } as value) ->
            Define { value with Define.body = List.concat_map ~f:transform_statement body }
        | With ({ With.body; _ } as value) ->
            With { value with With.body = List.concat_map ~f:transform_statement body }
        | For ({ For.body; orelse; _ } as value) ->
            let body = List.concat_map ~f:transform_statement body in
            let orelse = List.concat_map ~f:transform_statement orelse in
            For { value with For.body; orelse }
        | If ({ If.body; orelse; _ } as value) ->
            let body = List.concat_map ~f:transform_statement body in
            let orelse = List.concat_map ~f:transform_statement orelse in
            If { value with If.body; orelse }
        | Match ({ Match.cases; _ } as value) ->
            let transform_case ({ Match.Case.body; _ } as value) =
              { value with Match.Case.body = List.concat_map ~f:transform_statement body }
            in
            Match { value with Match.cases = List.map ~f:transform_case cases }
        | While ({ While.body; orelse; _ } as value) ->
            let body = List.concat_map ~f:transform_statement body in
            let orelse = List.concat_map ~f:transform_statement orelse in
            While { value with While.body; orelse }
        | Try { Try.body; handlers; orelse; finally; handles_exception_group } ->
            let transform_handler ({ Try.Handler.body; _ } as value) =
              { value with Try.Handler.body = List.concat_map ~f:transform_statement body }
            in
            let body = List.concat_map ~f:transform_statement body in
            let handlers = List.map ~f:transform_handler handlers in
            let orelse = List.concat_map ~f:transform_statement orelse in
            let finally = List.concat_map ~f:transform_statement finally in
            Try { Try.body; handlers; orelse; finally; handles_exception_group }
      in
      let new_state, statements = Transformer.statement !state { Node.location; value } in
      state := new_state;
      statements
    in
    let statements = List.concat_map ~f:transform_statement source.Source.statements in
    { state = !state; source = { source with Source.statements } }
end

let transform_in_statement ~transform statement =
  let module TransformExpressions = Make (struct
    type t = unit

    let transform_expression_children _ _ = true

    let expression _ { Node.value; location } = { Node.value = transform value; location }

    let transform_children state _ = state, true

    let statement state statement = state, [statement]
  end)
  in
  TransformExpressions.transform () (Source.create [Node.create_with_default_location statement])
  |> (fun { TransformExpressions.source; _ } -> source)
  |> Source.statements
  |> function
  | [{ Node.value = statement; _ }] -> statement
  | _ -> failwith "expected single statement"


let transform_in_expression ~transform expression =
  transform_in_statement ~transform (Statement.Expression expression)
  |> function
  | Statement.Expression expression -> expression
  | _ -> failwith "expected an expression statement"


let sanitize_expression =
  transform_in_expression ~transform:(function
      | Expression.Name (Name.Identifier identifier) ->
          Expression.Name (Name.Identifier (Identifier.sanitized identifier))
      | Expression.Name (Name.Attribute ({ Name.Attribute.attribute; _ } as attribute_expression))
        ->
          Expression.Name
            (Name.Attribute
               {
                 attribute_expression with
                 Name.Attribute.attribute = Identifier.sanitized attribute;
               })
      | expression -> expression)


let sanitize_statement statement =
  (* The names in the function signatures are not strictly "expressions", so
     [transform_in_statement] doesn't transform them. We have to transform them in a separate
     pass. *)
  let module SanitizeSignatures = MakeStatementTransformer (struct
    type t = unit

    let statement state = function
      | {
          Node.value =
            Statement.Define
              ({ Define.signature = { Define.Signature.name; parameters; _ } as signature; _ } as
              define);
          _;
        } as statement ->
          let transform_parameter
              ({ Node.value = { Parameter.name; _ } as parameter; _ } as parameter_node)
            =
            {
              parameter_node with
              Node.value = { parameter with Parameter.name = Identifier.sanitized name };
            }
          in

          ( state,
            [
              {
                statement with
                Node.value =
                  Statement.Define
                    {
                      define with
                      Define.signature =
                        {
                          signature with
                          Define.Signature.name = Reference.sanitized name;
                          parameters = List.map parameters ~f:transform_parameter;
                        };
                    };
              };
            ] )
      | statement -> state, [statement]
  end)
  in
  let sanitized_statement =
    let sanitize_expression expression =
      Node.create_with_default_location expression |> sanitize_expression |> Node.value
    in
    let sanitize_signatures statement =
      SanitizeSignatures.transform () (Source.create [Node.create_with_default_location statement])
    in
    transform_in_statement ~transform:sanitize_expression statement
    |> sanitize_signatures
    |> SanitizeSignatures.source
    |> Source.statements
  in
  match sanitized_statement with
  | [{ Node.value = statement; _ }] -> statement
  | _ -> statement


let map_location ~transform_location expression =
  let module TransformExpressions = Make (struct
    type t = unit

    let transform_expression_children _ _ = true

    let expression _ ({ Node.location; _ } as expression) =
      { expression with Node.location = transform_location location }


    let transform_children state _ = state, true

    let statement state statement = state, [statement]
  end)
  in
  TransformExpressions.transform
    ()
    (Source.create [Node.create_with_default_location (Statement.Expression expression)])
  |> (fun { TransformExpressions.source; _ } -> source)
  |> Source.statements
  |> function
  | [{ Node.value = Statement.Expression expression; _ }] -> expression
  | _ -> failwith "expected single expression statement"
