(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre
open Expression
open Statement

module type Transformer = sig
  type t

  val transform_expression_children : t -> Expression.t -> bool

  val expression : t -> Expression.t -> Expression.t

  val transform_children : t -> Statement.t -> bool

  val statement : t -> Statement.t -> t * Statement.t list
end

module type StatementTransformer = sig
  type t

  val statement : t -> Statement.t -> t * Statement.t list
end

module Identity : sig
  val transform_expression_children : 't -> Expression.t -> bool

  val expression : 't -> Expression.t -> Expression.t

  val transform_children : 't -> Statement.t -> bool

  val statement : 't -> Statement.t -> 't * Statement.t list
end = struct
  let transform_expression_children _ _ = true

  let expression _ expression = expression

  let transform_children _ _ = true

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
    let transform_argument { Expression.Call.Argument.name; value } ~transform_expression =
      { Expression.Call.Argument.name; value = transform_expression value }
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
        { Comprehension.target; iterator; conditions; async }
        ~transform_expression
      =
      {
        Comprehension.target = transform_expression target;
        iterator = transform_expression iterator;
        conditions = transform_list conditions ~f:transform_expression;
        async;
      }
    in
    let transform_entry { Dictionary.key; value } ~transform_expression =
      { Dictionary.key = transform_expression key; value = transform_expression value }
    in
    let rec transform_expression expression =
      let transform_children value =
        match value with
        | Await expression -> Await (transform_expression expression)
        | BooleanOperator { BooleanOperator.left; operator; right } ->
            BooleanOperator
              {
                BooleanOperator.left = transform_expression left;
                operator;
                right = transform_expression right;
              }
        | Call { callee; arguments } ->
            let transform_arguments arguments =
              let transform_argument { Call.Argument.name; value } =
                { Call.Argument.name; value = transform_expression value }
              in
              transform_list arguments ~f:transform_argument
            in
            Call
              { callee = transform_expression callee; arguments = transform_arguments arguments }
        | ComparisonOperator { ComparisonOperator.left; operator; right } ->
            ComparisonOperator
              {
                ComparisonOperator.left = transform_expression left;
                operator;
                right = transform_expression right;
              }
        | Complex _ -> value
        | Dictionary { Dictionary.entries; keywords } ->
            Dictionary
              {
                Dictionary.entries =
                  transform_list entries ~f:(transform_entry ~transform_expression);
                keywords = transform_list keywords ~f:transform_expression;
              }
        | DictionaryComprehension { Comprehension.element; generators } ->
            DictionaryComprehension
              {
                Comprehension.element = transform_entry element ~transform_expression;
                generators =
                  transform_list generators ~f:(transform_generator ~transform_expression);
              }
        | Ellipsis -> value
        | False -> value
        | Float _ -> value
        | Generator { Comprehension.element; generators } ->
            Generator
              {
                Comprehension.element = transform_expression element;
                generators =
                  transform_list generators ~f:(transform_generator ~transform_expression);
              }
        | Integer _ -> value
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
        | Name (Name.Attribute ({ base; _ } as name)) ->
            Name (Name.Attribute { name with base = transform_expression base })
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
        | String _ -> value
        | Ternary { Ternary.target; test; alternative } ->
            Ternary
              {
                Ternary.target = transform_expression target;
                test = transform_expression test;
                alternative = transform_expression alternative;
              }
        | True -> value
        | Tuple elements -> Tuple (transform_list elements ~f:transform_expression)
        | UnaryOperator { UnaryOperator.operator; operand } ->
            UnaryOperator { UnaryOperator.operator; operand = transform_expression operand }
        | Expression.Yield expression -> Expression.Yield (expression >>| transform_expression)
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
    in
    let rec transform_statement statement =
      let transform_children value =
        match value with
        | Assign { Assign.target; annotation; value; parent } ->
            Assign
              {
                Assign.target = transform_expression target;
                annotation = annotation >>| transform_expression;
                value = transform_expression value;
                parent;
              }
        | Assert { Assert.test; message; origin } ->
            Assert
              {
                Assert.test = transform_expression test;
                message = message >>| transform_expression;
                origin;
              }
        | Break -> value
        | Class { Class.name; bases; body; decorators; docstring } ->
            Class
              {
                Class.name;
                bases = transform_list bases ~f:(transform_argument ~transform_expression);
                body = transform_list body ~f:transform_statement |> List.concat;
                decorators = transform_list decorators ~f:transform_expression;
                docstring;
              }
        | Continue -> value
        | Define
            {
              signature =
                { name; parameters; decorators; return_annotation; async; parent; docstring };
              body;
            } ->
            Define
              {
                signature =
                  {
                    name;
                    parameters =
                      transform_list parameters ~f:(transform_parameter ~transform_expression);
                    decorators = transform_list decorators ~f:transform_expression;
                    return_annotation = return_annotation >>| transform_expression;
                    async;
                    parent;
                    docstring;
                  };
                body = transform_list body ~f:transform_statement |> List.concat;
              }
        | Delete expression -> Delete (transform_expression expression)
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
        | Try { Try.body; handlers; orelse; finally } ->
            let transform_handler { Try.kind; name; handler_body } =
              {
                Try.kind = kind >>| transform_expression;
                name;
                handler_body = transform_list handler_body ~f:transform_statement |> List.concat;
              }
            in
            (* Body needs to be evaluated first to update local scope *)
            let body = transform_list body ~f:transform_statement |> List.concat in
            let handlers = transform_list handlers ~f:transform_handler in
            let orelse = transform_list orelse ~f:transform_statement |> List.concat in
            let finally = transform_list finally ~f:transform_statement |> List.concat in
            Try { Try.body; handlers; orelse; finally }
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
        | Statement.Yield expression -> Statement.Yield (transform_expression expression)
        | Statement.YieldFrom expression -> Statement.YieldFrom (transform_expression expression)
      in
      let statement =
        if Transformer.transform_children !state statement then
          { statement with Node.value = transform_children (Node.value statement) }
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
        | Break
        | Continue
        | Delete _
        | Expression _
        | Global _
        | Import _
        | Pass
        | Raise _
        | Return _
        | Nonlocal _
        | Yield _
        | YieldFrom _ ->
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
        | While ({ While.body; orelse; _ } as value) ->
            let body = List.concat_map ~f:transform_statement body in
            let orelse = List.concat_map ~f:transform_statement orelse in
            While { value with While.body; orelse }
        | Try { Try.body; handlers; orelse; finally } ->
            let transform_handler ({ Try.handler_body; _ } as value) =
              { value with Try.handler_body = List.concat_map ~f:transform_statement handler_body }
            in
            let body = List.concat_map ~f:transform_statement body in
            let handlers = List.map ~f:transform_handler handlers in
            let orelse = List.concat_map ~f:transform_statement orelse in
            let finally = List.concat_map ~f:transform_statement finally in
            Try { Try.body; handlers; orelse; finally }
      in
      let new_state, statements = Transformer.statement !state { Node.location; value } in
      state := new_state;
      statements
    in
    let statements = List.concat_map ~f:transform_statement source.Source.statements in
    { state = !state; source = { source with Source.statements } }
end
