(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open AstExpression
open Pyre
open AstStatement

module Expression = AstExpression
module Statement = AstStatement
module Source = AstSource


type recursion_behavior =
  | Recurse
  | Stop


module type Transformer = sig
  type t
  val expression_postorder: t -> Expression.t -> t * Expression.t
  val statement_preorder: t -> Statement.t -> t * Statement.t
  val statement_keep_recursing: t -> Statement.t -> recursion_behavior
  val statement_postorder: t -> Statement.t -> t * Statement.t list
end


module Identity : sig
  val expression_postorder: 't -> Expression.t -> 't * Expression.t
  val statement_preorder: 't -> Statement.t -> 't * Statement.t
  val statement_keep_recursing: 't -> Statement.t -> recursion_behavior
  val statement_postorder: 't -> Statement.t -> 't * Statement.t list
end = struct
  let expression_postorder state expression =
    state, expression

  let statement_preorder state statement =
    state, statement

  let statement_keep_recursing _state _statement =
    Recurse

  let statement_postorder state statement =
    state, [statement]
end


module Make (Transformer : Transformer) = struct
  let transform state source =
    let state = ref state in

    let transform_list list ~f =
      let accumulate list element = (f element)::list in
      List.fold_left list ~f:accumulate ~init:[]
      |> List.rev
    in

    let transform_argument { Argument.name; value } ~transform_expression =
      { Argument.name; value = transform_expression value }
    in

    let transform_parameter
        ({ Node.value = { Parameter.name; value; annotation }; _ } as node)
        ~transform_expression =
      {
        node with
        Node.value = {
          Parameter.name;
          value = value >>| transform_expression;
          annotation = annotation >>| transform_expression;
        }
      }
    in

    let transform_generator
        { Comprehension.target; iterator; conditions; async }
        ~transform_expression =
      {
        Comprehension.target = transform_expression target;
        iterator = transform_expression iterator;
        conditions = transform_list conditions ~f:transform_expression;
        async;
      }
    in

    let transform_entry { Dictionary.key; value } ~transform_expression =
      {
        Dictionary.key = transform_expression key;
        value = transform_expression value;
      }
    in

    let rec transform_expression expression =
      let transform_children value =
        match value with
        | Access access ->
            let transform_access access =
              let transform_subscript subscript =
                let transform_slice { Access.lower; upper; step } =
                  {
                    Access.lower = lower >>| transform_expression;
                    upper = upper >>| transform_expression;
                    step = step >>| transform_expression;
                  }
                in
                match subscript with
                | Access.Index index ->
                    Access.Index (transform_expression index)
                | Access.Slice slice ->
                    Access.Slice (transform_slice slice)
              in
              match access with
              | Access.Call { Node.location; value = { Call.name; arguments } } ->
                  Access.Call {
                    Node.location;
                    value = {
                      Call.name = transform_expression name;
                      arguments = transform_list
                          arguments
                          ~f:(transform_argument ~transform_expression);
                    };
                  }
              | Access.Identifier _ -> access
              | Access.Expression expression ->
                  Access.Expression (transform_expression expression)
              | Access.Subscript subscripts ->
                  Access.Subscript
                    (transform_list subscripts ~f:transform_subscript)
            in
            Access (transform_list access ~f:transform_access)
        | Await expression ->
            Await (transform_expression expression)
        | BinaryOperator { BinaryOperator.left; operator; right; } ->
            BinaryOperator {
              BinaryOperator.left = transform_expression left;
              operator;
              right = transform_expression right;
            }
        | BooleanOperator { BooleanOperator.left; operator; right; } ->
            BooleanOperator {
              BooleanOperator.left = transform_expression left;
              operator;
              right = transform_expression right;
            }
        | Bytes _ ->
            value
        | ComparisonOperator { ComparisonOperator.left; right } ->
            let transform_right (operator, operand) =
              operator, transform_expression operand
            in
            ComparisonOperator {
              ComparisonOperator.left = transform_expression left;
              right = transform_list right ~f:transform_right;
            }
        | Complex _ ->
            value
        | Dictionary { Dictionary.entries; keywords } ->
            Dictionary {
              Dictionary.entries =
                transform_list
                  entries
                  ~f:(transform_entry ~transform_expression);
              keywords = keywords >>| transform_expression;
            }
        | DictionaryComprehension { Comprehension.element; generators } ->
            DictionaryComprehension {
              Comprehension.element =
                transform_entry element ~transform_expression;
              generators = transform_list
                  generators
                  ~f:(transform_generator ~transform_expression);
            }
        | False ->
            value
        | Float _ ->
            value
        | Format _ ->
            value
        | Generator { Comprehension.element; generators } ->
            Generator {
              Comprehension.element = transform_expression element;
              generators = transform_list
                  generators
                  ~f:(transform_generator ~transform_expression);
            }
        | Integer _ ->
            value
        | Lambda { Lambda.parameters; body } ->
            Lambda {
              Lambda.parameters = transform_list
                  parameters
                  ~f:(transform_parameter ~transform_expression);
              body = transform_expression body;
            }
        | List elements ->
            List (transform_list elements ~f:transform_expression)
        | ListComprehension { Comprehension.element; generators } ->
            ListComprehension {
              Comprehension.element = transform_expression element;
              generators = transform_list
                  generators
                  ~f:(transform_generator ~transform_expression);
            }
        | Set elements ->
            Set (transform_list elements ~f:transform_expression)
        | SetComprehension { Comprehension.element; generators } ->
            SetComprehension {
              Comprehension.element = transform_expression element;
              generators = transform_list
                  generators
                  ~f:(transform_generator ~transform_expression);
            }
        | Starred starred ->
            let starred =
              match starred with
              | Starred.Once expression ->
                  Starred.Once (transform_expression expression)
              | Starred.Twice expression ->
                  Starred.Twice (transform_expression expression)
            in
            Starred starred
        | String _ ->
            value
        | Ternary { Ternary.target; test; alternative } ->
            Ternary {
              Ternary.target = transform_expression target;
              test = transform_expression test;
              alternative = transform_expression alternative;
            }
        | True ->
            value
        | Tuple elements ->
            Tuple (transform_list elements ~f:transform_expression)
        | UnaryOperator { UnaryOperator.operator; operand } ->
            UnaryOperator {
              UnaryOperator.operator;
              operand = transform_expression operand;
            }
        | Expression.Yield expression ->
            Expression.Yield (expression >>| transform_expression)
      in

      let expression =
        {
          expression with
          Node.value = transform_children (Node.value expression)
        }
      in
      let new_state, expression =
        Transformer.expression_postorder !state expression
      in
      state := new_state;
      expression
    in

    let rec transform_statement statement =
      let transform_children value =
        match value with
        | Assign { Assign.target; annotation; value; compound; parent } ->
            Assign {
              Assign.target = transform_expression target;
              annotation = annotation >>| transform_expression;
              value = value >>| transform_expression;
              compound;
              parent;
            }
        | Assert { Assert.test; message } ->
            Assert {
              Assert.test = transform_expression test;
              message = message >>| transform_expression;
            }
        | Break ->
            value
        | Class { Class.name; bases; body; decorators; docstring; } ->
            Class {
              Class.name = name;
              bases = transform_list
                  bases
                  ~f:(transform_argument ~transform_expression);
              body = transform_list body ~f:transform_statement |> List.concat;
              decorators = transform_list decorators ~f:transform_expression;
              docstring;
            }
        | Continue ->
            value
        | Define {
            Define.name;
            parameters;
            body;
            decorators;
            return_annotation;
            async;
            generated;
            parent;
            docstring;
          } ->
            Define {
              Define.name;
              parameters = transform_list
                  parameters
                  ~f:(transform_parameter ~transform_expression);
              body = transform_list body ~f:transform_statement |> List.concat;
              decorators = transform_list decorators ~f:transform_expression;
              return_annotation = return_annotation >>| transform_expression;
              async;
              generated;
              parent;
              docstring;
            }
        | Delete expression ->
            Delete (transform_expression expression)
        | Expression expression ->
            Expression (transform_expression expression)
        | For { For.target; iterator; body; orelse; async } ->
            For {
              For.target = transform_expression target;
              iterator = transform_expression iterator;
              body = transform_list body ~f:transform_statement |> List.concat;
              orelse =
                transform_list orelse ~f:transform_statement
                |> List.concat;
              async;
            }
        | Global _ ->
            value
        | If { If.test; body; orelse } ->
            If {
              If.test = transform_expression test;
              body = transform_list body ~f:transform_statement |> List.concat;
              orelse =
                transform_list orelse ~f:transform_statement
                |> List.concat;
            }
        | Import _ ->
            value
        | Nonlocal _ ->
            value
        | Pass ->
            value
        | Raise expression ->
            Raise (expression >>| transform_expression)
        | Return expression ->
            Return (expression >>| transform_expression)
        | Try { Try.body; handlers; orelse; finally } ->
            let transform_handler { Try.kind; name; handler_body } =
              {
                Try.kind = kind >>| transform_expression;
                name;
                handler_body =
                  transform_list handler_body ~f:transform_statement
                  |> List.concat;
              }
            in
            Try {
              Try.body =
                transform_list body ~f:transform_statement
                |> List.concat;
              handlers = transform_list handlers ~f:transform_handler;
              orelse =
                transform_list orelse ~f:transform_statement
                |> List.concat;
              finally =
                transform_list finally ~f:transform_statement
                |> List.concat;
            }
        | Stub stub ->
            let stub =
              match stub with
              | Stub.Assign { Assign.target; annotation; value; compound; parent } ->
                  Stub.Assign {
                    Assign.target = transform_expression target;
                    annotation = annotation >>| transform_expression;
                    value = value >>| transform_expression;
                    compound;
                    parent;
                  }
              | Stub.Class { Class.name; bases; body; decorators; docstring; } ->
                  Stub.Class {
                    Class.name = name;
                    bases = transform_list
                        bases
                        ~f:(transform_argument ~transform_expression);
                    body =
                      transform_list body ~f:transform_statement
                      |> List.concat;
                    decorators =
                      transform_list decorators ~f:transform_expression;
                    docstring;
                  }
              | Stub.Define {
                  Define.name;
                  parameters;
                  body;
                  decorators;
                  return_annotation;
                  async;
                  generated;
                  parent;
                  docstring;
                } ->
                  Stub.Define {
                    Define.name;
                    parameters = transform_list
                        parameters
                        ~f:(transform_parameter ~transform_expression);
                    body =
                      transform_list body ~f:transform_statement
                      |> List.concat;
                    decorators =
                      transform_list decorators ~f:transform_expression;
                    return_annotation = return_annotation >>| transform_expression;
                    async;
                    generated;
                    parent;
                    docstring;
                  }
            in
            Stub stub
        | With { With.items; body; async } ->
            let transform_item (item, alias) =
              transform_expression item, alias >>| transform_expression
            in
            With {
              With.items = transform_list items ~f:transform_item;
              body = transform_list body ~f:transform_statement |> List.concat;
              async;
            }
        | While { While.test; body; orelse } ->
            While {
              While.test = transform_expression test;
              body = transform_list body ~f:transform_statement |> List.concat;
              orelse =
                transform_list orelse ~f:transform_statement
                |> List.concat;
            }
        | Statement.Yield expression ->
            Statement.Yield (transform_expression expression)

        | Statement.YieldFrom expression ->
            Statement.YieldFrom (transform_expression expression)
      in

      let new_state, statement =
        Transformer.statement_preorder !state statement
      in
      state := new_state;

      let statement =
        match Transformer.statement_keep_recursing !state statement with
        | Recurse ->
            { statement with Node.value = transform_children (Node.value statement) }
        | Stop ->
            statement
      in

      let new_state, statements =
        Transformer.statement_postorder
          !state
          statement
      in
      state := new_state;
      statements
    in

    let statements =
      transform_list source.Source.statements ~f:transform_statement
      |> List.concat
    in
    !state, { source with Source.statements }
end
