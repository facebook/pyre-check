(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


open Core

open Expression
open Statement


module type Visitor = sig
  type t
  val expression: t -> Expression.t -> t
  val statement: t -> Statement.t -> t
end

module type StatementVisitor = sig
  type t
  val visit_children: Statement.t -> bool
  val statement: Source.t -> t -> Statement.t -> t
end


module Make (Visitor: Visitor) = struct
  let visit_argument { Argument.value; _ } ~visit_expression = visit_expression value

  let visit_parameter
      { Node.value = { Parameter.value; annotation; _ }; _ }
      ~visit_expression =
    Option.iter ~f:visit_expression value;
    Option.iter ~f:visit_expression annotation

  let rec visit_expression ~state ~visitor expression =
    let visit_expression = visit_expression ~state ~visitor in

    let visit_generator
        { Comprehension.target; iterator; conditions; _ }
        ~visit_expression =
      visit_expression target;
      visit_expression iterator;
      List.iter conditions ~f:visit_expression
    in

    let visit_entry { Dictionary.key; value } ~visit_expression =
      visit_expression key;
      visit_expression value
    in

    let visit_children value =
      match value with
      | Access access ->
          let visit_access access =
            match access with
            | Access.Call { Node.value = arguments; _ } ->
                List.iter arguments ~f:(visit_argument ~visit_expression);
            | Access.Identifier _ ->
                ()
            | Access.Expression expression ->
                visit_expression expression
          in
          List.iter access ~f:visit_access
      | Await expression ->
          visit_expression expression
      | BooleanOperator { BooleanOperator.left; right; _ }
      | ComparisonOperator { ComparisonOperator.left; right; _ } ->
          visit_expression left;
          visit_expression right;
      | Dictionary { Dictionary.entries; keywords } ->
          List.iter entries ~f:(visit_entry ~visit_expression);
          List.iter keywords ~f:(visit_expression) |> ignore
      | DictionaryComprehension { Comprehension.element; generators } ->
          visit_entry element ~visit_expression;
          List.iter generators ~f:(visit_generator ~visit_expression)
      | Generator { Comprehension.element; generators } ->
          visit_expression element;
          List.iter generators ~f:(visit_generator ~visit_expression);
      | Lambda { Lambda.parameters; body } ->
          List.iter parameters ~f:(visit_parameter ~visit_expression);
          visit_expression body
      | List elements ->
          List.iter elements ~f:visit_expression
      | ListComprehension { Comprehension.element; generators } ->
          visit_expression element;
          List.iter generators ~f:(visit_generator ~visit_expression);
      | Set elements ->
          List.iter elements ~f:visit_expression
      | SetComprehension { Comprehension.element; generators } ->
          visit_expression element;
          List.iter generators ~f:(visit_generator ~visit_expression)
      | Starred starred ->
          begin
            match starred with
            | Starred.Once expression
            | Starred.Twice expression ->
                visit_expression expression
          end
      | Ternary { Ternary.target; test; alternative } ->
          visit_expression target;
          visit_expression test;
          visit_expression alternative
      | Tuple elements ->
          List.iter elements ~f:visit_expression
      | UnaryOperator { UnaryOperator.operand; _ } ->
          visit_expression operand
      | Expression.Yield expression ->
          Option.iter ~f:visit_expression expression
      | Complex _ | Ellipses | String _ | Integer _ | True | False | Float _ ->
          ()

    in

    visit_children (Node.value expression);
    state := visitor !state expression

  let rec visit_statement ~state ~visitor statement =
    let visit_expression = visit_expression ~state ~visitor:Visitor.expression in
    let visit_statement = visit_statement ~state ~visitor in
    let visit_children value =
      match value with
      | Assign { Assign.target; annotation; value; _ } ->
          visit_expression target;
          Option.iter ~f:visit_expression annotation;
          visit_expression value
      | Assert { Assert.test; message } ->
          visit_expression test;
          Option.iter ~f:visit_expression message
      | Class { Class.bases; body; decorators; _ } ->
          List.iter bases ~f:(visit_argument ~visit_expression);
          List.iter body ~f:visit_statement;
          List.iter decorators ~f:visit_expression;
      | Define { Define.parameters; body; decorators; return_annotation; _ } ->
          List.iter parameters ~f:(visit_parameter ~visit_expression);
          List.iter body ~f:visit_statement;
          List.iter decorators ~f:visit_expression;
          Option.iter ~f:visit_expression return_annotation
      | Delete expression
      | Expression expression ->
          visit_expression expression
      | For { For.target; iterator; body; orelse; _ } ->
          visit_expression target;
          visit_expression iterator;
          List.iter body ~f:visit_statement;
          List.iter orelse ~f:visit_statement;
      | If { If.test; body; orelse } ->
          visit_expression test;
          List.iter body ~f:visit_statement;
          List.iter orelse ~f:visit_statement
      | Raise expression ->
          Option.iter ~f:visit_expression expression
      | Return { Return.expression; _ } ->
          Option.iter ~f:visit_expression expression
      | Try { Try.body; handlers; orelse; finally } ->
          let visit_handler { Try.kind; handler_body; _ } =
            Option.iter ~f:visit_expression kind;
            List.iter handler_body ~f:visit_statement
          in
          List.iter body ~f:visit_statement;
          List.iter handlers ~f:visit_handler;
          List.iter orelse ~f:visit_statement;
          List.iter finally ~f:visit_statement
      | With { With.items; body; _ } ->
          let visit_item (item, alias) =
            visit_expression item;
            Option.iter ~f:visit_expression alias
          in
          List.iter items ~f:visit_item;
          List.iter body ~f:visit_statement
      | While { While.test; body; orelse } ->
          visit_expression test;
          List.iter body ~f:visit_statement;
          List.iter orelse ~f:visit_statement
      | Statement.Yield expression
      | Statement.YieldFrom expression ->
          visit_expression expression
      | Import _
      | Nonlocal _
      | Global _
      | Pass
      | Continue
      | Break ->
          ()
    in

    visit_children (Node.value statement);
    state := visitor !state statement

  let visit state source =
    let state = ref state in
    List.iter source.Source.statements ~f:(visit_statement ~state ~visitor:Visitor.statement);
    !state
end


module MakeStatementVisitor (Visitor: StatementVisitor) = struct
  let visit state ({ Source.statements; _ } as source) =
    let state = ref state in
    let open Statement in
    let rec visit_statement { Node.location; value } =
      begin
        if Visitor.visit_children { Node.location; value } then
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
              ()

          | Class { Class.body; _ }
          | Define { Define.body; _ }
          | With { With.body; _ } ->
              List.iter ~f:visit_statement body

          | For { For.body; orelse; _ }
          | If { If.body; orelse; _ }
          | While { While.body; orelse; _ } ->
              List.iter ~f:visit_statement body;
              List.iter ~f:visit_statement orelse

          | Try { Try.body; handlers; orelse; finally } ->
              let visit_handler { Try.handler_body; _ } =
                List.iter ~f:visit_statement handler_body
              in
              List.iter ~f:visit_statement body;
              List.iter ~f:visit_handler handlers;
              List.iter ~f:visit_statement orelse;
              List.iter ~f:visit_statement finally;
        else
          ()
      end;
      state := Visitor.statement source !state { Node.location; value }
    in
    List.iter ~f:visit_statement statements;
    !state
end


module type ExpressionPredicate = sig
  type t
  val predicate: Expression.t -> t option
end

module type StatementPredicate = sig
  type t
  val visit_children: Statement.t -> bool
  val predicate: Statement.t -> t option
end

module Collector
    (ExpressionPredicate: ExpressionPredicate)
    (StatementPredicate: StatementPredicate) = struct
  let collect source =
    let module CollectingVisitor = struct
      type t = ExpressionPredicate.t list * StatementPredicate.t list

      let expression (expressions, statements) expression =
        match ExpressionPredicate.predicate expression with
        | Some result ->
            result::expressions, statements
        | None ->
            expressions, statements

      let statement (expressions, statements) statement =
        match StatementPredicate.predicate statement with
        | Some result ->
            expressions, result::statements
        | None ->
            expressions, statements
    end in
    let module CollectingVisit = Make(CollectingVisitor) in
    CollectingVisit.visit ([], []) source
end

module UnitPredicate = struct
  type t = unit

  let visit_children _ =
    true

  let predicate _ = None
end


module ExpressionCollector (Predicate: ExpressionPredicate) = struct
  let collect source =
    let module Collector = Collector(Predicate)(UnitPredicate) in
    Collector.collect source
    |> fst
end

module StatementCollector (Predicate: StatementPredicate) = struct
  module CollectingVisit = MakeStatementVisitor(struct
      type t = Predicate.t list

      let visit_children =
        Predicate.visit_children

      let statement _ statements statement =
        match Predicate.predicate statement with
        | Some result ->
            result::statements
        | None ->
            statements
    end)

  let collect source = CollectingVisit.visit [] source
end

let collect_accesses statement =
  let open Expression in
  let module Collector = ExpressionCollector(struct
      type t = Access.t
      let predicate = function
        | { Node.value = Access access; _ } ->
            Some access
        | _ ->
            None
    end) in
  Collector.collect (Source.create [statement])


let collect_accesses_with_location statement =
  let open Expression in
  let module Collector = ExpressionCollector(struct
      type t = Access.t Node.t
      let predicate = function
        | { Node.value = Access access; location } ->
            Some { Node.value = access; location }
        | _ ->
            None
    end) in
  Collector.collect (Source.create [statement])


let collect_accesses_in_position statement { Location.line; column } =
  let open Expression in
  let module Collector = ExpressionCollector(struct
      type t = Access.t Node.t
      let predicate = function
        | {
          Node.value = Access access;
          location = {
            Location.start;
            stop;
            _
          } as location;
        }
          when start.Location.line = line
            && stop.Location.line = line
            && start.Location.column <= column
            && stop.Location.column >= column ->
            Some { Node.value = access; location }
        | _ -> None
    end) in
  Collector.collect (Source.create [statement])
