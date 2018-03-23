(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

module Expression = AstExpression
module Statement = AstStatement
module Transform = AstTransform
module Source = AstSource
module Location = AstLocation
module Node = AstNode

module type Visitor = sig
  type t
  val expression: t -> Expression.t -> t
  val statement: t -> Statement.t -> t
end

module type StatementVisitor = sig
  type t
  val statement: t -> Statement.t -> t
end

module Make (Visitor: Visitor) = struct
  module Adaptor : sig
    type t = Visitor.t
    include Transform.Transformer with type t := t
  end = struct
    type t = Visitor.t

    let expression state expression =
      (Visitor.expression state expression), expression

    let statement state statement =
      (Visitor.statement state statement), [statement]
  end

  module VisitingTransform = Transform.Make(Adaptor)

  let visit visitor source =
    VisitingTransform.transform visitor source
    |> fst
end

module type ExpressionPredicate = sig
  type t
  val predicate: Expression.t -> t option
end

module type StatementPredicate = sig
  type t
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
  let predicate _ = None
end

module ExpressionCollector (Predicate: ExpressionPredicate) = struct
  let collect source =
    let module Collector = Collector(Predicate)(UnitPredicate) in
    Collector.collect source
    |> fst
end

module StatementCollector (Predicate: StatementPredicate) = struct
  let collect source =
    let module Collector = Collector(UnitPredicate)(Predicate) in
    Collector.collect source
    |> snd
end


let collect_accesses statement =
  let open Expression in
  let module Collector = ExpressionCollector(struct
      type t = Expression.Access.t
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


module MakeStatementVisitor (Visitor: StatementVisitor) = struct
  let visit state source =
    let state = ref state in
    let open Statement in
    let rec visit_statement { Node.location; value } =
      (* Visit sub-statements. *)
      begin
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
        | Stub (Stub.Assign _)
        | Nonlocal _
        | Yield _
        | YieldFrom _ ->
            ()

        | Class { Class.body; _ }
        | Define { Define.body; _ }
        | Stub (Stub.Class { Class.body; _ })
        | Stub (Stub.Define { Define.body; _ })
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
      end;
      state := Visitor.statement !state { Node.location; value }
    in
    List.iter ~f:visit_statement source.Source.statements;
    !state
end
