(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core


module type Visitor = sig
  type t
  val expression: t -> Expression.t -> t
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
