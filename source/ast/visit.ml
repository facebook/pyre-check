(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre
open Expression
open Statement

type node =
  | Expression of Expression.t
  | Statement of Statement.t
  | Argument of Identifier.t Node.t
  | Parameter of Parameter.t
  | Reference of Reference.t Node.t
  | Substring of Substring.t
  | Generator of Comprehension.Generator.t

module type NodeVisitor = sig
  type t

  val node : t -> node -> t

  val visit_statement_children : t -> Statement.t -> bool

  val visit_expression_children : t -> Expression.t -> bool

  val visit_format_string_children : t -> Expression.t -> bool
end

module MakeNodeVisitor (Visitor : NodeVisitor) = struct
  let visit_node ~state ~visitor node = state := visitor !state node

  let visit_argument { Call.Argument.value; _ } ~visit_expression = visit_expression value

  let visit_parameter
      ({ Node.value = { Parameter.value; annotation; _ }; _ } as parameter)
      ~visitor
      ~visit_expression
      ~state
    =
    visit_node ~state ~visitor (Parameter parameter);
    Option.iter ~f:visit_expression value;
    Option.iter ~f:visit_expression annotation


  let rec visit_expression ~state ?visitor_override expression =
    let visitor = Option.value visitor_override ~default:Visitor.node in
    let visit_expression = visit_expression ~state ?visitor_override in
    let visit_generator
        ({ Comprehension.Generator.target; iterator; conditions; _ } as generator)
        ~visit_expression
      =
      visit_node ~state ~visitor (Generator generator);
      visit_expression target;
      visit_expression iterator;
      List.iter conditions ~f:visit_expression
    in
    let visit_entry { Dictionary.Entry.key; value } ~visit_expression =
      visit_expression key;
      visit_expression value
    in
    let visit_children value =
      match value with
      | Expression.Await expression -> visit_expression expression
      | BooleanOperator { BooleanOperator.left; right; _ }
      | ComparisonOperator { ComparisonOperator.left; right; _ } ->
          visit_expression left;
          visit_expression right
      | Call { Call.callee; arguments } ->
          visit_expression callee;
          let visit_argument { Call.Argument.value; name } =
            name >>| (fun name -> visit_node ~state ~visitor (Argument name)) |> ignore;
            visit_expression value
          in
          List.iter arguments ~f:visit_argument
      | Dictionary { Dictionary.entries; keywords } ->
          List.iter entries ~f:(visit_entry ~visit_expression);
          List.iter keywords ~f:visit_expression |> ignore
      | DictionaryComprehension { Comprehension.element; generators } ->
          List.iter generators ~f:(visit_generator ~visit_expression);
          visit_entry element ~visit_expression
      | Generator { Comprehension.element; generators } ->
          List.iter generators ~f:(visit_generator ~visit_expression);
          visit_expression element
      | Lambda { Lambda.parameters; body } ->
          List.iter parameters ~f:(visit_parameter ~state ~visitor ~visit_expression);
          visit_expression body
      | List elements -> List.iter elements ~f:visit_expression
      | ListComprehension { Comprehension.element; generators } ->
          List.iter generators ~f:(visit_generator ~visit_expression);
          visit_expression element
      | Name (Name.Identifier _) -> ()
      | Name (Name.Attribute { base; _ }) -> visit_expression base
      | Set elements -> List.iter elements ~f:visit_expression
      | SetComprehension { Comprehension.element; generators } ->
          List.iter generators ~f:(visit_generator ~visit_expression);
          visit_expression element
      | Starred starred -> (
          match starred with
          | Starred.Once expression
          | Starred.Twice expression ->
              visit_expression expression)
      | FormatString substrings ->
          List.iter
            ~f:(fun substring -> visit_node ~state ~visitor (Substring substring))
            substrings;
          if Visitor.visit_format_string_children !state expression then
            let visit_children = function
              | Substring.Format expression -> visit_expression expression
              | _ -> ()
            in
            List.iter ~f:visit_children substrings
      | Ternary { Ternary.target; test; alternative } ->
          visit_expression target;
          visit_expression test;
          visit_expression alternative
      | Tuple elements -> List.iter elements ~f:visit_expression
      | UnaryOperator { UnaryOperator.operand; _ } -> visit_expression operand
      | WalrusOperator { target; value } ->
          visit_expression target;
          visit_expression value
      | Expression.Yield expression -> Option.iter ~f:visit_expression expression
      | Expression.YieldFrom expression -> visit_expression expression
      | Constant _ -> ()
    in
    if Visitor.visit_expression_children !state expression then
      visit_children (Node.value expression);
    visit_node ~state ~visitor (Expression expression)


  let rec visit_statement ~state statement =
    let location = Node.location statement in
    let visitor = Visitor.node in
    let visit_expression = visit_expression ~state in
    let visit_statement = visit_statement ~state in
    let visit_children value =
      match value with
      | Statement.Assign { Assign.target; annotation; value; _ } ->
          visit_expression target;
          Option.iter ~f:visit_expression annotation;
          visit_expression value
      | Assert { Assert.test; message; _ } ->
          visit_expression test;
          Option.iter ~f:visit_expression message
      | Class ({ Class.name; base_arguments; body; decorators; _ } as class_) ->
          visit_node
            ~state
            ~visitor
            (Reference
               (Node.create ~location:(Class.name_location ~body_location:location class_) name));
          List.iter base_arguments ~f:(visit_argument ~visit_expression);
          List.iter body ~f:visit_statement;
          List.iter ~f:visit_expression decorators
      | Define ({ Define.signature; captures; body; unbound_names = _ } as define) ->
          let iter_signature { Define.Signature.name; parameters; decorators; return_annotation; _ }
            =
            visit_node
              ~state
              ~visitor
              (Reference
                 (Node.create ~location:(Define.name_location ~body_location:location define) name));
            List.iter parameters ~f:(visit_parameter ~state ~visitor ~visit_expression);
            List.iter ~f:visit_expression decorators;
            Option.iter ~f:visit_expression return_annotation
          in
          let iter_capture { Define.Capture.kind; _ } =
            match kind with
            | Define.Capture.Kind.Annotation annotation ->
                Option.iter annotation ~f:visit_expression
            | Define.Capture.Kind.DefineSignature value -> iter_signature value
            | Define.Capture.Kind.(Self _ | ClassSelf _) -> ()
          in
          iter_signature signature;
          List.iter body ~f:visit_statement;
          List.iter captures ~f:iter_capture
      | Delete expressions -> List.iter expressions ~f:visit_expression
      | Expression expression -> visit_expression expression
      | For { For.target; iterator; body; orelse; _ } ->
          visit_expression target;
          visit_expression iterator;
          List.iter body ~f:visit_statement;
          List.iter orelse ~f:visit_statement
      | If { If.test; body; orelse } ->
          visit_expression test;
          List.iter body ~f:visit_statement;
          List.iter orelse ~f:visit_statement
      | Match { Match.subject; cases } ->
          let rec visit_pattern { Node.value; location } =
            match value with
            | Match.Pattern.MatchAs { pattern; _ } -> Option.iter pattern ~f:visit_pattern
            | MatchClass { patterns; keyword_patterns; _ } ->
                List.iter patterns ~f:visit_pattern;
                List.iter keyword_patterns ~f:visit_pattern
            | MatchMapping { keys; patterns; _ } ->
                List.iter keys ~f:visit_expression;
                List.iter patterns ~f:visit_pattern
            | MatchOr patterns
            | MatchSequence patterns ->
                List.iter patterns ~f:visit_pattern
            | MatchSingleton constant ->
                visit_expression { Node.value = Expression.Constant constant; location }
            | MatchValue expression -> visit_expression expression
            | MatchStar _
            | MatchWildcard ->
                ()
          in
          let visit_case { Match.Case.pattern; guard; body } =
            visit_pattern pattern;
            Option.iter guard ~f:visit_expression;
            List.iter body ~f:visit_statement
          in
          visit_expression subject;
          List.iter cases ~f:visit_case
      | Raise { Raise.expression; from } ->
          Option.iter ~f:visit_expression expression;
          Option.iter ~f:visit_expression from
      | Return { Return.expression; _ } -> Option.iter ~f:visit_expression expression
      | Try { Try.body; handlers; orelse; finally } ->
          let visit_handler { Try.Handler.kind; body; _ } =
            Option.iter ~f:visit_expression kind;
            List.iter body ~f:visit_statement
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
      | Import _
      | Nonlocal _
      | Global _
      | Pass
      | Continue
      | Break ->
          ()
    in
    if Visitor.visit_statement_children !state statement then
      visit_children (Node.value statement);
    visit_node ~state ~visitor (Statement statement)


  let visit state source =
    let state = ref state in
    List.iter source.Source.statements ~f:(visit_statement ~state);
    !state
end

module type Visitor = sig
  type t

  val expression : t -> Expression.t -> t

  val statement : t -> Statement.t -> t
end

module Make (Visitor : Visitor) = struct
  let visit =
    let module NodeVisitor = MakeNodeVisitor (struct
      type t = Visitor.t

      let node state = function
        | Expression expression -> Visitor.expression state expression
        | Statement statement -> Visitor.statement state statement
        | _ -> state


      let visit_statement_children _ _ = true

      let visit_expression_children _ _ = true

      let visit_format_string_children _ _ = false
    end)
    in
    NodeVisitor.visit
end

module type StatementVisitor = sig
  type t

  val visit_children : Statement.t -> bool

  val statement : Source.t -> t -> Statement.t -> t
end

module MakeStatementVisitor (Visitor : StatementVisitor) = struct
  let visit state ({ Source.statements; _ } as source) =
    let state = ref state in
    let open Statement in
    let rec visit_statement { Node.location; value } =
      if Visitor.visit_children { Node.location; value } then (
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
        | Nonlocal _ ->
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
        | Match { Match.cases; _ } ->
            let visit_case { Match.Case.body; _ } = List.iter ~f:visit_statement body in
            List.iter ~f:visit_case cases
        | Try { Try.body; handlers; orelse; finally } ->
            let visit_handler { Try.Handler.body; _ } = List.iter ~f:visit_statement body in
            List.iter ~f:visit_statement body;
            List.iter ~f:visit_handler handlers;
            List.iter ~f:visit_statement orelse;
            List.iter ~f:visit_statement finally)
      else
        ();
      state := Visitor.statement source !state { Node.location; value }
    in
    List.iter ~f:visit_statement statements;
    !state
end

module type ExpressionPredicate = sig
  type t

  val visit_children : Expression.t -> bool

  val predicate : Expression.t -> t option
end

module type StatementPredicate = sig
  type t

  val visit_children : Statement.t -> bool

  val predicate : Statement.t -> t option
end

module type NodePredicate = sig
  type t

  val predicate : node -> t option
end

module Collector
    (ExpressionPredicate : ExpressionPredicate)
    (StatementPredicate : StatementPredicate)
    (NodePredicate : NodePredicate) =
struct
  type collection = {
    expressions: ExpressionPredicate.t list;
    statements: StatementPredicate.t list;
    nodes: NodePredicate.t list;
  }

  let collect source =
    let module CollectingVisitor = struct
      type t = collection

      let expression ({ expressions; _ } as state) expression =
        match ExpressionPredicate.predicate expression with
        | Some result -> { state with expressions = result :: expressions }
        | None -> state


      let statement ({ statements; _ } as state) statement =
        match StatementPredicate.predicate statement with
        | Some result -> { state with statements = result :: statements }
        | None -> state


      let node ({ nodes; _ } as state) node =
        let state =
          match node with
          | Expression expression_node -> expression state expression_node
          | Statement statement_node -> statement state statement_node
          | _ -> state
        in
        match NodePredicate.predicate node with
        | Some result -> { state with nodes = result :: nodes }
        | None -> state


      let visit_statement_children _ = StatementPredicate.visit_children

      let visit_expression_children _ = ExpressionPredicate.visit_children

      let visit_format_string_children _ _ = false
    end
    in
    let module CollectingVisit = MakeNodeVisitor (CollectingVisitor) in
    CollectingVisit.visit { expressions = []; statements = []; nodes = [] } source
end

module UnitPredicate = struct
  type t = unit

  let visit_children _ = true

  let predicate _ = None
end

module ExpressionCollector (Predicate : ExpressionPredicate) = struct
  let visit_children = Predicate.visit_children

  let collect source =
    let module Collector = Collector (Predicate) (UnitPredicate) (UnitPredicate) in
    let { Collector.expressions; _ } = Collector.collect source in
    expressions
end

module StatementCollector (Predicate : StatementPredicate) = struct
  module CollectingVisit = MakeStatementVisitor (struct
    type t = Predicate.t list

    let visit_children = Predicate.visit_children

    let statement _ statements statement =
      match Predicate.predicate statement with
      | Some result -> result :: statements
      | None -> statements
  end)

  let collect source = CollectingVisit.visit [] source
end

module NodeCollector (Predicate : NodePredicate) = struct
  let collect source =
    let module Collector = Collector (UnitPredicate) (UnitPredicate) (Predicate) in
    let { Collector.nodes; _ } = Collector.collect source in
    nodes
end

let collect_locations source =
  let module Collector =
    Collector (UnitPredicate) (UnitPredicate)
      (struct
        type t = Location.t

        let predicate = function
          | Expression node -> Some (Node.location node)
          | Statement node -> Some (Node.location node)
          | Argument node -> Some (Node.location node)
          | Parameter node -> Some (Node.location node)
          | Reference node -> Some (Node.location node)
          | Substring (Substring.Format node) -> Some (Node.location node)
          | Substring _
          | Generator _ ->
              None
      end)
  in
  let { Collector.nodes; _ } = Collector.collect source in
  nodes


let collect_calls statement =
  let open Expression in
  let module Collector = ExpressionCollector (struct
    type t = Call.t Node.t

    let visit_children _ = true

    let predicate expression =
      match expression with
      | { Node.location; value = Call call } -> Some { Node.location; value = call }
      | _ -> None
  end)
  in
  Collector.collect (Source.create [statement])


let collect_names ?(only_simple = false) statement =
  let open Expression in
  let module Collector = ExpressionCollector (struct
    type t = Name.t Node.t

    let visit_children _ = true

    let predicate expression =
      match expression with
      | { Node.location; value = Name name } ->
          if only_simple && not (is_simple_name name) then
            None
          else
            Some { Node.location; value = name }
      | _ -> None
  end)
  in
  Collector.collect (Source.create [statement])


let collect_calls_and_names statement =
  let open Expression in
  let module Collector = ExpressionCollector (struct
    type t = Expression.t

    let visit_children _ = true

    let predicate expression =
      match expression with
      | { Node.value = Call _; _ } -> Some expression
      | { Node.value = Name _; _ } -> Some expression
      | _ -> None
  end)
  in
  Collector.collect (Source.create [statement])


let collect_base_identifiers statement =
  let open Expression in
  let module Collector = ExpressionCollector (struct
    type t = Identifier.t Node.t

    let visit_children _ = true

    let predicate expression =
      match expression with
      | { Node.location; value = Name (Name.Identifier identifier) } ->
          Some { Node.location; value = identifier }
      | _ -> None
  end)
  in
  Collector.collect (Source.create [statement])


let rec collect_non_generic_type_names { Node.value; _ } =
  match value with
  | Expression.Call { Call.arguments; _ } ->
      List.concat_map
        ~f:(fun { Call.Argument.value; _ } -> collect_non_generic_type_names value)
        arguments
  | Tuple elements
  | List elements ->
      List.concat_map ~f:collect_non_generic_type_names elements
  | Name name -> name_to_reference name >>| Reference.show |> Option.to_list
  | _ -> []


let collect_format_strings_with_ignores ~ignore_line_map source =
  let module CollectIgnoredFormatStrings = ExpressionCollector (struct
    type t = Expression.t * Ignore.t list

    let visit_children _ = true

    let predicate = function
      | { Node.value = Expression.FormatString _; location } as expression ->
          Map.find ignore_line_map (Location.line location) >>| fun ignores -> expression, ignores
      | _ -> None
  end)
  in
  CollectIgnoredFormatStrings.collect source
