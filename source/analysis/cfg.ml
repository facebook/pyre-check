(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Pyre
open Statement

module MatchTranslate = struct
  open Expression

  let create_boolean_and ~location ~left ~right =
    Expression.BooleanOperator { left; operator = BooleanOperator.And; right }
    |> Node.create ~location


  let create_boolean_or ~location ~left ~right =
    Expression.BooleanOperator { left; operator = BooleanOperator.Or; right }
    |> Node.create ~location


  let create_constant ~location constant = Expression.Constant constant |> Node.create ~location

  let create_name ~location name = Expression.Name name |> Node.create ~location

  let create_attribute_name ~location ~base ~attribute =
    create_name ~location (Name.Attribute { base; attribute; special = false })


  let create_identifier_name ~location name = create_name ~location (Name.Identifier name)

  let create_walrus ~location ~target ~value =
    Expression.WalrusOperator { target; value } |> Node.create ~location


  let create_comparison_equals ~location ~left ~right =
    Expression.ComparisonOperator { left; operator = ComparisonOperator.Equals; right }
    |> Node.create ~location


  let create_comparison_is ~location left right =
    Expression.ComparisonOperator { left; operator = ComparisonOperator.Is; right }
    |> Node.create ~location


  let create_call ~location ~callee ~arguments =
    Expression.Call { callee; arguments } |> Node.create ~location


  let create_getitem ~location ~container ~key =
    create_call
      ~location
      ~callee:
        (create_name
           ~location
           (Name.Attribute { base = container; attribute = "__getitem__"; special = true }))
      ~arguments:[{ Call.Argument.value = key; name = None }]


  let create_getitem_index ~location ~sequence ~index =
    create_getitem
      ~location
      ~container:sequence
      ~key:(create_constant ~location (Constant.Integer index))


  let create_slice ~location ~lower ~upper =
    let to_constant = function
      | None -> Constant.NoneLiteral
      | Some index -> Constant.Integer index
    in
    let step = None in
    create_call
      ~location
      ~callee:(create_identifier_name ~location "slice")
      ~arguments:
        [
          { Call.Argument.value = create_constant ~location (to_constant lower); name = None };
          { Call.Argument.value = create_constant ~location (to_constant upper); name = None };
          { Call.Argument.value = create_constant ~location (to_constant step); name = None };
        ]


  let create_isinstance ~location object_expression type_expression =
    create_call
      ~location
      ~callee:(create_identifier_name ~location "isinstance")
      ~arguments:
        [
          { Call.Argument.value = object_expression; name = None };
          { Call.Argument.value = type_expression; name = None };
        ]


  let create_getattr ~location base attribute =
    create_call
      ~location
      ~callee:(create_identifier_name ~location "getattr")
      ~arguments:
        [
          { Call.Argument.value = base; name = None };
          { Call.Argument.value = attribute; name = None };
        ]


  let create_list ~location expression =
    create_call
      ~location
      ~callee:(create_identifier_name ~location "list")
      ~arguments:[{ Call.Argument.value = expression; name = None }]


  let create_dict ~location expression =
    create_call
      ~location
      ~callee:(create_identifier_name ~location "dict")
      ~arguments:[{ Call.Argument.value = expression; name = None }]


  let create_typing_sequence ~location =
    create_attribute_name
      ~location
      ~base:(create_identifier_name ~location "typing")
      ~attribute:"Sequence"


  let create_typing_mapping ~location =
    create_attribute_name
      ~location
      ~base:(create_identifier_name ~location "typing")
      ~attribute:"Mapping"


  let is_special_builtin_for_class_pattern cls =
    let is_special = function
      | ["bool"]
      | ["bytearray"]
      | ["bytes"]
      | ["dict"]
      | ["float"]
      | ["frozenset"]
      | ["int"]
      | ["list"]
      | ["set"]
      | ["str"]
      | ["tuple"] ->
          true
      | _ -> false
    in
    cls |> name_to_identifiers >>| is_special |> Option.value ~default:false


  let rec pattern_to_condition ~subject { Node.location; value = pattern } =
    let boolean_expression_capture ~location ~target ~value =
      (* Since we are creating boolean expression, we use walrus for capture, and add a trivial
         equality to make it a boolean expression that would always evaluate to True *)
      create_comparison_equals
        ~location
        ~left:(create_walrus ~location ~target ~value)
        ~right:target
    in
    match pattern with
    | Match.Pattern.MatchAs { pattern; name } -> (
        let name = create_identifier_name ~location name in
        let capture = boolean_expression_capture ~location ~target:name ~value:subject in
        match pattern >>| pattern_to_condition ~subject:name with
        | Some condition -> create_boolean_and ~location ~left:capture ~right:condition
        | None -> capture)
    | MatchClass { class_name; patterns; keyword_attributes; keyword_patterns } ->
        let of_positional_pattern index =
          if index == 0 && is_special_builtin_for_class_pattern (Node.value class_name) then
            pattern_to_condition ~subject
          else
            let attribute =
              create_getitem_index
                ~location
                ~sequence:
                  (create_attribute_name ~location ~base:subject ~attribute:"__match_args__")
                ~index
            in
            pattern_to_condition ~subject:(create_getattr ~location subject attribute)
        in
        let of_attribute_pattern attribute =
          pattern_to_condition ~subject:(create_attribute_name ~location ~base:subject ~attribute)
        in
        create_isinstance
          ~location
          subject
          (create_name ~location:(Node.location class_name) (Node.value class_name))
        :: List.mapi ~f:of_positional_pattern patterns
        @ List.map2_exn ~f:of_attribute_pattern keyword_attributes keyword_patterns
        |> List.reduce_exn ~f:(fun left right -> create_boolean_and ~location ~left ~right)
    | MatchMapping { keys; patterns; rest } ->
        let of_key_pattern key =
          pattern_to_condition ~subject:(create_getitem ~location ~container:subject ~key)
        in
        let of_rest rest =
          let target = create_identifier_name ~location rest in
          (* Translation is not semantic here: in the runtime, "keys" that are matched would be
             removed from rest. We can skip doing that, as none of the current analyses are affected
             by it. *)
          let value = create_dict ~location subject in
          boolean_expression_capture ~location ~target ~value
        in
        create_isinstance ~location subject (create_typing_mapping ~location)
        :: List.map2_exn keys patterns ~f:of_key_pattern
        @ (Option.map rest ~f:of_rest |> Option.to_list)
        |> List.reduce_exn ~f:(fun left right -> create_boolean_and ~location ~left ~right)
    | MatchOr patterns ->
        List.map patterns ~f:(pattern_to_condition ~subject)
        |> List.reduce_exn ~f:(fun left right -> create_boolean_or ~location ~left ~right)
    | MatchSingleton constant ->
        create_comparison_is ~location subject (create_constant ~location constant)
    | MatchSequence patterns ->
        let prefix, rest, suffix =
          let is_not_star_pattern = function
            | { Ast.Node.value = Match.Pattern.MatchStar _; _ } -> false
            | _ -> true
          in
          let prefix, star_and_suffix = List.split_while patterns ~f:is_not_star_pattern in
          match star_and_suffix with
          | { Node.value = Match.Pattern.MatchStar rest; _ } :: suffix -> prefix, rest, suffix
          | _ -> prefix, None, []
        in
        let prefix_length, suffix_length = List.length prefix, List.length suffix in
        let of_rest rest =
          let target = create_identifier_name ~location rest in
          let value =
            let lower = if prefix_length == 0 then None else Some prefix_length in
            let upper = if suffix_length == 0 then None else Some (-suffix_length) in
            create_getitem ~location ~container:subject ~key:(create_slice ~location ~lower ~upper)
            |> create_list ~location
          in
          boolean_expression_capture ~location ~target ~value
        in
        let of_prefix_pattern index =
          pattern_to_condition ~subject:(create_getitem_index ~location ~sequence:subject ~index)
        in
        let of_suffix_pattern index =
          pattern_to_condition
            ~subject:
              (create_getitem_index ~location ~sequence:subject ~index:(index - suffix_length))
        in
        create_isinstance ~location subject (create_typing_sequence ~location)
        :: List.mapi prefix ~f:of_prefix_pattern
        @ (Option.map rest ~f:of_rest |> Option.to_list)
        @ List.mapi suffix ~f:of_suffix_pattern
        |> List.reduce_exn ~f:(fun left right -> create_boolean_and ~location ~left ~right)
    | MatchValue value -> create_comparison_equals ~location ~left:subject ~right:value
    | MatchWildcard -> Expression.Constant Constant.True |> Node.create ~location
    | _ -> Expression.Constant Constant.False |> Node.create ~location


  let to_condition ~subject ~case:{ Match.Case.pattern = { Node.location; _ } as pattern; guard; _ }
    =
    match pattern_to_condition ~subject pattern, guard with
    | pattern_condition, None -> pattern_condition
    | { Node.value = Expression.Constant Constant.True; _ }, Some guard -> guard
    | pattern_condition, Some guard ->
        create_boolean_and ~location ~left:pattern_condition ~right:guard
end

module Node = struct
  type kind =
    | Block of Ast.Statement.t list
    | Dispatch
    | Entry
    | Error
    | Normal
    | Final
    | For of For.t
    | If of If.t
    | Join
    | Try of Try.t
    | With of With.t
    | While of While.t
  [@@deriving compare, show, sexp]

  type t = {
    id: int;
    mutable kind: kind;
    mutable predecessors: Int.Set.t;
    mutable successors: Int.Set.t;
  }
  [@@deriving compare, sexp]

  let location_insensitive_equal left right =
    let equal_kind left right =
      let compare_equal compare left right = Int.equal (compare left right) 0 in
      match left, right with
      | Block left, Block right ->
          List.equal (compare_equal Statement.location_insensitive_compare) left right
      | For left, For right -> compare_equal For.location_insensitive_compare left right
      | If left, If right -> compare_equal If.location_insensitive_compare left right
      | Try left, Try right -> compare_equal Try.location_insensitive_compare left right
      | With left, With right -> compare_equal With.location_insensitive_compare left right
      | While left, While right -> compare_equal While.location_insensitive_compare left right
      | _ -> [%compare.equal: kind] left right
    in

    Int.equal left.id right.id
    && equal_kind left.kind right.kind
    && Int.Set.equal left.predecessors right.predecessors
    && Int.Set.equal left.successors right.successors


  let pp format node =
    Format.fprintf
      format
      "[Node %d]\n%a\nPredecessors: %a, Successors: %a\n"
      node.id
      pp_kind
      node.kind
      Sexp.pp
      [%message (Set.elements node.predecessors : int list)]
      Sexp.pp
      [%message (Set.elements node.successors : int list)]


  let create id kind predecessors successors = { id; kind; predecessors; successors }

  let node_count = ref 0

  let reset_count () = node_count := 0

  let empty graph kind =
    let node =
      { id = !node_count; kind; predecessors = Int.Set.empty; successors = Int.Set.empty }
    in
    Hashtbl.set graph ~key:!node_count ~data:node;
    node_count := !node_count + 1;
    node


  let id { id; _ } = id

  let statements { kind; _ } =
    match kind with
    | Block statements -> statements
    | _ -> []


  let successors { successors; _ } = successors

  let predecessors { predecessors; _ } = predecessors

  let connect predecessor successor =
    predecessor.successors <- Set.add predecessor.successors successor.id;
    successor.predecessors <- Set.add successor.predecessors predecessor.id


  let connect_option predecessor successor =
    predecessor >>| (fun predecessor -> connect predecessor successor) |> ignore


  let has_predecessors { predecessors; _ } = not (Set.is_empty predecessors)

  let description { kind; _ } =
    let add_newlines string =
      if String.length string > 100 then
        String.split ~on:',' string |> String.concat ~sep:",\n"
      else
        string
    in
    let process_statement statement =
      Ast.Node.create_with_default_location statement |> show |> add_newlines
    in
    match kind with
    | Block statement_list ->
        List.map ~f:(fun statement -> show statement |> add_newlines) statement_list
        |> String.concat ~sep:"\n"
    | Dispatch -> "Dispatch"
    | Entry -> "Entry"
    | Error -> "Error"
    | Normal -> "Normal"
    | Final -> "Final"
    | For statement -> Statement.For statement |> process_statement
    | If statement -> Statement.If statement |> process_statement
    | Join -> "Join"
    | Try statement -> Statement.Try statement |> process_statement
    | With statement -> Statement.With statement |> process_statement
    | While statement -> Statement.While statement |> process_statement
end

type t = Node.t Int.Table.t

type jumps = {
  break: Node.t;
  continue: Node.t;
  error: Node.t;
  normal: Node.t;
}

let match_cases_refutable cases =
  (* The parser guarantees irrefutable case will only appear at the last one. *)
  List.last cases >>| Match.Case.is_refutable |> Option.value ~default:true


let pp format graph =
  let print_node index = Format.fprintf format "%a\n" Node.pp (Hashtbl.find_exn graph index) in
  Hashtbl.keys graph |> List.sort ~compare:Int.compare |> List.iter ~f:print_node


let to_dot ?(precondition = fun _ -> "") ?(sort_labels = false) ?(single_line = false) graph =
  let newline_or_space = if single_line then " " else "\n" in
  let sorted_iteri table ~f =
    let map =
      Hashtbl.fold table ~init:Int.Map.empty ~f:(fun ~key ~data map -> Map.set ~key ~data map)
    in
    Map.iteri map ~f
  in
  let buffer = Buffer.create 10000 in
  Buffer.add_string buffer "digraph {";
  Buffer.add_string buffer newline_or_space;
  let iteri = if sort_labels then sorted_iteri else Hashtbl.iteri in
  iteri
    ~f:(fun ~key ~data ->
      let label = Node.description data |> String.escaped in
      let label = Printf.sprintf "  %d[label=\"%s\"]%s" key label newline_or_space in
      Buffer.add_string buffer label)
    graph;
  iteri
    ~f:(fun ~key ~data ->
      Set.iter
        ~f:(fun successor_id ->
          let edge =
            Printf.sprintf
              "  %d -> %d [label=\"%s\", fontcolor=blue]%s"
              key
              successor_id
              (precondition successor_id)
              newline_or_space
          in
          Buffer.add_string buffer edge)
        (Node.successors data))
    graph;
  Buffer.add_string buffer "}";
  Buffer.contents buffer


let show graph = Format.asprintf "%a" pp graph

let entry_index = 0

let normal_index = 1

let error_index = 2

let exit_index = 3

let create define =
  Node.reset_count ();
  let graph = Int.Table.create () in
  let entry = Node.empty graph Node.Entry in
  assert (entry.Node.id = entry_index);
  let normal = Node.empty graph Node.Normal in
  assert (normal.Node.id = normal_index);
  let error = Node.empty graph Node.Error in
  assert (error.Node.id = error_index);
  let exit = Node.empty graph Node.Final in
  assert (exit.Node.id = exit_index);
  Node.connect normal exit;
  Node.connect error exit;

  (* Forward creation of the control flow for a list of statements. Returns the last node or `None`
     if the flow has ended in a jump. *)
  let rec create statements jumps predecessor =
    match statements with
    | { Ast.Node.value = Statement.For ({ For.body; orelse; _ } as loop); _ } :: statements ->
        (*       ____________
         *       v          |
         *       -> [split] -> [preamble; body]
         *       | \_________
         *       v          v
         *       [orelse] -> [join] -> *)
        let split = Node.empty graph (Node.For loop) in
        let join = Node.empty graph Node.Join in
        let loop_jumps = { jumps with break = join; continue = split } in
        Node.connect predecessor split;
        let preamble = For.preamble loop in
        let body = create (preamble :: body) loop_jumps split in
        Node.connect_option body split;
        let orelse = create orelse jumps split in
        Node.connect_option orelse join;
        Node.connect split join;
        create statements jumps join
    | { Ast.Node.value = Statement.If ({ If.test; body; orelse; _ } as conditional); _ }
      :: statements ->
        (* -> [split] -> [body]
         *       |          |
         *       v          v
         *    [orelse] -> [join] -> *)
        let split = Node.empty graph (Node.If conditional) in
        let join = Node.empty graph Node.Join in
        Node.connect predecessor split;
        let body_node =
          let body_statements =
            let test = Expression.normalize test in
            Statement.assume ~origin:(Assert.Origin.If { true_branch = true }) test :: body
          in
          create body_statements jumps split
        in
        Node.connect_option body_node join;
        let orelse_statements =
          let test = Expression.negate test |> Expression.normalize in
          Statement.assume ~origin:(Assert.Origin.If { true_branch = false }) test :: orelse
        in
        let orelse = create orelse_statements jumps split in
        Node.connect_option orelse join;
        create statements jumps join
    | { Ast.Node.value = Match { subject; cases }; _ } :: statements ->
        (* The final else is optionally connected to the join node if it is refutable.
         *
         *  predecessor -> [else 1] -> [else 2] -> ... -> [else n-1] -> [else n]
         *      |           |           |                  |             :
         *      v           v           v                  v             :
         *     [case 1     [case 2     [case 3     ...    [case n        :
         *      body]       body]       body]              body]         :
         *      |           |           |                  |             :
         *      |           |           |                  |             :
         *      |           \           \                  \             v
         *      \-----------------------------------------------------> [join]
         *)
        let join = Node.empty graph Node.Join in
        let from_case predecessor case =
          let test = MatchTranslate.to_condition ~subject ~case in
          let case_node =
            let test = Expression.normalize test in
            Node.empty graph (Node.Block [Statement.assume ~origin:Assert.Origin.Match test])
          in
          Node.connect predecessor case_node;
          let case_node = create case.body jumps case_node in
          Node.connect_option case_node join;
          let else_node =
            let test = Expression.negate test |> Expression.normalize in
            Node.empty graph (Node.Block [Statement.assume ~origin:Assert.Origin.Match test])
          in
          Node.connect predecessor else_node;
          else_node
        in
        let final_else = List.fold cases ~init:predecessor ~f:from_case in
        if match_cases_refutable cases then Node.connect final_else join;
        create statements jumps join
    | { Ast.Node.value = Try ({ Try.body; orelse; finally; handlers } as block); _ } :: statements
      ->
        (* We need to add edges to the "finally" block for all paths, because that block is always
           executed, regardless of the exit path (normal, return, or error), and we need to
           preserve the state that got us into the "finally".

         * -> [split] -> [body] -> [orelse] -> [finally_entry; finally_exit] -> next
         *       |         |            |           ^                           statements
         *       |  -------/            | (on       |
         *       |  | (on error)        | error)    |
         *       V  V                   |           |
         *   [dispatch] --> [handler1] -+-----------|
         *       |      --> [handler2] -+-----------/
         *       |              |       |
         *       | <------------/       |
         *       |    (on error)        |
         *       | <--------------------/
         *       V
         *   [global error exit]
         *)
        (* Scaffolding. *)
        let split = Node.empty graph (Node.Try block) in
        Node.connect predecessor split;
        let dispatch = Node.empty graph Node.Dispatch in
        Node.connect split dispatch;
        Node.connect dispatch jumps.error;
        let finally_entry = Node.empty graph (Node.Block []) in
        let finally_exit = create finally jumps finally_entry in
        (* Normal execution. *)
        let body_orelse =
          let body_jumps = { jumps with error = dispatch } in
          create body body_jumps split >>= create orelse jumps
        in
        Node.connect_option body_orelse finally_entry;

        (* Exception handling. *)
        let handler ({ Try.Handler.body; _ } as handler) =
          let preamble = Try.preamble handler in
          create (preamble @ body) jumps dispatch |> (Fn.flip Node.connect_option) finally_entry
        in
        List.iter handlers ~f:handler;

        if not (Node.has_predecessors finally_entry) then (
          (* If the `finally` block has no predecessor, this must be because the body
             and all error handlers always return or raise exceptions. In that case,
             we add an edge from dispatch to `finally_entry` to make it reachable.
             We also assume `finally_exit` re-raises exceptions.

           * -> [split] -> [body]                [finally_entry; finally_exit]-\
           *       |         |                       ^                         |
           *       |  -------/                       |                         |
           *       |  | (on error)                   |                         |
           *       V  V                              |                         |
           *   [dispatch] --------------------------/                          |
           *       |      --> [handler1]                                       |
           *       |      --> [handler2]                                       |
           *       |              |                                            |
           *       | <------------/                                            |
           *       |    (on error)                                             |
           *       | <---------------------------------------------------------/
           *       V
           *   [global error exit]
           *)
          Node.connect dispatch finally_entry;
          Node.connect_option finally_exit jumps.error;
          None)
        else
          (* `finally_exit` might legitimately not have an exit point if there is
           * a return in the `finally` clause. *)
          finally_exit >>= create statements jumps
    | { Ast.Node.value = With ({ With.body; _ } as block); _ } :: statements ->
        (* -> [split] -> [preamble; body] -> *)
        let split = Node.empty graph (Node.With block) in
        let preamble = With.preamble block in
        Node.connect predecessor split;
        create (preamble @ body) jumps split >>= create statements jumps
    | { Ast.Node.value = While ({ While.test; body; orelse } as loop); _ } :: statements ->
        (*       _____________
         *       v           |
         *  -> [split] -> [body]
         *       |          :
         *       v          :
         *    [orelse] -> [join] -> *)
        let split = Node.empty graph (Node.While loop) in
        let join = Node.empty graph Node.Join in
        let loop_jumps = { jumps with break = join; continue = split } in
        Node.connect predecessor split;
        let body =
          let body_statements =
            let test = Expression.normalize test in
            Statement.assume ~origin:(Assert.Origin.While { true_branch = true }) test :: body
          in
          create body_statements loop_jumps split
        in
        Node.connect_option body split;
        let orelse =
          let orelse_statements =
            let test = Expression.negate test |> Expression.normalize in
            Statement.assume ~origin:(Assert.Origin.While { true_branch = false }) test :: orelse
          in
          create orelse_statements jumps split
        in
        Node.connect_option orelse join;
        if Set.is_empty join.predecessors then
          Some split
        else
          create statements jumps join
    | statement :: statements -> (
        (* -> [statement] ->
         *       |
         *       ?
         * [break | continue | error | normal ] *)
        let node =
          match predecessor.Node.kind with
          | Node.Block preceding_statements ->
              predecessor.Node.kind <- Node.Block (preceding_statements @ [statement]);
              predecessor
          | _ ->
              let node = Node.empty graph (Node.Block [statement]) in
              Node.connect predecessor node;
              node
        in
        match statement with
        | { Ast.Node.value = Assert { Assert.test; origin = Assert.Origin.Assertion; _ }; _ }
          when Expression.is_false test ->
            Node.connect node jumps.error;
            None
        | { Ast.Node.value = Assert { Assert.test; origin = Assert.Origin.While _; _ }; _ }
          when Expression.is_false test ->
            None
        | { Ast.Node.value = Break; _ } ->
            Node.connect node jumps.break;
            None
        | { Ast.Node.value = Continue; _ } ->
            Node.connect node jumps.continue;
            None
        | { Ast.Node.value = Raise _; _ } ->
            Node.connect node jumps.error;
            None
        | { Ast.Node.value = Return _; _ } ->
            Node.connect node jumps.normal;
            None
        | _ -> create statements jumps node)
    | [] -> Some predecessor
  in
  let jumps = { break = normal; continue = normal; error; normal } in
  let node = create define.Define.body jumps entry in
  Node.connect_option node normal;
  graph


let node cfg ~id = Hashtbl.find_exn cfg id
