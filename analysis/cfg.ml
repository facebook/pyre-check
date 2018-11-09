(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Pyre
open Statement


module Node = struct
  type kind =
    | Block of Statement.t list
    | Dispatch
    | Entry
    | Error
    | Normal
    | Final
    | For of For.t
    | If of Statement.t If.t
    | Join
    | Try of Try.t
    | With of With.t
    | While of Statement.t While.t
    | Yield
  [@@deriving compare, eq, show]


  type t = {
    id: int;
    mutable kind: kind;
    mutable predecessors: Int.Set.t;
    mutable successors: Int.Set.t;
  }
  [@@deriving compare, eq]


  let pp format node =
    Format.fprintf
      format
      "[Node %d]\n%a\nPredecessors: %a, Successors: %a\n"
      node.id
      pp_kind node.kind
      Sexp.pp [%message (Set.elements node.predecessors : int list)]
      Sexp.pp [%message (Set.elements node.successors : int list)]


  let show graph =
    Format.asprintf "%a" pp graph


  let create id kind predecessors successors =
    { id; kind; predecessors; successors }


  let node_count =
    ref 0


  let reset_count () =
    node_count := 0


  let empty graph kind =
    let node = {
      id = !node_count;
      kind;
      predecessors = Int.Set.empty;
      successors = Int.Set.empty;
    } in
    Hashtbl.set graph ~key:!node_count ~data:node;
    node_count := !node_count + 1;
    node


  let statements { kind; _ } =
    match kind with
    | Block statements -> statements
    | _ -> []


  let successors { successors; _ } =
    successors


  let predecessors { predecessors; _ } =
    predecessors


  let connect predecessor successor =
    predecessor.successors <- Set.add predecessor.successors successor.id;
    successor.predecessors <- Set.add successor.predecessors predecessor.id


  let connect_option predecessor successor =
    predecessor
    >>| (fun predecessor -> connect predecessor successor)
    |> ignore


  let description { kind; _ } =
    let add_newlines string =
      if String.length string > 100 then
        String.split ~on:',' string
        |> String.concat ~sep:",\n"
      else
        string
    in
    let process_statement statement =
      Ast.Node.create_with_default_location statement
      |> Statement.show
      |> add_newlines
    in
    match kind with
    | Block statement_list ->
        List.map ~f:(fun statement -> Statement.show statement |> add_newlines) statement_list
        |> String.concat ~sep:"\n"
    | Dispatch ->
        "Dispatch"
    | Entry ->
        "Entry"
    | Error ->
        "Error"
    | Normal ->
        "Normal"
    | Final ->
        "Final"
    | For statement ->
        Statement.For statement
        |> process_statement
    | If statement ->
        Statement.If statement
        |> process_statement
    | Join -> "Join"
    | Try statement ->
        Statement.Try statement
        |> process_statement
    | With statement ->
        Statement.With statement
        |> process_statement
    | While statement ->
        Statement.While statement
        |> process_statement
    | Yield ->
        "Yield"
end


type t = Node.t Int.Table.t


type jumps = {
  break: Node.t;
  continue: Node.t;
  error: Node.t;
  normal: Node.t;
  yield: Node.t;
}


let equal left right =
  Hashtbl.equal left right Node.equal


let pp format graph =
  let print_node index =
    Format.fprintf format "%a\n" Node.pp (Hashtbl.find_exn graph index)
  in
  Hashtbl.keys graph
  |> List.sort ~compare:Int.compare
  |> List.iter ~f:print_node


let to_dot ?(precondition=fun _ -> "") ?(sort_labels=false) graph =
  let sorted_iteri table ~f =
    let map =
      Hashtbl.fold table
        ~init:Int.Map.empty
        ~f:(fun ~key ~data map -> Map.set ~key ~data map)
    in
    Map.iteri map ~f
  in
  let buffer = Buffer.create 10000 in
  Buffer.add_string buffer "digraph {\n";
  let iteri = if sort_labels then sorted_iteri else Hashtbl.iteri in
  iteri
    ~f:(fun ~key ~data ->
        let label = Node.description data |> String.escaped in
        let label = Printf.sprintf "  %d[label=\"%s\"]\n" key label in
        Buffer.add_string buffer label;
      )
    graph;
  iteri
    ~f:(fun ~key ~data ->
        Set.iter
          ~f:(fun successor_id ->
              let edge =
                Printf.sprintf
                  "  %d -> %d [label=\"%s\", fontcolor=blue]\n"
                  key
                  successor_id
                  (precondition successor_id)
              in
              Buffer.add_string buffer edge;
            )
          (Node.successors data)
      )
    graph;
  Buffer.add_string buffer "}";
  Buffer.contents buffer


let show graph =
  Format.asprintf "%a" pp graph


let entry_index =
  0


let normal_index =
  1


let error_index =
  2


let exit_index =
  3


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
  let yield = Node.empty graph Node.Yield in

  Node.connect normal exit;
  Node.connect error exit;

  (* Forward creation of the control flow for a list of statements. Returns
     the last node or `None` if the flow has ended in a jump. *)
  let rec create statements jumps predecessor =
    match statements with
    | { Ast.Node.value = For ({ For.body; orelse; _ } as loop); _ } :: statements ->
        (*       ____________
                 v          |
                 -> [split] -> [preamble; body]
                 | \_________
                 v          v
                 [orelse] -> [join] -> *)
        let split = Node.empty graph (Node.For loop) in
        let join = Node.empty graph Node.Join in
        let loop_jumps = { jumps with break = join; continue = split } in
        Node.connect predecessor split;
        let body = create ((For.preamble loop) :: body) loop_jumps split in
        Node.connect_option body split;
        let orelse = create orelse jumps split in
        Node.connect_option orelse join;
        Node.connect split join;
        create statements jumps join

    | { Ast.Node.value = If ({ If.test; body; orelse; _ } as conditional); _ } :: statements ->
        (* -> [split] -> [body]
                 |          |
                 v          v
              [orelse] -> [join] -> *)
        let split = Node.empty graph (Node.If conditional) in
        let join = Node.empty graph Node.Join in
        Node.connect predecessor split;
        let body_node =
          let body_statements =
            let test = Expression.normalize test in
            (assume test) :: body
          in
          create body_statements jumps split
        in
        Node.connect_option body_node join;

        let orelse_statements =
          let test =
            Expression.negate test
            |> Expression.normalize
            |> fun test -> { test with location = Location.Reference.synthetic }
          in
          (assume test) :: orelse
        in
        let orelse = create orelse_statements jumps split in
        Node.connect_option orelse join;

        let post_statements =
          let test =
            Expression.negate test
            |> Expression.normalize
            |> fun test -> { test with location = Location.Reference.synthetic }
          in
          if Statement.terminates body then
            (assume test) :: statements
          else
            statements
        in
        create post_statements jumps join

    | {
      Ast.Node.value = Try ({ Try.body; orelse; finally; handlers } as block);
      _;
    } :: statements ->
        (* -> [split] -> [body] -> [orelse] -> [normal exit] ->
                 |                                    ^
             [dispatch] -----> [preamble; handler] -------------|
                 |               ...
           [uncaught finally]
                 |
              [error]

           [return exit] -> [normal] *)
        let finally () =
          let node =
            Node.empty graph (Node.Block []) in
          create finally jumps node |> ignore;
          node
        in

        (* Scaffolding. *)
        let split = Node.empty graph (Node.Try block) in
        Node.connect predecessor split;
        let dispatch = Node.empty graph Node.Dispatch in
        Node.connect split dispatch;
        let normal = finally () in
        let uncaught = finally () in
        Node.connect dispatch uncaught;
        Node.connect uncaught jumps.error;
        let return = finally () in
        Node.connect return jumps.normal;

        let try_jumps = { jumps with error = dispatch; normal = return } in

        (* Normal execution. *)
        let body_orelse =
          create body try_jumps split
          >>= create orelse jumps in
        Node.connect_option body_orelse normal;

        (* Exception handling. *)
        let handler ({ Try.handler_body; _ } as handler) =
          create (Try.preamble handler @ handler_body) jumps dispatch
          |> (Fn.flip Node.connect_option) normal in
        List.iter handlers ~f:handler;

        create statements jumps normal

    | { Ast.Node.value = With ({ With.body; _ } as block); _ } :: statements ->
        (* -> [split] -> [preamble; body] -> *)
        let split = Node.empty graph (Node.With block) in
        Node.connect predecessor split;
        create ((With.preamble block) @ body) jumps split
        >>= create statements jumps

    | { Ast.Node.value = While ({ While.test; body; orelse } as loop); _ } :: statements ->
        (*       ____________
                 v          |
                 -> [split] -> [body]
                 | \_________
                 v          v
                 [orelse] -> [join] -> *)
        let split = Node.empty graph (Node.While loop) in
        let join = Node.empty graph Node.Join in
        let loop_jumps = { jumps with break = join; continue = split } in
        Node.connect predecessor split;
        let body =
          let body_statements =
            let test = Expression.normalize test in
            (assume test) :: body
          in
          create body_statements loop_jumps split
        in
        Node.connect_option body split;
        let orelse = create orelse jumps split in
        Node.connect_option orelse join;
        Node.connect split join;
        create statements jumps join

    | statement :: statements ->
        (* -> [statement] ->
                 |      \        ^
                 |       ?       |
                 |    [yield] -- |
                 |
                 ?
           [break | continue | error | normal ] *)
        let node =
          match predecessor.Node.kind with
          | Node.Block statements ->
              predecessor.Node.kind <- Node.Block (statements @ [statement]);
              predecessor
          | _ ->
              let node = Node.empty graph (Node.Block [statement]) in
              Node.connect predecessor node;
              node
        in
        begin
          match statement with
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
          | { Ast.Node.value = Yield _; _ } ->
              Node.connect node jumps.yield;
              create statements jumps node
          | _ ->
              create statements jumps node
        end

    | [] ->
        Some predecessor
  in

  let jumps = { break = normal; continue = normal; error; normal = normal; yield} in
  let node = create define.Define.body jumps entry in
  Node.connect_option node normal;
  graph


let node cfg ~id =
  Hashtbl.find_exn cfg id
