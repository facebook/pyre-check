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
    | Exit
    | For of Statement.t For.t
    | If of Statement.t If.t
    | Join
    | Try of Statement.t Try.t
    | With of Statement.t With.t
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
      Sexp.pp (sexp_of_list sexp_of_int (Set.to_list node.predecessors))
      Sexp.pp (sexp_of_list sexp_of_int (Set.to_list node.successors))

  let show graph =
    Format.asprintf "%a" pp graph

  let create id kind predecessors successors =
    { id; kind; predecessors; successors }

  let node_count = ref 0

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

  let statements node =
    match node.kind with
    | Block statements -> statements
    | _ -> []

  let successors node =
    node.successors

  let predecessors node =
    node.predecessors

  let connect predecessor successor =
    predecessor.successors <- Set.add predecessor.successors successor.id;
    successor.predecessors <- Set.add successor.predecessors predecessor.id

  let connect_option predecessor successor =
    Option.iter predecessor ~f:(fun node -> connect node successor)

  let description { kind; _ } =
    let add_newlines string =
      if String.length string > 100 then
        String.split ~on:',' string
        |> String.concat ~sep:",\n"
      else
        string
    in
    let process_statement statement =
      Ast.Node.create statement
      |> Statement.show
      |> add_newlines
    in
    match kind with
    | Block statement_list ->
        List.map ~f:(fun statement -> Statement.show statement |> add_newlines) statement_list
        |> String.concat ~sep:"\n"
    | Dispatch -> "Dispatch"
    | Entry -> "Entry"
    | Error -> "Error"
    | Exit -> "Exit"
    | For statement -> Statement.For statement |> process_statement
    | If statement -> Statement.If statement |> process_statement
    | Join -> "Join"
    | Try statement -> Statement.Try statement |> process_statement
    | With statement -> Statement.With statement |> process_statement
    | While statement ->Statement.While statement |> process_statement
    | Yield -> "Yield"
end

type t = Node.t Int.Table.t

type jumps = {
  break: Node.t;
  continue: Node.t;
  error: Node.t;
  exit: Node.t;
  yield: Node.t;
}

let equal left right =
  Hashtbl.equal left right Node.equal

let pp format graph =
  let print_node ~key:_ ~data =
    Format.fprintf format "%a\n" Node.pp data in
  Hashtbl.iteri graph ~f:print_node

let to_dot ?(precondition=fun _ -> "") graph =
  let buffer = Buffer.create 10000 in
  Buffer.add_bytes buffer "digraph {\n";
  Hashtbl.iteri
    ~f:(fun ~key ~data ->
        let label = Node.description data |> String.escaped in
        let label = Printf.sprintf "  %d[label=\"%s\"]\n" key label in
        Buffer.add_bytes buffer label;
      )
    graph;
  Hashtbl.iteri
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
              Buffer.add_bytes buffer edge;
            )
          (Node.successors data)
      )
    graph;
  Buffer.add_bytes buffer "}";
  Buffer.contents buffer

let show graph =
  Format.asprintf "%a" pp graph

let entry_index =
  0

let exit_index =
  1

let create define =
  Node.reset_count ();

  let graph = Int.Table.create () in
  let entry = Node.empty graph Node.Entry in
  let exit = Node.empty graph Node.Exit in
  assert (exit.Node.id = exit_index);
  let error = Node.empty graph Node.Error in
  let yield = Node.empty graph Node.Yield in

  (* Forward creation of the control flow for a list of statements. Returns
     the last node or `None` if the flow has ended in a jump. *)
  let rec create statements jumps predecessor =
    match statements with
    | { Ast.Node.value = For loop; _ }::statements ->
        (*       ____________
                 v          |
                 -> [split] -> [body]
                 | \_________
                 v          v
                 [orelse] -> [join] -> *)
        let split = Node.empty graph (Node.For loop) in
        let join = Node.empty graph Node.Join in
        let loop_jumps = { jumps with break = join; continue = split } in
        Node.connect predecessor split;
        let body = create loop.For.body loop_jumps split in
        Node.connect_option body split;
        let orelse = create loop.For.orelse jumps split in
        Node.connect_option orelse join;
        Node.connect split join;
        create statements jumps join

    | { Ast.Node.value = If conditional; _ }::statements ->
        (* -> [split] -> [body]
                 |          |
                 v          v
              [orelse] -> [join] -> *)
        let split = Node.empty graph (Node.If conditional) in
        let join = Node.empty graph Node.Join in
        Node.connect predecessor split;

        let body_statements =
          let test = Expression.normalize conditional.If.test in
          (assume test) :: conditional.If.body in
        let body = create body_statements jumps split in
        Node.connect_option body join;

        let orelse_statements =
          if List.length conditional.If.orelse > 0 then
            let test =
              Expression.negate conditional.If.test
              |> Expression.normalize in
            (assume test) :: conditional.If.orelse;
          else
            [] in
        let orelse = create orelse_statements jumps split in
        Node.connect_option orelse join;

        let post_statements =
          let test =
            Expression.negate conditional.If.test
            |> Expression.normalize in
          if Statement.terminates conditional.If.body then
            (assume test) :: statements
          else
            statements
        in
        create post_statements jumps join

    | { Ast.Node.value = Try block; _ }::statements ->
        (* -> [split] -> [body] -> [orelse] -> [normal finally] ->
                 |                                    ^
             [dispatch] -----> [handler] -------------|
                 |               ...
           [uncaught finally]
                 |
              [error]

           [return finally] -> [exit] *)
        let finally () =
          let finally =
            Node.empty graph (Node.Block []) in
          create block.Try.finally jumps finally |> ignore;
          finally in

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
        Node.connect return jumps.exit;

        let try_jumps = { jumps with error = dispatch; exit = return } in

        (* Normal execution. *)
        let body_orelse =
          create block.Try.body try_jumps split
          >>= create block.Try.orelse jumps in
        Node.connect_option body_orelse normal;

        (* Exception handling. *)
        let handler handler =
          create handler.Try.handler_body jumps dispatch
          |> (Fn.flip Node.connect_option) normal in
        List.iter block.Try.handlers ~f:handler;

        create statements jumps normal

    | { Ast.Node.value = With block; _ }::statements ->
        (* -> [split] -> [body] -> *)
        let split = Node.empty graph (Node.With block) in
        Node.connect predecessor split;
        create block.With.body jumps split
        >>= create statements jumps

    | { Ast.Node.value = While loop; _ }::statements ->
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
        let body = create loop.While.body loop_jumps split in
        Node.connect_option body split;
        let orelse = create loop.While.orelse jumps split in
        Node.connect_option orelse join;
        Node.connect split join;
        create statements jumps join

    | statement::statements ->
        (* -> [statement] ->
                 |      \        ^
                 |       ?       |
                 |    [yield] -- |
                 |
                 ?
           [break | continue | error | exit ] *)
        let node =
          match predecessor.Node.kind with
          | Node.Block statements ->
              predecessor.Node.kind <- Node.Block (statements @ [statement]);
              predecessor
          | _ ->
              let node = Node.empty graph (Node.Block [statement]) in
              Node.connect predecessor node;
              node in
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
              Node.connect node jumps.exit;
              None
          | { Ast.Node.value = Yield _; _ } ->
              Node.connect node jumps.yield;
              create statements jumps node
          | _ ->
              create statements jumps node
        end

    | [] ->
        Some predecessor in

  let jumps = { break = exit; continue = exit; error; exit = exit; yield} in
  let node = create define.Define.body jumps entry in
  Node.connect_option node exit;
  graph

let node cfg ~id =
  Hashtbl.find_exn cfg id
