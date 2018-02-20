(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast.Expression
open Ast.Statement
open Analysis.Cfg

open Test


let test_to_dot _ =
  let assert_dot ?(precondition=fun _ -> "") body expected =
    let define =
      {
        Define.name = Access.create "foo";
        parameters = [];
        body;
        decorators = [];
        docstring = None;
        return_annotation = None;
        async = false;
        generated = false;
        parent = None;
      }
    in
    let make_dot dot_list =
      let buffer = Buffer.create 1024 in
      Buffer.add_string buffer "digraph {\n";
      List.iter ~f:(fun string -> Buffer.add_string buffer ("  " ^ string ^ "\n")) dot_list;
      Buffer.add_string buffer "}";
      Buffer.contents buffer
    in
    assert_equal
      ~cmp:String.equal
      ~printer:(fun id -> id)
      ~pp_diff:(diff ~print:(fun format -> Format.fprintf format "%s\n"))
      (make_dot expected)
      (to_dot ~precondition ~sort_labels:true (create define))
  in
  assert_dot
    [+Pass]
    [
      {|0[label="Entry"]|};
      {|1[label="Exit"]|};
      {|2[label="Error"]|};
      {|3[label="Yield"]|};
      {|4[label="pass"]|};
      "0 -> 4 [label=\"\", fontcolor=blue]";
      "4 -> 1 [label=\"\", fontcolor=blue]";
    ];
  assert_dot
    ~precondition:Int.to_string
    [+Expression !"b"]
    [
      {|0[label="Entry"]|};
      {|1[label="Exit"]|};
      {|2[label="Error"]|};
      {|3[label="Yield"]|};
      {|4[label="b"]|};
      "0 -> 4 [label=\"4\", fontcolor=blue]";
      "4 -> 1 [label=\"1\", fontcolor=blue]";
    ];

  let conditional = {
    If.test = +True;
    body = [!!"body"];
    orelse = [!!"orelse"];
  } in
  assert_dot
    [+If conditional]
    [
      {|0[label="Entry"]|};
      {|1[label="Exit"]|};
      {|2[label="Error"]|};
      {|3[label="Yield"]|};
      {|4[label="if True:\n  body\nelse:\n  orelse"]|};
      {|5[label="Join"]|};
      {|6[label="assert True, \nbody"]|};
      {|7[label="assert False, \norelse"]|};
      "0 -> 4 [label=\"\", fontcolor=blue]";
      "4 -> 6 [label=\"\", fontcolor=blue]";
      "4 -> 7 [label=\"\", fontcolor=blue]";
      "5 -> 1 [label=\"\", fontcolor=blue]";
      "6 -> 5 [label=\"\", fontcolor=blue]";
      "7 -> 5 [label=\"\", fontcolor=blue]";
    ]


let assert_cfg body expected =
  let define = {
    Define.name = Access.create "foo";
    parameters = [];
    body;
    decorators = [];
    docstring = None;
    return_annotation = None;
    async = false;
    generated = false;
    parent = None;
  } in
  assert_equal
    ~cmp:equal
    ~printer:(fun cfg -> Format.asprintf "%a" pp cfg)
    ~pp_diff:(diff ~print:pp)
    expected
    (create define)


let node id kind predecessors successors =
  id,
  Node.create
    id
    kind
    (Int.Set.of_list predecessors)
    (Int.Set.of_list successors)


let test_block _ =
  assert_cfg
    [+Pass]
    (Int.Table.of_alist_exn [
        node 0 Node.Entry [] [4];
        node 1 Node.Exit [4] [];
        node 2 Node.Error [] [];
        node 3 Node.Yield [] [];
        node 4 (Node.Block [+Pass]) [0] [1];
      ]);

  assert_cfg
    [!!"first"; !!"second"]
    (Int.Table.of_alist_exn [
        node 0 Node.Entry [] [4];
        node 1 Node.Exit [4] [];
        node 2 Node.Error [] [];
        node 3 Node.Yield [] [];
        node 4 (Node.Block [!!"first"; !!"second"]) [0] [1];
      ])


let test_for _ =
  let loop = {
    For.target = +Access (Access.create "a");
    iterator = +List [];
    body = [!!"body"];
    orelse = [!!"orelse"];
    async = false;
  } in
  assert_cfg
    [+For loop]
    (Int.Table.of_alist_exn [
        node 0 Node.Entry [] [4];
        node 1 Node.Exit [5] [];
        node 2 Node.Error [] [];
        node 3 Node.Yield [] [];
        node 4 (Node.For loop) [0; 6] [5; 6; 7];
        node 5 Node.Join [4; 7] [1];
        node 6 (Node.Block [!!"body"]) [4] [4];
        node 7 (Node.Block [!!"orelse"]) [4] [5];
      ])


let test_if _ =
  let conditional = {
    If.test = +True;
    body = [!!"body"];
    orelse = [!!"orelse"];
  } in
  assert_cfg
    [+If conditional]
    (Int.Table.of_alist_exn [
        node 0 Node.Entry [] [4];
        node 1 Node.Exit [5] [];
        node 2 Node.Error [] [];
        node 3 Node.Yield [] [];
        node 4 (Node.If conditional) [0] [6; 7];
        node 5 Node.Join [6; 7] [1];
        node 6 (Node.Block [assume (+True); !!"body"]) [4] [5];
        node 7 (Node.Block [assume (+False); !!"orelse"]) [4] [5];
      ]);

  let conditional = { If.test = +True; body = [!!"body"]; orelse = [] } in
  assert_cfg
    [+If conditional]
    (Int.Table.of_alist_exn [
        node 0 Node.Entry [] [4];
        node 1 Node.Exit [5] [];
        node 2 Node.Error [] [];
        node 3 Node.Yield [] [];
        node 4 (Node.If conditional) [0] [5; 6];
        node 5 Node.Join [4; 6] [1];
        node 6 (Node.Block [assume (+True); !!"body"]) [4] [5];
      ]);

  let conditional = {
    If.test = +True;
    body = [!!"first"; !!"second"];
    orelse = [+Pass];
  } in
  assert_cfg
    [+If conditional]
    (Int.Table.of_alist_exn [
        node 0 Node.Entry [] [4];
        node 1 Node.Exit [5] [];
        node 2 Node.Error [] [];
        node 3 Node.Yield [] [];
        node 4 (Node.If conditional) [0] [6; 7];
        node 5 Node.Join [6; 7] [1];
        node 6 (Node.Block [assume (+True); !!"first"; !!"second"]) [4] [5];
        node 7 (Node.Block [assume (+False); +Pass]) [4] [5];
      ]);

  let conditional = {
    If.test = +True;
    body = [!!"first"; !!"second"];
    orelse = [!!"orelse"];
  } in
  assert_cfg
    [!!"before"; +If conditional; !!"after"]
    (Int.Table.of_alist_exn [
        node 0 Node.Entry [] [4];
        node 1 Node.Exit [9] [];
        node 2 Node.Error [] [];
        node 3 Node.Yield [] [];
        node 4 (Node.Block [!!"before"]) [0] [5];
        node 5 (Node.If conditional) [4] [7; 8];
        node 6 Node.Join [7; 8] [9];
        node 7 (Node.Block [assume (+True); !!"first"; !!"second"]) [5] [6];
        node 8 (Node.Block [assume (+False); !!"orelse"]) [5] [6];
        node 9 (Node.Block [!!"after"]) [6] [1];
      ])


let test_raise _ =
  let error = +Raise None in
  assert_cfg
    [error]
    (Int.Table.of_alist_exn [
        node 0 Node.Entry [] [4];
        node 1 Node.Exit [] [];
        node 2 Node.Error [4] [];
        node 3 Node.Yield [] [];
        node 4 (Node.Block [error]) [0] [2];
      ]);
  assert_cfg
    [!!"reached"; error; !!"unreached"]
    (Int.Table.of_alist_exn [
        node 0 Node.Entry [] [4];
        node 1 Node.Exit [] [];
        node 2 Node.Error [4] [];
        node 3 Node.Yield [] [];
        node 4 (Node.Block [!!"reached"; error]) [0] [2];
      ])


let test_return _ =
  let return = +Return None in
  assert_cfg
    [return]
    (Int.Table.of_alist_exn [
        node 0 Node.Entry [] [4];
        node 1 Node.Exit [4] [];
        node 2 Node.Error [] [];
        node 3 Node.Yield [] [];
        node 4 (Node.Block [return]) [0] [1];
      ]);

  let body = [!!"reached"; return; !!"unreached"] in
  assert_cfg
    body
    (Int.Table.of_alist_exn [
        node 0 Node.Entry [] [4];
        node 1 Node.Exit [4] [];
        node 2 Node.Error [] [];
        node 3 Node.Yield [] [];
        node 4 (Node.Block [!!"reached"; return]) [0] [1];
      ])


let test_try _ =
  let handler name = {
    Try.kind = None;
    name = None;
    handler_body = [!!name];
  } in

  let block = {
    Try.body = [!!"body"];
    handlers = [handler "handler"];
    orelse = [!!"orelse"];
    finally = [!!"finally"];
  } in
  assert_cfg
    [+Try block]
    (Int.Table.of_alist_exn [
        node 0 Node.Entry [] [4];
        node 1 Node.Exit [6; 8] [];
        node 2 Node.Error [7] [];
        node 3 Node.Yield [] [];
        node 4 (Node.Try block)  [0] [5; 9];
        node 5 Node.Dispatch [4] [7; 10];
        node 6 (Node.Block [!!"finally"]) [9; 10] [1]; (* finally *)
        node 7 (Node.Block [!!"finally"]) [5] [2]; (* uncaught *)
        node 8 (Node.Block [!!"finally"]) [] [1]; (* return *)
        node 9 (Node.Block [!!"body"; !!"orelse"]) [4] [6];
        node 10 (Node.Block [!!"handler"]) [5] [6];
      ]);

  let block = {
    Try.body = [!!"body"];
    handlers = [handler "handler"];
    orelse = [];
    finally = [!!"finally"];
  } in
  assert_cfg
    [+Try block]
    (Int.Table.of_alist_exn [
        node 0 Node.Entry [] [4];
        node 1 Node.Exit [6; 8] [];
        node 2 Node.Error [7] [];
        node 3 Node.Yield [] [];
        node 4 (Node.Try block)  [0] [5; 9];
        node 5 Node.Dispatch [4] [7; 10];
        node 6 (Node.Block [!!"finally"]) [9; 10] [1]; (* finally *)
        node 7 (Node.Block [!!"finally"]) [5] [2]; (* uncaught *)
        node 8 (Node.Block [!!"finally"]) [] [1]; (* return *)
        node 9 (Node.Block [!!"body"]) [4] [6];
        node 10 (Node.Block [!!"handler"]) [5] [6];
      ]);

  let block = {
    Try.body = [!!"body"];
    handlers = [handler "handler"];
    orelse = [];
    finally = [];
  } in
  assert_cfg
    [+Try block]
    (Int.Table.of_alist_exn [
        node 0 Node.Entry [] [4];
        node 1 Node.Exit [6; 8] [];
        node 2 Node.Error [7] [];
        node 3 Node.Yield [] [];
        node 4 (Node.Try block)  [0] [5; 9];
        node 5 Node.Dispatch [4] [7; 10];
        node 6 (Node.Block []) [9; 10] [1]; (* finally *)
        node 7 (Node.Block []) [5] [2]; (* uncaught *)
        node 8 (Node.Block []) [] [1]; (* return *)
        node 9 (Node.Block [!!"body"]) [4] [6];
        node 10 (Node.Block [!!"handler"]) [5] [6];
      ]);

  let block = {
    Try.body = [!!"body"];
    handlers = [handler "handler 1"; handler "handler 2"];
    orelse = [];
    finally = [];
  } in
  assert_cfg
    [+Try block]
    (Int.Table.of_alist_exn [
        node 0 Node.Entry [] [4];
        node 1 Node.Exit [6; 8] [];
        node 2 Node.Error [7] [];
        node 3 Node.Yield [] [];
        node 4 (Node.Try block)  [0] [5; 9];
        node 5 Node.Dispatch [4] [7; 10; 11];
        node 6 (Node.Block []) [9; 10; 11] [1]; (* finally *)
        node 7 (Node.Block []) [5] [2]; (* uncaught *)
        node 8 (Node.Block []) [] [1]; (* return *)
        node 9 (Node.Block [!!"body"]) [4] [6];
        node 10 (Node.Block [!!"handler 1"]) [5] [6];
        node 11 (Node.Block [!!"handler 2"]) [5] [6];
      ]);

  let return = +Return None in
  let block = {
    Try.body = [!!"body"; return; !!"unreached"];
    handlers = [handler "handler"];
    orelse = [];
    finally = [];
  } in
  assert_cfg
    [+Try block]
    (Int.Table.of_alist_exn [
        node 0 Node.Entry [] [4];
        node 1 Node.Exit [6; 8] [];
        node 2 Node.Error [7] [];
        node 3 Node.Yield [] [];
        node 4 (Node.Try block)  [0] [5; 9];
        node 5 Node.Dispatch [4] [7; 10];
        node 6 (Node.Block []) [10] [1]; (* finally *)
        node 7 (Node.Block []) [5] [2]; (* uncaught *)
        node 8 (Node.Block []) [9] [1]; (* return *)
        node 9 (Node.Block [!!"body"; return]) [4] [8];
        node 10 (Node.Block [!!"handler"]) [5] [6];
      ]);

  let error = +Raise None in
  let block = {
    Try.body = [!!"body"; error; !!"unreached"];
    handlers = [handler "handler"];
    orelse = [];
    finally = [];
  } in
  assert_cfg
    [+Try block]
    (Int.Table.of_alist_exn [
        node 0 Node.Entry [] [4];
        node 1 Node.Exit [6; 8] [];
        node 2 Node.Error [7] [];
        node 3 Node.Yield [] [];
        node 4 (Node.Try block)  [0] [5; 9];
        node 5 Node.Dispatch [4; 9] [7; 10];
        node 6 (Node.Block []) [10] [1]; (* finally *)
        node 7 (Node.Block []) [5] [2]; (* uncaught *)
        node 8 (Node.Block []) [] [1]; (* return *)
        node 9 (Node.Block [!!"body"; error]) [4] [5];
        node 10 (Node.Block [!!"handler"]) [5] [6];
      ]);

  let block = {
    Try.body = [!!"body"];
    handlers = [];
    orelse = [];
    finally = [!!"finally"];
  } in
  assert_cfg
    [+Try block]
    (Int.Table.of_alist_exn [
        node 0 Node.Entry [] [4];
        node 1 Node.Exit [6; 8] [];
        node 2 Node.Error [7] [];
        node 3 Node.Yield [] [];
        node 4 (Node.Try block) [0] [5; 9];
        node 5 Node.Dispatch [4] [7];
        node 6 (Node.Block [!!"finally"]) [9] [1]; (* finally *)
        node 7 (Node.Block [!!"finally"]) [5] [2]; (* uncaught *)
        node 8 (Node.Block [!!"finally"]) [] [1]; (* return *)
        node 9 (Node.Block [!!"body"]) [4] [6];
      ]);

  let block = {
    Try.body = [!!"body"];
    handlers = [];
    orelse = [];
    finally = [+Return None];
  } in
  assert_cfg
    [+Try block]
    (Int.Table.of_alist_exn [
        node 0 Node.Entry [] [4];
        node 1 Node.Exit [6; 7; 8] [];
        node 2 Node.Error [7] [];
        node 3 Node.Yield [] [];
        node 4 (Node.Try block) [0] [5; 9];
        node 5 Node.Dispatch [4] [7];
        node 6 (Node.Block [+Return None]) [9] [1]; (* finally *)
        node 7 (Node.Block [+Return None]) [5] [1; 2]; (* uncaught *)
        node 8 (Node.Block [+Return None]) [] [1]; (* return *)
        node 9 (Node.Block [!!"body"]) [4] [6];
      ])


let test_with _ =
  let block = {
    With.items = [];
    body = [!!"body"];
    async = false;
  } in
  assert_cfg
    [+With block; !!"after"]
    (Int.Table.of_alist_exn [
        node 0 Node.Entry [] [4];
        node 1 Node.Exit [5] [];
        node 2 Node.Error [] [];
        node 3 Node.Yield [] [];
        node 4 (Node.With block) [0] [5];
        node 5 (Node.Block [!!"body"; !!"after"]) [4] [1];
      ])


let test_while _ =
  let loop = {
    While.test = +True;
    body = [!!"body"];
    orelse = [!!"orelse"];
  } in
  assert_cfg
    [+While loop]
    (Int.Table.of_alist_exn [
        node 0 Node.Entry [] [4];
        node 1 Node.Exit [5] [];
        node 2 Node.Error [] [];
        node 3 Node.Yield [] [];
        node 4 (Node.While loop) [0; 6] [5; 6; 7];
        node 5 Node.Join [4; 7] [1];
        node 6 (Node.Block [!!"body"]) [4] [4];
        node 7 (Node.Block [!!"orelse"]) [4] [5];
      ]);

  let conditional = {
    If.test = +True;
    body = [+Break];
    orelse = [];
  } in
  let loop = {
    While.test = +True;
    body = [+If conditional; !!"body"];
    orelse = [!!"orelse"];
  } in
  assert_cfg
    [+While loop]
    (Int.Table.of_alist_exn [
        node 0 Node.Entry [] [4];
        node 1 Node.Exit [5] [];
        node 2 Node.Error [] [];
        node 3 Node.Yield [] [];
        node 4 (Node.While loop) [0; 9] [5; 6; 10];
        node 5 Node.Join [4; 8; 10] [1];
        node 6 (Node.If conditional) [4] [7; 8];
        node 7 Node.Join [6] [9];
        node 8 (Node.Block [assume (+True); +Break]) [6] [5];
        node 9 (Node.Block [!!"body"]) [7] [4];
        node 10 (Node.Block [!!"orelse"]) [4] [5];
      ]);

  let conditional = {
    If.test = +True;
    body = [+Continue];
    orelse = [];
  } in
  let loop = {
    While.test = +True;
    body = [+If conditional; !!"body"];
    orelse = [!!"orelse"];
  } in
  assert_cfg
    [+While loop]
    (Int.Table.of_alist_exn [
        node 0 Node.Entry [] [4];
        node 1 Node.Exit [5] [];
        node 2 Node.Error [] [];
        node 3 Node.Yield [] [];
        node 4 (Node.While loop) [0; 8; 9] [5; 6; 10];
        node 5 Node.Join [4; 10] [1];
        node 6 (Node.If conditional) [4] [7; 8];
        node 7 Node.Join [6] [9];
        node 8 (Node.Block [assume (+True); +Continue]) [6] [4];
        node 9 (Node.Block [assume (+False); !!"body"]) [7] [4];
        node 10 (Node.Block [!!"orelse"]) [4] [5];
      ]);

  (* Jumps are reset after the loop. *)
  let inner = {
    While.test = +True;
    body = [!!"body"];
    orelse = [];
  } in
  let outer = {
    While.test = +True;
    body = [+While inner; +Continue];
    orelse = [];
  } in
  assert_cfg
    [+While outer]
    (Int.Table.of_alist_exn [
        node 0 Node.Entry [] [4];
        node 1 Node.Exit [5] [];
        node 2 Node.Error [] [];
        node 3 Node.Yield [] [];
        node 4 (Node.While outer) [0; 9] [5; 6];
        node 5 Node.Join [4] [1];
        node 6 (Node.While inner) [4; 8] [7; 8];
        node 7 Node.Join [6] [9];
        node 8 (Node.Block [!!"body"]) [6] [6];
        node 9 (Node.Block [+Continue]) [7] [4]
      ])


let test_yield _ =
  let yield = +Ast.Statement.Yield (+True) in
  assert_cfg
    [yield]
    (Int.Table.of_alist_exn [
        node 0 Node.Entry [] [4];
        node 1 Node.Exit [4] [];
        node 2 Node.Error [] [];
        node 3 Node.Yield [4] [];
        node 4 (Node.Block [yield]) [0] [1; 3];
      ])


let () =
  "cfg">:::[
    "to_dot">::test_to_dot;
    "block">::test_block;
    "for">::test_for;
    "if">::test_if;
    "raise">::test_raise;
    "return">::test_return;
    "try">::test_try;
    "with">::test_with;
    "while">::test_while;
    "yield">::test_yield;
  ]
  |> run_test_tt_main
