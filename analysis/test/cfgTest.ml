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
      {|1[label="Normal"]|};
      {|2[label="Error"]|};
      {|3[label="Final"]|};
      {|4[label="Yield"]|};
      {|5[label="pass"]|};
      "0 -> 5 [label=\"\", fontcolor=blue]";
      "1 -> 3 [label=\"\", fontcolor=blue]";
      "2 -> 3 [label=\"\", fontcolor=blue]";
      "5 -> 1 [label=\"\", fontcolor=blue]";
    ];
  assert_dot
    ~precondition:Int.to_string
    [+Expression !"b"]
    [
      {|0[label="Entry"]|};
      {|1[label="Normal"]|};
      {|2[label="Error"]|};
      {|3[label="Final"]|};
      {|4[label="Yield"]|};
      {|5[label="b"]|};
      "0 -> 5 [label=\"5\", fontcolor=blue]";
      "1 -> 3 [label=\"3\", fontcolor=blue]";
      "2 -> 3 [label=\"3\", fontcolor=blue]";
      "5 -> 1 [label=\"1\", fontcolor=blue]";
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
      {|1[label="Normal"]|};
      {|2[label="Error"]|};
      {|3[label="Final"]|};
      {|4[label="Yield"]|};
      {|5[label="if True:\n  body\nelse:\n  orelse"]|};
      {|6[label="Join"]|};
      {|7[label="assert True, \nbody"]|};
      {|8[label="assert False, \norelse"]|};
      "0 -> 5 [label=\"\", fontcolor=blue]";
      "1 -> 3 [label=\"\", fontcolor=blue]";
      "2 -> 3 [label=\"\", fontcolor=blue]";
      "5 -> 7 [label=\"\", fontcolor=blue]";
      "5 -> 8 [label=\"\", fontcolor=blue]";
      "6 -> 1 [label=\"\", fontcolor=blue]";
      "7 -> 6 [label=\"\", fontcolor=blue]";
      "8 -> 6 [label=\"\", fontcolor=blue]";
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
    parent = None;
  } in
  assert_equal
    ~cmp:equal
    ~printer:(fun cfg -> Format.asprintf "%a" pp cfg)
    ~pp_diff:(diff ~print:pp)
    (Int.Table.of_alist_exn expected)
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
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [5] [3];
      node 2 Node.Error [] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.Block [+Pass]) [0] [1];
    ];
  assert_cfg
    [!!"first"; !!"second"]
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [5] [3];
      node 2 Node.Error [] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.Block [!!"first"; !!"second"]) [0] [1];
    ]


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
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [6] [3];
      node 2 Node.Error [] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.For loop) [0; 7] [6; 7; 8];
      node 6 Node.Join [5; 8] [1];
      node
        7
        (Node.Block [parse_single_statement "a = [].__iter__().__next__()"; !!"body"]) [5] [5];
      node 8 (Node.Block [!!"orelse"]) [5] [6];
    ]


let test_if _ =
  let conditional = {
    If.test = +True;
    body = [!!"body"];
    orelse = [!!"orelse"];
  } in
  assert_cfg
    [+If conditional]
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [6] [3];
      node 2 Node.Error [] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.If conditional) [0] [7; 8];
      node 6 Node.Join [7; 8] [1];
      node 7 (Node.Block [assume (+True); !!"body"]) [5] [6];
      node 8 (Node.Block [assume (+False); !!"orelse"]) [5] [6];
    ];

  let conditional = { If.test = +True; body = [!!"body"]; orelse = [] } in
  assert_cfg
    [+If conditional]
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [6] [3];
      node 2 Node.Error [] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.If conditional) [0] [7; 8];
      node 6 Node.Join [7; 8] [1];
      node 7 (Node.Block [assume (+True); !!"body"]) [5] [6];
      node 8 (Node.Block [assume (+False)]) [5] [6];
    ];

  let conditional = {
    If.test = +True;
    body = [!!"first"; !!"second"];
    orelse = [+Pass];
  } in
  assert_cfg
    [+If conditional]
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [6] [3];
      node 2 Node.Error [] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.If conditional) [0] [7; 8];
      node 6 Node.Join [7; 8] [1];
      node 7 (Node.Block [assume (+True); !!"first"; !!"second"]) [5] [6];
      node 8 (Node.Block [assume (+False); +Pass]) [5] [6];
    ];

  let conditional = {
    If.test = +True;
    body = [!!"first"; !!"second"];
    orelse = [!!"orelse"];
  } in
  assert_cfg
    [!!"before"; +If conditional; !!"after"]
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [10] [3];
      node 2 Node.Error [] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.Block [!!"before"]) [0] [6];
      node 6 (Node.If conditional) [5] [8; 9];
      node 7 Node.Join [8; 9] [10];
      node 8 (Node.Block [assume (+True); !!"first"; !!"second"]) [6] [7];
      node 9 (Node.Block [assume (+False); !!"orelse"]) [6] [7];
      node 10 (Node.Block [!!"after"]) [7] [1];
    ]


let test_raise _ =
  let error = +Raise None in
  assert_cfg
    [error]
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [] [3];
      node 2 Node.Error [5] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.Block [error]) [0] [2];
    ];
  assert_cfg
    [!!"reached"; error; !!"unreached"]
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [] [3];
      node 2 Node.Error [5] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.Block [!!"reached"; error]) [0] [2];
    ]


let test_return _ =
  let return = +Return { Return.expression = None; is_implicit = false } in
  assert_cfg
    [return]
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [5] [3];
      node 2 Node.Error [] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.Block [return]) [0] [1];
    ];

  assert_cfg
    [!!"reached"; return; !!"unreached"]
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [5] [3];
      node 2 Node.Error [] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.Block [!!"reached"; return]) [0] [1];
    ]


let test_try _ =
  let handler ?kind ?name body =
    { Try.kind; name; handler_body = [!!body] }
  in

  let block = {
    Try.body = [!!"body"];
    handlers = [handler ~kind:(+Integer 1) "handler"];
    orelse = [!!"orelse"];
    finally = [!!"finally"];
  } in
  assert_cfg
    [+Try block]
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [7; 9] [3];
      node 2 Node.Error [8] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.Try block)  [0] [6; 10];
      node 6 Node.Dispatch [5] [8; 11];
      node 7 (Node.Block [!!"finally"]) [10; 11] [1]; (* finally *)
      node 8 (Node.Block [!!"finally"]) [6] [2]; (* uncaught *)
      node 9 (Node.Block [!!"finally"]) [] [1]; (* return *)
      node 10 (Node.Block [!!"body"; !!"orelse"]) [5] [7];
      node 11 (Node.Block [+Expression (+Integer 1); !!"handler"]) [6] [7];
    ];

  let block = {
    Try.body = [!!"body"];
    handlers = [handler "handler"];
    orelse = [];
    finally = [!!"finally"];
  } in
  assert_cfg
    [+Try block]
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [7; 9] [3];
      node 2 Node.Error [8] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.Try block)  [0] [6; 10];
      node 6 Node.Dispatch [5] [8; 11];
      node 7 (Node.Block [!!"finally"]) [10; 11] [1]; (* finally *)
      node 8 (Node.Block [!!"finally"]) [6] [2]; (* uncaught *)
      node 9 (Node.Block [!!"finally"]) [] [1]; (* return *)
      node 10 (Node.Block [!!"body"]) [5] [7];
      node 11 (Node.Block [!!"handler"]) [6] [7];
    ];

  let block = {
    Try.body = [!!"body"];
    handlers = [handler "handler"];
    orelse = [];
    finally = [];
  } in
  assert_cfg
    [+Try block]
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [7; 9] [3];
      node 2 Node.Error [8] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.Try block)  [0] [6; 10];
      node 6 Node.Dispatch [5] [8; 11];
      node 7 (Node.Block []) [10; 11] [1]; (* finally *)
      node 8 (Node.Block []) [6] [2]; (* uncaught *)
      node 9 (Node.Block []) [] [1]; (* return *)
      node 10 (Node.Block [!!"body"]) [5] [7];
      node 11 (Node.Block [!!"handler"]) [6] [7];
    ];

  let block = {
    Try.body = [!!"body"];
    handlers = [handler "handler 1"; handler "handler 2"];
    orelse = [];
    finally = [];
  } in
  assert_cfg
    [+Try block]
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [7; 9] [3];
      node 2 Node.Error [8] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.Try block)  [0] [6; 10];
      node 6 Node.Dispatch [5] [8; 11; 12];
      node 7 (Node.Block []) [10; 11; 12] [1]; (* finally *)
      node 8 (Node.Block []) [6] [2]; (* uncaught *)
      node 9 (Node.Block []) [] [1]; (* return *)
      node 10 (Node.Block [!!"body"]) [5] [7];
      node 11 (Node.Block [!!"handler 1"]) [6] [7];
      node 12 (Node.Block [!!"handler 2"]) [6] [7];
    ];

  let return = +Return { Return.expression = None; is_implicit = false } in
  let block = {
    Try.body = [!!"body"; return; !!"unreached"];
    handlers = [handler "handler"];
    orelse = [];
    finally = [];
  } in
  assert_cfg
    [+Try block]
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [7; 9] [3];
      node 2 Node.Error [8] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.Try block)  [0] [6; 10];
      node 6 Node.Dispatch [5] [8; 11];
      node 7 (Node.Block []) [11] [1]; (* finally *)
      node 8 (Node.Block []) [6] [2]; (* uncaught *)
      node 9 (Node.Block []) [10] [1]; (* return *)
      node 10 (Node.Block [!!"body"; return]) [5] [9];
      node 11 (Node.Block [!!"handler"]) [6] [7];
    ];

  let error = +Raise None in
  let block = {
    Try.body = [!!"body"; error; !!"unreached"];
    handlers = [handler "handler"];
    orelse = [];
    finally = [];
  } in
  assert_cfg
    [+Try block]
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [7; 9] [3];
      node 2 Node.Error [8] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.Try block)  [0] [6; 10];
      node 6 Node.Dispatch [5; 10] [8; 11];
      node 7 (Node.Block []) [11] [1]; (* finally *)
      node 8 (Node.Block []) [6] [2]; (* uncaught *)
      node 9 (Node.Block []) [] [1]; (* return *)
      node 10 (Node.Block [!!"body"; error]) [5] [6];
      node 11 (Node.Block [!!"handler"]) [6] [7];
    ];

  let block = {
    Try.body = [!!"body"];
    handlers = [];
    orelse = [];
    finally = [!!"finally"];
  } in
  assert_cfg
    [+Try block]
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [7; 9] [3];
      node 2 Node.Error [8] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.Try block) [0] [6; 10];
      node 6 Node.Dispatch [5] [8];
      node 7 (Node.Block [!!"finally"]) [10] [1]; (* finally *)
      node 8 (Node.Block [!!"finally"]) [6] [2]; (* uncaught *)
      node 9 (Node.Block [!!"finally"]) [] [1]; (* return *)
      node 10 (Node.Block [!!"body"]) [5] [7];
    ];

  let block = {
    Try.body = [!!"body"];
    handlers = [];
    orelse = [];
    finally = [+Return { Return.expression = None; is_implicit = false }];
  } in
  assert_cfg
    [+Try block]
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [7; 8; 9] [3];
      node 2 Node.Error [8] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.Try block) [0] [6; 10];
      node 6 Node.Dispatch [5] [8];
      node
        7
        (Node.Block [+Return { Return.expression = None; is_implicit = false }])
        [10]
        [1]; (* finally *)
      node
        8
        (Node.Block [+Return { Return.expression = None; is_implicit = false }])
        [6]
        [1; 2]; (* uncaught *)
      node
        9
        (Node.Block [+Return { Return.expression = None; is_implicit = false }])
        []
        [1]; (* return *)
      node 10 (Node.Block [!!"body"]) [5] [7];
    ]


let test_with _ =
  let block = {
    With.items = [!"item", None];
    body = [!!"body"];
    async = false;
  } in
  assert_cfg
    [+With block; !!"after"]
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [6] [3];
      node 2 Node.Error [] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.With block) [0] [6];
      node 6 (Node.Block [!!"item"; !!"body"; !!"after"]) [5] [1];
    ]


let test_while _ =
  let loop = {
    While.test = +True;
    body = [!!"body"];
    orelse = [!!"orelse"];
  } in
  assert_cfg
    [+While loop]
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [6] [3];
      node 2 Node.Error [] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.While loop) [0; 7] [6; 7; 8];
      node 6 Node.Join [5; 8] [1];
      node 7 (Node.Block [assume (+True); !!"body"]) [5] [5];
      node 8 (Node.Block [!!"orelse"]) [5] [6];
    ];

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
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [6] [3];
      node 2 Node.Error [] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.While loop) [0; 12] [6; 7; 13];
      node 6 Node.Join [5; 10; 13] [1];
      node 7 (Node.Block [assume (+True)]) [5] [8];
      node 8 (Node.If conditional) [7] [10; 11];
      node 9 Node.Join [11] [12];
      node 10 (Node.Block [assume (+True); +Break]) [8] [6];
      node 11 (Node.Block [assume (+False)]) [8] [9];
      node 12 (Node.Block [!!"body"]) [9] [5];
      node 13 (Node.Block [!!"orelse"]) [5] [6];
    ];

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
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [6] [3];
      node 2 Node.Error [] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.While loop) [0; 10; 12] [6; 7; 13];
      node 6 Node.Join [5; 13] [1];
      node 7 (Node.Block [assume (+True)]) [5] [8];
      node 8 (Node.If conditional) [7] [10; 11];
      node 9 Node.Join [11] [12];
      node 10 (Node.Block [assume (+True); +Continue]) [8] [5];
      node 11 (Node.Block [assume (+False)]) [8] [9];
      node 12 (Node.Block [assume (+False); !!"body"]) [9] [5];
      node 13 (Node.Block [!!"orelse"]) [5] [6];
    ];

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
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [6] [3];
      node 2 Node.Error [] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.While outer) [0; 11] [6; 7];
      node 6 Node.Join [5] [1];
      node 7 (Node.Block [assume (+True)]) [5] [8];
      node 8 (Node.While inner) [7; 10] [9; 10];
      node 9 Node.Join [8] [11];
      node 10 (Node.Block [assume (+True); !!"body"]) [8] [8];
      node 11 (Node.Block [+Continue]) [9] [5]
    ]


let test_yield _ =
  let yield = +Ast.Statement.Yield (+True) in
  assert_cfg
    [yield]
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [5] [3];
      node 2 Node.Error [] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [5] [];
      node 5 (Node.Block [yield]) [0] [1; 4];
    ]


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
  |> Test.run
