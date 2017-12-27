(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast.Expression
open Ast.Statement
open Cfg

module Instantiated = Ast.Instantiated

open Test


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

let assert_dot ?(precondition=fun _ -> "") body expected =
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
    ~cmp:String.equal
    ~printer:(fun id -> id)
    ~pp_diff:(diff ~print:(fun format -> Format.fprintf format "%s\n"))
    expected
    (to_dot ~precondition (create define))

let make_dot dot_list =
  let buffer = Buffer.create 1024 in
  Buffer.add_bytes buffer "digraph {\n";
  List.iter ~f:(fun string -> Buffer.add_bytes buffer ("  " ^ string ^ "\n")) dot_list;
  Buffer.add_bytes buffer "}";
  Buffer.to_bytes buffer

let node id kind predecessors successors =
  id,
  Node.create
    id
    kind
    (Int.Set.of_list predecessors)
    (Int.Set.of_list successors)

let test_block _ =
  let body = [+Pass] in
  assert_cfg
    body
    (Int.Table.of_alist_exn [
        node 0 Node.Entry [] [4];
        node 1 Node.Exit [4] [];
        node 2 Node.Error [] [];
        node 3 Node.Yield [] [];
        node 4 (Node.Block [+Pass]) [0] [1];
      ]);
  assert_dot
    body
    (make_dot [
        {|0[label="Entry"]|};
        {|1[label="Exit"]|};
        {|2[label="Error"]|};
        {|3[label="Yield"]|};
        {|4[label="pass"]|};
        "0 -> 4 [label=\"\", fontcolor=blue]";
        "4 -> 1 [label=\"\", fontcolor=blue]";
      ]);

  let body = [+Expression !"b"] in
  let fun_name id = Int.to_string id in
  assert_dot
    body
    ~precondition:fun_name
    (make_dot [
        {|0[label="Entry"]|};
        {|1[label="Exit"]|};
        {|2[label="Error"]|};
        {|3[label="Yield"]|};
        {|4[label="b"]|};
        "0 -> 4 [label=\"4\", fontcolor=blue]";
        "4 -> 1 [label=\"1\", fontcolor=blue]";
      ]);

  let body = [!!"first"; !!"second"] in
  assert_cfg
    body
    (Int.Table.of_alist_exn [
        node 0 Node.Entry [] [4];
        node 1 Node.Exit [4] [];
        node 2 Node.Error [] [];
        node 3 Node.Yield [] [];
        node 4 (Node.Block [!!"first"; !!"second"]) [0] [1];
      ]);
  assert_dot
    body
    (make_dot [
        {|0[label="Entry"]|};
        {|1[label="Exit"]|};
        {|2[label="Error"]|};
        {|3[label="Yield"]|};
        {|4[label="first\nsecond"]|};
        "0 -> 4 [label=\"\", fontcolor=blue]";
        "4 -> 1 [label=\"\", fontcolor=blue]";
      ])

let test_for _ =
  let loop = {
    For.target = +Access (Access.create "a");
    iterator = +List [];
    body = [!!"body"];
    orelse = [!!"orelse"];
    async = false;
  } in
  let body = [+For loop] in
  assert_cfg
    body
    (Int.Table.of_alist_exn [
        node 0 Node.Entry [] [4];
        node 1 Node.Exit [5] [];
        node 2 Node.Error [] [];
        node 3 Node.Yield [] [];
        node 4 (Node.For loop) [0; 6] [5; 6; 7];
        node 5 Node.Join [4; 7] [1];
        node 6 (Node.Block [!!"body"]) [4] [4];
        node 7 (Node.Block [!!"orelse"]) [4] [5];
      ]);
  assert_dot
    body
    (make_dot [
        {|0[label="Entry"]|};
        {|1[label="Exit"]|};
        {|2[label="Error"]|};
        {|3[label="Yield"]|};
        {|4[label="for a in []:\n  bodyorelse"]|};
        {|5[label="Join"]|};
        {|6[label="body"]|};
        {|7[label="orelse"]|};
        "0 -> 4 [label=\"\", fontcolor=blue]";
        "4 -> 5 [label=\"\", fontcolor=blue]";
        "4 -> 6 [label=\"\", fontcolor=blue]";
        "4 -> 7 [label=\"\", fontcolor=blue]";
        "5 -> 1 [label=\"\", fontcolor=blue]";
        "6 -> 4 [label=\"\", fontcolor=blue]";
        "7 -> 5 [label=\"\", fontcolor=blue]";
      ])

let test_if _ =
  let conditional = {
    If.test = +True;
    body = [!!"body"];
    orelse = [!!"orelse"];
  } in
  let body =  [+If conditional] in
  assert_cfg
    body
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
  assert_dot
    body
    (make_dot [
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
      ]);

  let conditional = { If.test = +True; body = [!!"body"]; orelse = [] } in
  let body =  [+If conditional] in
  assert_cfg
    body
    (Int.Table.of_alist_exn [
        node 0 Node.Entry [] [4];
        node 1 Node.Exit [5] [];
        node 2 Node.Error [] [];
        node 3 Node.Yield [] [];
        node 4 (Node.If conditional) [0] [5; 6];
        node 5 Node.Join [4; 6] [1];
        node 6 (Node.Block [assume (+True); !!"body"]) [4] [5];
      ]);
  assert_dot
    body
    (make_dot [
        {|0[label="Entry"]|};
        {|1[label="Exit"]|};
        {|2[label="Error"]|};
        {|3[label="Yield"]|};
        {|4[label="if True:\n  body\nelse:\n  "]|};
        {|5[label="Join"]|};
        {|6[label="assert True, \nbody"]|};
        "0 -> 4 [label=\"\", fontcolor=blue]";
        "4 -> 5 [label=\"\", fontcolor=blue]";
        "4 -> 6 [label=\"\", fontcolor=blue]";
        "5 -> 1 [label=\"\", fontcolor=blue]";
        "6 -> 5 [label=\"\", fontcolor=blue]";
      ]);

  let conditional = {
    If.test = +True;
    body = [!!"first"; !!"second"];
    orelse = [+Pass];
  } in
  let body =  [+If conditional] in
  assert_cfg
    body
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
  assert_dot
    body
    (make_dot [
        {|0[label="Entry"]|};
        {|1[label="Exit"]|};
        {|2[label="Error"]|};
        {|3[label="Yield"]|};
        {|4[label="if True:\n  first\n  second\nelse:\n  pass"]|};
        {|5[label="Join"]|};
        {|6[label="assert True, \nfirst\nsecond"]|};
        {|7[label="assert False, \npass"]|};
        "0 -> 4 [label=\"\", fontcolor=blue]";
        "4 -> 6 [label=\"\", fontcolor=blue]";
        "4 -> 7 [label=\"\", fontcolor=blue]";
        "5 -> 1 [label=\"\", fontcolor=blue]";
        "6 -> 5 [label=\"\", fontcolor=blue]";
        "7 -> 5 [label=\"\", fontcolor=blue]";
      ]);

  let conditional = {
    If.test = +True;
    body = [!!"first"; !!"second"];
    orelse = [!!"orelse"];
  } in
  let body = [
    !!"before";
    +If conditional;
    !!"after";
  ]
  in
  assert_cfg
    body
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
      ]);
  assert_dot
    body
    (make_dot [
        {|0[label="Entry"]|};
        {|1[label="Exit"]|};
        {|2[label="Error"]|};
        {|3[label="Yield"]|};
        {|4[label="before"]|};
        {|5[label="if True:\n  first\n  second\nelse:\n  orelse"]|};
        {|6[label="Join"]|};
        {|7[label="assert True, \nfirst\nsecond"]|};
        {|8[label="assert False, \norelse"]|};
        {|9[label="after"]|};
        "0 -> 4 [label=\"\", fontcolor=blue]";
        "4 -> 5 [label=\"\", fontcolor=blue]";
        "5 -> 7 [label=\"\", fontcolor=blue]";
        "5 -> 8 [label=\"\", fontcolor=blue]";
        "6 -> 9 [label=\"\", fontcolor=blue]";
        "7 -> 6 [label=\"\", fontcolor=blue]";
        "8 -> 6 [label=\"\", fontcolor=blue]";
        "9 -> 1 [label=\"\", fontcolor=blue]";
      ])

let test_raise _ =
  let error = +Raise None in
  let body = [error] in
  assert_cfg
    body
    (Int.Table.of_alist_exn [
        node 0 Node.Entry [] [4];
        node 1 Node.Exit [] [];
        node 2 Node.Error [4] [];
        node 3 Node.Yield [] [];
        node 4 (Node.Block [error]) [0] [2];
      ]);
  assert_dot
    body
    (make_dot [
        {|0[label="Entry"]|};
        {|1[label="Exit"]|};
        {|2[label="Error"]|};
        {|3[label="Yield"]|};
        {|4[label="raise "]|};
        "0 -> 4 [label=\"\", fontcolor=blue]";
        "4 -> 2 [label=\"\", fontcolor=blue]";
      ]);
  let body = [!!"reached"; error; !!"unreached"] in
  assert_cfg
    body
    (Int.Table.of_alist_exn [
        node 0 Node.Entry [] [4];
        node 1 Node.Exit [] [];
        node 2 Node.Error [4] [];
        node 3 Node.Yield [] [];
        node 4 (Node.Block [!!"reached"; error]) [0] [2];
      ]);
  assert_dot
    body
    (make_dot [
        {|0[label="Entry"]|};
        {|1[label="Exit"]|};
        {|2[label="Error"]|};
        {|3[label="Yield"]|};
        {|4[label="reached\nraise "]|};
        "0 -> 4 [label=\"\", fontcolor=blue]";
        "4 -> 2 [label=\"\", fontcolor=blue]";
      ])

let test_return _ =
  let return = +Return None in
  let body = [return] in
  assert_cfg
    body
    (Int.Table.of_alist_exn [
        node 0 Node.Entry [] [4];
        node 1 Node.Exit [4] [];
        node 2 Node.Error [] [];
        node 3 Node.Yield [] [];
        node 4 (Node.Block [return]) [0] [1];
      ]);
  assert_dot
    body
    (make_dot [
        {|0[label="Entry"]|};
        {|1[label="Exit"]|};
        {|2[label="Error"]|};
        {|3[label="Yield"]|};
        {|4[label="return "]|};
        "0 -> 4 [label=\"\", fontcolor=blue]";
        "4 -> 1 [label=\"\", fontcolor=blue]";
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
      ]);
  assert_dot
    body
    (make_dot [
        {|0[label="Entry"]|};
        {|1[label="Exit"]|};
        {|2[label="Error"]|};
        {|3[label="Yield"]|};
        {|4[label="reached\nreturn "]|};
        "0 -> 4 [label=\"\", fontcolor=blue]";
        "4 -> 1 [label=\"\", fontcolor=blue]";
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
  let body = [+Try block] in
  assert_cfg
    body
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
  assert_dot
    body
    (make_dot [
        {|0[label="Entry"]|};
        {|1[label="Exit"]|};
        {|2[label="Error"]|};
        {|3[label="Yield"]|};
        {|4[label="try:\n  body\nexcept:\n  handler\nelse:\n  orelse\nfinally:\n  finally"]|};
        {|5[label="Dispatch"]|};
        {|6[label="finally"]|};
        {|7[label="finally"]|};
        {|8[label="finally"]|};
        {|9[label="body\norelse"]|};
        {|10[label="handler"]|};
        "0 -> 4 [label=\"\", fontcolor=blue]";
        "4 -> 5 [label=\"\", fontcolor=blue]";
        "4 -> 9 [label=\"\", fontcolor=blue]";
        "5 -> 7 [label=\"\", fontcolor=blue]";
        "5 -> 10 [label=\"\", fontcolor=blue]";
        "6 -> 1 [label=\"\", fontcolor=blue]";
        "7 -> 2 [label=\"\", fontcolor=blue]";
        "8 -> 1 [label=\"\", fontcolor=blue]";
        "9 -> 6 [label=\"\", fontcolor=blue]";
        "10 -> 6 [label=\"\", fontcolor=blue]";
      ]);

  let block = {
    Try.body = [!!"body"];
    handlers = [handler "handler"];
    orelse = [];
    finally = [!!"finally"];
  } in
  let body = [+Try block] in
  assert_cfg
    body
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
  assert_dot
    body
    (make_dot [
        {|0[label="Entry"]|};
        {|1[label="Exit"]|};
        {|2[label="Error"]|};
        {|3[label="Yield"]|};
        {|4[label="try:\n  body\nexcept:\n  handler\n\nfinally:\n  finally"]|};
        {|5[label="Dispatch"]|};
        {|6[label="finally"]|};
        {|7[label="finally"]|};
        {|8[label="finally"]|};
        {|9[label="body"]|};
        {|10[label="handler"]|};
        "0 -> 4 [label=\"\", fontcolor=blue]";
        "4 -> 5 [label=\"\", fontcolor=blue]";
        "4 -> 9 [label=\"\", fontcolor=blue]";
        "5 -> 7 [label=\"\", fontcolor=blue]";
        "5 -> 10 [label=\"\", fontcolor=blue]";
        "6 -> 1 [label=\"\", fontcolor=blue]";
        "7 -> 2 [label=\"\", fontcolor=blue]";
        "8 -> 1 [label=\"\", fontcolor=blue]";
        "9 -> 6 [label=\"\", fontcolor=blue]";
        "10 -> 6 [label=\"\", fontcolor=blue]";
      ]);

  let block = {
    Try.body = [!!"body"];
    handlers = [handler "handler"];
    orelse = [];
    finally = [];
  } in
  let body = [+Try block] in
  assert_cfg
    body
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
  assert_dot
    body
    (make_dot [
        {|0[label="Entry"]|};
        {|1[label="Exit"]|};
        {|2[label="Error"]|};
        {|3[label="Yield"]|};
        {|4[label="try:\n  body\nexcept:\n  handler\n\n"]|};
        {|5[label="Dispatch"]|};
        {|6[label=""]|};
        {|7[label=""]|};
        {|8[label=""]|};
        {|9[label="body"]|};
        {|10[label="handler"]|};
        "0 -> 4 [label=\"\", fontcolor=blue]";
        "4 -> 5 [label=\"\", fontcolor=blue]";
        "4 -> 9 [label=\"\", fontcolor=blue]";
        "5 -> 7 [label=\"\", fontcolor=blue]";
        "5 -> 10 [label=\"\", fontcolor=blue]";
        "6 -> 1 [label=\"\", fontcolor=blue]";
        "7 -> 2 [label=\"\", fontcolor=blue]";
        "8 -> 1 [label=\"\", fontcolor=blue]";
        "9 -> 6 [label=\"\", fontcolor=blue]";
        "10 -> 6 [label=\"\", fontcolor=blue]";
      ]);

  let block = {
    Try.body = [!!"body"];
    handlers = [handler "handler 1"; handler "handler 2"];
    orelse = [];
    finally = [];
  } in
  let body = [+Try block] in
  assert_cfg
    body
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
  assert_dot
    body
    (make_dot [
        {|0[label="Entry"]|};
        {|1[label="Exit"]|};
        {|2[label="Error"]|};
        {|3[label="Yield"]|};
        {|4[label="try:\n  body\nexcept:\n  handler 1\nexcept:\n  handler 2\n\n"]|};
        {|5[label="Dispatch"]|};
        {|6[label=""]|};
        {|7[label=""]|};
        {|8[label=""]|};
        {|9[label="body"]|};
        {|10[label="handler 1"]|};
        {|11[label="handler 2"]|};
        "0 -> 4 [label=\"\", fontcolor=blue]";
        "4 -> 5 [label=\"\", fontcolor=blue]";
        "4 -> 9 [label=\"\", fontcolor=blue]";
        "5 -> 7 [label=\"\", fontcolor=blue]";
        "5 -> 10 [label=\"\", fontcolor=blue]";
        "5 -> 11 [label=\"\", fontcolor=blue]";
        "6 -> 1 [label=\"\", fontcolor=blue]";
        "7 -> 2 [label=\"\", fontcolor=blue]";
        "8 -> 1 [label=\"\", fontcolor=blue]";
        "9 -> 6 [label=\"\", fontcolor=blue]";
        "10 -> 6 [label=\"\", fontcolor=blue]";
        "11 -> 6 [label=\"\", fontcolor=blue]";
      ]);

  let return = +Return None in
  let block = {
    Try.body = [!!"body"; return; !!"unreached"];
    handlers = [handler "handler"];
    orelse = [];
    finally = [];
  } in
  let body = [+Try block] in
  assert_cfg
    body
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
  assert_dot
    body
    (make_dot [
        {|0[label="Entry"]|};
        {|1[label="Exit"]|};
        {|2[label="Error"]|};
        {|3[label="Yield"]|};
        {|4[label="try:\n  body\n  return \n  unreached\nexcept:\n  handler\n\n"]|};
        {|5[label="Dispatch"]|};
        {|6[label=""]|};
        {|7[label=""]|};
        {|8[label=""]|};
        {|9[label="body\nreturn "]|};
        {|10[label="handler"]|};
        "0 -> 4 [label=\"\", fontcolor=blue]";
        "4 -> 5 [label=\"\", fontcolor=blue]";
        "4 -> 9 [label=\"\", fontcolor=blue]";
        "5 -> 7 [label=\"\", fontcolor=blue]";
        "5 -> 10 [label=\"\", fontcolor=blue]";
        "6 -> 1 [label=\"\", fontcolor=blue]";
        "7 -> 2 [label=\"\", fontcolor=blue]";
        "8 -> 1 [label=\"\", fontcolor=blue]";
        "9 -> 8 [label=\"\", fontcolor=blue]";
        "10 -> 6 [label=\"\", fontcolor=blue]";
      ]);

  let error = +Raise None in
  let block = {
    Try.body = [!!"body"; error; !!"unreached"];
    handlers = [handler "handler"];
    orelse = [];
    finally = [];
  } in
  let body = [+Try block] in
  assert_cfg
    body
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
  assert_dot
    body
    (make_dot [
        {|0[label="Entry"]|};
        {|1[label="Exit"]|};
        {|2[label="Error"]|};
        {|3[label="Yield"]|};
        {|4[label="try:\n  body\n  raise \n  unreached\nexcept:\n  handler\n\n"]|};
        {|5[label="Dispatch"]|};
        {|6[label=""]|};
        {|7[label=""]|};
        {|8[label=""]|};
        {|9[label="body\nraise "]|};
        {|10[label="handler"]|};
        "0 -> 4 [label=\"\", fontcolor=blue]";
        "4 -> 5 [label=\"\", fontcolor=blue]";
        "4 -> 9 [label=\"\", fontcolor=blue]";
        "5 -> 7 [label=\"\", fontcolor=blue]";
        "5 -> 10 [label=\"\", fontcolor=blue]";
        "6 -> 1 [label=\"\", fontcolor=blue]";
        "7 -> 2 [label=\"\", fontcolor=blue]";
        "8 -> 1 [label=\"\", fontcolor=blue]";
        "9 -> 5 [label=\"\", fontcolor=blue]";
        "10 -> 6 [label=\"\", fontcolor=blue]";
      ]);

  let block = {
    Try.body = [!!"body"];
    handlers = [];
    orelse = [];
    finally = [!!"finally"];
  } in
  let body = [+Try block] in
  assert_cfg
    body
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
  assert_dot
    body
    (make_dot [
        {|0[label="Entry"]|};
        {|1[label="Exit"]|};
        {|2[label="Error"]|};
        {|3[label="Yield"]|};
        {|4[label="try:\n  body\n\n\nfinally:\n  finally"]|};
        {|5[label="Dispatch"]|};
        {|6[label="finally"]|};
        {|7[label="finally"]|};
        {|8[label="finally"]|};
        {|9[label="body"]|};
        "0 -> 4 [label=\"\", fontcolor=blue]";
        "4 -> 5 [label=\"\", fontcolor=blue]";
        "4 -> 9 [label=\"\", fontcolor=blue]";
        "5 -> 7 [label=\"\", fontcolor=blue]";
        "6 -> 1 [label=\"\", fontcolor=blue]";
        "7 -> 2 [label=\"\", fontcolor=blue]";
        "8 -> 1 [label=\"\", fontcolor=blue]";
        "9 -> 6 [label=\"\", fontcolor=blue]";
      ]);

  let block = {
    Try.body = [!!"body"];
    handlers = [];
    orelse = [];
    finally = [+Return None];
  } in
  let body = [+Try block] in
  assert_cfg
    body
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
      ]);
  assert_dot
    body
    (make_dot [
        {|0[label="Entry"]|};
        {|1[label="Exit"]|};
        {|2[label="Error"]|};
        {|3[label="Yield"]|};
        {|4[label="try:\n  body\n\n\nfinally:\n  return "]|};
        {|5[label="Dispatch"]|};
        {|6[label="return "]|};
        {|7[label="return "]|};
        {|8[label="return "]|};
        {|9[label="body"]|};
        "0 -> 4 [label=\"\", fontcolor=blue]";
        "4 -> 5 [label=\"\", fontcolor=blue]";
        "4 -> 9 [label=\"\", fontcolor=blue]";
        "5 -> 7 [label=\"\", fontcolor=blue]";
        "6 -> 1 [label=\"\", fontcolor=blue]";
        "7 -> 1 [label=\"\", fontcolor=blue]";
        "7 -> 2 [label=\"\", fontcolor=blue]";
        "8 -> 1 [label=\"\", fontcolor=blue]";
        "9 -> 6 [label=\"\", fontcolor=blue]";
      ])

let test_with _ =
  let block = {
    With.items = [];
    body = [!!"body"];
    async = false;
  } in
  let body = [+With block; !!"after"] in
  assert_cfg
    body
    (Int.Table.of_alist_exn [
        node 0 Node.Entry [] [4];
        node 1 Node.Exit [5] [];
        node 2 Node.Error [] [];
        node 3 Node.Yield [] [];
        node 4 (Node.With block) [0] [5];
        node 5 (Node.Block [!!"body"; !!"after"]) [4] [1];
      ]);
  assert_dot
    body
    (make_dot [
        {|0[label="Entry"]|};
        {|1[label="Exit"]|};
        {|2[label="Error"]|};
        {|3[label="Yield"]|};
        {|4[label=" with :\n  body"]|};
        {|5[label="body\nafter"]|};
        "0 -> 4 [label=\"\", fontcolor=blue]";
        "4 -> 5 [label=\"\", fontcolor=blue]";
        "5 -> 1 [label=\"\", fontcolor=blue]";
      ])


let test_while _ =
  let loop = {
    While.test = +True;
    body = [!!"body"];
    orelse = [!!"orelse"];
  } in
  let body = [+While loop] in
  assert_cfg
    body
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
  assert_dot
    body
    (make_dot [
        {|0[label="Entry"]|};
        {|1[label="Exit"]|};
        {|2[label="Error"]|};
        {|3[label="Yield"]|};
        {|4[label="while True:\n  bodyorelse"]|};
        {|5[label="Join"]|};
        {|6[label="body"]|};
        {|7[label="orelse"]|};
        "0 -> 4 [label=\"\", fontcolor=blue]";
        "4 -> 5 [label=\"\", fontcolor=blue]";
        "4 -> 6 [label=\"\", fontcolor=blue]";
        "4 -> 7 [label=\"\", fontcolor=blue]";
        "5 -> 1 [label=\"\", fontcolor=blue]";
        "6 -> 4 [label=\"\", fontcolor=blue]";
        "7 -> 5 [label=\"\", fontcolor=blue]";
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
  let body = [+While loop] in
  assert_cfg
    body
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
  assert_dot
    body
    (make_dot [
        {|0[label="Entry"]|};
        {|1[label="Exit"]|};
        {|2[label="Error"]|};
        {|3[label="Yield"]|};
        {|4[label="while True:\n  if True:\n    break\n  else:\n    \n  bodyorelse"]|};
        {|5[label="Join"]|};
        {|6[label="if True:\n  break\nelse:\n  "]|};
        {|7[label="Join"]|};
        {|8[label="assert True, \nbreak"]|};
        {|9[label="body"]|};
        {|10[label="orelse"]|};
        "0 -> 4 [label=\"\", fontcolor=blue]";
        "4 -> 5 [label=\"\", fontcolor=blue]";
        "4 -> 6 [label=\"\", fontcolor=blue]";
        "4 -> 10 [label=\"\", fontcolor=blue]";
        "5 -> 1 [label=\"\", fontcolor=blue]";
        "6 -> 7 [label=\"\", fontcolor=blue]";
        "6 -> 8 [label=\"\", fontcolor=blue]";
        "7 -> 9 [label=\"\", fontcolor=blue]";
        "8 -> 5 [label=\"\", fontcolor=blue]";
        "9 -> 4 [label=\"\", fontcolor=blue]";
        "10 -> 5 [label=\"\", fontcolor=blue]";
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
  let body = [+While loop] in
  assert_cfg
    body
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
  assert_dot
    body
    (make_dot [
        {|0[label="Entry"]|};
        {|1[label="Exit"]|};
        {|2[label="Error"]|};
        {|3[label="Yield"]|};
        {|4[label="while True:\n  if True:\n    continue\n  else:\n    \n  bodyorelse"]|};
        {|5[label="Join"]|};
        {|6[label="if True:\n  continue\nelse:\n  "]|};
        {|7[label="Join"]|};
        {|8[label="assert True, \ncontinue"]|};
        {|9[label="assert False, \nbody"]|};
        {|10[label="orelse"]|};
        "0 -> 4 [label=\"\", fontcolor=blue]";
        "4 -> 5 [label=\"\", fontcolor=blue]";
        "4 -> 6 [label=\"\", fontcolor=blue]";
        "4 -> 10 [label=\"\", fontcolor=blue]";
        "5 -> 1 [label=\"\", fontcolor=blue]";
        "6 -> 7 [label=\"\", fontcolor=blue]";
        "6 -> 8 [label=\"\", fontcolor=blue]";
        "7 -> 9 [label=\"\", fontcolor=blue]";
        "8 -> 4 [label=\"\", fontcolor=blue]";
        "9 -> 4 [label=\"\", fontcolor=blue]";
        "10 -> 5 [label=\"\", fontcolor=blue]";
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
  let body = [+While outer] in
  assert_cfg
    body
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
      ]);
  assert_dot
    body
    (make_dot [
        {|0[label="Entry"]|};
        {|1[label="Exit"]|};
        {|2[label="Error"]|};
        {|3[label="Yield"]|};
        {|4[label="while True:\n  while True:\n    body\n  continue"]|};
        {|5[label="Join"]|};
        {|6[label="while True:\n  body"]|};
        {|7[label="Join"]|};
        {|8[label="body"]|};
        {|9[label="continue"]|};
        "0 -> 4 [label=\"\", fontcolor=blue]";
        "4 -> 5 [label=\"\", fontcolor=blue]";
        "4 -> 6 [label=\"\", fontcolor=blue]";
        "5 -> 1 [label=\"\", fontcolor=blue]";
        "6 -> 7 [label=\"\", fontcolor=blue]";
        "6 -> 8 [label=\"\", fontcolor=blue]";
        "7 -> 9 [label=\"\", fontcolor=blue]";
        "8 -> 6 [label=\"\", fontcolor=blue]";
        "9 -> 4 [label=\"\", fontcolor=blue]";
      ])

let test_yield _ =
  let yield = +Ast.Statement.Yield (+True) in
  let body = [yield] in
  assert_cfg
    body
    (Int.Table.of_alist_exn [
        node 0 Node.Entry [] [4];
        node 1 Node.Exit [4] [];
        node 2 Node.Error [] [];
        node 3 Node.Yield [4] [];
        node 4 (Node.Block [yield]) [0] [1; 3];
      ]);
  assert_dot
    body
    (make_dot [
        {|0[label="Entry"]|};
        {|1[label="Exit"]|};
        {|2[label="Error"]|};
        {|3[label="Yield"]|};
        {|4[label="True"]|};
        "0 -> 4 [label=\"\", fontcolor=blue]";
        "4 -> 1 [label=\"\", fontcolor=blue]";
        "4 -> 3 [label=\"\", fontcolor=blue]";
      ])

let () =
  "cfg">:::[
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
