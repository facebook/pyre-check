(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Ast.Expression
open Ast.Statement
open Analysis.Cfg
open Test

let test_to_dot _ =
  let assert_dot ?(precondition = fun _ -> "") body expected =
    let define =
      {
        Define.signature =
          {
            name = !&"foo";
            parameters = [];
            decorators = [];
            return_annotation = None;
            async = false;
            generator = false;
            parent = None;
            nesting_define = None;
          };
        captures = [];
        unbound_names = [];
        body;
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
    [+Statement.Pass]
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
    [+Statement.Expression !"b"]
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
  let conditional =
    { If.test = +Expression.Constant Constant.True; body = [!!"body"]; orelse = [!!"orelse"] }
  in
  assert_dot
    [+Statement.If conditional]
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
  let define =
    {
      Define.signature =
        {
          name = !&"foo";
          parameters = [];
          decorators = [];
          return_annotation = None;
          async = false;
          generator = false;
          parent = None;
          nesting_define = None;
        };
      captures = [];
      unbound_names = [];
      body;
    }
  in
  let pp formatter control_flow_graph =
    Hashtbl.to_alist control_flow_graph
    |> List.map ~f:snd
    |> List.map ~f:Analysis.Cfg.Node.sexp_of_t
    |> List.map ~f:Sexp.to_string_hum
    |> List.sort ~compare:String.compare
    |> String.concat ~sep:"\n"
    |> String.pp formatter
  in
  let cmp left right = Core.Hashtbl.equal Node.location_insensitive_equal left right in
  assert_equal
    ~cmp
    ~printer:(fun cfg -> Format.asprintf "%a" pp cfg)
    ~pp_diff:(diff ~print:pp)
    (Int.Table.of_alist_exn expected)
    (create define)


let node id kind predecessors successors =
  id, Node.create id kind (Int.Set.of_list predecessors) (Int.Set.of_list successors)


let test_block _ =
  assert_cfg
    [+Statement.Pass]
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [5] [3];
      node 2 Node.Error [] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.Block [+Statement.Pass]) [0] [1];
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
  let loop =
    {
      For.target = +Expression.Name (Name.Identifier "a");
      iterator = +Expression.List [];
      body = [!!"body"];
      orelse = [!!"orelse"];
      async = false;
    }
  in
  assert_cfg
    [+Statement.For loop]
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
        (Node.Block
           [
             parse_single_statement ~coerce_special_methods:true "a = [].__iter__().__next__()";
             !!"body";
           ])
        [5]
        [5];
      node 8 (Node.Block [!!"orelse"]) [5] [6];
    ]


let test_if _ =
  let conditional =
    { If.test = +Expression.Constant Constant.True; body = [!!"body"]; orelse = [!!"orelse"] }
  in
  assert_cfg
    [+Statement.If conditional]
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [6] [3];
      node 2 Node.Error [] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.If conditional) [0] [7; 8];
      node 6 Node.Join [7; 8] [1];
      node
        7
        (Node.Block
           [
             Statement.assume
               ~origin:(Assert.Origin.If { true_branch = true })
               (+Expression.Constant Constant.True);
             !!"body";
           ])
        [5]
        [6];
      node
        8
        (Node.Block
           [
             Statement.assume
               ~origin:(Assert.Origin.If { true_branch = false })
               (+Expression.Constant Constant.False);
             !!"orelse";
           ])
        [5]
        [6];
    ];
  let conditional =
    { If.test = +Expression.Constant Constant.True; body = [!!"body"]; orelse = [] }
  in
  assert_cfg
    [+Statement.If conditional]
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [6] [3];
      node 2 Node.Error [] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.If conditional) [0] [7; 8];
      node 6 Node.Join [7; 8] [1];
      node
        7
        (Node.Block
           [
             Statement.assume
               ~origin:(Assert.Origin.If { true_branch = true })
               (+Expression.Constant Constant.True);
             !!"body";
           ])
        [5]
        [6];
      node
        8
        (Node.Block
           [
             Statement.assume
               ~origin:(Assert.Origin.If { true_branch = false })
               (+Expression.Constant Constant.False);
           ])
        [5]
        [6];
    ];
  let conditional =
    {
      If.test = +Expression.Constant Constant.True;
      body = [!!"first"; !!"second"];
      orelse = [+Statement.Pass];
    }
  in
  assert_cfg
    [+Statement.If conditional]
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [6] [3];
      node 2 Node.Error [] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.If conditional) [0] [7; 8];
      node 6 Node.Join [7; 8] [1];
      node
        7
        (Node.Block
           [
             Statement.assume
               ~origin:(Assert.Origin.If { true_branch = true })
               (+Expression.Constant Constant.True);
             !!"first";
             !!"second";
           ])
        [5]
        [6];
      node
        8
        (Node.Block
           [
             Statement.assume
               ~origin:(Assert.Origin.If { true_branch = false })
               (+Expression.Constant Constant.False);
             +Statement.Pass;
           ])
        [5]
        [6];
    ];
  let conditional =
    {
      If.test = +Expression.Constant Constant.True;
      body = [!!"first"; !!"second"];
      orelse = [!!"orelse"];
    }
  in
  assert_cfg
    [!!"before"; +Statement.If conditional; !!"after"]
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [10] [3];
      node 2 Node.Error [] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.Block [!!"before"]) [0] [6];
      node 6 (Node.If conditional) [5] [8; 9];
      node 7 Node.Join [8; 9] [10];
      node
        8
        (Node.Block
           [
             Statement.assume
               ~origin:(Assert.Origin.If { true_branch = true })
               (+Expression.Constant Constant.True);
             !!"first";
             !!"second";
           ])
        [6]
        [7];
      node
        9
        (Node.Block
           [
             Statement.assume
               ~origin:(Assert.Origin.If { true_branch = false })
               (+Expression.Constant Constant.False);
             !!"orelse";
           ])
        [6]
        [7];
      node 10 (Node.Block [!!"after"]) [7] [1];
    ]


let test_match _ =
  let refutable_case =
    {
      Match.Case.pattern = +Match.Pattern.MatchWildcard;
      guard = Some !"guard";
      body = [!!"refutable_body"];
    }
  in
  let expected_refutable_case_pass_block =
    Node.Block [Statement.assume ~origin:Assert.Origin.Match !"guard"; !!"refutable_body"]
  in
  let expected_refutable_case_fail_block =
    Node.Block
      [
        Statement.assume
          ~origin:Assert.Origin.Match
          (+Expression.UnaryOperator { operator = UnaryOperator.Not; operand = !"guard" });
      ]
  in
  assert_cfg
    [+Statement.Match { Match.subject = !"x"; cases = [refutable_case] }]
    [
      node 0 Node.Entry [] [6; 7];
      node 1 Node.Normal [5] [3];
      node 2 Node.Error [] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 Node.Join [6; 7] [1];
      node 6 expected_refutable_case_pass_block [0] [5];
      node 7 expected_refutable_case_fail_block [0] [5];
    ];
  let irrefutable_case =
    {
      Match.Case.pattern = +Match.Pattern.MatchWildcard;
      guard = None;
      body = [!!"irrefutable_body"];
    }
  in
  let expected_irrefutable_case_pass_block =
    Node.Block
      [
        Statement.assume ~origin:Assert.Origin.Match (+Expression.Constant Constant.True);
        !!"irrefutable_body";
      ]
  in
  let expected_irrefutable_case_fail_block =
    Node.Block [Statement.assume ~origin:Assert.Origin.Match (+Expression.Constant Constant.False)]
  in
  assert_cfg
    [+Statement.Match { Match.subject = !"x"; cases = [refutable_case; irrefutable_case] }]
    [
      node 0 Node.Entry [] [6; 7];
      node 1 Node.Normal [5] [3];
      node 2 Node.Error [] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 Node.Join [6; 8] [1];
      node 6 expected_refutable_case_pass_block [0] [5];
      node 7 expected_refutable_case_fail_block [0] [8; 9];
      node 8 expected_irrefutable_case_pass_block [7] [5];
      node 9 expected_irrefutable_case_fail_block [7] [];
    ];
  assert_true
    (match_cases_refutable
       [{ Match.Case.pattern = +Match.Pattern.MatchWildcard; guard = Some !"guard"; body = [] }]);
  assert_true
    (match_cases_refutable
       [
         {
           Match.Case.pattern = +Match.Pattern.MatchSingleton Ast.Expression.Constant.NoneLiteral;
           guard = None;
           body = [];
         };
       ]);
  assert_true
    (match_cases_refutable
       [
         { Match.Case.pattern = +Match.Pattern.MatchWildcard; guard = Some !"guard1"; body = [] };
         { Match.Case.pattern = +Match.Pattern.MatchWildcard; guard = Some !"guard2"; body = [] };
       ]);
  assert_false
    (match_cases_refutable
       [{ Match.Case.pattern = +Match.Pattern.MatchWildcard; guard = None; body = [] }]);
  assert_false
    (match_cases_refutable
       [
         { Match.Case.pattern = +Match.Pattern.MatchWildcard; guard = Some !"guard"; body = [] };
         { Match.Case.pattern = +Match.Pattern.MatchWildcard; guard = None; body = [] };
       ]);
  assert_false
    (match_cases_refutable
       [
         {
           Match.Case.pattern = +Match.Pattern.MatchAs { pattern = None; name = "x" };
           guard = None;
           body = [];
         };
       ]);
  assert_false
    (match_cases_refutable
       [
         {
           Match.Case.pattern =
             +Match.Pattern.MatchAs
                {
                  pattern = Some (+Match.Pattern.MatchAs { pattern = None; name = "y" });
                  name = "x";
                };
           guard = None;
           body = [];
         };
       ]);
  assert_false
    (match_cases_refutable
       [
         {
           Match.Case.pattern =
             +Match.Pattern.MatchOr
                [
                  +Match.Pattern.MatchSingleton Ast.Expression.Constant.NoneLiteral;
                  +Match.Pattern.MatchWildcard;
                ];
           guard = None;
           body = [];
         };
       ]);
  ()


let test_raise _ =
  let error = +Statement.Raise { Raise.expression = None; from = None } in
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


let test_assert_false _ =
  let error =
    +Statement.Assert
       {
         Assert.test = +Expression.Constant Constant.False;
         message = None;
         origin = Assert.Origin.Assertion;
       }
  in
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
  let return = +Statement.Return { Return.expression = None; is_implicit = false } in
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
  let handler ?kind ?name body = { Try.Handler.kind; name; body = [!!body] } in
  let block = { Try.body = [!!"body"]; handlers = []; orelse = []; finally = [] } in
  assert_cfg
    [+Statement.Try block; !!"fall-through"]
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [7] [3];
      node 2 Node.Error [6] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.Try block) [0] [6; 8];
      node 6 Node.Dispatch [5] [2];
      node 7 (Node.Block [!!"fall-through"]) [8] [1];
      node 8 (Node.Block [!!"body"]) [5] [7];
    ];
  let block =
    {
      Try.body = [!!"body"];
      handlers = [handler ~kind:(+Expression.Constant (Constant.Integer 1)) "handler"];
      orelse = [!!"orelse"];
      finally = [!!"finally"];
    }
  in
  assert_cfg
    [+Statement.Try block]
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [7] [3];
      node 2 Node.Error [6] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.Try block) [0] [6; 8];
      (* split *)
      node 6 Node.Dispatch [5] [2; 9];
      node 7 (Node.Block [!!"finally"]) [8; 9] [1];
      (* normal *)
      node 8 (Node.Block [!!"body"; !!"orelse"]) [5] [7];
      node
        9
        (Node.Block [+Statement.Expression (+Expression.Constant (Constant.Integer 1)); !!"handler"])
        [6]
        [7];
    ];
  let block =
    { Try.body = [!!"body"]; handlers = [handler "handler"]; orelse = []; finally = [!!"finally"] }
  in
  assert_cfg
    [+Statement.Try block]
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [7] [3];
      node 2 Node.Error [6] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.Try block) [0] [6; 8];
      node 6 Node.Dispatch [5] [2; 9];
      node 7 (Node.Block [!!"finally"]) [8; 9] [1];
      node 8 (Node.Block [!!"body"]) [5] [7];
      node 9 (Node.Block [!!"handler"]) [6] [7];
    ];
  let block =
    { Try.body = [!!"body"]; handlers = [handler "handler"]; orelse = []; finally = [] }
  in
  assert_cfg
    [+Statement.Try block]
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [7] [3];
      node 2 Node.Error [6] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.Try block) [0] [6; 8];
      node 6 Node.Dispatch [5] [2; 9];
      node 7 (Node.Block []) [8; 9] [1];
      node 8 (Node.Block [!!"body"]) [5] [7];
      node 9 (Node.Block [!!"handler"]) [6] [7];
    ];
  let block =
    {
      Try.body = [!!"body"];
      handlers = [handler "handler 1"; handler "handler 2"];
      orelse = [];
      finally = [];
    }
  in
  assert_cfg
    [+Statement.Try block]
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [7] [3];
      node 2 Node.Error [6] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.Try block) [0] [6; 8];
      node 6 Node.Dispatch [5] [2; 9; 10];
      node 7 (Node.Block []) [8; 9; 10] [1];
      node 8 (Node.Block [!!"body"]) [5] [7];
      node 9 (Node.Block [!!"handler 1"]) [6] [7];
      node 10 (Node.Block [!!"handler 2"]) [6] [7];
    ];
  let return = +Statement.Return { Return.expression = None; is_implicit = false } in
  let block =
    {
      Try.body = [!!"body"; return; !!"unreached"];
      handlers = [handler "handler"];
      orelse = [];
      finally = [];
    }
  in
  assert_cfg
    [+Statement.Try block]
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [7; 8] [3];
      node 2 Node.Error [6] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.Try block) [0] [6; 8];
      node 6 Node.Dispatch [5] [2; 9];
      node 7 (Node.Block []) [9] [1];
      node 8 (Node.Block [!!"body"; return]) [5] [1];
      node 9 (Node.Block [!!"handler"]) [6] [7];
    ];
  let error = +Statement.Raise { Raise.expression = None; from = None } in
  let block =
    {
      Try.body = [!!"body"; error; !!"unreached"];
      handlers = [handler "handler"];
      orelse = [];
      finally = [];
    }
  in
  assert_cfg
    [+Statement.Try block]
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [7] [3];
      node 2 Node.Error [6] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.Try block) [0] [6; 8];
      node 6 Node.Dispatch [5; 8] [2; 9];
      node 7 (Node.Block []) [9] [1];
      node 8 (Node.Block [!!"body"; error]) [5] [6];
      node 9 (Node.Block [!!"handler"]) [6] [7];
    ];
  let block =
    {
      Try.body = [!!"body"];
      handlers = [handler "handler"];
      orelse = [!!"orelse"; error; !!"unreached"];
      finally = [];
    }
  in
  assert_cfg
    [+Statement.Try block]
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [7] [3];
      node 2 Node.Error [6; 8] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.Try block) [0] [6; 8];
      node 6 Node.Dispatch [5] [2; 9];
      node 7 (Node.Block []) [9] [1];
      node 8 (Node.Block [!!"body"; !!"orelse"; error]) [5] [2];
      node 9 (Node.Block [!!"handler"]) [6] [7];
    ];
  let block = { Try.body = [!!"body"]; handlers = []; orelse = []; finally = [!!"finally"] } in
  assert_cfg
    [+Statement.Try block]
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [7] [3];
      node 2 Node.Error [6] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.Try block) [0] [6; 8];
      node 6 Node.Dispatch [5] [2];
      node 7 (Node.Block [!!"finally"]) [8] [1];
      node 8 (Node.Block [!!"body"]) [5] [7];
    ];
  let block =
    {
      Try.body = [!!"body"];
      handlers = [];
      orelse = [];
      finally = [+Statement.Return { Return.expression = None; is_implicit = false }];
    }
  in
  assert_cfg
    [+Statement.Try block; !!"unreached"]
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [7] [3];
      node 2 Node.Error [6] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.Try block) [0] [6; 8];
      node 6 Node.Dispatch [5] [2];
      node
        7
        (Node.Block [+Statement.Return { Return.expression = None; is_implicit = false }])
        [8]
        [1];
      node 8 (Node.Block [!!"body"]) [5] [7];
    ];
  let error = +Statement.Raise { Raise.expression = None; from = None } in
  let block = { Try.body = [!!"body"]; handlers = []; orelse = []; finally = [error] } in
  assert_cfg
    [+Statement.Try block; !!"unreached"]
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [] [3];
      node 2 Node.Error [6; 7] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.Try block) [0] [6; 8];
      node 6 Node.Dispatch [5] [2];
      node 7 (Node.Block [error]) [8] [2];
      node 8 (Node.Block [!!"body"]) [5] [7];
    ];
  let bool_handler =
    +Expression.BooleanOperator
       { BooleanOperator.left = !"a"; operator = BooleanOperator.Or; right = !"b" }
  in
  let block =
    {
      Try.body = [!!"body"];
      handlers = [handler ~kind:bool_handler "handler"];
      orelse = [];
      finally = [];
    }
  in
  assert_cfg
    [+Statement.Try block]
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [7] [3];
      node 2 Node.Error [6] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.Try block) [0] [6; 8];
      node 6 Node.Dispatch [5] [2; 9];
      node 7 (Node.Block []) [8; 9] [1];
      node 8 (Node.Block [!!"body"]) [5] [7];
      node 9 (Node.Block [+Statement.Expression bool_handler; !!"handler"]) [6] [7];
    ];
  let block =
    {
      Try.body = [+Statement.Return { is_implicit = false; expression = None }];
      handlers = [];
      orelse = [];
      finally = [!!"finally"];
    }
  in
  assert_cfg
    [+Statement.Try block]
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [8] [3];
      node 2 Node.Error [6; 7] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.Try block) [0] [6; 8];
      node 6 Node.Dispatch [5] [2; 7];
      node 7 (Node.Block [!!"finally"]) [6] [2];
      node 8 (Node.Block [+Statement.Return { is_implicit = false; expression = None }]) [5] [1];
    ];
  let block =
    {
      Try.body =
        [
          +Statement.Return
             { is_implicit = false; expression = Some (+Expression.Constant (Constant.Integer 1)) };
        ];
      handlers = [];
      orelse = [];
      finally =
        [
          +Statement.Return
             { is_implicit = false; expression = Some (+Expression.Constant (Constant.Integer 2)) };
        ];
    }
  in
  assert_cfg
    [+Statement.Try block]
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [7; 8] [3];
      node 2 Node.Error [6] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.Try block) [0] [6; 8];
      node 6 Node.Dispatch [5] [2; 7];
      node
        7
        (Node.Block
           [
             +Statement.Return
                {
                  is_implicit = false;
                  expression = Some (+Expression.Constant (Constant.Integer 2));
                };
           ])
        [6]
        [1];
      node
        8
        (Node.Block
           [
             +Statement.Return
                {
                  is_implicit = false;
                  expression = Some (+Expression.Constant (Constant.Integer 1));
                };
           ])
        [5]
        [1];
    ];
  ()


let test_with _ =
  let block =
    {
      With.items = [+Expression.Name (Name.Identifier "item"), None];
      body = [!!"body"];
      async = false;
    }
  in
  assert_cfg
    [+Statement.With block; !!"after"]
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [6] [3];
      node 2 Node.Error [] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.With block) [0] [6];
      node
        6
        (Node.Block
           [
             +Statement.Expression
                (+Expression.Call
                    {
                      Call.callee =
                        +Expression.Name
                           (Name.Attribute
                              { base = !"item"; attribute = "__enter__"; special = true });
                      arguments = [];
                    });
             !!"body";
             !!"after";
           ])
        [5]
        [1];
    ]


let test_while _ =
  let x = +Expression.Name (Name.Identifier "x") in
  let not_x = +Expression.UnaryOperator { operator = UnaryOperator.Not; operand = x } in
  let loop = { While.test = x; body = [!!"body"]; orelse = [!!"orelse"] } in
  assert_cfg
    [+Statement.While loop]
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [6] [3];
      node 2 Node.Error [] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.While loop) [0; 7] [7; 8];
      node 6 Node.Join [8] [1];
      node
        7
        (Node.Block
           [Statement.assume ~origin:(Assert.Origin.While { true_branch = true }) x; !!"body"])
        [5]
        [5];
      node
        8
        (Node.Block
           [
             Statement.assume ~origin:(Assert.Origin.While { true_branch = false }) not_x; !!"orelse";
           ])
        [5]
        [6];
    ];
  let conditional =
    { If.test = +Expression.Constant Constant.True; body = [+Statement.Break]; orelse = [] }
  in
  let loop =
    { While.test = x; body = [+Statement.If conditional; !!"body"]; orelse = [!!"orelse"] }
  in
  assert_cfg
    [+Statement.While loop]
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [6] [3];
      node 2 Node.Error [] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.While loop) [0; 12] [7; 13];
      node 6 Node.Join [10; 13] [1];
      node
        7
        (Node.Block [Statement.assume ~origin:(Assert.Origin.While { true_branch = true }) x])
        [5]
        [8];
      node 8 (Node.If conditional) [7] [10; 11];
      node 9 Node.Join [11] [12];
      node
        10
        (Node.Block
           [
             Statement.assume
               ~origin:(Assert.Origin.If { true_branch = true })
               (+Expression.Constant Constant.True);
             +Statement.Break;
           ])
        [8]
        [6];
      node
        11
        (Node.Block
           [
             Statement.assume
               ~origin:(Assert.Origin.If { true_branch = false })
               (+Expression.Constant Constant.False);
           ])
        [8]
        [9];
      node 12 (Node.Block [!!"body"]) [9] [5];
      node
        13
        (Node.Block
           [
             Statement.assume ~origin:(Assert.Origin.While { true_branch = false }) not_x; !!"orelse";
           ])
        [5]
        [6];
    ];
  let conditional =
    { If.test = +Expression.Constant Constant.True; body = [+Statement.Continue]; orelse = [] }
  in
  let loop =
    { While.test = x; body = [+Statement.If conditional; !!"body"]; orelse = [!!"orelse"] }
  in
  assert_cfg
    [+Statement.While loop]
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [6] [3];
      node 2 Node.Error [] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.While loop) [0; 10; 12] [7; 13];
      node 6 Node.Join [13] [1];
      node
        7
        (Node.Block [Statement.assume ~origin:(Assert.Origin.While { true_branch = true }) x])
        [5]
        [8];
      node 8 (Node.If conditional) [7] [10; 11];
      node 9 Node.Join [11] [12];
      node
        10
        (Node.Block
           [
             Statement.assume
               ~origin:(Assert.Origin.If { true_branch = true })
               (+Expression.Constant Constant.True);
             +Statement.Continue;
           ])
        [8]
        [5];
      node
        11
        (Node.Block
           [
             Statement.assume
               ~origin:(Assert.Origin.If { true_branch = false })
               (+Expression.Constant Constant.False);
           ])
        [8]
        [9];
      node 12 (Node.Block [!!"body"]) [9] [5];
      node
        13
        (Node.Block
           [
             Statement.assume ~origin:(Assert.Origin.While { true_branch = false }) not_x; !!"orelse";
           ])
        [5]
        [6];
    ];

  (* Jumps are reset after the loop. *)
  let inner = { While.test = x; body = [!!"body"]; orelse = [] } in
  let outer =
    { While.test = x; body = [+Statement.While inner; +Statement.Continue]; orelse = [] }
  in
  assert_cfg
    [+Statement.While outer]
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [6] [3];
      node 2 Node.Error [] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.While outer) [0; 12] [7; 13];
      node 6 Node.Join [13] [1];
      node
        7
        (Node.Block [Statement.assume ~origin:(Assert.Origin.While { true_branch = true }) x])
        [5]
        [8];
      node 8 (Node.While inner) [7; 10] [10; 11];
      node 9 Node.Join [11] [12];
      node
        10
        (Node.Block
           [Statement.assume ~origin:(Assert.Origin.While { true_branch = true }) x; !!"body"])
        [8]
        [8];
      node
        11
        (Node.Block [Statement.assume ~origin:(Assert.Origin.While { true_branch = false }) not_x])
        [8]
        [9];
      node 12 (Node.Block [+Statement.Continue]) [9] [5];
      node
        13
        (Node.Block [Statement.assume ~origin:(Assert.Origin.While { true_branch = false }) not_x])
        [5]
        [6];
    ]


let test_while_true _ =
  let loop =
    { While.test = +Expression.Constant Constant.True; body = [!!"body"]; orelse = [!!"orelse"] }
  in
  assert_cfg
    [+Statement.While loop; !!"rest"]
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [5] [3];
      node 2 Node.Error [] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.While loop) [0; 7] [1; 7; 8];
      node 6 Node.Join [] [];
      node
        7
        (Node.Block
           [
             Statement.assume
               ~origin:(Assert.Origin.While { true_branch = true })
               (+Expression.Constant Constant.True);
             !!"body";
           ])
        [5]
        [5];
      node
        8
        (Node.Block
           [
             Statement.assume
               ~origin:(Assert.Origin.While { true_branch = false })
               (+Expression.Constant Constant.False);
           ])
        [5]
        [];
    ];
  let loop =
    {
      While.test = +Expression.Constant Constant.True;
      body = [+Statement.Break];
      orelse = [!!"orelse"];
    }
  in
  assert_cfg
    [+Statement.While loop; !!"rest"]
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [9] [3];
      node 2 Node.Error [] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.While loop) [0] [7; 8];
      node 6 Node.Join [7] [9];
      node
        7
        (Node.Block
           [
             Statement.assume
               ~origin:(Assert.Origin.While { true_branch = true })
               (+Expression.Constant Constant.True);
             +Statement.Break;
           ])
        [5]
        [6];
      node
        8
        (Node.Block
           [
             Statement.assume
               ~origin:(Assert.Origin.While { true_branch = false })
               (+Expression.Constant Constant.False);
           ])
        [5]
        [];
      node 9 (Node.Block [!!"rest"]) [6] [1];
    ]


let test_while_false _ =
  let loop =
    { While.test = +Expression.Constant Constant.False; body = [!!"body"]; orelse = [!!"orelse"] }
  in
  assert_cfg
    [+Statement.While loop]
    [
      node 0 Node.Entry [] [5];
      node 1 Node.Normal [6] [3];
      node 2 Node.Error [] [3];
      node 3 Node.Final [1; 2] [];
      node 4 Node.Yield [] [];
      node 5 (Node.While loop) [0] [7; 8];
      node 6 Node.Join [8] [1];
      node
        7
        (Node.Block
           [
             Statement.assume
               ~origin:(Assert.Origin.While { true_branch = true })
               (+Expression.Constant Constant.False);
           ])
        [5]
        [];
      node
        8
        (Node.Block
           [
             Statement.assume
               ~origin:(Assert.Origin.While { true_branch = false })
               (+Expression.Constant Constant.True);
             !!"orelse";
           ])
        [5]
        [6];
    ]


let () =
  "cfg"
  >::: [
         "to_dot" >:: test_to_dot;
         "block" >:: test_block;
         "for" >:: test_for;
         "if" >:: test_if;
         "match" >:: test_match;
         "raise" >:: test_raise;
         "assert_false" >:: test_assert_false;
         "return" >:: test_return;
         "try" >:: test_try;
         "with" >:: test_with;
         "while" >:: test_while;
         "while_true" >:: test_while_true;
         "while_false" >:: test_while_false;
       ]
  |> Test.run
