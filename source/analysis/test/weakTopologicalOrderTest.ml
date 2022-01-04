(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Analysis

let assert_wto ?(entry_index = 0) cfg_nodes expected_wto =
  let cfg = Int.Table.create () in
  let insert_node node = Hashtbl.set cfg ~key:(Cfg.Node.id node) ~data:node in
  List.iter ~f:insert_node cfg_nodes;

  let actual_wto = WeakTopologicalOrder.create ~cfg ~entry_index ~successors:Cfg.Node.successors in

  let rec compare_component left right =
    let open WeakTopologicalOrder in
    match left.Component.kind, right.Component.kind with
    | Component.Node left, Component.Node right -> Int.equal (Cfg.Node.id left) (Cfg.Node.id right)
    | ( Component.Cycle { head = left_head; components = left_components },
        Component.Cycle { head = right_head; components = right_components } ) ->
        Int.equal (Cfg.Node.id left_head) (Cfg.Node.id right_head)
        && compare_components left_components right_components
    | _ -> false
  and compare_components left right = List.equal compare_component left right in

  let pp formatter weak_topological_order =
    String.pp
      formatter
      (weak_topological_order |> WeakTopologicalOrder.sexp_of_t |> Sexp.to_string_hum)
  in
  assert_equal
    ~cmp:compare_components
    ~printer:(Format.asprintf "%a" pp)
    ~pp_diff:(Test.diff ~print:pp)
    expected_wto
    actual_wto


let cfg_node ?(predecessors = []) ?(successors = []) id =
  Cfg.Node.create id Cfg.Node.Normal (Int.Set.of_list predecessors) (Int.Set.of_list successors)


let wto_node id = { WeakTopologicalOrder.Component.id = 0; kind = Node (cfg_node id) }

let wto_cycle ~head ~components =
  { WeakTopologicalOrder.Component.id = 0; kind = Cycle { head = cfg_node head; components } }


let test_empty _ = assert_wto [cfg_node 0 ~predecessors:[] ~successors:[]] [wto_node 0]

let test_sequential _ =
  assert_wto
    [
      cfg_node 0 ~predecessors:[] ~successors:[1];
      cfg_node 1 ~predecessors:[0] ~successors:[2];
      cfg_node 2 ~predecessors:[1] ~successors:[3];
      cfg_node 3 ~predecessors:[2] ~successors:[];
    ]
    [wto_node 0; wto_node 1; wto_node 2; wto_node 3]


let test_branch _ =
  assert_wto
    [
      cfg_node 0 ~predecessors:[] ~successors:[1];
      cfg_node 1 ~predecessors:[0] ~successors:[2; 3];
      cfg_node 2 ~predecessors:[1] ~successors:[4];
      cfg_node 3 ~predecessors:[1] ~successors:[4];
      cfg_node 4 ~predecessors:[2; 3] ~successors:[];
    ]
    [wto_node 0; wto_node 1; wto_node 3; wto_node 2; wto_node 4]


let test_nested_branch _ =
  assert_wto
    [
      cfg_node 0 ~predecessors:[] ~successors:[1];
      cfg_node 1 ~predecessors:[0] ~successors:[2; 3];
      cfg_node 2 ~predecessors:[1] ~successors:[4; 5];
      cfg_node 3 ~predecessors:[1] ~successors:[7];
      cfg_node 4 ~predecessors:[2] ~successors:[6];
      cfg_node 5 ~predecessors:[2] ~successors:[6];
      cfg_node 6 ~predecessors:[4; 5] ~successors:[7];
      cfg_node 7 ~predecessors:[3; 6] ~successors:[];
    ]
    [wto_node 0; wto_node 1; wto_node 3; wto_node 2; wto_node 5; wto_node 4; wto_node 6; wto_node 7];
  assert_wto
    [
      cfg_node 0 ~predecessors:[] ~successors:[1];
      cfg_node 1 ~predecessors:[0] ~successors:[2; 3];
      cfg_node 2 ~predecessors:[1] ~successors:[4; 5];
      cfg_node 3 ~predecessors:[1] ~successors:[6; 7];
      cfg_node 4 ~predecessors:[2] ~successors:[8];
      cfg_node 5 ~predecessors:[2] ~successors:[8];
      cfg_node 6 ~predecessors:[3] ~successors:[9];
      cfg_node 7 ~predecessors:[3] ~successors:[9];
      cfg_node 8 ~predecessors:[4; 5] ~successors:[10];
      cfg_node 9 ~predecessors:[6; 7] ~successors:[10];
      cfg_node 10 ~predecessors:[8; 9] ~successors:[];
    ]
    [
      wto_node 0;
      wto_node 1;
      wto_node 3;
      wto_node 7;
      wto_node 6;
      wto_node 9;
      wto_node 2;
      wto_node 5;
      wto_node 4;
      wto_node 8;
      wto_node 10;
    ];
  (* Same graph but nodes are indexed differently. *)
  (* 1 <-> 10 *)
  (* 2 <-> 9 *)
  (* 4 <-> 7 *)
  assert_wto
    [
      cfg_node 0 ~predecessors:[] ~successors:[10];
      cfg_node 10 ~predecessors:[0] ~successors:[9; 3];
      cfg_node 9 ~predecessors:[10] ~successors:[7; 5];
      cfg_node 3 ~predecessors:[10] ~successors:[6; 4];
      cfg_node 7 ~predecessors:[9] ~successors:[8];
      cfg_node 5 ~predecessors:[9] ~successors:[8];
      cfg_node 6 ~predecessors:[3] ~successors:[2];
      cfg_node 4 ~predecessors:[3] ~successors:[2];
      cfg_node 8 ~predecessors:[7; 5] ~successors:[1];
      cfg_node 2 ~predecessors:[6; 4] ~successors:[1];
      cfg_node 1 ~predecessors:[8; 2] ~successors:[];
    ]
    [
      wto_node 0;
      wto_node 10;
      wto_node 9;
      wto_node 7;
      wto_node 5;
      wto_node 8;
      wto_node 3;
      wto_node 6;
      wto_node 4;
      wto_node 2;
      wto_node 1;
    ]


let test_unreachable _ =
  assert_wto
    [
      cfg_node 0 ~predecessors:[] ~successors:[2];
      cfg_node 1 ~predecessors:[3] ~successors:[2];
      cfg_node 2 ~predecessors:[0; 1] ~successors:[];
      cfg_node 3 ~predecessors:[] ~successors:[1];
    ]
    [wto_node 0; wto_node 2]


let test_loop _ =
  assert_wto
    [
      cfg_node 0 ~predecessors:[] ~successors:[1];
      cfg_node 1 ~predecessors:[0; 2] ~successors:[2; 3];
      cfg_node 2 ~predecessors:[1] ~successors:[1];
      cfg_node 3 ~predecessors:[1] ~successors:[];
    ]
    [wto_node 0; wto_cycle ~head:1 ~components:[wto_node 2]; wto_node 3];
  assert_wto
    [
      cfg_node 0 ~predecessors:[] ~successors:[1];
      cfg_node 1 ~predecessors:[0; 3] ~successors:[2; 4];
      cfg_node 2 ~predecessors:[1] ~successors:[3];
      cfg_node 3 ~predecessors:[2] ~successors:[1];
      cfg_node 4 ~predecessors:[1] ~successors:[];
    ]
    [wto_node 0; wto_cycle ~head:1 ~components:[wto_node 2; wto_node 3]; wto_node 4]


let test_loop_with_branch _ =
  assert_wto
    [
      cfg_node 0 ~predecessors:[] ~successors:[1];
      cfg_node 1 ~predecessors:[0; 5] ~successors:[2; 6];
      cfg_node 2 ~predecessors:[1] ~successors:[3; 4];
      cfg_node 3 ~predecessors:[2] ~successors:[5];
      cfg_node 4 ~predecessors:[2] ~successors:[5];
      cfg_node 5 ~predecessors:[3; 4] ~successors:[1];
      cfg_node 6 ~predecessors:[1] ~successors:[];
    ]
    [
      wto_node 0;
      wto_cycle ~head:1 ~components:[wto_node 2; wto_node 4; wto_node 3; wto_node 5];
      wto_node 6;
    ];
  (* Same graph but nodes are indexed differently. *)
  (* 2 <-> 4 *)
  (* 6 <-> 3 *)
  assert_wto
    [
      cfg_node 0 ~predecessors:[] ~successors:[1];
      cfg_node 1 ~predecessors:[0; 5] ~successors:[4; 3];
      cfg_node 4 ~predecessors:[1] ~successors:[6; 2];
      cfg_node 6 ~predecessors:[4] ~successors:[5];
      cfg_node 2 ~predecessors:[4] ~successors:[5];
      cfg_node 5 ~predecessors:[6; 2] ~successors:[1];
      cfg_node 3 ~predecessors:[1] ~successors:[];
    ]
    [
      wto_node 0;
      wto_cycle ~head:1 ~components:[wto_node 4; wto_node 6; wto_node 2; wto_node 5];
      wto_node 3;
    ];
  assert_wto
    [
      cfg_node 0 ~predecessors:[] ~successors:[1];
      cfg_node 1 ~predecessors:[0] ~successors:[2; 3];
      cfg_node 2 ~predecessors:[1; 4] ~successors:[4; 5];
      cfg_node 3 ~predecessors:[1] ~successors:[5];
      cfg_node 4 ~predecessors:[2] ~successors:[2];
      cfg_node 5 ~predecessors:[2; 3] ~successors:[];
    ]
    [wto_node 0; wto_node 1; wto_node 3; wto_cycle ~head:2 ~components:[wto_node 4]; wto_node 5]


let test_nested_loop _ =
  assert_wto
    [
      cfg_node 0 ~predecessors:[] ~successors:[1];
      cfg_node 1 ~predecessors:[0; 4] ~successors:[2; 3];
      cfg_node 2 ~predecessors:[1] ~successors:[4; 5];
      cfg_node 3 ~predecessors:[1] ~successors:[];
      cfg_node 4 ~predecessors:[2] ~successors:[1];
      cfg_node 5 ~predecessors:[2] ~successors:[2];
    ]
    [
      wto_node 0;
      wto_cycle ~head:1 ~components:[wto_cycle ~head:2 ~components:[wto_node 5]; wto_node 4];
      wto_node 3;
    ];
  assert_wto
    [
      cfg_node 0 ~predecessors:[3] ~successors:[1; 6];
      cfg_node 1 ~predecessors:[0; 4] ~successors:[2; 3];
      cfg_node 2 ~predecessors:[1] ~successors:[4; 5];
      cfg_node 3 ~predecessors:[1] ~successors:[0];
      cfg_node 4 ~predecessors:[2] ~successors:[1];
      cfg_node 5 ~predecessors:[2] ~successors:[2];
      cfg_node 6 ~predecessors:[0] ~successors:[];
    ]
    [
      wto_cycle
        ~head:0
        ~components:
          [
            wto_cycle ~head:1 ~components:[wto_cycle ~head:2 ~components:[wto_node 5]; wto_node 4];
            wto_node 3;
          ];
      wto_node 6;
    ];
  assert_wto
    [
      cfg_node 0 ~predecessors:[7] ~successors:[1; 2; 8];
      cfg_node 1 ~predecessors:[0; 3] ~successors:[3; 4];
      cfg_node 2 ~predecessors:[0; 6] ~successors:[5; 6];
      cfg_node 3 ~predecessors:[1] ~successors:[1];
      cfg_node 4 ~predecessors:[1] ~successors:[7];
      cfg_node 5 ~predecessors:[2] ~successors:[7];
      cfg_node 6 ~predecessors:[2] ~successors:[2];
      cfg_node 7 ~predecessors:[4; 5] ~successors:[0];
      cfg_node 8 ~predecessors:[0] ~successors:[];
    ]
    [
      wto_cycle
        ~head:0
        ~components:
          [
            wto_cycle ~head:2 ~components:[wto_node 6];
            wto_node 5;
            wto_cycle ~head:1 ~components:[wto_node 3];
            wto_node 4;
            wto_node 7;
          ];
      wto_node 8;
    ]


let test_bourdon _ =
  (*
   * This graph and the corresponding weak topological ordering are described
   * on page 4 of Bourdoncle's paper:
   *   F. Bourdoncle. Efficient chaotic iteration strategies with widenings.
   *   In Formal Methods in Programming and Their Applications, pp 128-141.
   * The graph is given as follows:
   *
   *                 +-----------------------+
   *                 |           +-----+     |
   *                 |           |     |     |
   *                 V           V     |     |
   *     1 --> 2 --> 3 --> 4 --> 5 --> 6 --> 7 --> 8
   *           |           |                 ^     ^
   *           |           |                 |     |
   *           |           +-----------------+     |
   *           +-----------------------------------+
   *
   * Bourdoncle's algorithm computes the following weak topological ordering:
   *
   *     1 2 (3 4 (5 6) 7) 8
   *)
  assert_wto
    ~entry_index:1
    [
      cfg_node 1 ~predecessors:[] ~successors:[2];
      cfg_node 2 ~predecessors:[1] ~successors:[3; 8];
      cfg_node 3 ~predecessors:[2; 7] ~successors:[4];
      cfg_node 4 ~predecessors:[3] ~successors:[5; 7];
      cfg_node 5 ~predecessors:[4; 6] ~successors:[6];
      cfg_node 6 ~predecessors:[5] ~successors:[5; 7];
      cfg_node 7 ~predecessors:[6; 4] ~successors:[3; 8];
      cfg_node 8 ~predecessors:[2; 7] ~successors:[];
    ]
    [
      wto_node 1;
      wto_node 2;
      wto_cycle
        ~head:3
        ~components:[wto_node 4; wto_cycle ~head:5 ~components:[wto_node 6]; wto_node 7];
      wto_node 8;
    ]


let () =
  "weakTopologicalOrder"
  >::: [
         "empty" >:: test_empty;
         "sequential" >:: test_sequential;
         "branch" >:: test_branch;
         "nested_branch" >:: test_nested_branch;
         "unreachable" >:: test_unreachable;
         "loop" >:: test_loop;
         "loop_with_branch" >:: test_loop_with_branch;
         "nested_loop" >:: test_nested_loop;
         "bourdon" >:: test_bourdon;
       ]
  |> Test.run
