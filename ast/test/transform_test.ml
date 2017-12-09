(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast
open Expression
open Statement

open Test


module ModifyingTransformer : sig
  type t = int
  include Transform.Transformer with type t := t
  val final: t -> int
end = struct
  type t = int

  let final count =
    count

  let expression count = function
    | { Node.location; value = Integer number } ->
        count + number,
        { Node.location; value = Integer (number + 1) }
    | expression ->
        count, expression

  let statement count statement =
    count, [statement]
end

module ModifyingTransform = Transform.Make(ModifyingTransformer)

let assert_modifying_source ?(shallow=false) statements expected_statements expected_sum =
  let state, modified =
    ModifyingTransform.transform ~shallow 0 (Source.create statements)
  in
  assert_source_equal
    modified
    (Source.create expected_statements);
  assert_equal
    (ModifyingTransformer.final state)
    expected_sum
    ~printer:string_of_int

let test_transform _ =
  assert_modifying_source
    [
      +Expression (+Integer 1);
      +Expression (+Integer 2);
    ]
    [
      +Expression (+Integer 2);
      +Expression (+Integer 3);
    ]
    3;
  assert_modifying_source
    [
      +If {
        If.test = +Integer 1;
        body = [
          +If {
            If.test = +Integer 2;
            body = [+Expression (+Integer 3)];
            orelse = [+Expression (+Integer 4)];
          };
        ];
        orelse = [+Expression (+Integer 5)];
      };
    ]
    [
      +If {
        If.test = +Integer 2;
        body = [
          +If {
            If.test = +Integer 3;
            body = [+Expression (+Integer 4)];
            orelse = [+Expression (+Integer 5)];
          };
        ];
        orelse = [+Expression (+Integer 6)];
      };
    ]
    15;
  assert_modifying_source
    ~shallow:true
    [
      +If {
        If.test = +Integer 1;
        body = [
          +If {
            If.test = +Integer 2;
            body = [+Expression (+Integer 3)];
            orelse = [+Expression (+Integer 4)];
          };
        ];
        orelse = [+Expression (+Integer 5)];
      };
    ]
    [
      +If {
        If.test = +Integer 1;
        body = [
          +If {
            If.test = +Integer 2;
            body = [+Expression (+Integer 3)];
            orelse = [+Expression (+Integer 4)];
          };
        ];
        orelse = [+Expression (+Integer 5)];
      };
    ]
    0

module ExpandingTransformer : sig
  type t = unit
  include Transform.Transformer with type t := t
end = struct
  type t = unit

  let expression state expression =
    state, expression

  let statement state statement =
    state, [statement; statement]
end

module ExpandingTransform = Transform.Make(ExpandingTransformer)

let assert_expanded_source ?(shallow=false) statements expected_statements =
  let _, modified =
    ExpandingTransform.transform ~shallow () (Source.create statements) in
  assert_source_equal
    modified
    (Source.create expected_statements)

let test_expansion _ =
  assert_expanded_source
    [
      +Expression (+Float 1.0);
      +Expression (+Float 2.0);
    ]
    [
      +Expression (+Float 1.0);
      +Expression (+Float 1.0);
      +Expression (+Float 2.0);
      +Expression (+Float 2.0);
    ];
  assert_expanded_source
    ~shallow:true
    [
      +Expression (+Float 1.0);
      +Expression (+Float 2.0);
    ]
    [
      +Expression (+Float 1.0);
      +Expression (+Float 1.0);
      +Expression (+Float 2.0);
      +Expression (+Float 2.0);
    ];
  assert_expanded_source
    [
      +If {
        If.test = +Integer 1;
        body = [+Expression (+Integer 3)];
        orelse = [+Expression (+Integer 5)];
      };
    ]
    [
      +If {
        If.test = +Integer 1;
        body = [
          +Expression (+Integer 3);
          +Expression (+Integer 3);
        ];
        orelse = [
          +Expression (+Integer 5);
          +Expression (+Integer 5);
        ];
      };
      +If {
        If.test = +Integer 1;
        body = [
          +Expression (+Integer 3);
          +Expression (+Integer 3);
        ];
        orelse = [
          +Expression (+Integer 5);
          +Expression (+Integer 5);
        ];
      };
    ];
  assert_expanded_source
    ~shallow:true
    [
      +If {
        If.test = +Integer 1;
        body = [+Expression (+Integer 3)];
        orelse = [+Expression (+Integer 5)];
      };
    ]
    [
      +If {
        If.test = +Integer 1;
        body = [+Expression (+Integer 3)];
        orelse = [+Expression (+Integer 5)];
      };
      +If {
        If.test = +Integer 1;
        body = [+Expression (+Integer 3)];
        orelse = [+Expression (+Integer 5)];
      };
    ]


let () =
  "transform">:::[
    "transform">::test_transform;
    "expansion">::test_expansion;
  ]
  |> run_test_tt_main
