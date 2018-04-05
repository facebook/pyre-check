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
  include Transform.Identity
  type t = int

  let final count =
    count

  let expression_postorder count = function
    | { Node.location; value = Integer number } ->
        count + number,
        { Node.location; value = Integer (number + 1) }
    | expression ->
        count, expression
end


module ShallowModifyingTransformer : sig
  type t = int
  include Transform.Transformer with type t := t
  val final: t -> int
end = struct
  include Transform.Identity
  include ModifyingTransformer

  let statement_keep_recursing _state _statement =
    Transform.Stop
end


module ModifyingTransform = Transform.Make(ModifyingTransformer)

module ShallowModifyingTransform = Transform.Make(ShallowModifyingTransformer)

let assert_modifying_source ?(shallow=false) statements expected_statements expected_sum =
  let state, modified =
    if shallow then
      ShallowModifyingTransform.transform 0 (Source.create statements)
    else
      ModifyingTransform.transform 0 (Source.create statements)
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
  include Transform.Identity
  type t = unit

  let statement_postorder state statement =
    state, [statement; statement]
end

module ShallowExpandingTransformer : sig
  type t = unit
  include Transform.Transformer with type t := t
end = struct
  include Transform.Identity
  include ExpandingTransformer
  let statement_keep_recursing _state _statement =
    Transform.Stop
end

module ExpandingTransform = Transform.Make(ExpandingTransformer)

module ShallowExpandingTransform = Transform.Make(ShallowExpandingTransformer)

let assert_expanded_source ?(shallow=false) statements expected_statements =
  let _, modified =
    if shallow then
      ShallowExpandingTransform.transform () (Source.create statements)
    else
      ExpandingTransform.transform () (Source.create statements)
  in
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

let test_expansion_with_stop _ =
  let module StoppingExpandingTransformer : sig
    type t = unit
    include Transform.Transformer with type t := t
  end = struct
    include ExpandingTransformer

    let statement_keep_recursing _ _ =
      Transform.Stop
  end
  in

  let module StoppingExpandingTransform = Transform.Make(StoppingExpandingTransformer) in

  let assert_expanded_source_with_stop source expected_source =
    let _, modified =
      StoppingExpandingTransform.transform () (parse source) in
    assert_source_equal
      modified
      (parse expected_source)
  in

  assert_expanded_source_with_stop
    {|
       if (1):
         if (2):
           3
         else:
           4
       else:
         if (5):
           6
         else:
           7
    |}
    {|
       if (1):
         if (2):
           3
         else:
           4
       else:
         if (5):
           6
         else:
           7
       if (1):
         if (2):
           3
         else:
           4
       else:
         if (5):
           6
         else:
           7
    |}


let test_double_count _ =
  let module DoubleCounterTransformer : sig
    type t = int
    include Transform.Transformer with type t := t
    val final: t -> int
  end = struct
    include Transform.Identity
    type t = int

    let final count =
      count

    let statement_preorder count statement =
      count + 1, statement

    let statement_postorder count statement =
      count + 1, [statement]
  end
  in

  let module ShallowDoubleCounterTransformer : sig
    type t = int
    include Transform.Transformer with type t := t
    val final: t -> int
  end = struct
    include Transform.Identity
    include DoubleCounterTransformer

    let statement_keep_recursing _state _statement =
      Transform.Stop
  end
  in

  let module DoubleCounterTransform = Transform.Make(DoubleCounterTransformer) in

  let module ShallowDoubleCounterTransform = Transform.Make(ShallowDoubleCounterTransformer) in

  let assert_double_count ?(shallow=false) source expected_sum =
    let state, modified =
      if shallow then
        ShallowDoubleCounterTransform.transform 0 (parse source)
      else
        DoubleCounterTransform.transform 0 (parse source)
    in
    (* expect no change in the source *)
    assert_source_equal
      modified
      (parse source);
    assert_equal
      (ModifyingTransformer.final state)
      expected_sum
      ~printer:string_of_int
  in

  assert_double_count
    {|
      1.0
      2.0
    |}
    4;
  assert_double_count
    ~shallow:true
    {|
      1.0
      2.0
    |}
    4;
  assert_double_count
    {|
      if (1):
        3
      else:
        5
    |}
    6;
  assert_double_count
    ~shallow:true
    {|
      if (1):
        3
      else:
        5
    |}
    2;
  assert_double_count
    {|
      if (1):
        if (2):
          3
        else:
          4
      else:
        if (5):
          6
        else:
          7
    |}
    14;
  assert_double_count
    ~shallow:true
    {|
      if (1):
        if (2):
          3
        else:
          4
      else:
        if (5):
          6
        else:
          7
    |}
    2


let () =
  "transform">:::[
    "transform">::test_transform;
    "expansion">::test_expansion;
    "expansion_with_stop">::test_expansion_with_stop;
    "statement_double_counter">::test_double_count;
  ]
  |> run_test_tt_main
