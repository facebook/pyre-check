(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base
open OUnit2
open Newserver

let assert_lookup ~context ~lookup expected =
  assert_equal
    ~ctxt:context
    ~cmp:[%compare.equal: int list]
    ~printer:(fun elements -> [%sexp_of: int list] elements |> Sexp.to_string_hum)
    (expected |> List.sort ~compare:Int.compare)
    (lookup () |> List.sort ~compare:Int.compare)


module IntToIntBidirectionalMapping = BidirectionalMapping.Make (Base.Int) (Base.Int)

let test_lookup context =
  let assert_lookup_key ~mapping ~expected key =
    assert_lookup
      ~context
      ~lookup:(fun _ -> IntToIntBidirectionalMapping.Unindexed.lookup_key mapping key)
      expected
  in
  let assert_lookup_value ~mapping ~expected value =
    assert_lookup
      ~context
      ~lookup:(fun _ -> IntToIntBidirectionalMapping.Unindexed.lookup_value mapping value)
      expected
  in

  let mapping =
    IntToIntBidirectionalMapping.Unindexed.of_list [1, 2; 1, 3; 2, 3; 2, 4; 3, 2; 3, 5; 3, 6; 4, 3]
  in
  assert_equal
    ~ctxt:context
    ~cmp:Int.equal
    ~printer:Int.to_string
    8
    (IntToIntBidirectionalMapping.Unindexed.length mapping);
  assert_lookup_key ~mapping 1 ~expected:[2; 3];
  assert_lookup_key ~mapping 2 ~expected:[3; 4];
  assert_lookup_key ~mapping 3 ~expected:[2; 5; 6];
  assert_lookup_key ~mapping 4 ~expected:[3];
  assert_lookup_key ~mapping 5 ~expected:[];

  assert_lookup_value ~mapping 1 ~expected:[];
  assert_lookup_value ~mapping 2 ~expected:[1; 3];
  assert_lookup_value ~mapping 3 ~expected:[1; 2; 4];
  assert_lookup_value ~mapping 4 ~expected:[2];
  assert_lookup_value ~mapping 5 ~expected:[3];
  assert_lookup_value ~mapping 6 ~expected:[3];

  ()


let test_difference context =
  let assert_set_equal ~expected ~actual () =
    assert_equal
      ~ctxt:context
      ~cmp:[%compare.equal: (int * int) list]
      ~printer:(fun elements -> [%sexp_of: (int * int) list] elements |> Sexp.to_string_hum)
      (expected |> List.sort ~compare:IntToIntBidirectionalMapping.Item.compare)
      (actual |> List.sort ~compare:IntToIntBidirectionalMapping.Item.compare)
  in
  let assert_difference ~original ~current ~expected_added ~expected_removed () =
    let { IntToIntBidirectionalMapping.Difference.added = actual_added; removed = actual_removed } =
      IntToIntBidirectionalMapping.Unindexed.difference
        ~original:(IntToIntBidirectionalMapping.Unindexed.of_list original)
        (IntToIntBidirectionalMapping.Unindexed.of_list current)
    in
    assert_set_equal
      ~expected:expected_added
      ~actual:(IntToIntBidirectionalMapping.ItemSet.to_list actual_added)
      ();
    assert_set_equal
      ~expected:expected_removed
      ~actual:(IntToIntBidirectionalMapping.ItemSet.to_list actual_removed)
      ()
  in
  assert_difference
    ~original:[1, 2; 1, 3; 2, 3]
    ~current:[1, 2; 1, 3; 2, 3]
    ~expected_added:[]
    ~expected_removed:[]
    ();
  assert_difference
    ~original:[1, 2; 1, 3; 2, 3; 1, 4]
    ~current:[1, 2; 1, 3; 2, 3]
    ~expected_added:[]
    ~expected_removed:[1, 4]
    ();
  assert_difference
    ~original:[1, 2; 1, 3; 2, 3]
    ~current:[1, 2; 1, 3; 2, 3; 1, 4]
    ~expected_added:[1, 4]
    ~expected_removed:[]
    ();
  assert_difference
    ~original:[1, 2; 1, 3; 2, 3; 2, 4]
    ~current:[1, 2; 1, 3; 2, 3; 1, 4]
    ~expected_added:[1, 4]
    ~expected_removed:[2, 4]
    ();
  assert_difference
    ~original:[1, 2; 1, 3; 2, 3; 2, 4; 3, 5]
    ~current:[1, 2; 1, 3; 2, 3; 1, 4; 4, 6]
    ~expected_added:[1, 4; 4, 6]
    ~expected_removed:[2, 4; 3, 5]
    ();
  ()


let () =
  "bidirectional_mapping"
  >::: ["lookup" >:: test_lookup; "difference" >:: test_difference]
  |> Test.run
