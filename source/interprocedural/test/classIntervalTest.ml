(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Interprocedural

let test_meet _ =
  let assert_meet ~left ~right ~result = assert_equal result (ClassInterval.meet left right) in
  assert_meet
    ~left:ClassInterval.empty
    ~right:(ClassInterval.create 3 6)
    ~result:ClassInterval.empty;
  assert_meet
    ~left:(ClassInterval.create 3 6)
    ~right:ClassInterval.empty
    ~result:ClassInterval.empty;
  assert_meet
    ~left:(ClassInterval.create 3 6)
    ~right:(ClassInterval.create 2 2)
    ~result:ClassInterval.empty;
  assert_meet
    ~left:(ClassInterval.create 3 6)
    ~right:(ClassInterval.create 2 4)
    ~result:(ClassInterval.create 3 4);
  assert_meet
    ~left:(ClassInterval.create 3 6)
    ~right:(ClassInterval.create 2 7)
    ~result:(ClassInterval.create 3 6);
  assert_meet
    ~left:(ClassInterval.create 3 6)
    ~right:(ClassInterval.create 4 5)
    ~result:(ClassInterval.create 4 5);
  assert_meet
    ~left:(ClassInterval.create 3 6)
    ~right:(ClassInterval.create 4 7)
    ~result:(ClassInterval.create 4 6);
  assert_meet
    ~left:(ClassInterval.create 3 6)
    ~right:(ClassInterval.create 7 8)
    ~result:ClassInterval.empty


let test_join _ =
  let assert_join ~left ~right ~result = assert_equal result (ClassInterval.join left right) in
  assert_join
    ~left:ClassInterval.empty
    ~right:(ClassInterval.create 3 6)
    ~result:(ClassInterval.create 3 6);
  assert_join
    ~left:(ClassInterval.create 1 2)
    ~right:(ClassInterval.create 3 4)
    ~result:(ClassInterval.create 1 4);
  assert_join
    ~left:(ClassInterval.create 1 2)
    ~right:(ClassInterval.create 2 3)
    ~result:(ClassInterval.create 1 3)


let () = "class_interval" >::: ["meet" >:: test_meet; "join" >:: test_join] |> Test.run
