(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2

let assert_enabled section = assert_bool "Section should be enabled" (Log.is_enabled section)

let assert_disabled section =
  assert_bool "Section should be disabled" (not (Log.is_enabled section))


let test_initialize_default_off _ =
  assert_disabled `Fixpoint;
  Log.GlobalState.initialize ~debug:false ~sections:["fixpoint"];
  assert_enabled `Fixpoint;
  Log.GlobalState.initialize ~debug:false ~sections:["-fixpoint"];
  assert_disabled `Fixpoint


let test_initialize_default_on _ =
  assert_enabled `Warning;
  Log.GlobalState.initialize ~debug:false ~sections:["-warning"];
  assert_disabled `Warning;
  Log.GlobalState.initialize ~debug:false ~sections:["warning"];
  assert_enabled `Warning


let () =
  "log"
  >::: [
         "initialize_default_off" >:: test_initialize_default_off;
         "initialize_default_on" >:: test_initialize_default_on;
       ]
  |> run_test_tt_main
