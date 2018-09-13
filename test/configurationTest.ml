(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


open OUnit2
open Test

open Pyre


let test_equal _ =
  assert_true
    (Configuration.equal
       (Configuration.create ~start_time:1.0 ())
       (Configuration.create ~start_time:2.0 ()));

  assert_false
    (Configuration.equal
       (Configuration.create ~infer:true ())
       (Configuration.create ~infer:false ()));

  assert_false
    (Configuration.equal
       (Configuration.create ~recursive_infer:true ())
       (Configuration.create ~recursive_infer:false ()));

  assert_true
    (Configuration.equal
       (Configuration.create ~parallel:true ())
       (Configuration.create ~parallel:false ()));

  let root = Path.current_working_directory () in
  assert_false
    (Configuration.equal
       (Configuration.create ~filter_directories:[] ())
       (Configuration.create ~filter_directories:[root] ()));

  assert_true
    (Configuration.equal
       (Configuration.create ~number_of_workers:42 ())
       (Configuration.create ~number_of_workers:84 ()));

  assert_false
    (Configuration.equal
       (Configuration.create ~search_path:[] ())
       (Configuration.create ~search_path:[root] ()));

  assert_false
    (Configuration.equal
       (Configuration.create ~typeshed:(Path.create_relative ~root ~relative:"a") ())
       (Configuration.create ~typeshed:(Path.create_relative ~root ~relative:"b") ()));

  assert_true
    (Configuration.equal
       (Configuration.create ~verbose:true ())
       (Configuration.create ~verbose:false ()));

 assert_false
     (Configuration.equal
       (Configuration.create ~expected_version:"a" ())
       (Configuration.create ~expected_version:"b" ()));

  assert_false
    (Configuration.equal
       (Configuration.create ~strict:true ())
       (Configuration.create ~strict:false ()));

  assert_false
    (Configuration.equal
       (Configuration.create ~declare:true ())
       (Configuration.create ~declare:false ()));

  assert_false
    (Configuration.equal
       (Configuration.create ~debug:true ())
       (Configuration.create ~debug:false ()))


let () =
  "configuration">:::[
    "equal">::test_equal;
  ]
  |> Test.run_tests
