(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


open OUnit2
open Test

open Pyre


let test_equal _ =
  assert_true
    (Configuration.Analysis.equal
       (Configuration.Analysis.create ~start_time:1.0 ())
       (Configuration.Analysis.create ~start_time:2.0 ()));

  assert_false
    (Configuration.Analysis.equal
       (Configuration.Analysis.create ~infer:true ())
       (Configuration.Analysis.create ~infer:false ()));

  assert_false
    (Configuration.Analysis.equal
       (Configuration.Analysis.create ~recursive_infer:true ())
       (Configuration.Analysis.create ~recursive_infer:false ()));

  assert_true
    (Configuration.Analysis.equal
       (Configuration.Analysis.create ~parallel:true ())
       (Configuration.Analysis.create ~parallel:false ()));

  let root = Path.current_working_directory () in
  assert_true
    (Configuration.Analysis.equal
       (Configuration.Analysis.create ~filter_directories:[] ())
       (Configuration.Analysis.create ~filter_directories:[root] ()));

  assert_true
    (Configuration.Analysis.equal
       (Configuration.Analysis.create ~number_of_workers:42 ())
       (Configuration.Analysis.create ~number_of_workers:84 ()));

  assert_true
    (Configuration.Analysis.equal
       (Configuration.Analysis.create ~search_path:[] ())
       (Configuration.Analysis.create ~search_path:[root] ()));

  assert_true
    (Configuration.Analysis.equal
       (Configuration.Analysis.create ~typeshed:(Path.create_relative ~root ~relative:"a") ())
       (Configuration.Analysis.create ~typeshed:(Path.create_relative ~root ~relative:"b") ()));

  assert_true
    (Configuration.Analysis.equal
       (Configuration.Analysis.create ~verbose:true ())
       (Configuration.Analysis.create ~verbose:false ()));

  assert_false
    (Configuration.Analysis.equal
       (Configuration.Analysis.create ~expected_version:"a" ())
       (Configuration.Analysis.create ~expected_version:"b" ()));

  assert_false
    (Configuration.Analysis.equal
       (Configuration.Analysis.create ~strict:true ())
       (Configuration.Analysis.create ~strict:false ()));

  assert_false
    (Configuration.Analysis.equal
       (Configuration.Analysis.create ~declare:true ())
       (Configuration.Analysis.create ~declare:false ()));

  assert_false
    (Configuration.Analysis.equal
       (Configuration.Analysis.create ~debug:true ())
       (Configuration.Analysis.create ~debug:false ()))


let () =
  "configuration">:::[
    "equal">::test_equal;
  ]
  |> Test.run
