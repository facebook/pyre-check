(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2

let test_directory = "source/interprocedural_analyses/taint/test/integration/"

let () =
  match Sys.getenv "PYSA_INTEGRATION_TEST" with
  | Some test_file ->
      let root = TestHelper.find_pyre_source_code_root () in
      PyrePath.create_relative ~root ~relative:(test_directory ^ test_file)
      |> TestHelper.end_to_end_integration_test
      |> (fun test -> "taint" >:: test)
      |> Test.run
  | None ->
      (* This branch is only used when running `dune exec <path>/integrationTest.exe`. *)
      TestHelper.end_to_end_test_paths test_directory
      |> List.map ~f:(fun path ->
             PyrePath.last path >:: TestHelper.end_to_end_integration_test path)
      |> List.cons ("paths_found" >:: TestHelper.end_to_end_test_paths_found test_directory)
      |> (fun tests -> "taint" >::: tests)
      |> Test.run
