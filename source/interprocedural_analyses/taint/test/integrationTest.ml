(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open TestHelper

let () =
  (* dummy ocaml module so when the test files change, the test gets triggered again *)
  ignore TaintIntegrationTest.Files.dummy_dependency;
  end_to_end_test_paths "source/interprocedural_analyses/taint/test/integration/"
  |> List.map ~f:(fun path -> PyrePath.last path >:: end_to_end_integration_test path)
  |> List.cons
       ("paths_found"
       >:: end_to_end_test_paths_found "source/interprocedural_analyses/taint/test/integration/")
  |> (fun tests -> "taint" >::: tests)
  |> Test.run
