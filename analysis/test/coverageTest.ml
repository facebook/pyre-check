(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Analysis

let test_coverage context =
  let coverage =
    let qualifiers =
      Test.ScratchProject.setup
        ~context
        [ "a.py", "#pyre-strict\ndef foo()->int:\n    return 1\n";
          "b.py", "#pyre-strict\ndef foo()->int:\n    return 1\n";
          "c.py", "#pyre-ignore-all-errors\ndef foo()->int:\n    return 1\n" ]
      |> Test.ScratchProject.parse_sources
      |> fun (sources, _) -> List.map sources ~f:(fun { Ast.Source.qualifier; _ } -> qualifier)
    in
    Coverage.coverage ~number_of_files:3 ~sources:qualifiers
  in
  assert_equal
    coverage
    { Coverage.strict_coverage = 2; declare_coverage = 1; default_coverage = 0; source_files = 3 }


let () = "coverage" >::: ["compute_coverage" >:: test_coverage] |> Test.run
