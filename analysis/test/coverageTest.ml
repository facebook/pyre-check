(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Analysis

let test_coverage context =
  let assert_coverage ?external_sources sources expected =
    let sources, _ =
      Test.ScratchProject.setup ~context ?external_sources sources
      |> Test.ScratchProject.parse_sources
    in
    Coverage.coverage ~sources |> assert_equal expected
  in
  assert_coverage
    [ "a.py", "#pyre-strict\ndef foo()->int:\n    return 1\n";
      "b.py", "#pyre-strict\ndef foo()->int:\n    return 1\n";
      "c.py", "#pyre-ignore-all-errors\ndef foo()->int:\n    return 1\n" ]
    { Coverage.strict_coverage = 2; declare_coverage = 1; default_coverage = 0; source_files = 3 };
  assert_coverage
    ~external_sources:
      [ "external_a.py", "#pyre-strict\ndef foo()->int:\n    return 1\n";
        "external_b.py", "#pyre-strict\ndef foo()->int:\n    return 1\n";
        "external_c.py", "#pyre-ignore-all-errors\ndef foo()->int:\n    return 1\n" ]
    ["a.py", "#pyre-strict\ndef foo()->int:\n    return 1\n"]
    { Coverage.strict_coverage = 1; declare_coverage = 0; default_coverage = 0; source_files = 1 }


let () = "coverage" >::: ["compute_coverage" >:: test_coverage] |> Test.run
