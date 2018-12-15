(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


open Core

open OUnit2

open Pyre
open Service

let test_coverage _ =
  let coverage =
    let { Service.Parser.parsed; _ } =
      Service.Parser.parse_sources
        ~configuration:(Configuration.Analysis.create ())
        ~scheduler:(Scheduler.mock ())
        ~files:[
          File.create
            ~content:"#pyre-strict\ndef foo()->int:\n    return 1\n"
            (Path.create_relative ~root:(Path.current_working_directory ()) ~relative:"a.py");
          File.create
            ~content:"#pyre-strict\ndef foo()->int:\n    return 1\n"
            (Path.create_relative ~root:(Path.current_working_directory ()) ~relative:"b.py");
          File.create
            ~content:"#pyre-ignore-all-errors\ndef foo()->int:\n    return 1\n"
            (Path.create_relative ~root:(Path.current_working_directory ()) ~relative:"c.py");
        ]
    in
    Service.Coverage.coverage ~number_of_files:3 ~sources:parsed
  in
  assert_equal
    coverage
    { Coverage.strict_coverage = 2; declare_coverage = 1; default_coverage = 0; source_files = 3 }


let () =
  "coverage">:::[
    "compute_coverage">::test_coverage;
  ]
  |> Test.run
