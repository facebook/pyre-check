open Core
open OUnit2

open Pyre
open Service

let test_coverage _ =
  let coverage =
    let handles =
      Service.Parser.parse_sources
        ~configuration:(Configuration.create ())
        ~scheduler:(Scheduler.mock ())
        ~files:[
          File.create
            ~content:"#pyre-strict\ndef foo()->int:\n    return 1\n"
            (Path.create_relative ~root:(Path.current_working_directory ()) ~relative:"a.py");
          File.create
            ~content:"#pyre-strict\ndef foo()->int:\n    return 1\n"
            (Path.create_relative ~root:(Path.current_working_directory ()) ~relative:"b.py");
          File.create
            ~content:"#pyre-do-not-check\ndef foo()->int:\n    return 1\n"
            (Path.create_relative ~root:(Path.current_working_directory ()) ~relative:"c.py");
        ]
    in
    Service.Coverage.coverage ~number_of_files:3 ~sources:handles
  in
  assert_equal
    coverage
    { Coverage.strict_coverage = 2; declare_coverage = 1; default_coverage = 0; source_files = 3 }


let () =
  "coverage">:::[
    "compute_coverage">::test_coverage;
  ]
  |> run_test_tt_main
