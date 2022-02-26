(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Commands.Check

let test_json_parsing context =
  let assert_parsed ~expected json =
    match CheckConfiguration.of_yojson json with
    | Result.Error message ->
        let message = Format.sprintf "Unexpected JSON parsing failure: %s" message in
        assert_failure message
    | Result.Ok actual ->
        assert_equal
          ~ctxt:context
          ~cmp:[%compare.equal: CheckConfiguration.t]
          ~printer:(fun result -> Sexp.to_string ([%sexp_of: CheckConfiguration.t] result))
          expected
          actual
  in

  let dummy_check_configuration =
    {
      CheckConfiguration.base = BaseConfigurationTest.dummy_base_configuration;
      strict = false;
      show_error_traces = false;
      additional_logging_sections = [];
    }
  in

  assert_parsed
    (`Assoc (("strict", `Bool true) :: BaseConfigurationTest.dummy_base_json))
    ~expected:{ dummy_check_configuration with strict = true };
  assert_parsed
    (`Assoc (("show_error_traces", `Bool true) :: BaseConfigurationTest.dummy_base_json))
    ~expected:{ dummy_check_configuration with show_error_traces = true };
  assert_parsed
    (`Assoc
      (("additional_logging_sections", `List [`String "foo"; `String "bar"])
       :: BaseConfigurationTest.dummy_base_json))
    ~expected:{ dummy_check_configuration with additional_logging_sections = ["foo"; "bar"] };
  ()


let () = "configuration" >::: ["json_parsing" >:: test_json_parsing] |> Test.run
