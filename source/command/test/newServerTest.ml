(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Commands.NewServer
module Path = PyrePath

let test_json_parsing context =
  let assert_parsed ~expected json =
    match ServerConfiguration.of_yojson json with
    | Result.Error message ->
        let message = Format.sprintf "Unexpected JSON parsing failure: %s" message in
        assert_failure message
    | Result.Ok actual ->
        assert_equal
          ~ctxt:context
          ~cmp:[%compare.equal: ServerConfiguration.t]
          ~printer:(fun result -> Sexp.to_string ([%sexp_of: ServerConfiguration.t] result))
          expected
          actual
  in

  let dummy_server_configuration =
    {
      ServerConfiguration.base = BaseConfigurationTest.dummy_base_configuration;
      strict = false;
      show_error_traces = false;
      additional_logging_sections = [];
      watchman_root = None;
      critical_files = [];
      taint_model_paths = [];
      store_type_check_resolution = false;
      saved_state_action = None;
    }
  in

  assert_parsed
    (`Assoc (("strict", `Bool true) :: BaseConfigurationTest.dummy_base_json))
    ~expected:{ dummy_server_configuration with strict = true };
  assert_parsed
    (`Assoc (("show_error_traces", `Bool true) :: BaseConfigurationTest.dummy_base_json))
    ~expected:{ dummy_server_configuration with show_error_traces = true };
  assert_parsed
    (`Assoc
      (("additional_logging_sections", `List [`String "foo"; `String "bar"])
       :: BaseConfigurationTest.dummy_base_json))
    ~expected:{ dummy_server_configuration with additional_logging_sections = ["foo"; "bar"] };
  assert_parsed
    (`Assoc (("watchman_root", `String "/project") :: BaseConfigurationTest.dummy_base_json))
    ~expected:
      { dummy_server_configuration with watchman_root = Some (Path.create_absolute "/project") };
  assert_parsed
    (`Assoc
      (( "critical_files",
         `List
           [
             `Assoc ["base_name", `String "foo.py"];
             `Assoc ["extension", `String "derp"];
             `Assoc ["full_path", `String "/home/bar.txt"];
           ] )
       :: BaseConfigurationTest.dummy_base_json))
    ~expected:
      {
        dummy_server_configuration with
        critical_files =
          [
            Newserver.CriticalFile.BaseName "foo.py";
            Newserver.CriticalFile.Extension "derp";
            Newserver.CriticalFile.FullPath (Path.create_absolute "/home/bar.txt");
          ];
      };
  assert_parsed
    (`Assoc
      (("taint_model_paths", `List [`String "/taint/model"])
       :: BaseConfigurationTest.dummy_base_json))
    ~expected:
      { dummy_server_configuration with taint_model_paths = [Path.create_absolute "/taint/model"] };
  assert_parsed
    (`Assoc (("store_type_check_resolution", `Bool true) :: BaseConfigurationTest.dummy_base_json))
    ~expected:{ dummy_server_configuration with store_type_check_resolution = true };
  assert_parsed
    (`Assoc
      (( "saved_state_action",
         `List [`String "load_from_project"; `Assoc ["project_name", `String "project"]] )
       :: BaseConfigurationTest.dummy_base_json))
    ~expected:
      {
        dummy_server_configuration with
        saved_state_action =
          Some
            (Newserver.SavedStateAction.LoadFromProject
               { project_name = "project"; project_metadata = None });
      };
  ()


let () = "configuration" >::: ["json_parsing" >:: test_json_parsing] |> Test.run
