(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Commands.Server

let dummy_server_json =
  ("socket_path", `String "pyre_server_hash.sock") :: BaseConfigurationTest.dummy_base_json


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
      socket_path = PyrePath.create_absolute "pyre_server_hash.sock";
      watchman_root = None;
      critical_files = [];
      taint_model_paths = [];
      store_type_check_resolution = false;
      saved_state_action = None;
      skip_initial_type_check = false;
    }
  in

  assert_parsed
    (`Assoc (("strict", `Bool true) :: dummy_server_json))
    ~expected:{ dummy_server_configuration with strict = true };
  assert_parsed
    (`Assoc (("show_error_traces", `Bool true) :: dummy_server_json))
    ~expected:{ dummy_server_configuration with show_error_traces = true };
  assert_parsed
    (`Assoc
      (("additional_logging_sections", `List [`String "foo"; `String "bar"]) :: dummy_server_json))
    ~expected:{ dummy_server_configuration with additional_logging_sections = ["foo"; "bar"] };
  assert_parsed
    (`Assoc (("watchman_root", `String "/project") :: dummy_server_json))
    ~expected:
      { dummy_server_configuration with watchman_root = Some (PyrePath.create_absolute "/project") };
  assert_parsed
    (`Assoc
      (( "critical_files",
         `List
           [
             `Assoc ["base_name", `String "foo.py"];
             `Assoc ["extension", `String "derp"];
             `Assoc ["full_path", `String "/home/bar.txt"];
           ] )
       :: dummy_server_json))
    ~expected:
      {
        dummy_server_configuration with
        critical_files =
          [
            Server.CriticalFile.BaseName "foo.py";
            Server.CriticalFile.Extension "derp";
            Server.CriticalFile.FullPath (PyrePath.create_absolute "/home/bar.txt");
          ];
      };
  assert_parsed
    (`Assoc (("taint_model_paths", `List [`String "/taint/model"]) :: dummy_server_json))
    ~expected:
      {
        dummy_server_configuration with
        taint_model_paths = [PyrePath.create_absolute "/taint/model"];
      };
  assert_parsed
    (`Assoc (("store_type_check_resolution", `Bool true) :: dummy_server_json))
    ~expected:{ dummy_server_configuration with store_type_check_resolution = true };
  assert_parsed
    (`Assoc
      (( "saved_state_action",
         `List [`String "load_from_project"; `Assoc ["project_name", `String "project"]] )
       :: dummy_server_json))
    ~expected:
      {
        dummy_server_configuration with
        saved_state_action =
          Some
            (Server.SavedStateAction.LoadFromProject
               { project_name = "project"; project_metadata = None });
      };
  assert_parsed
    (`Assoc (("skip_initial_type_check", `Bool true) :: dummy_server_json))
    ~expected:{ dummy_server_configuration with skip_initial_type_check = true };
  ()


let () = "configuration" >::: ["json_parsing" >:: test_json_parsing] |> Test.run
