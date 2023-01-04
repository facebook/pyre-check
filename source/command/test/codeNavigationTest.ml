(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Commands.CodeNavigation

let dummy_base_json =
  ("socket_path", `String "pyre_server_hash.sock") :: BaseConfigurationTest.dummy_base_json


let test_json_parsing context =
  let assert_parsed ~expected json =
    match CodeNavigationConfiguration.of_yojson json with
    | Result.Error message ->
        let message = Format.sprintf "Unexpected JSON parsing failure: %s" message in
        assert_failure message
    | Result.Ok actual ->
        assert_equal
          ~ctxt:context
          ~cmp:[%compare.equal: CodeNavigationConfiguration.t]
          ~printer:(fun result -> Sexp.to_string ([%sexp_of: CodeNavigationConfiguration.t] result))
          expected
          actual
  in

  let dummy_codenav_configuration =
    {
      CodeNavigationConfiguration.base = BaseConfigurationTest.dummy_base_configuration;
      additional_logging_sections = [];
      socket_path = PyrePath.create_absolute "pyre_server_hash.sock";
      watchman_root = None;
      critical_files = [];
    }
  in

  assert_parsed
    (`Assoc
      (("additional_logging_sections", `List [`String "foo"; `String "bar"]) :: dummy_base_json))
    ~expected:{ dummy_codenav_configuration with additional_logging_sections = ["foo"; "bar"] };
  assert_parsed
    (`Assoc (("watchman_root", `String "/project") :: dummy_base_json))
    ~expected:
      {
        dummy_codenav_configuration with
        watchman_root = Some (PyrePath.create_absolute "/project");
      };
  assert_parsed
    (`Assoc
      (( "critical_files",
         `List
           [
             `Assoc ["base_name", `String "foo.py"];
             `Assoc ["extension", `String "derp"];
             `Assoc ["full_path", `String "/home/bar.txt"];
           ] )
      :: dummy_base_json))
    ~expected:
      {
        dummy_codenav_configuration with
        critical_files =
          [
            Server.CriticalFile.BaseName "foo.py";
            Server.CriticalFile.Extension "derp";
            Server.CriticalFile.FullPath (PyrePath.create_absolute "/home/bar.txt");
          ];
      };
  ()


let () = "configuration" >::: ["json_parsing" >:: test_json_parsing] |> Test.run
