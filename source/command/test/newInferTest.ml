(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Commands.NewInfer

let test_json_parsing context =
  let assert_parsed ~expected json =
    match InferConfiguration.of_yojson json with
    | Result.Error message ->
        let message = Format.sprintf "Unexpected JSON parsing failure: %s" message in
        assert_failure message
    | Result.Ok actual ->
        assert_equal
          ~ctxt:context
          ~cmp:[%compare.equal: InferConfiguration.t]
          ~printer:(fun result -> Sexp.to_string ([%sexp_of: InferConfiguration.t] result))
          expected
          actual
  in
  assert_parsed
    (`Assoc (("infer_mode", `List [`String "Local"]) :: BaseConfigurationTest.dummy_base_json))
    ~expected:
      {
        InferConfiguration.base = BaseConfigurationTest.dummy_base_configuration;
        paths_to_modify = None;
        infer_mode = InferMode.Local;
      };
  assert_parsed
    (`Assoc
      (("paths_to_modify", `List [`String "my/module.py"])
       :: ("infer_mode", `List [`String "Interprocedural"]) :: BaseConfigurationTest.dummy_base_json))
    ~expected:
      {
        InferConfiguration.base = BaseConfigurationTest.dummy_base_configuration;
        paths_to_modify = Some ["my/module.py"];
        infer_mode = InferMode.Interprocedural;
      };
  ()


let () = "configuration" >::: ["json_parsing" >:: test_json_parsing] |> Test.run
