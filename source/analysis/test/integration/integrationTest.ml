(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

let type_check ~environment ~source =
  let { Ast.Source.module_path = { Ast.ModulePath.qualifier; _ }; _ } = source in
  let scheduler = Scheduler.create_sequential () in
  Analysis.TypeEnvironment.populate_for_modules ~scheduler environment [qualifier];
  Analysis.Postprocessing.run
    ~scheduler
    ~environment:(Analysis.TypeEnvironment.read_only environment)
    [qualifier]


let assert_type_errors = Test.assert_errors ~check:type_check ~debug:true

let assert_strict_type_errors = Test.assert_errors ~check:type_check ~debug:false ~strict:true

let assert_default_type_errors = Test.assert_errors ~check:type_check ~debug:false

let assert_type_errors_inject_typing_and_typing_extensions
    ?(typing_placeholder = "__TYPING_PLACEHOLDER")
    ~context
    source
    expected_errors
  =
  let assert_type_errors_with_typing_as typing_module =
    Test.assert_errors
      ~context
      ~check:type_check
      ~debug:true
      (String.substr_replace_all ~pattern:typing_placeholder ~with_:typing_module source)
      expected_errors
  in
  assert_type_errors_with_typing_as "typing";
  assert_type_errors_with_typing_as "typing_extensions"
