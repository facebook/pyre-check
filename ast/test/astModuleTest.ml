(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast
open Expression

open Test


let test_export _ =
  let assert_exports source exports =
    let module_definition =
      let { Source.statements; _ } = parse source in
      Module.create statements
    in
    let assert_export (source, expected_target) =
      let actual_target =
        Access.create source
        |> Module.export module_definition
        |> (fun value -> Option.value_exn value)
        |> Access.show
      in
      assert_equal
        expected_target
        actual_target
    in
    List.iter ~f:assert_export exports
  in

  assert_exports
    {|
      from other.module import Class
      from different.module import function
    |}
    [
      "Class", "other.module.Class";
      "function", "different.module.function";
    ];
  assert_exports
    "from some.module import aliased as alias"
    ["alias", "some.module.aliased"];
  assert_exports
    "from some.module import one, two"
    [
      "one", "some.module.one";
      "two", "some.module.two";
    ]


let () =
  "module">:::[
    "export">::test_export;
  ]
  |> run_test_tt_main
