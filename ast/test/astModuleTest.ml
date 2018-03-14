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


let test_resolve_export _ =
  let assert_resolve source access expected_resolved =
    let module_definition =
      let { Source.statements; _ } = parse source in
      Module.create statements
    in
    let actual_resolved =
      let qualifier, head =
        Module.resolve_export
          module_definition
          ~head:(List.hd_exn (parse_single_access access))
        |> fun value -> Option.value_exn value
      in
      qualifier @ [head]
    in
    assert_equal
      ~cmp:Access.equal
      ~printer:Access.show
      (parse_single_access expected_resolved)
      actual_resolved
  in

  assert_resolve
    "from implementation import identifier"
    "identifier"
    "implementation.identifier";
  assert_resolve
    "from implementation import identifier as alias"
    "alias"
    "implementation.identifier";
  assert_resolve
    "from implementation import function"
    "function()"
    "implementation.function()";
  assert_resolve
    "from implementation import function"
    "function(a, b)"
    "implementation.function(a, b)"


let () =
  "module">:::[
    "export">::test_export;
    "resolve_export">::test_resolve_export;
  ]
  |> run_test_tt_main
