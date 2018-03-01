(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast
open Source.Ignore


let test_parse _ =
  let test_path = "test.py" in

  let assert_ignore lines expected_ignore_lines =
    let { Source.Metadata.ignore_lines; _ } = Source.Metadata.parse test_path lines in
    assert_equal
      ~printer:(fun ignores -> List.to_string ~f:show ignores)
      expected_ignore_lines
      (List.rev ignore_lines)
  in

  let create_ignore ignored_line codes kind start_line start_column end_line end_column =
    let location =
      let start = { Location.line = start_line; column = start_column } in
      let stop = { Location.line = end_line; column = end_column } in
      { Location.path = test_path; start; stop }
    in
    create ~ignored_line ~codes ~kind ~location
  in

  assert_ignore
    ["def foo() -> int: return 1.0  # pyre-ignore"]
    [create_ignore 1 [] PyreIgnore 1 32 1 43];

  assert_ignore
    ["def foo() -> str: return 1.0  # pyre-ignore"]
    [create_ignore 1 [] PyreIgnore 1 32 1 43];

  assert_ignore
    ["def foo() -> str: return  # pyre-ignore"]
    [create_ignore 1 [] PyreIgnore 1 28 1 39];

  assert_ignore
    ["def foo() -> typing.List[str]: return 1  # pyre-ignore"]
    [create_ignore 1 [] PyreIgnore 1 43 1 54];

  assert_ignore
    ["def foo() -> str: return 1.0  # pyre-ignore"; "def bar() -> int: return ''  # pyre-ignore"]
    [
      create_ignore 1 [] PyreIgnore 1 32 1 43;
      create_ignore 2 [] PyreIgnore 2 31 2 42
    ];

  assert_ignore
    ["class A: pass"; "def foo() -> A: return 1  # pyre-ignore"]
    [create_ignore 2 [] PyreIgnore 2 28 2 39];

  assert_ignore
    ["def foo() -> str: return bar()  # pyre-ignore"]
    [create_ignore 1 [] PyreIgnore 1 34 1 45];

  assert_ignore
    ["def foo() -> other:  # pyre-ignore"; "result = 0";
     "if True:"; "result = durp()"; "return result"]
    [create_ignore 1 [] PyreIgnore 1 23 1 34];

  assert_ignore
    ["def foo() -> int: return 1.0  # type: ignore"]
    [create_ignore 1 [] TypeIgnore 1 32 1 44];

  assert_ignore
    ["def foo() -> str: return 1.0  # type: ignore"]
    [create_ignore 1 [] TypeIgnore 1 32 1 44];

  assert_ignore
    ["def foo() -> str: return 1.0  # pyre-ignore[7]"]
    [create_ignore 1 [7] PyreIgnore 1 32 1 46];

  assert_ignore
    ["def foo() -> str: return  # pyre-ignore[7]"]
    [create_ignore 1 [7] PyreIgnore 1 28 1 42];

  assert_ignore
    ["def foo() -> typing.List[str]: return 1  # pyre-ignore[7]"]
    [create_ignore 1 [7] PyreIgnore 1 43 1 57];

  assert_ignore
    [
      "def foo() -> str: return 1.0  # pyre-ignore[7]";
      "def bar() -> int: return ''  # pyre-ignore[7]"
    ]
    [
      create_ignore 1 [7] PyreIgnore 1 32 1 46;
      create_ignore 2 [7] PyreIgnore 2 31 2 45
    ];

  assert_ignore
    [
      "def foo() -> str: return 1  # pyre-ignore[7, 1, 2]"
    ]
    [create_ignore 1 [7; 1; 2] PyreIgnore 1 30 1 50];

  assert_ignore
    [
      "def foo() -> str: return 1  # pyre-fixme[7, 1, 2]"
    ]
    [create_ignore 1 [7; 1; 2] PyreFixme 1 30 1 49];

  assert_ignore
    [
      "def foo() -> str: return 1  # pyre-fixme[7, 1, 2]: something about Class[Derp4]"
    ]
    [create_ignore 1 [7; 1; 2] PyreFixme 1 30 1 79];

  (* Comment on preceding line. *)
  assert_ignore
    ["# pyre-ignore[7]"; "def foo() -> str: return"]
    [create_ignore 2 [7] PyreIgnore 1 2 1 16]


let test_qualifier _ =
  let qualifier modules =
    List.map
      ~f:Expression.Access.create
      modules
    |> List.concat
  in

  assert_equal
    (Source.qualifier ~path:"module.py")
    (qualifier ["module"]);

  assert_equal
    (Source.qualifier ~path:"module/submodule.py")
    (qualifier ["module"; "submodule"]);

  assert_equal
    (Source.qualifier ~path:"builtins.pyi")
    (qualifier []);

  assert_equal
    (Source.qualifier ~path:"module/builtins.pyi")
    (qualifier ["module"]);

  assert_equal
    (Source.qualifier ~path:"module/__init__.pyi")
    (qualifier ["module"])


let () =
  "metadata">:::[
    "parse">::test_parse;
  ]
  |> run_test_tt_main;
  "source">:::[
    "qualifier">::test_qualifier;
  ]
  |> run_test_tt_main
