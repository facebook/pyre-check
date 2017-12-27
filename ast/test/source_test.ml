(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast


let test_parse _ =
  let assert_ignore lines expected_ignore_lines =
    let { Source.Metadata.ignore_lines; _ } = Source.Metadata.parse lines in
    assert_equal
      expected_ignore_lines
      (List.rev ignore_lines)
  in

  assert_ignore
    ["def foo() -> int: return 1.0  # pyre-ignore"]
    [1,[]];

  assert_ignore
    ["def foo() -> str: return 1.0  # pyre-ignore"]
    [1,[]];

  assert_ignore
    ["def foo() -> str: return  # pyre-ignore"]
    [1,[]];

  assert_ignore
    ["def foo() -> typing.List[str]: return 1  # pyre-ignore"]
    [1,[]];

  assert_ignore
    ["def foo() -> str: return 1.0  # pyre-ignore"; "def bar() -> int: return ''  # pyre-ignore"]
    [1,[]; 2,[]];

  assert_ignore
    ["class A: pass"; "def foo() -> A: return 1  # pyre-ignore"]
    [2,[]];

  assert_ignore
    ["def foo() -> str: return bar()  # pyre-ignore"]
    [1,[]];

  assert_ignore
    ["def foo() -> other:  # pyre-ignore"; "result = 0";
     "if True:"; "result = durp()"; "return result"]
    [1,[]];

  assert_ignore
    ["def foo() -> int: return 1.0  # type: ignore"]
    [1,[]];

  assert_ignore
    ["def foo() -> str: return 1.0  # type: ignore"]
    [1,[]];

  assert_ignore
    ["def foo() -> str: return 1.0  # pyre-ignore[7]"]
    [1,[7]];

  assert_ignore
    ["def foo() -> str: return  # pyre-ignore[7]"]
    [1,[7]];

  assert_ignore
    ["def foo() -> typing.List[str]: return 1  # pyre-ignore[7]"]
    [1,[7]];

  assert_ignore
    [
      "def foo() -> str: return 1.0  # pyre-ignore[7]";
      "def bar() -> int: return ''  # pyre-ignore[7]"
    ]
    [1,[7]; 2,[7]];

  assert_ignore
    [
      "def foo() -> str: return 1  # pyre-ignore[7, 1, 2]"
    ]
    [1,[7; 1; 2]];
  assert_ignore
    [
      "def foo() -> str: return 1  # pyre-fixme[7, 1, 2]"
    ]
    [1,[7; 1; 2]];

  (* Comment on preceding line. *)
  assert_ignore
    ["# pyre-ignore[7]"; "def foo() -> str: return"]
    [2,[7]]


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
