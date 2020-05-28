(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open OUnit2
open IntegrationTest

let test_check_imports context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      import durp
    |}
    ["Undefined import [21]: Could not find a module corresponding to import `durp`."];
  assert_type_errors {|
      import typing
    |} [];
  assert_type_errors
    {|
      import typing, durp
    |}
    ["Undefined import [21]: Could not find a module corresponding to import `durp`."];
  assert_type_errors
    {|
      from typing import durp
    |}
    ["Undefined import [21]: Could not find a name `durp` defined in module `typing`."];
  assert_type_errors
    {|
      from durp import typing
    |}
    ["Undefined import [21]: Could not find a module corresponding to import `durp`."];

  (* Ensure we don't double-error. *)
  assert_type_errors
    {|
      a = durp.x
    |}
    [
      "Missing global annotation [5]: Globally accessible variable `a` has no type specified.";
      "Unbound name [10]: Name `durp` is used but not defined in the current scope.";
    ];
  assert_type_errors
    {|
      import durp
      a = durp.x
    |}
    [
      "Undefined import [21]: Could not find a module corresponding to import `durp`.";
      "Missing global annotation [5]: Globally accessible variable `a` has no type specified.";
    ];
  assert_type_errors
    {|
      from typing import Optional
      def foo() -> None: return 1
    |}
    ["Incompatible return type [7]: Expected `None` but got `int`."];

  assert_type_errors
    ~update_environment_with:
      [
        {
          handle = "durp.py";
          source =
            {|
                from typing import Any
                class Foo:
                  a: int = 1
                b: int = 2
                c: Any = ...
        |};
        };
      ]
    {|
      from durp import Foo
      from durp import b
      from durp import c
    |}
    [];
  assert_type_errors
    ~update_environment_with:
      [
        {
          handle = "durp.py";
          source =
            {|
                from typing import Any
                class Foo:
                    a: int = 1
                b: int = 2
                c: Any = ...
            |};
        };
      ]
    {|
      from durp.Foo import a
      from durp.b import b
      from durp.c import c  # This is NOT ok
    |}
    [
      "Undefined import [21]: Could not find a module corresponding to import `durp.Foo`.";
      "Undefined import [21]: Could not find a module corresponding to import `durp.b`.";
      "Undefined import [21]: Could not find a module corresponding to import `durp.c`.";
    ];
  assert_type_errors
    ~update_environment_with:
      [
        {
          handle = "durp.py";
          source =
            {|
                from typing import Any
                class Foo:
                    a: int = 1
                b: int = 2
                c: Any = ...
            |};
        };
      ]
    {|
      import durp
      import durp.Foo
      import durp.b
      import durp.c
    |}
    [
      "Undefined import [21]: Could not find a module corresponding to import `durp.Foo`.";
      "Undefined import [21]: Could not find a module corresponding to import `durp.b`.";
      "Undefined import [21]: Could not find a module corresponding to import `durp.c`.";
    ];
  assert_type_errors
    ~update_environment_with:
      [
        {
          handle = "constants.py";
          source =
            {|
                class StoryEvent:
                  USERNAME = "username"
                  PASSWORD = "password"
        |};
        };
      ]
    {|
      from constants import StoryEvent
      from typing import List
      keys: List[str] = list()
      def expects_str(x: str) -> None:
        pass
      if StoryEvent.USERNAME in keys:
        expects_str(StoryEvent.USERNAME)
    |}
    [];
  assert_type_errors
    ~update_environment_with:
      [
        {
          handle = "utils/constants.py";
          source =
            {|
                class StoryEvent:
                  USERNAME = "username"
                  PASSWORD = "password"
        |};
        };
      ]
    {|
      from utils.constants import StoryEvent
      from typing import Dict
      keys: Dict[str, int] = {}
      def expects_str(x: str) -> None:
        pass
      if StoryEvent.USERNAME in keys:
        expects_str(StoryEvent.USERNAME)
    |}
    [];
  ()


let () = "import" >::: ["check_imports" >:: test_check_imports] |> Test.run
