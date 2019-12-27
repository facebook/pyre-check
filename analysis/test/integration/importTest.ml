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
    [
      "Undefined import [21]: Could not find a module corresponding to import `durp`. (For common \
       reasons, see \
       https://pyre-check.org/docs/error-types.html#pyre-errors-1821-undefined-name-undefined-import)";
    ];
  assert_type_errors {|
      import typing
    |} [];
  assert_type_errors
    {|
      import typing, durp
    |}
    [
      "Undefined import [21]: Could not find a module corresponding to import `durp`. (For common \
       reasons, see \
       https://pyre-check.org/docs/error-types.html#pyre-errors-1821-undefined-name-undefined-import)";
    ];
  assert_type_errors {|
      from typing import durp
    |} [];
  assert_type_errors
    {|
      from durp import typing
    |}
    [
      "Undefined import [21]: Could not find a module corresponding to import `durp`. (For common \
       reasons, see \
       https://pyre-check.org/docs/error-types.html#pyre-errors-1821-undefined-name-undefined-import)";
    ];

  (* Ensure we don't double-error. *)
  assert_type_errors
    {|
      a = durp.x
    |}
    [
      "Missing global annotation [5]: Globally accessible variable `a` has no type specified.";
      "Undefined name [18]: Global name `durp` is not defined, or there is at least one control \
       flow path that doesn't define `durp`.";
    ];
  assert_type_errors
    {|
      import durp
      a = durp.x
    |}
    [
      "Undefined import [21]: Could not find a module corresponding to import `durp`. (For common \
       reasons, see \
       https://pyre-check.org/docs/error-types.html#pyre-errors-1821-undefined-name-undefined-import)";
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
      from durp.c import c  # This is ok
    |}
    [
      "Undefined import [21]: Could not find a module corresponding to import `durp.Foo`. (For \
       common reasons, see \
       https://pyre-check.org/docs/error-types.html#pyre-errors-1821-undefined-name-undefined-import)";
      "Undefined import [21]: Could not find a module corresponding to import `durp.b`. (For \
       common reasons, see \
       https://pyre-check.org/docs/error-types.html#pyre-errors-1821-undefined-name-undefined-import)";
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
      "Undefined import [21]: Could not find a module corresponding to import `durp.Foo`. (For \
       common reasons, see \
       https://pyre-check.org/docs/error-types.html#pyre-errors-1821-undefined-name-undefined-import)";
      "Undefined import [21]: Could not find a module corresponding to import `durp.b`. (For \
       common reasons, see \
       https://pyre-check.org/docs/error-types.html#pyre-errors-1821-undefined-name-undefined-import)";
    ];
  assert_type_errors
    ~update_environment_with:
      [
        {
          handle = "derp.py";
          source =
            {|
                from typing import Any
                a: Any = ...
            |};
        };
      ]
    {|
      import derp
      import derp.a
      import derp.a.b
      import derp.a.b.c
    |}
    [];
  assert_type_errors
    ~update_environment_with:
      [
        {
          handle = "derp.py";
          source =
            {|
                from typing import Any
                a: Any = ...
            |};
        };
      ]
    {|
      from derp import a
      from derp.a import b
      from derp.a.b import c
    |}
    [];
  ()


let () = "import" >::: ["check_imports" >:: test_check_imports] |> Test.run
