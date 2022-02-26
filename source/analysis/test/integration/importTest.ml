(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

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
  let source = { Test.handle = "existing_module.py"; source = {| class Foo: ... |} } in
  assert_type_errors
    ~update_environment_with:[source]
    {|
      from typing import Optional

      class Bar(int):
        def __init__(
          self,
          foo: Optional[existing_module.Foo],
        ) -> None: ...
    |}
    ["Unbound name [10]: Name `existing_module` is used but not defined in the current scope."];
  (* TODO(T80454071): This should raise an error about `existing_module` since it was not imported. *)
  assert_type_errors
    ~update_environment_with:[source]
    {|
      from typing import Optional

      class Bar(int):
        def __init__(
          self,
          foo: Optional["existing_module.Foo"],
        ) -> None: ...
    |}
    [];
  ()


let test_check_stub_imports context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    ~show_error_traces:true
    ~update_environment_with:
      [
        {
          handle = "stubbed.pyi";
          source =
            {|
                from typing import Any
                class Foo:
                  a: int = 1
                b: int = 2
                c: Any = ...
        |};
        };
        {
          handle = "stubbed.py";
          source =
            {|
                class Foo:
                  a: int = 1
                b: int = 2
                c: int = 2
                d: int = 4
        |};
        };
        {
          handle = "not_stubbed.py";
          source =
            {|
                class Foo:
                  a: int = 1
                b: int = 2
                c: int = 2
                d: int = 4
        |};
        };
      ]
    {|
      import stubbed
      from stubbed import Foo
      from stubbed import a, b, c, d
      from not_stubbed import a, b, c, d
    |}
    [
      "Undefined import [21]: Could not find a name `a` defined in module `stubbed`. This module \
       is shadowed by a stub file at `stubbed.pyi`. Ensure `a` is defined in the stub file.";
      "Undefined import [21]: Could not find a name `d` defined in module `stubbed`. This module \
       is shadowed by a stub file at `stubbed.pyi`. Ensure `d` is defined in the stub file.";
      "Undefined import [21]: Could not find a name `a` defined in module `not_stubbed`. For \
       common reasons, see \
       https://pyre-check.org/docs/errors/#1821-undefined-name-undefined-import";
    ];
  assert_type_errors
    ~show_error_traces:true
    ~update_environment_with:
      [
        {
          handle = "qualifier.stubbed.pyi";
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
      import qualifier.stubbed
      from qualifier import stubbed
      from qualifier.stubbed import Foo
      from qualifier.stubbed import a, b, c
      from qualifier import a
    |}
    [
      "Undefined import [21]: Could not find a name `a` defined in module `qualifier.stubbed`. \
       This module is shadowed by a stub file at `qualifier.stubbed.pyi`. Ensure `a` is defined in \
       the stub file.";
      "Undefined import [21]: Could not find a name `a` defined in module `qualifier`. For common \
       reasons, see https://pyre-check.org/docs/errors/#1821-undefined-name-undefined-import";
    ];
  assert_strict_type_errors
    ~context
    ~show_error_traces:true
    ~update_environment_with:
      [
        {
          handle = "stubbed.pyi";
          source =
            {|
             from typing import Any
             class Foo:
               a: Any = ...
           |};
        };
        {
          handle = "stubbed.py";
          source =
            {|
             from typing import Any
             class Foo:
               a: Any = 1
               b: Any = 2
           |};
        };
      ]
    {|
      from stubbed import Foo

      def test() -> None:
        foo = Foo()
        foo.a
        foo.b
    |}
    [
      "Undefined attribute [16]: `Foo` has no attribute `b`. `Foo` is defined in a stub file at \
       `stubbed.pyi`. Ensure attribute `b` is defined in the stub file.";
    ];
  assert_strict_type_errors
    ~context
    ~show_error_traces:true
    ~update_environment_with:
      [
        {
          handle = "stubbed.pyi";
          source =
            {|
             from typing import Any
             class Foo:
               a: Any = ...
           |};
        };
        {
          handle = "not_stubbed.py";
          source =
            {|
             from typing import Any
             class Bar:
               a: Any = ...
           |};
        };
      ]
    {|
      from stubbed import Foo
      from not_stubbed import Bar

      def test() -> None:
        foo = Foo()
        foo.a
        foo.b

        bar = Bar()
        bar.a
        bar.b
    |}
    [
      "Undefined attribute [16]: `Foo` has no attribute `b`. `Foo` is defined in a stub file at \
       `stubbed.pyi`. Ensure attribute `b` is defined in the stub file.";
      "Undefined attribute [16]: `Bar` has no attribute `b`.";
    ];
  assert_strict_type_errors
    ~context
    ~show_error_traces:true
    ~update_environment_with:
      [
        { handle = "stubbed.pyi"; source = {|
             a: Any = ...
           |} };
        { handle = "not_stubbed.py"; source = {|
             a: Any = ...
           |} };
      ]
    {|
      import stubbed
      import not_stubbed

      def test() -> None:
        stubbed.a
        stubbed.b

        not_stubbed.a
        not_stubbed.b
    |}
    [
      "Undefined attribute [16]: Module `stubbed` has no attribute `b`. This module is shadowed by \
       a stub file at `stubbed.pyi`. Ensure `b` is defined in the stub file.";
      "Undefined attribute [16]: Module `not_stubbed` has no attribute `b`.";
    ];
  ()


let () =
  "import"
  >::: ["check_imports" >:: test_check_imports; "check_stub_imports" >:: test_check_stub_imports]
  |> Test.run
