(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest

let test_check_imports context =
  assert_type_errors
    {|
      import durp
    |}
    ["Undefined import [21]: Could not find a module corresponding to import `durp`."]
    context;
  assert_type_errors {|
      import typing
    |} [] context;
  assert_type_errors
    {|
      import typing, durp
    |}
    ["Undefined import [21]: Could not find a module corresponding to import `durp`."]
    context;
  assert_type_errors
    {|
      from typing import durp
    |}
    ["Undefined import [21]: Could not find a name `durp` defined in module `typing`."]
    context;
  assert_type_errors
    {|
      from durp import typing
    |}
    ["Undefined import [21]: Could not find a module corresponding to import `durp`."]
    context;

  (* Ensure we don't double-error. *)
  assert_type_errors
    {|
      a = durp.x
    |}
    [
      "Missing global annotation [5]: Globally accessible variable `a` has no type specified.";
      "Unbound name [10]: Name `durp` is used but not defined in the current scope.";
    ]
    context;
  assert_type_errors
    {|
      import durp
      a = durp.x
    |}
    [
      "Undefined import [21]: Could not find a module corresponding to import `durp`.";
      "Missing global annotation [5]: Globally accessible variable `a` has no type specified.";
    ]
    context;
  assert_type_errors
    {|
      from typing import Optional
      def foo() -> None: return 1
    |}
    ["Incompatible return type [7]: Expected `None` but got `int`."]
    context;

  assert_type_errors
    ~other_sources:
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
                def d() -> None: pass
        |};
        };
      ]
    {|
      from durp import Foo
      from durp import b
      from durp import c
      from durp import d
    |}
    []
    context;
  assert_type_errors
    ~other_sources:
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
                def d() -> None: pass
            |};
        };
      ]
    {|
      from durp.Foo import a
      from durp.b import b
      from durp.c import c  # This is NOT ok
      from durp.d import d
    |}
    [
      "Undefined import [21]: Could not find a module corresponding to import `durp.Foo`. A \
       definition with that name exists but it's a class.";
      "Undefined import [21]: Could not find a module corresponding to import `durp.b`. A \
       definition with that name exists but it's a variable.";
      "Undefined import [21]: Could not find a module corresponding to import `durp.c`. A \
       definition with that name exists but it's a variable.";
      "Undefined import [21]: Could not find a module corresponding to import `durp.d`. A \
       definition with that name exists but it's a function.";
    ]
    context;
  assert_type_errors
    ~other_sources:
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
                def d() -> None: pass
            |};
        };
      ]
    {|
      import durp
      import durp.Foo
      import durp.b
      import durp.c
      import durp.d
    |}
    [
      "Undefined import [21]: Could not find a module corresponding to import `durp.Foo`. A \
       definition with that name exists but it's a class.";
      "Undefined import [21]: Could not find a module corresponding to import `durp.b`. A \
       definition with that name exists but it's a variable.";
      "Undefined import [21]: Could not find a module corresponding to import `durp.c`. A \
       definition with that name exists but it's a variable.";
      "Undefined import [21]: Could not find a module corresponding to import `durp.d`. A \
       definition with that name exists but it's a function.";
    ]
    context;
  assert_type_errors
    ~other_sources:[{ handle = "a/b.py"; source = "def b() -> None: pass" }]
    ~handle:"a/__init__.py"
    {|
      from .b import b
    |}
    []
    context;
  assert_type_errors
    ~other_sources:
      [{ handle = "a/__init__.py"; source = "import .b as c" }; { handle = "a/b.py"; source = "" }]
    {|
      import a.c
    |}
    ["Undefined import [21]: Could not find a module corresponding to import `a.c`."]
    context;
  assert_type_errors
    ~other_sources:
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
    []
    context;
  assert_type_errors
    ~other_sources:
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
    []
    context;
  let source = { Test.handle = "existing_module.py"; source = {| class Foo: ... |} } in
  assert_type_errors
    ~other_sources:[source]
    {|
      from typing import Optional

      class Bar(int):
        def __init__(
          self,
          foo: Optional[existing_module.Foo],
        ) -> None: ...
    |}
    ["Unbound name [10]: Name `existing_module` is used but not defined in the current scope."]
    context;
  (* TODO(T80454071): This should raise an error about `existing_module` since it was not
     imported. *)
  assert_type_errors
    ~other_sources:[source]
    {|
      from typing import Optional

      class Bar(int):
        def __init__(
          self,
          foo: Optional["existing_module.Foo"],
        ) -> None: ...
    |}
    []
    context;
  ()


let test_check_stub_imports context =
  assert_type_errors
    ~show_error_traces:true
    ~other_sources:
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
    ]
    context;
  assert_type_errors
    ~show_error_traces:true
    ~other_sources:
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
    ]
    context;
  assert_strict_type_errors
    ~show_error_traces:true
    ~other_sources:
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
    ]
    context;
  assert_strict_type_errors
    ~show_error_traces:true
    ~other_sources:
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
    ]
    context;
  assert_strict_type_errors
    ~show_error_traces:true
    ~other_sources:
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
    ]
    context;
  ()


let test_check_stub_dunder_all_reexport context =
  (* Names listed in explicit __all__ should be re-exported from stub files, even without the `y as
     y` pattern *)
  assert_type_errors
    ~other_sources:
      [
        { handle = "base.pyi"; source = {|
              def y() -> int: ...
          |} };
        {
          handle = "reexporter.pyi";
          source = {|
              from base import y
              __all__ = ["y"]
          |};
        };
      ]
    {|
      from reexporter import y
      reveal_type(y())
    |}
    ["Revealed type [-1]: Revealed type for `reexporter.y()` is `int`."]
    context;
  (* Without __all__, plain imports in stubs should NOT be re-exported *)
  assert_type_errors
    ~other_sources:
      [
        { handle = "base2.pyi"; source = {|
              def z() -> int: ...
          |} };
        { handle = "no_all.pyi"; source = {|
              from base2 import z
          |} };
      ]
    {|
      from no_all import z
    |}
    ["Undefined import [21]: Could not find a name `z` defined in module `no_all`."]
    context;
  ()


let () =
  "import"
  >::: [
         "check_imports" >:: test_check_imports;
         "check_stub_imports" >:: test_check_stub_imports;
         "check_stub_dunder_all_reexport" >:: test_check_stub_dunder_all_reexport;
       ]
  |> Test.run
