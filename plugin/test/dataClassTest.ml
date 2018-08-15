(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2


let test_transform_environment _ =
  PluginTest.assert_environment_expand
    {|
      @dataclass
      class Foo:
        ...
    |}
    {|
      @dataclass
      class Foo:
        def __init__(self) -> None:
          pass
        def __repr__(self) -> str:
          pass
        def __eq__(self, o) -> bool:
          pass
    |};
  PluginTest.assert_environment_expand
    {|
      @dataclass
      class Foo:
        def foo() -> None:
          pass
    |}
    {|
      @dataclass
      class Foo:
        def foo() -> None:
          pass
        def __init__(self) -> None:
          pass
        def __repr__(self) -> str:
          pass
        def __eq__(self, o) -> bool:
          pass
    |};
  PluginTest.assert_environment_expand
    {|
      @dataclasses.dataclass
      class Foo:
        def foo() -> None:
          pass
    |}
    {|
      @dataclasses.dataclass
      class Foo:
        def foo() -> None:
          pass
        def __init__(self) -> None:
          pass
        def __repr__(self) -> str:
          pass
        def __eq__(self, o) -> bool:
         pass
    |};
  PluginTest.assert_environment_expand
    {|
      @dataclass
      class Foo:
        def __init__(self) -> None:
          pass
        def __repr__(self) -> str:
          pass
    |}
    {|
     @dataclass
     class Foo:
       def __init__(self) -> None:
         pass
       def __repr__(self) -> str:
         pass
       def __eq__(self, o) -> bool:
         pass
    |};
  PluginTest.assert_environment_expand
    {|
      @dataclass
      class Foo:
        name = 'abc'
    |}
    {|
      @dataclass
      class Foo:
        name = 'abc'
        def __init__(self) -> None:
          pass
        def __repr__(self) -> str:
          pass
        def __eq__(self, o) -> bool:
          pass
    |};
  PluginTest.assert_environment_expand
    {|
      @dataclass
      class Foo:
        name: str
    |}
    {|
      @dataclass
      class Foo:
        name: str
        def __init__(self, name: str) -> None:
          pass
        def __repr__(self) -> str:
          pass
        def __eq__(self, o) -> bool:
          pass
    |};
  PluginTest.assert_environment_expand
    {|
      @dataclass
      class Foo:
        name: str
        age: int
    |}
    {|
      @dataclass
      class Foo:
        name: str
        age: int
        def __init__(self, name: str, age: int) -> None:
          pass
        def __repr__(self) -> str:
          pass
        def __eq__(self, o) -> bool:
          pass
    |};
  PluginTest.assert_environment_expand
    {|
      @dataclass
      class Foo:
        name: str
        age: int
        def __init__(self) -> None:
          pass
    |}
    {|
      @dataclass
      class Foo:
        name: str
        age: int
        def __init__(self) -> None:
          pass
        def __repr__(self) -> str:
          pass
        def __eq__(self, o) -> bool:
          pass
    |};
  PluginTest.assert_environment_expand
    {|
      @dataclass
      class Foo:
        name: str
        age = 3
    |}
    {|
      @dataclass
      class Foo:
        name: str
        age = 3
        def __init__(self, name: str) -> None:
          pass
        def __repr__(self) -> str:
          pass
        def __eq__(self, o) -> bool:
          pass
    |};
  PluginTest.assert_environment_expand
    {|
      @dataclass
      class Foo:
        name: str
        age: int = 3
    |}
    {|
      @dataclass
      class Foo:
        name: str
        age: int = 3
        def __init__(self, name: str, age: int = 3) -> None:
          pass
        def __repr__(self) -> str:
          pass
        def __eq__(self, o) -> bool:
          pass
    |};
  PluginTest.assert_environment_expand
    {|
      @dataclass
      class Foo:
        name: str
        age: List[int]
        parent: typing.Tuple['int', 'str']
    |}
    {|
      @dataclass
      class Foo:
        name: str
        age: List[int]
        parent: typing.Tuple['int', 'str']
        def __init__(self, name: str, age: List[int], parent: typing.Tuple['int', 'str']) -> None:
          pass
        def __repr__(self) -> str:
          pass
        def __eq__(self, o) -> bool:
          pass
    |};
  (* TODO(T30619164): We currently do not add methods if arguments are present *)
  PluginTest.assert_environment_expand
    {|
      @dataclass(eq = False)
      class Foo:
        def foo(self) -> None:
          pass
    |}
    {|
      @dataclass(eq = False)
      class Foo:
        def foo(self) -> None:
          pass
    |}


let () =
  "plugin_data_class">:::[
    "transform_environment">::test_transform_environment;
  ]
  |> run_test_tt_main
