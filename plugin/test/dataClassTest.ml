(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2


let test_transform_environment _ =
  PluginTest.assert_environment_contains
    {|
      @dataclass
      class Foo:
        ...
    |}
    [
      {|
        @dataclass
        class Foo:
          def __init__(self) -> None:
            pass
          def __repr__(self) -> str:
            pass
          def __eq__(self, o) -> bool:
            pass
      |}
    ];

  PluginTest.assert_environment_contains
    {|
      if 1 > 2:
        @dataclass
        class Foo:
          ...
    |}
    [
      {|
        @dataclass
        class Foo:
          def __init__(self) -> None:
            pass
          def __repr__(self) -> str:
            pass
          def __eq__(self, o) -> bool:
            pass
      |}
    ];


  PluginTest.assert_environment_contains
    {|
      @dataclass
      class Foo:
        def foo() -> None:
          pass
    |}
    [
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
      |}
    ];

  PluginTest.assert_environment_contains
    {|
      @dataclasses.dataclass
      class Foo:
        def foo() -> None:
          pass
    |}
    [
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
      |}
    ];

  PluginTest.assert_environment_contains
    {|
      @dataclass
      class Foo:
        def __init__(self) -> None:
          pass
        def __repr__(self) -> str:
          pass
    |}
    [
      {|
       @dataclass
       class Foo:
         def __init__(self) -> None:
           pass
         def __repr__(self) -> str:
           pass
         def __eq__(self, o) -> bool:
           pass
      |}
    ];

  PluginTest.assert_environment_contains
    {|
      @dataclass
      class Foo:
        name = 'abc'
    |}
    [
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
      |}
    ];

  PluginTest.assert_environment_contains
    {|
      @dataclass
      class Foo:
        name: str
    |}
    [
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
      |}
    ];

  PluginTest.assert_environment_contains
    {|
      @dataclass
      class Foo:
        name: str
        age: int
    |}
    [
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
      |}
    ];

  PluginTest.assert_environment_contains
    {|
      @dataclass
      class Foo:
        name: str
        age: int
        def __init__(self) -> None:
          pass
    |}
    [
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
      |}
    ];

  PluginTest.assert_environment_contains
    {|
      @dataclass
      class Foo:
        name: str
        age = 3
    |}
    [
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
      |}
    ];

  PluginTest.assert_environment_contains
    {|
      @dataclass
      class Foo:
        name: str
        age: int = 3
    |}
    [
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
      |}
    ];

  PluginTest.assert_environment_contains
    {|
      @dataclass
      class Foo:
        name: str
        age: List[int]
        parent: typing.Tuple['int', 'str']
    |}
    [
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
      |}
    ];


  (* Dataclass boolean arguments *)
  PluginTest.assert_environment_contains
    {|
      @dataclass(init = False)
      class Foo:
        def foo(self) -> None:
          pass
    |}
    [
      {|
        @dataclass(init = False)
        class Foo:
          def foo(self) -> None:
            pass
          def __repr__(self) -> str:
            pass
          def __eq__(self, o) -> bool:
            pass
      |}
    ];

  PluginTest.assert_environment_contains
    {|
      @dataclass(repr = False)
      class Foo:
        def foo(self) -> None:
          pass
    |}
    [
      {|
        @dataclass(repr = False)
        class Foo:
          def foo(self) -> None:
            pass
          def __init__(self) -> None:
            pass
          def __eq__(self, o) -> bool:
            pass
      |}
    ];

  PluginTest.assert_environment_contains
    {|
      @dataclass(eq = False)
      class Foo:
        def foo(self) -> None:
          pass
    |}
    [
      {|
        @dataclass(eq = False)
        class Foo:
          def foo(self) -> None:
            pass
          def __init__(self) -> None:
            pass
          def __repr__(self) -> str:
            pass
      |}
    ];

  PluginTest.assert_environment_contains
    {|
      @dataclass(frozen = True)
      class Foo:
        def foo(self) -> None:
          pass
    |}
    [
      {|
        @dataclass(frozen = True)
        class Foo:
          def foo(self) -> None:
            pass
          def __init__(self) -> None:
            pass
          def __repr__(self) -> str:
            pass
          def __eq__(self, o) -> bool:
            pass
      |}
    ];

  (* Dataclass inheritance *)
  PluginTest.assert_environment_contains
    {|
      @dataclass
      class C(Base):
        z: int = 10
        x: int = 15
      @dataclass
      class Base:
        x: typing.Any = 15.0
        y: int = 0
        z: str = "a"
    |}
    [
      {|
        @dataclass
        class C(Base):
          z: int = 10
          x: int = 15
          def __init__(self, x: int = 15, y: int = 0, z: int = 10) -> None:
            pass
          def __repr__(self) -> str:
            pass
          def __eq__(self, o) -> bool:
            pass
      |};
      {|
        @dataclass
        class Base:
          x: typing.Any = 15.0
          y: int = 0
          z: str = "a"
          def __init__(self, x: object = 15.0, y: int = 0, z: str = "a") -> None:
            pass
          def __repr__(self) -> str:
            pass
          def __eq__(self, o) -> bool:
            pass
      |};
    ];

  PluginTest.assert_environment_contains
    {|
      @dataclass
      class C(Base):
        z: int = 10
        x: int = 15
      @dataclass
      class Base:
        x: typing.Any = 15.0
        y: int = 0
        z: str = "a"
    |}
    [
      {|
        @dataclass
        class C(Base):
          z: int = 10
          x: int = 15
          def __init__(self, x: int = 15, y: int = 0, z: int = 10) -> None:
            pass
          def __repr__(self) -> str:
            pass
          def __eq__(self, o) -> bool:
            pass
      |};
      {|
        @dataclass
        class Base:
          x: typing.Any = 15.0
          y: int = 0
          z: str = "a"
          def __init__(self, x: object = 15.0, y: int = 0, z: str = "a") -> None:
            pass
          def __repr__(self) -> str:
            pass
          def __eq__(self, o) -> bool:
            pass
      |};
    ];

  PluginTest.assert_environment_contains
    {|
      @dataclass
      class C(Base):
        z: int = 10
        x = 15
      @dataclass
      class Base:
        x: typing.Any = 15.0
        y: int = 0
    |}
    [
      {|
        @dataclass
        class C(Base):
          z: int = 10
          x = 15
          def __init__(self, x: object = 15.0, y: int = 0, z: int = 10) -> None:
            pass
          def __repr__(self) -> str:
            pass
          def __eq__(self, o) -> bool:
            pass
      |};
      {|
        @dataclass
        class Base:
          x: typing.Any = 15.0
          y: int = 0
          def __init__(self, x: object = 15.0, y: int = 0) -> None:
            pass
          def __repr__(self) -> str:
            pass
          def __eq__(self, o) -> bool:
            pass
      |};
    ];

  PluginTest.assert_environment_contains
    {|
      @dataclass
      class C(Base):
        z: int = 10
        x: int
      @dataclass
      class Base:
        x: str = "a"
        y: int = 0
    |}
    [
      {|
        @dataclass
        class C(Base):
          z: int = 10
          x: int
          def __init__(self, x: int = "a", y: int = 0, z: int = 10) -> None:
            pass
          def __repr__(self) -> str:
            pass
          def __eq__(self, o) -> bool:
            pass
      |};
      {|
        @dataclass
        class Base:
          x: str = "a"
          y: int = 0
          def __init__(self, x: str = "a", y: int = 0) -> None:
            pass
          def __repr__(self) -> str:
            pass
          def __eq__(self, o) -> bool:
            pass
      |};
    ];

  PluginTest.assert_environment_contains
    {|
      @dataclass
      class C(B):
        z: int = 10
        x: int = 15
      class B(Base):
        z: str = "a"
        y: int = 20
      @dataclass
      class Base:
        x: Any = 15.0
        y: int = 0
    |}
    [
      {|
        @dataclass
        class C(B):
          z: int = 10
          x: int = 15
          def __init__(self, x: int = 15, y: int = 0, z: int = 10) -> None:
            pass
          def __repr__(self) -> str:
            pass
          def __eq__(self, o) -> bool:
            pass
      |};
      {|
        class B(Base):
          z: str = "a"
          y: int = 20
      |};
      {|
        @dataclass
        class Base:
          x: Any = 15.0
          y: int = 0
          def __init__(self, x: Any = 15.0, y: int = 0) -> None:
            pass
          def __repr__(self) -> str:
            pass
          def __eq__(self, o) -> bool:
            pass
      |};
    ];

  PluginTest.assert_environment_contains
    {|
      @dataclass
      class C(B, A):
        z: int = 10
      @dataclass
      class B:
        y: int = 5
      @dataclass
      class A:
        x: int = 15
    |}
    [
      {|
        @dataclass
        class C(B, A):
          z: int = 10
          def __init__(self, x: int = 15, y: int = 5, z: int = 10) -> None:
            pass
          def __repr__(self) -> str:
            pass
          def __eq__(self, o) -> bool:
            pass
      |};
      {|
        @dataclass
        class B:
          y: int = 5
          def __init__(self, y: int = 5) -> None:
            pass
          def __repr__(self) -> str:
            pass
          def __eq__(self, o) -> bool:
            pass
      |};
      {|
        @dataclass
        class A:
          x: int = 15
          def __init__(self, x: int = 15) -> None:
            pass
          def __repr__(self) -> str:
            pass
          def __eq__(self, o) -> bool:
            pass
      |};
    ];

  PluginTest.assert_environment_contains
    {|
      class NotDataClass:
        x: int = 15
      @dataclass
      class DataClass(NotDataClass):
        y: int = 5
    |}
    [
      {|
        class NotDataClass:
          x: int = 15
      |};
      {|
        @dataclass
        class DataClass(NotDataClass):
          y: int = 5
          def __init__(self, y: int = 5) -> None:
            pass
          def __repr__(self) -> str:
            pass
          def __eq__(self, o) -> bool:
            pass
      |};
    ]


let () =
  "plugin_data_class">:::[
    "transform_environment">::test_transform_environment;
  ]
  |> Test.run
