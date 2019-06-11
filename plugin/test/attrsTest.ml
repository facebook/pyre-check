(* Copyright (c) 2019-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

let test_transform_environment _ =
  PluginTest.assert_environment_contains
    {|
      @attr.s
      class Foo:
        ...
    |}
    [ {|
        @attr.s
        class Foo:
          def __init__(self) -> None:
            pass
          def __repr__(self) -> str:
            pass
          def __eq__(self, o: object) -> bool:
            pass
          def __lt__(self, o: object) -> bool:
            pass
          def __le__(self, o: object) -> bool:
            pass
          def __gt__(self, o: object) -> bool:
            pass
          def __ge__(self, o: object) -> bool:
            pass
      |}
    ];
  PluginTest.assert_environment_contains
    {|
      if 1 > 2:
        @attr.s
        class Foo:
          ...
    |}
    [ {|
        @attr.s
        class Foo:
          def __init__(self) -> None:
            pass
          def __repr__(self) -> str:
            pass
          def __eq__(self, o: object) -> bool:
            pass
          def __lt__(self, o: object) -> bool:
            pass
          def __le__(self, o: object) -> bool:
            pass
          def __gt__(self, o: object) -> bool:
            pass
          def __ge__(self, o: object) -> bool:
            pass
      |}
    ];
  PluginTest.assert_environment_contains
    {|
      @attr.s
      class Foo:
        def foo() -> None:
          pass
    |}
    [ {|
        @attr.s
        class Foo:
          def foo() -> None:
            pass
          def __init__(self) -> None:
            pass
          def __repr__(self) -> str:
            pass
          def __eq__(self, o: object) -> bool:
            pass
          def __lt__(self, o: object) -> bool:
            pass
          def __le__(self, o: object) -> bool:
            pass
          def __gt__(self, o: object) -> bool:
            pass
          def __ge__(self, o: object) -> bool:
            pass
      |}
    ];
  PluginTest.assert_environment_contains
    {|
      @attr.s
      class Foo:
        def __init__(self) -> None:
          pass
        def __repr__(self) -> str:
          pass
    |}
    [ {|
       @attr.s
       class Foo:
         def __init__(self) -> None:
           pass
         def __repr__(self) -> str:
           pass
         def __eq__(self, o: object) -> bool:
           pass
         def __lt__(self, o: object) -> bool:
           pass
         def __le__(self, o: object) -> bool:
           pass
         def __gt__(self, o: object) -> bool:
           pass
         def __ge__(self, o: object) -> bool:
           pass
      |}
    ];
  PluginTest.assert_environment_contains
    {|
      @attr.s
      class Foo:
        name = 'abc'
    |}
    [ {|
        @attr.s
        class Foo:
          name = 'abc'
          def __init__(self, name: str ='abc') -> None:
            pass
          def __repr__(self) -> str:
            pass
          def __eq__(self, o: object) -> bool:
            pass
          def __lt__(self, o: object) -> bool:
            pass
          def __le__(self, o: object) -> bool:
            pass
          def __gt__(self, o: object) -> bool:
            pass
          def __ge__(self, o: object) -> bool:
            pass
      |}
    ];
  PluginTest.assert_environment_contains
    {|
      @attr.s
      class Foo:
        name: str
    |}
    [ {|
        @attr.s
        class Foo:
          name: str
          def __init__(self, name: str) -> None:
            pass
          def __repr__(self) -> str:
            pass
          def __eq__(self, o: object) -> bool:
            pass
          def __lt__(self, o: object) -> bool:
            pass
          def __le__(self, o: object) -> bool:
            pass
          def __gt__(self, o: object) -> bool:
            pass
          def __ge__(self, o: object) -> bool:
            pass
      |}
    ];
  PluginTest.assert_environment_contains
    {|
      @attr.s
      class Foo:
        name: str
        age: int
        def __init__(self) -> None:
          pass
    |}
    [ {|
        @attr.s
        class Foo:
          name: str
          age: int
          def __init__(self) -> None:
            pass
          def __repr__(self) -> str:
            pass
          def __eq__(self, o: object) -> bool:
            pass
          def __lt__(self, o: object) -> bool:
            pass
          def __le__(self, o: object) -> bool:
            pass
          def __gt__(self, o: object) -> bool:
            pass
          def __ge__(self, o: object) -> bool:
            pass
      |}
    ];

  (* Boolean arguments *)
  PluginTest.assert_environment_contains
    {|
      @attr.s(init = False)
      class Foo:
        def foo(self) -> None:
          pass
    |}
    [ {|
        @attr.s(init = False)
        class Foo:
          def foo(self) -> None:
            pass
          def __repr__(self) -> str:
            pass
          def __eq__(self, o: object) -> bool:
            pass
          def __lt__(self, o: object) -> bool:
            pass
          def __le__(self, o: object) -> bool:
            pass
          def __gt__(self, o: object) -> bool:
            pass
          def __ge__(self, o: object) -> bool:
            pass
      |}
    ];
  PluginTest.assert_environment_contains
    {|
      @attr.s(repr = False)
      class Foo:
        def foo(self) -> None:
          pass
    |}
    [ {|
        @attr.s(repr = False)
        class Foo:
          def foo(self) -> None:
            pass
          def __init__(self) -> None:
            pass
          def __eq__(self, o: object) -> bool:
            pass
          def __lt__(self, o: object) -> bool:
            pass
          def __le__(self, o: object) -> bool:
            pass
          def __gt__(self, o: object) -> bool:
            pass
          def __ge__(self, o: object) -> bool:
            pass
      |}
    ];
  PluginTest.assert_environment_contains
    {|
      @attr.s(cmp = False)
      class Foo:
        def foo(self) -> None:
          pass
    |}
    [ {|
        @attr.s(cmp = False)
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
      @attr.s(auto_attribs = True)
      class Foo:
        def foo(self) -> None:
          pass
    |}
    [ {|
        @attr.s(auto_attribs = True)
        class Foo:
          def foo(self) -> None:
            pass
          def __init__(self) -> None:
            pass
          def __repr__(self) -> str:
            pass
          def __eq__(self, o: object) -> bool:
            pass
          def __lt__(self, o: object) -> bool:
            pass
          def __le__(self, o: object) -> bool:
            pass
          def __gt__(self, o: object) -> bool:
            pass
          def __ge__(self, o: object) -> bool:
            pass
      |}
    ];

  (* Inheritance *)
  PluginTest.assert_environment_contains
    {|
      @attr.s
      class C(Base):
        z: int = 10
        x: int = 15
      @attr.s
      class Base:
        x: typing.Any = 15.0
        y: int = 0
        z: str = "a"
    |}
    [ {|
        @attr.s
        class C(Base):
          z: int = 10
          x: int = 15
          def __init__(self, x: int = 15, y: int = 0, z: int = 10) -> None:
            pass
          def __repr__(self) -> str:
            pass
          def __eq__(self, o: object) -> bool:
            pass
          def __lt__(self, o: object) -> bool:
            pass
          def __le__(self, o: object) -> bool:
            pass
          def __gt__(self, o: object) -> bool:
            pass
          def __ge__(self, o: object) -> bool:
            pass
      |};
      {|
        @attr.s
        class Base:
          x: typing.Any = 15.0
          y: int = 0
          z: str = "a"
          def __init__(self, x: typing.Any = 15.0, y: int = 0, z: str = "a") -> None:
            pass
          def __repr__(self) -> str:
            pass
          def __eq__(self, o: object) -> bool:
            pass
          def __lt__(self, o: object) -> bool:
            pass
          def __le__(self, o: object) -> bool:
            pass
          def __gt__(self, o: object) -> bool:
            pass
          def __ge__(self, o: object) -> bool:
            pass
      |}
    ];
  PluginTest.assert_environment_contains
    {|
      @attr.s
      class C(Base):
        z: int = 10
        x: int = 15
      @attr.s
      class Base:
        x: typing.Any = 15.0
        y: int = 0
        z: str = "a"
    |}
    [ {|
        @attr.s
        class C(Base):
          z: int = 10
          x: int = 15
          def __init__(self, x: int = 15, y: int = 0, z: int = 10) -> None:
            pass
          def __repr__(self) -> str:
            pass
          def __eq__(self, o: object) -> bool:
            pass
          def __lt__(self, o: object) -> bool:
            pass
          def __le__(self, o: object) -> bool:
            pass
          def __gt__(self, o: object) -> bool:
            pass
          def __ge__(self, o: object) -> bool:
            pass
      |};
      {|
        @attr.s
        class Base:
          x: typing.Any = 15.0
          y: int = 0
          z: str = "a"
          def __init__(self, x: typing.Any = 15.0, y: int = 0, z: str = "a") -> None:
            pass
          def __repr__(self) -> str:
            pass
          def __eq__(self, o: object) -> bool:
            pass
          def __lt__(self, o: object) -> bool:
            pass
          def __le__(self, o: object) -> bool:
            pass
          def __gt__(self, o: object) -> bool:
            pass
          def __ge__(self, o: object) -> bool:
            pass
      |}
    ];
  PluginTest.assert_environment_contains
    {|
      @attr.s
      class C(Base):
        z: int = 10
        x: int
      @attr.s
      class Base:
        x: str = "a"
        y: int = 0
    |}
    [ {|
        @attr.s
        class C(Base):
          z: int = 10
          x: int
          def __init__(self, x: int = "a", y: int = 0, z: int = 10) -> None:
            pass
          def __repr__(self) -> str:
            pass
          def __eq__(self, o: object) -> bool:
            pass
          def __lt__(self, o: object) -> bool:
            pass
          def __le__(self, o: object) -> bool:
            pass
          def __gt__(self, o: object) -> bool:
            pass
          def __ge__(self, o: object) -> bool:
            pass
      |};
      {|
        @attr.s
        class Base:
          x: str = "a"
          y: int = 0
          def __init__(self, x: str = "a", y: int = 0) -> None:
            pass
          def __repr__(self) -> str:
            pass
          def __eq__(self, o: object) -> bool:
            pass
          def __lt__(self, o: object) -> bool:
            pass
          def __le__(self, o: object) -> bool:
            pass
          def __gt__(self, o: object) -> bool:
            pass
          def __ge__(self, o: object) -> bool:
            pass
      |}
    ];
  PluginTest.assert_environment_contains
    {|
      @attr.s
      class C(B):
        z: int = 10
        x: int = 15
      class B(Base):
        z: str = "a"
        y: int = 20
      @attr.s
      class Base:
        x: typing.Any = 15.0
        y: int = 0
    |}
    [ {|
        @attr.s
        class C(B):
          z: int = 10
          x: int = 15
          def __init__(self, x: int = 15, y: int = 0, z: int = 10) -> None:
            pass
          def __repr__(self) -> str:
            pass
          def __eq__(self, o: object) -> bool:
            pass
          def __lt__(self, o: object) -> bool:
            pass
          def __le__(self, o: object) -> bool:
            pass
          def __gt__(self, o: object) -> bool:
            pass
          def __ge__(self, o: object) -> bool:
            pass
      |};
      {|
        class B(Base):
          z: str = "a"
          y: int = 20
      |};
      {|
        @attr.s
        class Base:
          x: typing.Any = 15.0
          y: int = 0
          def __init__(self, x: typing.Any = 15.0, y: int = 0) -> None:
            pass
          def __repr__(self) -> str:
            pass
          def __eq__(self, o: object) -> bool:
            pass
          def __lt__(self, o: object) -> bool:
            pass
          def __le__(self, o: object) -> bool:
            pass
          def __gt__(self, o: object) -> bool:
            pass
          def __ge__(self, o: object) -> bool:
            pass
      |}
    ];
  PluginTest.assert_environment_contains
    {|
      @attr.s
      class C(B, A):
        z: int = 10
      @attr.s
      class B:
        y: int = 5
      @attr.s
      class A:
        x: int = 15
    |}
    [ {|
        @attr.s
        class C(B, A):
          z: int = 10
          def __init__(self, x: int = 15, y: int = 5, z: int = 10) -> None:
            pass
          def __repr__(self) -> str:
            pass
          def __eq__(self, o: object) -> bool:
            pass
          def __lt__(self, o: object) -> bool:
            pass
          def __le__(self, o: object) -> bool:
            pass
          def __gt__(self, o: object) -> bool:
            pass
          def __ge__(self, o: object) -> bool:
            pass
      |};
      {|
        @attr.s
        class B:
          y: int = 5
          def __init__(self, y: int = 5) -> None:
            pass
          def __repr__(self) -> str:
            pass
          def __eq__(self, o: object) -> bool:
            pass
          def __lt__(self, o: object) -> bool:
            pass
          def __le__(self, o: object) -> bool:
            pass
          def __gt__(self, o: object) -> bool:
            pass
          def __ge__(self, o: object) -> bool:
            pass
      |};
      {|
        @attr.s
        class A:
          x: int = 15
          def __init__(self, x: int = 15) -> None:
            pass
          def __repr__(self) -> str:
            pass
          def __eq__(self, o: object) -> bool:
            pass
          def __lt__(self, o: object) -> bool:
            pass
          def __le__(self, o: object) -> bool:
            pass
          def __gt__(self, o: object) -> bool:
            pass
          def __ge__(self, o: object) -> bool:
            pass
      |}
    ];
  PluginTest.assert_environment_contains
    {|
      class NoAttr:
        x: int = 15
      @attr.s
      class WithAttr(NoAttr):
        y: int = 5
    |}
    [ {|
        class NoAttr:
          x: int = 15
      |};
      {|
        @attr.s
        class WithAttr(NoAttr):
          y: int = 5
          def __init__(self, y: int = 5) -> None:
            pass
          def __repr__(self) -> str:
            pass
          def __eq__(self, o: object) -> bool:
            pass
          def __lt__(self, o: object) -> bool:
            pass
          def __le__(self, o: object) -> bool:
            pass
          def __gt__(self, o: object) -> bool:
            pass
          def __ge__(self, o: object) -> bool:
            pass
      |}
    ]


let () = "plugin_attrs" >::: ["transform_environment" >:: test_transform_environment] |> Test.run
