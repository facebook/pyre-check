(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Test

let test_transform_environment context =
  let assert_equivalent_attributes = assert_equivalent_attributes_single_class ~context in
  assert_equivalent_attributes
    ~source:{|
      import attr
      @attr.s
      class Foo:
        ...
    |}
    ~class_name:"Foo"
    {|
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
      |};
  assert_equivalent_attributes
    ~source:
      {|
      import attr
      if 1 > 2:
        @attr.s
        class Foo:
          ...
    |}
    ~class_name:"Foo"
    {|
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
      |};
  assert_equivalent_attributes
    ~source:
      {|
      import attr
      @attr.s
      class Foo:
        def foo() -> None:
          pass
    |}
    ~class_name:"Foo"
    {|
        # spacer
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
      |};
  assert_equivalent_attributes
    ~source:
      {|
      import attr
      @attr.s
      class Foo:
        def __init__(self) -> None:
          pass
        def __repr__(self) -> str:
          pass
    |}
    ~class_name:"Foo"
    {|
       # spacer
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
      |};
  assert_equivalent_attributes
    ~source:{|
      import attr
      @attr.s
      class Foo:
        name = 'abc'
    |}
    ~class_name:"Foo"
    {|
        class Foo:
          name = 'abc'
          def __init__(self, name: str ='abc') -> None:
            self.name = name
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
  assert_equivalent_attributes
    ~source:{|
      import attr
      @attr.s
      class Foo:
        name: str
    |}
    ~class_name:"Foo"
    {|
        class Foo:
          name: str
          def __init__(self, name: str) -> None:
            self.name = name
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
  assert_equivalent_attributes
    ~source:
      {|
      import attr
      @attr.s
      class Foo:
        name: str
        age: int
        def __init__(self) -> None:
          pass
    |}
    ~class_name:"Foo"
    {|
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
      |};
  (* Boolean arguments *)
  assert_equivalent_attributes
    ~source:
      {|
      import attr
      @attr.s(init = False)
      class Foo:
        def foo(self) -> None:
          pass
    |}
    ~class_name:"Foo"
    {|
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
      |};
  assert_equivalent_attributes
    ~source:
      {|
      import attr
      @attr.s(repr = False)
      class Foo:
        def foo(self) -> None:
          pass
    |}
    ~class_name:"Foo"
    {|
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
      |};
  assert_equivalent_attributes
    ~source:
      {|
      import attr
      @attr.s(cmp = False)
      class Foo:
        def foo(self) -> None:
          pass
    |}
    ~class_name:"Foo"
    {|
        class Foo:
          def foo(self) -> None:
            pass
          def __init__(self) -> None:
            pass
          def __repr__(self) -> str:
            pass
      |};
  assert_equivalent_attributes
    ~source:
      {|
      import attr
      @attr.s(auto_attribs = True)
      class Foo:
        def foo(self) -> None:
          pass
    |}
    ~class_name:"Foo"
    {|
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
      |};
  (* Inheritance *)
  assert_equivalent_attributes
    ~source:
      {|
      import attr
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
    ~class_name:"C"
    {|
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
  assert_equivalent_attributes
    ~source:
      {|
      import attr
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
    ~class_name:"C"
    {|
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
  assert_equivalent_attributes
    ~source:
      {|
      import attr
      @attr.s
      class C(Base):
        z: int = 10
        x: int
      @attr.s
      class Base:
        x: str = "a"
        y: int = 0
    |}
    ~class_name:"C"
    {|
        class C(Base):
          z: int = 10
          x: int
          def __init__(self, x: int = "a", y: int = 0, z: int = 10) -> None:
            self.x = x
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
  assert_equivalent_attributes
    ~source:
      {|
      import attr
      class B(Base):
        z: str = "a"
        y: int = 20
      @attr.s
      class Base:
        x: typing.Any = 15.0
        y: int = 0
    |}
    ~class_name:"B"
    {|
        class B(Base):
          z: str = "a"
          y: int = 20
      |};
  assert_equivalent_attributes
    ~source:
      {|
      import attr
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
    ~class_name:"C"
    {|
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
  assert_equivalent_attributes
    ~source:
      {|
      import attr
      @attr.s
      class C1(B1, A1):
        z: int = 10
      @attr.s
      class B1:
        y: int = 5
      @attr.s
      class A1:
        x: int = 15
    |}
    ~class_name:"C1"
    {|
        class C1(B1, A1):
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
  assert_equivalent_attributes
    ~source:
      {|
      import attr
      class NoAttr:
        x: int = 15
      @attr.s
      class WithAttr(NoAttr):
        y: int = 5
    |}
    ~class_name:"WithAttr"
    {|
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
      |};
  ()


let () =
  "attrs"
  >::: ["transform_environment" >: test_case ~length:Long test_transform_environment]
  |> Test.run
