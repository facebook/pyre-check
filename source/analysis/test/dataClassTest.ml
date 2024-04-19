(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Test

let test_transform_environment =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass

              @dataclass(match_args=False)
              class Foo:
                ...
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  def __init__(self) -> None: ...
                  def __repr__(self) -> str: ...
                  def __eq__(self, o: object) -> bool: ...
              |};
      (* TODO T178998636: Support InitVars. The correct behavior should be as follows: 1- In the
         testcase result, we need to remove x and y from the class fields (so the fields should be
         a,b and c only) 2- The generated init method should indeed take x and y as parameters but
         should not initialize them *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass
              @dataclass
              class DC1:
                  a: int
                  b: int
                  c: int
                  x: dataclasses.InitVar[int]
                  y: dataclasses.InitVar[str]

                  def __post_init__(self, x: int, y: str) -> None:
                      pass
            |}
           ~class_name:"DC1"
           {|
                class DC1:
                  a: int
                  b: int
                  c: int
                  x: dataclasses.InitVar[int]
                  y: dataclasses.InitVar[str]

                  def __post_init__(self, x: int, y: str) -> None:
                    pass
                  __match_args__ = ("a", "b", "c", "x", "y")
                  def __init__(self, a: int, b:int, c: int, x: int, y: str) -> None:
                    self.a = a
                    self.b = b
                    self.c = c
                    self.x = x
                    self.y = y

                  def __repr__(self) -> str: ...
                  def __eq__(self, o: object) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass

              if 1 > 2:
                @dataclass(match_args=False)
                class Foo:
                  ...
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  def __init__(self) -> None: ...
                  def __repr__(self) -> str: ...
                  def __eq__(self, o: object) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass

              @dataclass(match_args=False)
              class Foo:
                def foo() -> None: ...
            |}
           ~class_name:"Foo"
           {|
                # spacer
                class Foo:
                  def foo() -> None: ...
                  def __init__(self) -> None: ...
                  def __repr__(self) -> str: ...
                  def __eq__(self, o: object) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              import dataclasses

              @dataclasses.dataclass(match_args=False)
              class Foo:
                def foo() -> None: ...
            |}
           ~class_name:"Foo"
           {|
                @spacer
                class Foo:
                  def foo() -> None: ...
                  def __init__(self) -> None: ...
                  def __repr__(self) -> str: ...
                  def __eq__(self, o: object) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass

              @dataclass(match_args=False)
              class Foo:
                def __init__(self) -> None: ...
                def __repr__(self) -> str: ...
            |}
           ~class_name:"Foo"
           {|
               class Foo:
                 def __init__(self) -> None: ...
                 def __repr__(self) -> str: ...
                 def __eq__(self, o: object) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass

              @dataclass(match_args=False)
              class Foo:
                name = 'abc'
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  name: unknown = 'abc'
                  def __init__(self) -> None: ...
                  def __repr__(self) -> str: ...
                  def __eq__(self, o: object) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass

              @dataclass(match_args=False)
              class Foo:
                name: str
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  name: str
                  def __init__(self, name: str) -> None:
                    self.name = name
                  def __repr__(self) -> str: ...
                  def __eq__(self, o: object) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass

              @dataclass(match_args=False)
              class Foo:
                name: str
                age: int
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  name: str
                  age: int
                  def __init__(self, name: str, age: int) -> None:
                    self.name = name
                    self.age = age
                  def __repr__(self) -> str: ...
                  def __eq__(self, o: object) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass

              @dataclass(match_args=False)
              class Foo:
                name: str
                age: int
                def __init__(self) -> None: ...
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  name: str
                  age: int
                  def __init__(self) -> None: ...
                  def __repr__(self) -> str: ...
                  def __eq__(self, o: object) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass

              @dataclass(match_args=False)
              class Foo:
                name: str
                age = 3
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  name: str
                  age: unknown = 3
                  def __init__(self, name: str) -> None:
                    self.name = name
                  def __repr__(self) -> str: ...
                  def __eq__(self, o: object) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass

              @dataclass(match_args=False)
              class Foo:
                name: str
                age: int = 3
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  name: str
                  age: int = 3
                  def __init__(self, name: str, age: int = 3) -> None:
                    self.name = name
                  def __repr__(self) -> str: ...
                  def __eq__(self, o: object) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass

              @dataclass(match_args=False)
              class Foo:
                name: str
                age: typing.List[int]
                parent: typing.Tuple['int', 'str']
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  name: str
                  age: typing.List[int]
                  parent: typing.Tuple['int', 'str']
                  def __init__(self, name: str, age: typing.List[int], parent: typing.Tuple['int', 'str']) -> None:
                    self.name = name
                    self.age = age
                    self.parent = parent
                  def __repr__(self) -> str: ...
                  def __eq__(self, o: object) -> bool: ...
              |};
      (* Dataclass boolean arguments *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass

              @dataclass(init = False, match_args = False)
              class Foo:
                def foo(self) -> None: ...
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  def foo(self) -> None: ...
                  def __repr__(self) -> str: ...
                  def __eq__(self, o: object) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass

              @dataclass(repr = False, match_args = False)
              class Foo:
                def foo(self) -> None: ...
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  def foo(self) -> None: ...
                  def __init__(self) -> None: ...
                  def __eq__(self, o: object) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass

              @dataclass(eq = False, match_args = False)
              class Foo:
                def foo(self) -> None: ...
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  def foo(self) -> None: ...
                  def __init__(self) -> None: ...
                  def __repr__(self) -> str: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass

              @dataclass(order = True, match_args = False)
              class Foo:
                def foo(self) -> None: ...
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  def foo(self) -> None: ...
                  def __init__(self) -> None: ...
                  def __repr__(self) -> str: ...
                  def __eq__(self, o: object) -> bool: ...
                  def __lt__(self, o: Foo) -> bool: ...
                  def __le__(self, o: Foo) -> bool: ...
                  def __gt__(self, o: Foo) -> bool: ...
                  def __ge__(self, o: Foo) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass

              @dataclass(eq = False, order = True, match_args = False)
              class Foo:
                def foo(self) -> None: ...
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  def foo(self) -> None: ...
                  def __init__(self) -> None: ...
                  def __repr__(self) -> str: ...
                  def __lt__(self, o: Foo) -> bool: ...
                  def __le__(self, o: Foo) -> bool: ...
                  def __gt__(self, o: Foo) -> bool: ...
                  def __ge__(self, o: Foo) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass

              @dataclass(frozen = True, match_args = False)
              class Foo:
                def foo(self) -> None: ...
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  def foo(self) -> None: ...
                  def __init__(self) -> None: ...
                  def __repr__(self) -> str: ...
                  def __eq__(self, o: object) -> bool: ...
              |};
      (* Dataclass inheritance *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass

              @dataclass(match_args=False)
              class Base:
                x: typing.Any = 15.0
                y: int = 0
                z: str = "a"

              @dataclass(match_args=False)
              class Child(Base):
                z: int = 10
                x: int = 15
            |}
           ~class_name:"Child"
           {|
                class Child(Base):
                  z: int = 10
                  x: int = 15
                  def __init__(self, x: int = 15, y: int = 0, z: int = 10) -> None: ...
                  def __repr__(self) -> str: ...
                  def __eq__(self, o: object) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass

              @dataclass(match_args=False)
              class Base:
                x: typing.Any = 15.0
                y: int = 0
                z: str = "a"

              @dataclass(match_args=False)
              class Child(Base):
                z: int = 10
                x: int = 15
            |}
           ~class_name:"Child"
           {|
                class Child(Base):
                  z: int = 10
                  x: int = 15
                  def __init__(self, x: int = 15, y: int = 0, z: int = 10) -> None: ...
                  def __repr__(self) -> str: ...
                  def __eq__(self, o: object) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass

              @dataclass(match_args=False)
              class Base:
                x: typing.Any = 15.0
                y: int = 0

              @dataclass(match_args=False)
              class Child(Base):
                z: int = 10
                x = 15
            |}
           ~class_name:"Child"
           {|
                class Child(Base):
                  z: int = 10
                  x: unknown = 15
                  def __init__(self, x: typing.Any = 15.0, y: int = 0, z: int = 10) -> None: ...
                  def __repr__(self) -> str: ...
                  def __eq__(self, o: object) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass

              @dataclass(match_args=False)
              class Base:
                x: str = "a"
                y: int = 0

              @dataclass(match_args=False)
              class Child(Base):
                z: int = 10
                x: int
            |}
           ~class_name:"Child"
           {|
                class Child(Base):
                  z: int = 10
                  x: int
                  def __init__(self, x: int = "a", y: int = 0, z: int = 10) -> None:
                   self.x = x
                  def __repr__(self) -> str: ...
                  def __eq__(self, o: object) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass

              @dataclass(match_args=False)
              class Base:
                x: typing.Any = 15.0
                y: int = 0

              class Child(Base):
                z: str = "a"
                y: int = 20
            |}
           ~class_name:"Child"
           {|
                class Child(Base):
                  z: str = "a"
                  y: int = 20
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass

              @dataclass(match_args=False)
              class Base:
                x: typing.Any = 15.0
                y: int = 0

              class Child(Base):
                z: str = "a"
                y: int = 20

              @dataclass(match_args=False)
              class GrandChild(Child):
                z: int = 10
                x: int = 15
            |}
           ~class_name:"GrandChild"
           {|
                class GrandChild(Child):
                  z: int = 10
                  x: int = 15
                  def __init__(self, x: int = 15, y: int = 0, z: int = 10) -> None: ...
                  def __repr__(self) -> str: ...
                  def __eq__(self, o: object) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass

              @dataclass(match_args=False)
              class Base1:
                x: int = 15

              @dataclass(match_args=False)
              class Base2:
                y: int = 5

              @dataclass(match_args=False)
              class Child(Base2, Base1):
                z: int = 10
            |}
           ~class_name:"Child"
           {|
                class Child(Base2, Base1):
                  z: int = 10
                  def __init__(self, x: int = 15, y: int = 5, z: int = 10) -> None: ...
                  def __repr__(self) -> str: ...
                  def __eq__(self, o: object) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass

              class NotDataClass:
                x: int = 15

              @dataclass(match_args=False)
              class DataClass(NotDataClass):
                y: int = 5
            |}
           ~class_name:"DataClass"
           {|
                class DataClass(NotDataClass):
                  y: int = 5
                  def __init__(self, y: int = 5) -> None: ...
                  def __repr__(self) -> str: ...
                  def __eq__(self, o: object) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass

              @dataclass(match_args=False)
              class Foo:
                x: int
                init_variable: dataclasses.InitVar[str]
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  x: int
                  init_variable: dataclasses.InitVar[str]
                  def __init__(self, x: int, init_variable: str) -> None:
                    self.x = x
                    self.init_variable = init_variable
                  def __repr__(self) -> str: ...
                  def __eq__(self, o: object) -> bool: ...
              |};
      (* Dataclass field init disabler *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass

              @dataclass(match_args=False)
              class Foo:
                x: int = dataclasses.field(init=False)
            |}
           ~class_name:"Foo"
           {|
                # spacer
                class Foo:
                  x: int = dataclasses.field(init=False)
                  def __init__(self) -> None: ...
                  def __repr__(self) -> str: ...
                  def __eq__(self, o: object) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass

              @dataclass(match_args=False)
              class Foo:
                x: int = dataclasses.field(init=True)
            |}
           ~class_name:"Foo"
           {|
                # spacer
                class Foo:
                  x: int = dataclasses.field(init=True)
                  def __init__(self, x: int) -> None: ...
                  def __repr__(self) -> str: ...
                  def __eq__(self, o: object) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass

              @dataclass(match_args=False)
              class Foo:
                x: int = dataclasses.field(init=True, default=1)
            |}
           ~class_name:"Foo"
           {|
                # spacer
                class Foo:
                  x: int = dataclasses.field(init=True, default=1)
                  def __init__(self, x: int = 1) -> None: ...
                  def __repr__(self) -> str: ...
                  def __eq__(self, o: object) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass

              @dataclass(match_args=False)
              class Foo:
                x: int = dataclasses.field(init=True, default_factory=foo)
            |}
           ~class_name:"Foo"
           {|
                # spacer
                class Foo:
                  x: int = dataclasses.field(init=True, default_factory=foo)
                  def __init__(self, x: int = foo()) -> None: ...
                  def __repr__(self) -> str: ...
                  def __eq__(self, o: object) -> bool: ...
              |};
      (* NOTE: Ideally we'd like to warn about this somehow *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass

              @dataclass(match_args=False)
              class Foo:
                x: int = dataclasses.field(init=False, default=1)
            |}
           ~class_name:"Foo"
           {|
                # spacer
                class Foo:
                  x: int = dataclasses.field(init=False, default=1)
                  def __init__(self) -> None: ...
                  def __repr__(self) -> str: ...
                  def __eq__(self, o: object) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass

              @dataclass(match_args=False)
              class Foo:
                x: int = dataclasses.field(init=False)
            |}
           ~class_name:"Foo"
           {|
                # spacer
                class Foo:
                  x: int = dataclasses.field(init=False)
                  def __init__(self) -> None: ...
                  def __repr__(self) -> str: ...
                  def __eq__(self, o: object) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass

              @dataclass(match_args=False)
              class B:
                y: str = "abc"
            |}
           ~class_name:"B"
           {|
                class B:
                  y: str = "abc"
                  def __init__(self, y: str = "abc") -> None: ...
                  def __repr__(self) -> str: ...
                  def __eq__(self, o: object) -> bool: ...
              |};
      (* TODO(T129344236) Fix inheritance for dataclass fields *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass

              @dataclass(match_args=False)
              class Base:
                x: int = dataclass.field(init=False)

              @dataclass(repr=False, eq=False, match_args=False)
              class Foo(Base):
                y: int = dataclass.field(init=True)
            |}
           ~class_name:"Foo"
           {|
                # spacer
                class Foo(Base):
                  y: int = dataclass.field(init=True)
                  def __init__(self, x: int = ..., y: int = ...) -> None: ...
              |};
    ]


let test_match_args =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass

              @dataclass
              class Foo:
                x: int
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  x: int
                  def __init__(self, x: int) -> None:
                    self.x = x
                  def __repr__(self) -> str: ...
                  def __eq__(self, o: object) -> bool: ...
                  __match_args__ = ("x",)
              |};
      (* For remaining tests we disable repr and eq, to make them more consice as they don't
         interact with match_args. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass

              @dataclass(repr=False, eq=False, match_args=True)
              class Foo:
                x: int
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  x: int
                  def __init__(self, x: int) -> None:
                    self.x = x
                  __match_args__ = ("x",)
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass

              @dataclass(repr=False, eq=False, match_args=False)
              class Foo:
                x: int
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  x: int
                  def __init__(self, x: int) -> None:
                    self.x = x
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass

              @dataclass(repr=False, eq=False, init=False, match_args=True)
              class Foo:
                x: int
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  x: int
                  __match_args__ = ("x",)
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass

              @dataclass(repr=False, eq=False)
              class Foo:
                x: int
                y: int
                def __init__(self, y: int) -> None:
                  ...
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  x: int
                  y: int
                  def __init__(self, y: int) -> None:
                    ...
                  __match_args__ = ("x", "y")
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass

              @dataclass(repr=False, eq=False)
              class Foo:
                x: int
                y: int
                def __init__(self, y: int) -> None:
                  ...
                __match_args__ = ("y",)
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  x: int
                  y: int
                  def __init__(self, y: int) -> None:
                    ...
                  __match_args__ = ("y",)
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass

              @dataclass(repr=False, eq=False)
              class Base:
                x: typing.Any
                y: int

              @dataclass(repr=False, eq=False)
              class Child(Base):
                z: int
                x: int
            |}
           ~class_name:"Child"
           {|
                class Child(Base):
                  z: int
                  x: int
                  def __init__(self, x: int, y: int, z: int) -> None:
                    self.x = x
                    self.z = z
                  __match_args__ = ("x", "y", "z")
              |};
    ]


let test_dataclass_transform =
  let assert_equivalent_attributes_3_10 =
    assert_equivalent_attributes
      ~python_version:(Configuration.PythonVersion.create ~major:3 ~minor:10 ())
  in
  let assert_equivalent_attributes =
    assert_equivalent_attributes
      ~python_version:(Configuration.PythonVersion.create ~major:3 ~minor:11 ())
  in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from typing import dataclass_transform

              @dataclass_transform
              def mytransform():
                ...

              @mytransform
              class Foo:
                x: int
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  x: int
                  def __init__(self, x: int) -> None:
                    self.x = x
                  def __eq__(self, o: object) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from typing import dataclass_transform

              @dataclass_transform
              def mytransform():
                ...

              @mytransform()
              class Foo:
                x: int
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  x: int
                  def __init__(self, x: int) -> None:
                    self.x = x
                  def __eq__(self, o: object) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from typing import dataclass_transform

              @dataclass_transform
              def mytransform():
                ...

              @mytransform(eq=False)
              class Foo:
                x: int
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  x: int
                  def __init__(self, x: int) -> None:
                    self.x = x
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from typing import dataclass_transform

              @dataclass_transform
              class ModelMeta(type): ...

              class ModelBase(metaclass=ModelMeta): ...

              class CustomerModel(ModelBase):
                id: int
                name: str
            |}
           ~class_name:"CustomerModel"
           {|
              class CustomerModel(ModelBase):
                id: int
                name: str
                def __init__(self, id: int, name: str) -> None:
                  self.id = id
                  self.name = name
                def __eq__(self, o: object) -> bool: ...

              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from typing import dataclass_transform

              @dataclass_transform
              def mytransform():
                ...

              @mytransform(order=True)
              class Foo:
                x: int
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  x: int
                  def __init__(self, x: int) -> None:
                    self.x = x
                  def __eq__(self, o: object) -> bool: ...
                  def __lt__(self, o: Foo) -> bool: ...
                  def __le__(self, o: Foo) -> bool: ...
                  def __gt__(self, o: Foo) -> bool: ...
                  def __ge__(self, o: Foo) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from typing import dataclass_transform

              @dataclass_transform
              def mytransform():
                ...

              @mytransform(init=False)
              class Foo:
                x: int
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  x: int
                  def __eq__(self, o: object) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from typing import dataclass_transform

              @dataclass_transform(eq_default=False)
              def mytransform():
                ...

              @mytransform()
              class Foo:
                x: int
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  x: int
                  def __init__(self, x: int) -> None:
                    self.x = x
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from typing import dataclass_transform

              @dataclass_transform(eq_default=False)
              def mytransform():
                ...

              @mytransform(eq=True)
              class Foo:
                x: int
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  x: int
                  def __init__(self, x: int) -> None:
                    self.x = x
                  def __eq__(self, o: object) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from typing import dataclass_transform

              @dataclass_transform(eq_default=False)
              def mytransform():
                ...

              @mytransform(order=True)
              class Foo:
                x: int
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  x: int
                  def __init__(self, x: int) -> None:
                    self.x = x
                  def __lt__(self, o: Foo) -> bool: ...
                  def __le__(self, o: Foo) -> bool: ...
                  def __gt__(self, o: Foo) -> bool: ...
                  def __ge__(self, o: Foo) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from typing import dataclass_transform

              @dataclass_transform(eq_default=False)
              def mytransform():
                ...

              @mytransform(init=False)
              class Foo:
                x: int
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  x: int
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from typing import dataclass_transform

              @dataclass_transform(order_default=True)
              def mytransform():
                ...

              @mytransform()
              class Foo:
                x: int
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  x: int
                  def __init__(self, x: int) -> None:
                    self.x = x
                  def __eq__(self, o: object) -> bool: ...
                  def __lt__(self, o: Foo) -> bool: ...
                  def __le__(self, o: Foo) -> bool: ...
                  def __gt__(self, o: Foo) -> bool: ...
                  def __ge__(self, o: Foo) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from typing import dataclass_transform

              @dataclass_transform(order_default=True)
              def mytransform():
                ...

              @mytransform(eq=False)
              class Foo:
                x: int
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  x: int
                  def __init__(self, x: int) -> None:
                    self.x = x
                  def __lt__(self, o: Foo) -> bool: ...
                  def __le__(self, o: Foo) -> bool: ...
                  def __gt__(self, o: Foo) -> bool: ...
                  def __ge__(self, o: Foo) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from typing import dataclass_transform

              @dataclass_transform(order_default=True)
              def mytransform():
                ...

              @mytransform(order=False)
              class Foo:
                x: int
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  x: int
                  def __init__(self, x: int) -> None:
                    self.x = x
                  def __eq__(self, o: object) -> bool: ...
              |};
      (* TODO(T129464224) Fix kw_only / kw_only_default support in dataclass transforms *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from typing import dataclass_transform

              @dataclass_transform
              def mytransform(kw_only: bool = False):
                ...

              @mytransform(kw_only=True, eq=False)
              class Foo:
                x: int
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  x: int
                  # OOPS! Here `x` should be keyword-only
                  def __init__(self, *, x: int) -> None:
                    self.x = x
              |};
      (* TODO(T129464224) Fix kw_only / kw_only_default support in dataclass transforms *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from typing import dataclass_transform

              @dataclass_transform(kw_only_default=True, eq_default=False)
              def mytransform(kw_only: bool = True):
                ...

              @mytransform(eq=False)
              class Foo:
                x: int
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  x: int
                  # OOPS! Here `x` should be keyword-only
                  def __init__(self, *, x: int) -> None:
                    self.x = x
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from typing import dataclass_transform

              @dataclass_transform(order_default=True)
              def mytransform():
                ...

              @mytransform(init=False)
              class Foo:
                x: int
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  x: int
                  def __eq__(self, o: object) -> bool: ...
                  def __lt__(self, o: Foo) -> bool: ...
                  def __le__(self, o: Foo) -> bool: ...
                  def __gt__(self, o: Foo) -> bool: ...
                  def __ge__(self, o: Foo) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              def myfield(
                *,
                default: Optional[Any] = ...,
                default_factory: Optional[Callable[[], Any]] = ...,
                factory: Optional[Callable[[], Any]] = ...,
                init: bool = True,
              ):
                ...

              from typing import dataclass_transform

              @dataclass_transform(field_descriptors=(myfield,))
              def mytransform():
                ...

              @mytransform
              class Foo:
                x1: int = myfield(init=False)
                x2: int = myfield(init=True)
                x3: int = myfield(init=True, default=1)
                x4: int = myfield(init=True, default_factory=foo)
                x5: int = myfield(init=True, factory=foo)
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  x1: int = myfield(init=False)
                  x2: int = myfield(init=True)
                  x3: int = myfield(init=True, default=1)
                  x4: int = myfield(init=True, default_factory=foo)
                  x5: int = myfield(init=True, factory=foo)
                  def __init__(self, x2: int, x3: int = 1, x4: int = foo(), x5: int = foo()) -> None:
                    self.x2 = x2
                    self.x3 = x3
                    self.x4 = x4
                    self.x5 = x5
                  def __eq__(self, o: object) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from typing import dataclass_transform

              @dataclass_transform
              class Bar:
                ...

              class Foo(Bar):
                x: int
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  x: int
                  def __init__(self, x: int) -> None:
                    self.x = x
                  def __eq__(self, o: object) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from typing import dataclass_transform

              @dataclass_transform
              class Bar:
                def __init_subclass__(
                    cls,
                    *,
                    init: bool = True,
                    frozen: bool = False,
                    eq: bool = True,
                    order: bool = False,
                ) -> None: ...

              class Foo(Bar, eq=False, order=True):
                x: int
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  x: int
                  def __init__(self, x: int) -> None:
                    self.x = x
                  def __lt__(self, o: Foo) -> bool: ...
                  def __le__(self, o: Foo) -> bool: ...
                  def __gt__(self, o: Foo) -> bool: ...
                  def __ge__(self, o: Foo) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from typing import dataclass_transform

              @dataclass_transform(eq_default=False, order_default=True)
              class Bar:
                def __init_subclass__(
                    cls,
                    *,
                    init: bool = True,
                    frozen: bool = False,
                    eq: bool = False,
                    order: bool = True,
                ) -> None: ...

              class Foo(Bar):
                x: int
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  x: int
                  def __init__(self, x: int) -> None:
                    self.x = x
                  def __lt__(self, o: Foo) -> bool: ...
                  def __le__(self, o: Foo) -> bool: ...
                  def __gt__(self, o: Foo) -> bool: ...
                  def __ge__(self, o: Foo) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from typing import dataclass_transform

              @dataclass_transform(eq_default=False, order_default=True)
              class Bar:
                def __init_subclass__(
                    cls,
                    *,
                    init: bool = True,
                    frozen: bool = False,
                    eq: bool = False,
                    order: bool = True,
                ) -> None: ...

              class Foo(Bar, eq=True, order=False):
                x: int
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  x: int
                  def __init__(self, x: int) -> None:
                    self.x = x
                  def __eq__(self, o: object) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~external_sources:
             [
               ( "custom_dataclass_transform.py",
                 {|
                   from typing import dataclass_transform

                   @dataclass_transform(eq_default=False, order_default=True)
                   class Bar:
                     def __init_subclass__(
                         cls,
                         *,
                         init: bool = True,
                         frozen: bool = False,
                         eq: bool = False,
                         order: bool = True,
                     ) -> None: ...
                 |}
               );
             ]
           ~source:
             {|
              from custom_dataclass_transform import Bar

              class Foo(Bar, eq=True, order=False):
                x: int
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  x: int
                  def __init__(self, x: int) -> None:
                    self.x = x
                  def __eq__(self, o: object) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~external_sources:
             [
               ( "custom_dataclass_transform.py",
                 {|
                  from typing import dataclass_transform

                  @dataclass_transform
                  def mytransform():
                    ...

                |}
               );
             ]
           ~source:
             {|
              from custom_dataclass_transform import mytransform

              @mytransform
              class Foo:
                x: int
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  x: int
                  def __init__(self, x: int) -> None:
                    self.x = x
                  def __eq__(self, o: object) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~external_sources:
             [
               ( "custom_dataclass_transform.py",
                 {|
              from typing import Literal, overload, dataclass_transform

              @overload
              def overloaded_transform(eq: Literal[false]): Any

              @overload
              def overloaded_transform(eq: Literal[true]): Any

              @dataclass_transform
              def overloaded_transform():
                ...

            |}
               );
             ]
           ~source:
             {|
              from custom_dataclass_transform import overloaded_transform

              @overloaded_transform
              class Foo:
                x: int
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  x: int
                  def __init__(self, x: int) -> None:
                    self.x = x
                  def __eq__(self, o: object) -> bool: ...
              |};
      (* Verify that we handle the typing_extensions and legacy variants. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes_3_10
           ~source:
             {|
              from typing_extensions import dataclass_transform

              @dataclass_transform
              def mytransform():
                ...

              @mytransform
              class Foo:
                x: int
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  x: int
                  def __init__(self, x: int) -> None:
                    self.x = x
                  def __eq__(self, o: object) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes_3_10
           ~source:
             {|
              def __dataclass_transform__(): ...

              @__dataclass_transform__
              def mytransform():
                ...

              @mytransform
              class Foo:
                x: int
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  x: int
                  def __init__(self, x: int) -> None:
                    self.x = x
                  def __eq__(self, o: object) -> bool: ...
              |};
    ]


let test_keyword_only =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass
              @dataclass(kw_only=True, repr=False, eq=False, match_args=False)
              class A:
                x: int
                y: int
                z: int
            |}
           ~class_name:"A"
           {|
              class A:
                def __init__(self, *, x: int, y: int, z: int) -> None:
                  self.x = x
                  self.y = y
                  self.z = z
            |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass
              @dataclass(kw_only=True ,repr=False, eq=False, match_args=False)
              class A:
                x: int
                y: int
            |}
           ~class_name:"A"
           {|
              class A:
                x: int
                y: int
                def __init__(self, *, x: int, y: int) -> None:
                  self.x = x
                  self.y = y
            |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass, field
              @dataclass(repr=False, eq=False, match_args=False)
              class Base:
                x: int

              @dataclass(kw_only=True ,repr=False, eq=False, match_args=False)
              class A(Base):
                x: int
            |}
           ~class_name:"A"
           {|
              class A:
                x: int
                def __init__(self, *, x: int) -> None:
                  self.x = x
            |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass, field
              @dataclass(kw_only=True ,repr=False, eq=False, match_args=False)
              class Base:
                x: int

              @dataclass(repr=False, eq=False, match_args=False)
              class A(Base):
                x: int
            |}
           ~class_name:"A"
           {|
              class A:
                x: int
                def __init__(self, x: int) -> None:
                  self.x = x
            |};
      (* TODO(T129344236) Fix inheritance for dataclass fields *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass, field
              @dataclass(kw_only=True, repr=False, eq=False, match_args=False)
              class Base:
                x: int

              @dataclass(repr=False, eq=False, match_args=False)
              class A(Base):
                y: int
                z: int
            |}
           ~class_name:"A"
           {|
              class A(Base):
                y: int
                z: int
                def __init__(self, x: int, y: int, z: int) -> None:
                  self.y = y
                  self.z = z
            |};
      (* TODO(T129344236) Fix inheritance for dataclass fields *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass, field
              @dataclass(repr=False, eq=False, match_args=False)
              class Base:
                x: int

              @dataclass(kw_only=True, repr=False, eq=False, match_args=False)
              class A(Base):
                y: int
                z: int
            |}
           ~class_name:"A"
           {|
              class A(Base):
                y: int
                z: int
                def __init__(self, *, x: int, y: int, z: int) -> None:
                  self.y = y
                  self.z = z
            |};
      (* TODO(T129344236) Fix inheritance for dataclass fields *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass, field
              @dataclass(kw_only=True ,repr=False, eq=False, match_args=False)
              class Base:
                x: int

              @dataclass(repr=False, eq=False, match_args=False)
              class A(Base):
                y: int
                z: int
            |}
           ~class_name:"A"
           {|
              class A(Base):
                y: int
                z: int
                def __init__(self, x: int, y: int, z: int) -> None:
                  self.y = y
                  self.z = z
            |};
    ]


let test_keyword_only_fields =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass, field
              @dataclass(repr=False, eq=False, match_args=False)
              class A:
                x: int = field(kw_only=True)
                y: int = field(kw_only=True)
            |}
           ~class_name:"A"
           {|
              class A:
                x: int = field(kw_only=True)
                y: int = field(kw_only=True)
                def __init__(self, *, x: int, y: int) -> None:
                  self.x = x
                  self.y = y
            |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass, field
              @dataclass(repr=False, eq=False, match_args=False)
              class A:
                x: int
                y: int = field(kw_only=True)
            |}
           ~class_name:"A"
           {|
              class A:
                x: int
                y: int = field(kw_only=True)
                def __init__(self, x: int, *, y: int) -> None:
                  self.x = x
                  self.y = y
            |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass, field
              @dataclass(repr=False, eq=False, match_args=False)
              class A:
                x: int = field(kw_only=True)
                y: int
            |}
           ~class_name:"A"
           {|
              class A:
                x: int = field(kw_only=True)
                y: int
                def __init__(self, y: int, *, x: int) -> None:
                  self.x = x
                  self.y = y
            |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass, field
              @dataclass(kw_only=True ,repr=False, eq=False, match_args=False)
              class A:
                x: int = field(kw_only=True)
                y: int
            |}
           ~class_name:"A"
           {|
              class A:
                x: int = field(kw_only=True)
                y: int
                def __init__(self, *, x: int, y: int) -> None:
                  self.x = x
                  self.y = y
            |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass, field
              @dataclass(kw_only=True ,repr=False, eq=False, match_args=False)
              class A:
                x: int = field(kw_only=False)
                y: int
            |}
           ~class_name:"A"
           {|
              class A:
                x: int = field(kw_only=False)
                y: int
                def __init__(self, x: int, *, y: int) -> None:
                  self.x = x
                  self.y = y
            |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass, field
              @dataclass(kw_only=True ,repr=False, eq=False, match_args=False)
              class A:
                x: int
                y: int = field(kw_only=False)
            |}
           ~class_name:"A"
           {|
              class A:
                x: int
                y: int = field(kw_only=True)
                def __init__(self, y: int, *, x: int) -> None:
                  self.x = x
                  self.y = y
            |};
    ]


let test_preprocessing =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass, field, KW_ONLY
              @dataclass(repr=False, eq=False, match_args=False)
              class A:
                x: int = field(kw_only=True)
                _: KW_ONLY
                y: int
            |}
           ~class_name:"A"
           {|
              class A:
                x: int = field(kw_only=True)
                y: int = field(kw_only=True)
                def __init__(self, *, x: int, y: int = ...) -> None:
                  self.x = x
                  self.y = y
            |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass, field, KW_ONLY, InitVar
              @dataclass(repr=False, eq=False, match_args=False)
              class A:
                x: InitVar[int] = field(kw_only=True)
                _: KW_ONLY
                y: InitVar[int]
            |}
           ~class_name:"A"
           {|
              class A:
                x: dataclasses.InitVar[int] = field(kw_only=True)
                y: dataclasses.InitVar[int] = field(kw_only=True)
                def __init__(self, *, x: int, y: int = ...) -> None:
                  self.x = x
                  self.y = y
            |};
    ]


let test_slots =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass

              @dataclass(slots=True, repr=False, eq=False, match_args=False)
              class Foo:
                x: int
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  __slots__: typing.Tuple[str] = ('x',)
                  x: int
                  def __init__(self, x: int) -> None:
                    self.x = x
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass

              @dataclass(slots=True, repr=False, eq=False, match_args=False)
              class Foo:
                x: int
                y: int
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  __slots__: typing.Tuple[str, str] = ('x', 'y')
                  x: int
                  def __init__(self, x: int, y: int) -> None:
                    self.x = x
                    self.y = y
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass

              @dataclass(slots=True)
              class Base:
                y: int

              @dataclass(slots=True, repr=False, eq=False, match_args=False)
              class Foo(Base):
                x: int
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  __slots__: typing.Tuple[str, str] = ('y', 'x')
                  x: int
                  def __init__(self, y: int, x: int) -> None:
                    self.x = x
              |};
      (* TODO(T130663259) fix inheritance for dataclasses *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from dataclasses import dataclass

              @dataclass(slots=True)
              class Base:
                y: int

              @dataclass(slots=False, repr=False, eq=False, match_args=False)
              class Foo(Base):
                x: int
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  x: int
                  def __init__(self, y: int, x: int) -> None:
                    self.x = x
              |};
    ]


let () =
  "dataClass"
  >::: [
         test_transform_environment;
         test_match_args;
         test_dataclass_transform;
         test_keyword_only;
         test_keyword_only_fields;
         test_preprocessing;
         test_slots;
       ]
  |> Test.run
