(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Test
open IntegrationTest

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

                  def __post_init__(self, x: int, y: str) -> None:
                    pass
                  __match_args__ = ("a", "b", "c", "x", "y")
                  def __init__(self, a: int, b:int, c: int, x: int, y: str) -> None:
                    self.a = a
                    self.b = b
                    self.c = c

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
                  def __init__(self, x: int, init_variable: str) -> None:
                    self.x = x
                  def __repr__(self) -> str: ...
                  def __eq__(self, o: object) -> bool: ...
              |};
      (* TODO T178998636: investigate this testcase *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
                from typing import Callable, Literal, Optional, overload, dataclass_transform

                @overload
                def field2(
                    *,
                    resolver: Callable[[], int],
                    init: Literal[False] = False,
                ) -> int:
                    ...
                @overload
                def field2(
                    *,
                    resolver: None = None,
                    init: Literal[True] = True,
                ) -> int:
                    ...
                def field2(
                    *,
                    resolver: Optional[Callable[[], int]] = None,
                    init: bool = True,
                ) -> int:
                    ...


                @dataclass_transform(field_specifiers=(field2,))
                def create_model(*, init: bool = True) -> None:
                    pass

                @create_model
                class CustomerModel:
                    id: int = field2(resolver=lambda: 0)
         |}
           ~class_name:"CustomerModel"
           {|
              class CustomerModel:
                  id: int = dataclasses.field2()
                  def __init__(self, id: int) -> None: ...
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

              @dataclass_transform(field_specifiers=(myfield,))
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
                def __init__(self, *, x: int, y: int = ...) -> None:
                  ...
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


let test_check_dataclasses =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      (* TODO T178998636: At minimum, Unexpected keyword [28] should not happen *)
      @@ assert_type_errors
           {|
              from typing import dataclass_transform, Any, TypeVar, Type
              from dataclasses import dataclass
              T = TypeVar("T")

              @dataclass_transform()
              def custom_dataclass(cls: Type[T]) -> Type[T]:
                  return dataclass(cls, frozen=True)

              @custom_dataclass
              class A:
                  x: int
              a = A(x=10)
         |}
           [
             "Invalid decoration [56]: Decorator `typing.dataclass_transform(...)` could not be \
              called, because its type `unknown` is not callable.";
             "Unexpected keyword [28]: Unexpected keyword argument `frozen` to call `dataclass`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
            from typing import dataclass_transform, Any, TypeVar, Type
            from dataclasses import dataclass
            T = TypeVar("T")

            @dataclass_transform(frozen_default=True)
            def custom_dataclass(cls: type[T]) -> type[T]:
                return dataclass(cls)

            @custom_dataclass
            class Foo:
                x: int

            a = Foo(x=10)
            a.x = 20
         |}
           [
             "Invalid decoration [56]: Decorator `typing.dataclass_transform(...)` could not be \
              called, because its type `unknown` is not callable.";
             "Invalid assignment [41]: Cannot reassign final attribute `a.x`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
            from dataclasses import dataclass
            @dataclass(frozen=True)
            class Foo():
                x: int = 1

            @dataclass(frozen=False)
            class Bar(Foo):
                y : int = 3

            b = Bar()
            b.x = 5
            b.y = 4
         |}
           [
             "Invalid inheritance [39]: Non-frozen dataclass `Bar` cannot inherit from frozen \
              dataclass `Foo`.";
             "Invalid assignment [41]: Cannot reassign final attribute `b.x`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
            from dataclasses import dataclass
            class Foo():
                x: int = 1

            @dataclass(frozen=True)
            class Bar(Foo):
                y : int = 3
         |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
            from typing import dataclass_transform

            @dataclass_transform(frozen_default=True)
            class ModelBaseFrozen:
              pass

            class Customer3(ModelBaseFrozen):
                id: int

            c3_1 = Customer3(id=2)

            # This should generate an error because Customer3 is frozen.
            c3_1.id = 4
         |}
           ["Invalid assignment [41]: Cannot reassign final attribute `c3_1.id`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
            from dataclasses import dataclass
            from typing import assert_type, ClassVar, Final

            @dataclass
            class D:
                final_classvar: ClassVar[Final[int]] = 4
            D.final_classvar = 10
         |}
           ["Invalid assignment [41]: Cannot reassign final attribute `D.final_classvar`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
           from dataclasses import dataclass
           @dataclass
           class Foo():
             x: int = 1
           def boo() -> None:
               b = Foo('a')
         |}
           [
             "Incompatible parameter type [6]: In call `Foo.__init__`, for 1st positional \
              argument, expected `int` but got `str`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
           from dataclasses import dataclass
           @dataclass
           class Foo():
             x: int = 1
           def boo() -> None:
               b = Foo(4,5)
         |}
           [
             "Too many arguments [19]: Call `Foo.__init__` expects 1 positional argument, "
             ^ "2 were provided.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
           import dataclasses
           @dataclasses.dataclass
           class Foo():
             x: int = 1
           def boo() -> None:
               b = Foo(4,5)
         |}
           [
             "Too many arguments [19]: Call `Foo.__init__` expects 1 positional argument, "
             ^ "2 were provided.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
           from dataclasses import dataclass
           @dataclass
           class Foo():
             x = 1
           def boo() -> None:
               b = Foo(2)
         |}
           [
             "Missing attribute annotation [4]: Attribute `x` of class `Foo` has type `int` "
             ^ "but no type is specified.";
             "Too many arguments [19]: Call `Foo.__init__` expects 0 positional arguments, 1 was"
             ^ " provided.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
           from dataclasses import dataclass
           @dataclass
           class Foo():
             x: int = 1
           def boo() -> None:
               b = Foo()
         |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
           from dataclasses import dataclass
           @dataclass
           class Foo():
             dangan: int
           def boo() -> None:
               b = Foo(1)
         |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
           from dataclasses import dataclass
           @dataclass
           class Base():
             x: int
           class Child(Base):
             pass
           def boo() -> None:
               b = Child(1)
         |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
           from dataclasses import dataclass
           from typing import Dict, Any
           @dataclass(frozen=False)
           class Base:
             x: str

             def __init__(
                 self,
                 *,
                 x: str,
             ) -> None:
                 self.x = x

             def as_dict(self) -> Dict[str, Any]:
                 x = self.x
                 return {
                     "x": x,
                 }
         |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
           from dataclasses import dataclass
           @dataclass(frozen=True)
           class Base:
             x: float
           @dataclass(frozen=True)
           class Child(Base):
             x: int = 1
             y: str
         |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
           from dataclasses import dataclass
           @dataclass(frozen=True)
           class F:
             x = 1
         |}
           [
             "Missing attribute annotation [4]: Attribute `x` of class `F` has type `int` but no \
              type is specified.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
           from typing import ClassVar
           from dataclasses import dataclass
           @dataclass
           class A:
             x: ClassVar[int] = 42
             y: str = "a"
           A("a")
         |}
           [];
      (* Actually a test of descriptors to make sure it doesn't infinitely loop *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
           from dataclasses import dataclass
           @dataclass
           class D:
             x: C = C()
           @dataclass
           class C:
             x: D = D()
           def foo() -> None:
             reveal_type(D().x)
             reveal_type(D().x.x.x)
         |}
           [
             "Revealed type [-1]: Revealed type for `test.D().x` is `C`.";
             "Revealed type [-1]: Revealed type for `test.D().x.x.x` is `C`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
           from dataclasses import dataclass
           @dataclass(kw_only=True)
           class A:
             x: int
           reveal_type(A.__init__)
         |}
           [
             "Revealed type [-1]: Revealed type for `test.A.__init__` is \
              `typing.Callable(A.__init__)[[Named(self, A), KeywordOnly(x, int)], None]`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
           from dataclasses import dataclass, KW_ONLY
           @dataclass
           class A:
             x: int
             _: KW_ONLY
             y: int
           reveal_type(A.__init__)
         |}
           [
             "Revealed type [-1]: Revealed type for `test.A.__init__` is \
              `typing.Callable(A.__init__)[[Named(self, A), Named(x, int), KeywordOnly(y, int, \
              default)], None]`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
           from dataclasses import dataclass
           @dataclass(frozen=True)
           class A:
             x: int
           a = A(x=42)
           a.x = 43
         |}
           ["Invalid assignment [41]: Cannot reassign final attribute `a.x`."];
      (* TODO(T178998636) Find out why we have two "Undefined attribute" errors *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
           from dataclasses import dataclass, InitVar
           @dataclass
           class A:
             x: int
             y: InitVar[int]

           a = A(x=42, y=42)
           reveal_type(a.x)
           reveal_type(a.y)
         |}
           [
             "Undefined attribute [16]: `typing.Type` has no attribute `y`.";
             "Revealed type [-1]: Revealed type for `a.x` is `int`.";
             "Revealed type [-1]: Revealed type for `a.y` is `unknown`.";
             "Undefined attribute [16]: `A` has no attribute `y`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
           from dataclasses import dataclass, InitVar, field
           @dataclass
           class A:
             x: int
             y: InitVar[int] = field(kw_only=True)

           a = A(x=42, y=42)
           reveal_type(a.x)
           reveal_type(a.y)
         |}
           [
             "Undefined attribute [16]: `typing.Type` has no attribute `y`.";
             "Revealed type [-1]: Revealed type for `a.x` is `int`.";
             "Revealed type [-1]: Revealed type for `a.y` is `unknown`.";
             "Undefined attribute [16]: `A` has no attribute `y`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      (* TODO: Investigate the error "Undefined attribute [16]: `typing.Type` has no attribute
         `y`." *)
      @@ assert_type_errors
           {|
           from dataclasses import dataclass, InitVar, field
           @dataclass
           class A:
             x: int
             y: InitVar[int] = field(default=42)

           a = A(x=42, y=42)
           reveal_type(a.x)
           reveal_type(a.y)
         |}
           [
             "Undefined attribute [16]: `typing.Type` has no attribute `y`.";
             "Revealed type [-1]: Revealed type for `a.x` is `int`.";
             "Revealed type [-1]: Revealed type for `a.y` is `unknown`.";
             "Undefined attribute [16]: `A` has no attribute `y`.";
           ];
      (* TODO(T178998636) Pyright gives the error 'No parameter named id' on this code. We should
         also error out *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
            from typing import Callable, Literal, Optional, overload, dataclass_transform

            @overload
            def field(
                *,
                resolver: Callable[[], int],
                init: Literal[False] = False,
            ) -> int:
                ...
            @overload
            def field(
                *,
                resolver: None = None,
                init: Literal[True] = True,
            ) -> int:
                ...
            def field(
                *,
                resolver: Optional[Callable[[], int]] = None,
                init: bool = True,
            ) -> int:
                ...


            @dataclass_transform(field_specifiers=(field,))
            def create_model(*, init: bool = True) -> None:
                pass

            @create_model
            class CustomerModel:
                id: int = field(resolver=lambda: 0)
            # This should generate an error because "id" is not
            # supposed to be part of the init function.
            CustomerModel(id=1)  # Error
         |}
           [
             "Invalid decoration [56]: Decorator `typing.dataclass_transform(...)` could not be \
              called, because its type `unknown` is not callable.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
            from typing import Callable, Literal, Optional, overload, dataclass_transform

            @overload
            def field(
                *,
                resolver: Callable[[], int],
                init: Literal[True] = True,
            ) -> int:
                ...
            @overload
            def field(
                *,
                resolver: None = None,
                init: Literal[True] = True,
            ) -> int:
                ...
            def field(
                *,
                resolver: Optional[Callable[[], int]] = None,
                init: bool = True,
            ) -> int:
                ...


            @dataclass_transform(field_specifiers=(field,))
            def create_model(*, init: bool = True) -> None:
                pass

            @create_model
            class CustomerModel:
                id: int = field(resolver=lambda: 0)
            # This should generate an error because "id" is not
            # supposed to be part of the init function.
            CustomerModel(id=1)  # No Error
         |}
           [
             "Invalid decoration [56]: Decorator `typing.dataclass_transform(...)` could not be \
              called, because its type `unknown` is not callable.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
            from typing import Callable, Optional, dataclass_transform

            def field(
                *,
                resolver: Optional[Callable[[], int]] = None,
                init: bool = True,
            ) -> int:
                ...

            @dataclass_transform(field_specifiers=(field,))
            def create_model(*, init: bool = True) -> None:
                pass

            @create_model
            class CustomerModel:
                id: int = field(init=False)
            # This should generate an error because "id" is not
            # supposed to be part of the init function.
            CustomerModel(id=1)  # Error
         |}
           [
             "Invalid decoration [56]: Decorator `typing.dataclass_transform(...)` could not be \
              called, because its type `unknown` is not callable.";
             "Unexpected keyword [28]: Unexpected keyword argument `id` to call \
              `CustomerModel.__init__`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
           from dataclasses import dataclass
           @dataclass(frozen=False)
           class A:
             x: int
           @dataclass(frozen=False)
           class B(A):
             y: str
           @dataclass(frozen=True)
           class C(A):
             y: str

         |}
           [
             "Invalid inheritance [39]: Frozen dataclass `C` cannot inherit from non-frozen \
              dataclass `A`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
           from dataclasses import dataclass
           @dataclass(frozen=True)
           class A:
             x: int
           @dataclass(frozen=False)
           class B(A):
             y: str
           @dataclass(frozen=False)
           class C(A):
             y: str

         |}
           [
             "Invalid inheritance [39]: Non-frozen dataclass `B` cannot inherit from frozen \
              dataclass `A`.";
             "Invalid inheritance [39]: Non-frozen dataclass `C` cannot inherit from frozen \
              dataclass `A`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
            from typing import dataclass_transform

            @dataclass_transform(frozen_default=False)
            class Bar:
                def __init_subclass__(
                    cls,
                    *,
                    init: bool = True,
                    frozen: bool = False,
                ) -> None: ...

            class Foo(Bar, frozen=False):
                pass

            class Baz(Foo, frozen=True):
                pass
         |}
           [
             "Invalid inheritance [39]: Frozen dataclass `Baz` cannot inherit from non-frozen \
              dataclass `Foo`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
            from typing import Callable
            import dataclasses
            @dataclasses.dataclass(frozen=False)
            class C:
                matcher: Callable[[str], bool] = dataclasses.field()
                def matches(self) -> bool:
                    reveal_type(self.matcher)
                    return self.matcher("test")

         |}
           [
             "Revealed type [-1]: Revealed type for `self.matcher` is `typing.Callable[[str], \
              bool]`.";
           ];
      (* TODO: T190778258 The only type error here should be that that default value cannot be
         followed by non-default value*)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
            import dataclasses
            from typing import Callable, assert_type

            @dataclasses.dataclass  # E[DC1]
            class DC1:
                a: dataclasses.InitVar[int] = 0
                b: int  # E[DC1]: field with no default cannot follow field with default.

            def f(s: str) -> int:
                return int(s)

            @dataclasses.dataclass
            class DC6:
                c: Callable[[str], int] = f

            dc6 = DC6()
            assert_type(dc6.c, Callable[[str], int])
         |}
           ["Undefined attribute [16]: `typing.Type` has no attribute `a`."];
      (* TODO: T190780655 Report when dataclasses are not compatible with hashable protocol *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
            from dataclasses import dataclass
            from typing import Hashable


            @dataclass
            class DC1:
                a: int
            # This should generate an error because DC1 isn't hashable.
            v1: Hashable = DC1(0)  # E
         |}
           [];
      (* TODO: T190780655 The error here should be that y is missing from the __post_init__ argument
         list. We do not report anything about that. The existing errors are also incorrect. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from dataclasses import dataclass, InitVar

              @dataclass
              class DC1:
                  x: InitVar[int]
                  y: InitVar[str]

                  def __post_init__(self, x: int) -> None:
                      pass

         |}
           [
             "Undefined attribute [16]: `typing.Type` has no attribute `x`.";
             "Undefined attribute [16]: `typing.Type` has no attribute `y`.";
           ];
      (* TODO: T190786456 We should not produce a type error here since Desc1 has a setter which
         takes an int. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
            from dataclasses import dataclass
            from typing import Any, Generic, TypeVar, assert_type, overload

            T = TypeVar("T")

            class Desc1:

                def __set__(self, __obj: object, __value: int) -> None:
                    ...

            @dataclass
            class DC1:
                y: Desc1 = Desc1()

            dc1 = DC1(3)

         |}
           [
             "Incompatible parameter type [6]: In call `DC1.__init__`, for 1st positional \
              argument, expected `Desc1` but got `int`.";
           ];
      (* TODO: T190786456 we do not understand that the implementation of create_model is also a
         dataclass_transform. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|

            from typing import Callable, TypeVar, dataclass_transform, overload, Optional, Type, Union
            T = TypeVar('T')
            @overload
            @dataclass_transform()
            def create_model(cls: Type[T]) -> Callable[[], T]:
                ...


            @overload
            @dataclass_transform()
            def create_model() -> Callable[[Type[T]], Callable[[], T]]:
                ...


            def create_model(cls: Optional[Type[T]] = None) -> Union[Callable[[], T], Callable[[Type[T]], Callable[[], T]]]:
                ...

         |}
           [
             "Invalid type variable [34]: The type variable `Variable[T]` isn't present in the \
              function's parameters.";
             "Incompatible overload [43]: This definition does not have the same decorators as the \
              preceding overload(s).";
           ];
      (* TODO(@stroxler) Fix validation of frozen fields for parametric types. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
          from typing import Generic, TypeVar
          from dataclasses import dataclass

          T = TypeVar("T")

          @dataclass(frozen=True)
          class Base(Generic[T]):
            x: T

          @dataclass
          class Foo(Base[int]):
            y: int
        |}
           [
             "Invalid inheritance [39]: Non-frozen dataclass `Foo` cannot inherit from frozen \
              dataclass `Base`.";
           ];
    ]


let test_check_attrs =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           ~other_sources:
             [
               {
                 handle = "attr/__init__.pyi";
                 source =
                   {|
                    import typing
                    _T = typing.TypeVar("T")
                    class Attribute(typing.Generic[_T]):
                      name: str
                      default: Optional[_T]
                      validator: Optional[_ValidatorType[_T]]
                    def s( *args, **kwargs) -> typing.Any: ...
                    def ib(default: _T) -> _T: ...
                  |};
               };
             ]
           {|
           import typing
           import attr
           @attr.s
           class C:
             x: typing.Optional[int] = attr.ib(default=None)
             @x.validator
             def check(self, attribute: attr.Attribute[int], value: typing.Optional[int]) -> None:
               pass
         |}
           [];
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
         test_check_dataclasses;
         test_check_attrs;
       ]
  |> Test.run
