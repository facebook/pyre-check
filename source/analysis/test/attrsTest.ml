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
              import attr
              @attr.s
              class Foo:
                ...
            |}
           ~class_name:"Foo"
           {|
                class Foo:
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
              import attr
              if 1 > 2:
                @attr.s
                class Foo:
                  ...
            |}
           ~class_name:"Foo"
           {|
                class Foo:
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
              import attr
              @attr.s
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
                  def __lt__(self, o: Foo) -> bool: ...
                  def __le__(self, o: Foo) -> bool: ...
                  def __gt__(self, o: Foo) -> bool: ...
                  def __ge__(self, o: Foo) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              import attr
              @attr.s
              class Foo:
                def __init__(self) -> None: ...
                def __repr__(self) -> str: ...
            |}
           ~class_name:"Foo"
           {|
              # spacer
              class Foo:
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
              import attr
              @attr.s
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
                  def __lt__(self, o: Foo) -> bool: ...
                  def __le__(self, o: Foo) -> bool: ...
                  def __gt__(self, o: Foo) -> bool: ...
                  def __ge__(self, o: Foo) -> bool: ...
              |};
      (* Boolean arguments *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              import attr
              @attr.s(init = False)
              class Foo:
                def foo(self) -> None: ...
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  def foo(self) -> None: ...
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
              import attr
              @attr.s(repr = False)
              class Foo:
                def foo(self) -> None: ...
            |}
           ~class_name:"Foo"
           {|
                class Foo:
                  def foo(self) -> None: ...
                  def __init__(self) -> None: ...
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
              import attr
              @attr.s(cmp = False)
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
              import attr
              @attr.s(auto_attribs = True)
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
              import attr
              @attr.s(kw_only=True)
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
                def __repr__(self) -> str: ...
                def __eq__(self, o: object) -> bool: ...
                def __lt__(self, o: A) -> bool: ...
                def __le__(self, o: A) -> bool: ...
                def __gt__(self, o: A) -> bool: ...
                def __ge__(self, o: A) -> bool: ...
            |};
      (* TODO(T129741558) Support typed syntax in attr *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from attr import field
              import attr
              @attr.s
              class A:
                x: int = field(kw_only=True)
            |}
           ~class_name:"A"
           {|
              class A:
                x: int = field(kw_only=True)
                def __init__(self, x: int = ...) -> None:
                  self.x = x
                def __repr__(self) -> str: ...
                def __eq__(self, o: object) -> bool: ...
                def __lt__(self, o: A) -> bool: ...
                def __le__(self, o: A) -> bool: ...
                def __gt__(self, o: A) -> bool: ...
                def __ge__(self, o: A) -> bool: ...
            |};
      (* Inheritance *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
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
                  def __init__(self, x: int = 15, y: int = 0, z: int = 10) -> None: ...
                  def __repr__(self) -> str: ...
                  def __eq__(self, o: object) -> bool: ...
                  def __lt__(self, o: C) -> bool: ...
                  def __le__(self, o: C) -> bool: ...
                  def __gt__(self, o: C) -> bool: ...
                  def __ge__(self, o: C) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
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
                  def __init__(self, x: int = 15, y: int = 0, z: int = 10) -> None: ...
                  def __repr__(self) -> str: ...
                  def __eq__(self, o: object) -> bool: ...
                  def __lt__(self, o: C) -> bool: ...
                  def __le__(self, o: C) -> bool: ...
                  def __gt__(self, o: C) -> bool: ...
                  def __ge__(self, o: C) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
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
                  def __repr__(self) -> str: ...
                  def __eq__(self, o: object) -> bool: ...
                  def __lt__(self, o: C) -> bool: ...
                  def __le__(self, o: C) -> bool: ...
                  def __gt__(self, o: C) -> bool: ...
                  def __ge__(self, o: C) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
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
                  def __init__(self, x: int = 15, y: int = 0, z: int = 10) -> None: ...
                  def __repr__(self) -> str: ...
                  def __eq__(self, o: object) -> bool: ...
                  def __lt__(self, o: C) -> bool: ...
                  def __le__(self, o: C) -> bool: ...
                  def __gt__(self, o: C) -> bool: ...
                  def __ge__(self, o: C) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
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
                  def __init__(self, x: int = 15, y: int = 5, z: int = 10) -> None: ...
                  def __repr__(self) -> str: ...
                  def __eq__(self, o: object) -> bool: ...
                  def __lt__(self, o: C1) -> bool: ...
                  def __le__(self, o: C1) -> bool: ...
                  def __gt__(self, o: C1) -> bool: ...
                  def __ge__(self, o: C1) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
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
                  def __init__(self, y: int = 5) -> None: ...
                  def __repr__(self) -> str: ...
                  def __eq__(self, o: object) -> bool: ...
                  def __lt__(self, o: WithAttr) -> bool: ...
                  def __le__(self, o: WithAttr) -> bool: ...
                  def __gt__(self, o: WithAttr) -> bool: ...
                  def __ge__(self, o: WithAttr) -> bool: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              import attr
              @attr.s
              class Base:
                x: int

              @attr.s(kw_only=True)
              class A(Base):
                x: int
            |}
           ~class_name:"A"
           {|
              class A:
                x: int
                def __init__(self, *, x: int) -> None:
                  self.x = x
                def __repr__(self) -> str: ...
                def __eq__(self, o: object) -> bool: ...
                def __lt__(self, o: A) -> bool: ...
                def __le__(self, o: A) -> bool: ...
                def __gt__(self, o: A) -> bool: ...
                def __ge__(self, o: A) -> bool: ...
            |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from attr import field
              import attr
              @attr.s(kw_only=True)
              class Base:
                x: int

              @attr.s
              class A(Base):
                x: int
            |}
           ~class_name:"A"
           {|
              class A:
                x: int
                def __init__(self, x: int) -> None:
                  self.x = x
                def __repr__(self) -> str: ...
                def __eq__(self, o: object) -> bool: ...
                def __lt__(self, o: A) -> bool: ...
                def __le__(self, o: A) -> bool: ...
                def __gt__(self, o: A) -> bool: ...
                def __ge__(self, o: A) -> bool: ...
            |};
      (* TODO(T129344236) Fix inheritance for attrs fields *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from attr import field
              import attr
              @attr.s(kw_only=True)
              class Base:
                x: int

              @attr.s
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
                def __repr__(self) -> str: ...
                def __eq__(self, o: object) -> bool: ...
                def __lt__(self, o: A) -> bool: ...
                def __le__(self, o: A) -> bool: ...
                def __gt__(self, o: A) -> bool: ...
                def __ge__(self, o: A) -> bool: ...
            |};
      (* TODO(T129344236) Fix inheritance for attrs fields *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from attr import field
              import attr
              @attr.s
              class Base:
                x: int

              @attr.s(kw_only=True)
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
                def __repr__(self) -> str: ...
                def __eq__(self, o: object) -> bool: ...
                def __lt__(self, o: A) -> bool: ...
                def __le__(self, o: A) -> bool: ...
                def __gt__(self, o: A) -> bool: ...
                def __ge__(self, o: A) -> bool: ...
            |};
      (* TODO(T129344236) Fix inheritance for attrs fields *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              from attr import field
              import attr
              @attr.s(kw_only=True)
              class Base:
                x: int

              @attr.s
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
                def __repr__(self) -> str: ...
                def __eq__(self, o: object) -> bool: ...
                def __lt__(self, o: A) -> bool: ...
                def __le__(self, o: A) -> bool: ...
                def __gt__(self, o: A) -> bool: ...
                def __ge__(self, o: A) -> bool: ...
            |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              import attr
              @attr.s(slots=True)
              class Foo:
                x: int
            |}
           ~class_name:"Foo"
           {|
              class Foo:
                x: int
                def __init__(self, x: int) -> None:
                  self.x = x
                __slots__: typing.Tuple[str] = ('x',)
                def __repr__(self) -> str: ...
                def __eq__(self, o: object) -> bool: ...
                def __lt__(self, o: Foo) -> bool: ...
                def __le__(self, o: Foo) -> bool: ...
                def __gt__(self, o: Foo) -> bool: ...
                def __ge__(self, o: Foo) -> bool: ...
            |};
      (* TODO(T130663259) Fix inheritance for dataclasses *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_attributes
           ~source:
             {|
              import attr

              @attr.s(slots=True)
              class Base:
                x: int

              @attr.s(slots=False)
              class Foo(Base):
                y: int
            |}
           ~class_name:"Foo"
           {|
              class Foo:
                y: int
                def __init__(self, x: int, y: int) -> None:
                  self.y = y
                def __repr__(self) -> str: ...
                def __eq__(self, o: object) -> bool: ...
                def __lt__(self, o: Foo) -> bool: ...
                def __le__(self, o: Foo) -> bool: ...
                def __gt__(self, o: Foo) -> bool: ...
                def __ge__(self, o: Foo) -> bool: ...
            |};
    ]


let () = "attrs" >::: [test_transform_environment] |> Test.run
