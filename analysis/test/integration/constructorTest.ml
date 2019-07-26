(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open OUnit2
open IntegrationTest

let test_check_invalid_constructor context =
  assert_type_errors
    ~context
    {|
      class C:
        def __init__(self) -> None:
          return
    |}
    [];
  assert_type_errors
    ~context
    {|
      class C:
        def __init__(self) -> int:
          return 0
    |}
    [ "Incompatible constructor annotation [17]: `__init__` is annotated as "
      ^ "returning `int`, but it should return `None`." ];

  (* TODO(T45018328): We should error here. *)
  assert_type_errors
    ~context
    {|
      class C:
        def __new__(cls) -> None:
          ...
    |}
    []


let test_check_init context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      class Foo:
        attribute: int
        def __init__(self) -> None:
          pass
    |}
    [ "Uninitialized attribute [13]: Attribute `attribute` is declared in class `Foo` to have "
      ^ "type `int` but is never initialized." ];
  assert_type_errors
    {|
      class Foo:
        attribute: int
    |}
    [ "Uninitialized attribute [13]: Attribute `attribute` is declared in class `Foo` to have "
      ^ "type `int` but is never initialized." ];
  assert_type_errors
    {|
      class Foo:
        attribute: int
        def __init__(renamed_self) -> None:
          renamed_self.attribute = 0
    |}
    [];
  assert_type_errors
    {|
      class Foo:
        def __init__(renamed_self) -> None:
          renamed_self.attribute = 0
    |}
    [];
  assert_type_errors
    {|
      class Foo:
        x = 1
        def __init__(self) -> None:
          self.x = "string"
          self.y = self.x
          self.z = 1
    |}
    [ "Incompatible attribute type [8]: Attribute `x` declared in class `Foo` has type `int` "
      ^ "but is used as type `str`.";
      "Missing attribute annotation [4]: Attribute `y` of class `Foo` has type "
      ^ "`int` but no type is specified." ];
  assert_type_errors
    {|
      class Foo:
        def __init__(self) -> None:
          self.attribute: bool = False
    |}
    [];
  assert_type_errors
    {|
      class Foo:
        attribute: int = 1
        def __init__(self) -> None:
          pass
    |}
    [];
  assert_type_errors
    {|
      class Foo:
        attribute: int
        attribute_two: str
        def __init__(self) -> None:
          pass
    |}
    [ "Uninitialized attribute [13]: Attribute `attribute` is declared in class `Foo` to have "
      ^ "type `int` but is never initialized.";
      "Uninitialized attribute [13]: Attribute `attribute_two` is declared in class `Foo` to "
      ^ "have type `str` but is never initialized." ];
  assert_type_errors
    {|
      class Foo:
        attribute: int
        def __init__(self) -> None:
          self.attribute = 0
    |}
    [];
  assert_type_errors
    {|
      class Foo:
        attribute: int
        def __init__(self) -> None:
          self.attribute = 0
        def __enter__(self) -> "Foo":
          return self
    |}
    [];
  assert_type_errors
    {|
      class Foo:
        attribute: int
        def __init__(self) -> None:
          self.attribute = 0 if True else 1
    |}
    [];
  assert_type_errors
    {|
      class Foo:
        attribute: int
        def __init__(self) -> None:
          if condition():
            self.attribute = 0
          else:
            self.attribute = 1
    |}
    [];
  assert_type_errors
    {|
      class Foo:
        attribute: int
        def __init__(self) -> None:
          if False:
            return None
          self.attribute = 1
    |}
    [];
  assert_type_errors
    {|
      class Foo:
        attribute: int
        def __init__(self) -> None:
          if condition():
            raise
          self.attribute = 1
    |}
    [];
  assert_type_errors
    {|
      class Foo:
        attribute: int
        def __init__(self) -> None:
          self.attribute = unknown if condition() else unknown2
    |}
    [ "Incompatible attribute type [8]: Attribute `attribute` declared in class `Foo` "
      ^ "has type `int` but is used as type `unknown`.";
      "Undefined name [18]: Global name `unknown` is not defined, or there is at least one \
       control flow path that doesn't define `unknown`.";
      "Undefined name [18]: Global name `unknown2` is not defined, or there is at least one \
       control flow path that doesn't define `unknown2`." ];

  (* No need to initialize properties. *)
  assert_type_errors
    {|
      class Foo:
        def __init__(self) -> None:
          pass
        @property
        def foo(self) -> str:
          return "asdf"
    |}
    [];
  assert_type_errors
    {|
      class Foo:
        attribute: int
        def __init__(self) -> None:
          attribute = 0
    |}
    [ "Uninitialized attribute [13]: Attribute `attribute` is declared in class `Foo` to have "
      ^ "type `int` but is never initialized." ];
  assert_type_errors
    {|
      class Foo:
        def __init__(self) -> None:
          self.attribute = 0
    |}
    [];
  assert_type_errors
    {|
      class Foo:
        attribute: typing.Optional[int]
        def __init__(self) -> None:
          pass
    |}
    [ "Uninitialized attribute [13]: Attribute `attribute` is declared in class `Foo` to have "
      ^ "type `typing.Optional[int]` but is never initialized." ];
  assert_type_errors
    {|
      class Foo:
        attribute: typing.Optional[int]
        def __init__(self) -> None:
          self.attribute = None
          pass
    |}
    [];
  assert_type_errors
    {|
      class Foo:
        attribute: int
        def __init__(self) -> None:
          self.attribute = ""
    |}
    [ "Incompatible attribute type [8]: Attribute `attribute` declared in class `Foo` has type "
      ^ "`int` but is used as type `str`." ];
  assert_type_errors
    {|
      class Foo:
        def __init__(self, x:int) -> None:
          pass
      a = Foo("")
    |}
    [ "Incompatible parameter type [6]: "
      ^ "Expected `int` for 1st anonymous parameter to call `Foo.__init__` but got `str`." ];
  assert_type_errors
    {|
      class C:
        def __init__(self, x: int) -> None:
          self.a = x
        def a(self) -> int:
          return self.a
    |}
    [ "Missing attribute annotation [4]: Attribute `a` of class `C` has type `int` "
      ^ "but no type is specified.";
      "Incompatible return type [7]: Expected `int` but got `unknown`." ];
  assert_type_errors
    {|
      class C:
        def __init__(self, x: int, y: int) -> None:
          self.x = x
          self.y = y
    |}
    [];
  assert_type_errors
    {|
      class C:
        def __init__(self, x: int, y: int, test: bool) -> None:
          self.attribute = x
          self.x = x
          if test:
            self.y = y
    |}
    [ "Missing attribute annotation [4]: Attribute `attribute` of class `C` has type `int` "
      ^ "but no type is specified.";
      "Missing attribute annotation [4]: Attribute `y` of class `C` has type `int` "
      ^ "but no type is specified." ];
  assert_type_errors
    {|
      def identity(x: int) -> int:
        return x
      class C:
        def __init__(self, x: int) -> None:
          self.a = identity(x)
        def a(self) -> int:
          return self.a
    |}
    [ "Missing attribute annotation [4]: Attribute `a`"
      ^ " of class `C` has type `int` but no type is specified.";
      "Incompatible return type [7]: Expected `int` but got `unknown`." ];
  assert_type_errors {|
       alias = int
    |} [];
  assert_type_errors
    {|
      class C:
        class D:
          pass
      B = C
      reveal_type(B.D)
    |}
    ["Revealed type [-1]: Revealed type for `B.D` is `typing.Type[C.D]`."];
  assert_type_errors
    {|
      class Foo:
        def __new__(cls, x: int) -> None:
          pass
      a: Foo = Foo("")
    |}
    [ "Incompatible parameter type [6]: "
      ^ "Expected `int` for 1st anonymous parameter to call `Foo.__new__` but got `str`." ];

  (* Prefer init over new if both exist. *)
  assert_type_errors
    {|
      class Foo:
        def __new__(cls, x: int) -> None:
          pass
        def __init__(self, x: str) -> None:
          pass
      a: Foo = Foo("")
    |}
    [];
  assert_type_errors
    {|
      class Super:
        def __new__(cls, x: int) -> None: ...

      class C(Super):
        pass
      c: C = C("")
    |}
    [ "Incompatible parameter type [6]: "
      ^ "Expected `int` for 1st anonymous parameter to call `Super.__new__` but got `str`." ];

  (* We look at both __init__ and __new__ in the inheritance structure. *)
  assert_type_errors
    {|
      class SuperSuper:
        def __init__(self, x: str) -> None: ...
      class Super(SuperSuper):
        def __new__(cls, x: int) -> None: ...
      class C(Super):
        pass
      c: C = C("")
    |}
    [ "Incompatible parameter type [6]: Expected `int` for 1st anonymous parameter to call \
       `Super.__new__` but got `str`." ];
  assert_type_errors
    {|
      class SuperSuper:
        def __new__(self, x: str) -> None: ...
      class Super(SuperSuper):
        def __init__(cls, x: int) -> None: ...
      class C(Super):
        pass
      c: C = C("")
    |}
    [ "Incompatible parameter type [6]: "
      ^ "Expected `int` for 1st anonymous parameter to call `Super.__init__` but got `str`." ];
  assert_type_errors
    {|
      class A:
        foo:int = 3
      class B(A):
        foo = "string"
    |}
    [ "Inconsistent override [15]: `foo` overrides attribute defined in `A` inconsistently. "
      ^ "Type `str` is not a subtype of the overridden attribute `int`." ];
  assert_type_errors
    {|
      class A:
        foo:int = 3
      class B(A):
        foo = 100
    |}
    [];
  assert_type_errors
    {|
      from typing import Optional
      def example() -> int: return 1
      class A:
          x:Optional[int] = None
      class B(A):
          x = example()
    |}
    [ "Missing attribute annotation [4]:"
      ^ " Attribute `x` of class `B` has type `int` but no type is specified." ];
  assert_type_errors
    {|
      from abc import ABCMeta
      class A(metaclass=ABCMeta):
        foo: int
        def __init__(self) -> None:
           pass
      |}
    [ "Uninitialized attribute [13]: Attribute `foo` is declared in class `A` to have type `int` \
       but is never initialized." ];
  assert_type_errors
    {|
      from abc import ABCMeta
      class A(metaclass=ABCMeta):
        foo: int
      class B(A):
        pass
      |}
    [ "Uninitialized attribute [13]: Attribute `foo` inherited from abstract class `A` in class \
       `B` to have type `int` but is never initialized." ];
  assert_type_errors
    {|
      from abc import ABCMeta
      class A(metaclass=ABCMeta):
        foo: int

    |}
    []


let test_check_constructors context =
  let assert_type_errors = assert_type_errors ~context in
  let assert_default_type_errors = assert_default_type_errors ~context in
  let assert_strict_type_errors = assert_strict_type_errors ~context in
  assert_type_errors
    {|
      class Foo:
        def __init__(self) -> None:
          pass
      def foo() -> Foo:
        return Foo()
    |}
    [];
  assert_type_errors
    {|
      class Foo:
        def __init__(self, i: int) -> None:
          pass
      def foo() -> Foo:
        return Foo(10)
    |}
    [];
  assert_type_errors
    {|
      class Foo:
        def __init__(self, i: int) -> None:
          pass
      def foo() -> Foo:
        return Foo('asdf')
    |}
    [ "Incompatible parameter type [6]: "
      ^ "Expected `int` for 1st anonymous parameter to call `Foo.__init__` but got `str`." ];
  assert_type_errors
    {|
      class Foo:
        def __init__(self, i: int, s: typing.Optional[str] = None) -> None:
          pass
      def foo() -> None:
        Foo('asdf')
        Foo(1, 2)
    |}
    [ "Incompatible parameter type [6]: "
      ^ "Expected `int` for 1st anonymous parameter to call `Foo.__init__` but got `str`.";
      "Incompatible parameter type [6]: "
      ^ "Expected `typing.Optional[str]` for 2nd anonymous parameter to call `Foo.__init__` "
      ^ "but got `int`." ];

  (* Check abstract methods *)
  assert_type_errors
    {|
      from abc import abstractmethod, ABCMeta
      class Foo(metaclass=ABCMeta):
        @abstractmethod
        def bar(self) -> None:
          pass
      def foo() -> None:
        Foo()
      |}
    ["Invalid class instantiation [45]: Cannot instantiate abstract class `Foo`."];
  assert_type_errors
    {|
      from abc import abstractmethod, ABCMeta
      class Foo(metaclass=ABCMeta):
        @abstractmethod
        def bar(self) -> None:
          pass
        @abstractmethod
        def foo(self) -> None:
          pass
      def foo() -> None:
        Foo()
      |}
    ["Invalid class instantiation [45]: Cannot instantiate abstract class `Foo`."];
  assert_type_errors
    {|
      from abc import abstractmethod, ABCMeta
      class A(metaclass=ABCMeta):
          @abstractmethod
          def a(self) -> None:
              pass
          @abstractmethod
          def b(self) -> None:
              pass
          @abstractmethod
          def c(self) -> None:
              pass
          @abstractmethod
          def d(self) -> None:
              pass
          @abstractmethod
          def e(self) -> None:
              pass
          @abstractmethod
          def f(self) -> None:
              pass
      class B(A):
         pass
    |}
    [ "Abstract class [38]: Class `B` does not implement abstract methods `a`, `b`, `c` and 3 \
       others." ];
  assert_type_errors
    {|
      from abc import abstractmethod
      class Foo():
        @abstractmethod
        def bar(self) -> None:
          pass
      def foo() -> None:
        Foo()
      |}
    [ "Invalid class [44]: `Foo` is a non-abstract class with abstract methods. Did you mean to \
       make this class abstract?" ];
  assert_type_errors
    {|
      from abc import abstractmethod, ABCMeta
      class Foo(metaclass=ABCMeta):
        def bar(self) -> None:
          pass
      def foo() -> None:
        Foo()
      |}
    ["Invalid class instantiation [45]: Cannot instantiate abstract class `Foo`."];
  assert_type_errors
    {|
      from abc import abstractmethod, ABCMeta
      class A(metaclass=ABCMeta):
        @abstractmethod
        def f(self) -> None:
            pass
      class B(A):
         pass
      def foo() -> None:
         B()
   |}
    ["Abstract class [38]: Class `B` does not implement abstract method `f`."];
  assert_type_errors
    {|
      from abc import abstractproperty, ABCMeta
      class A(metaclass=ABCMeta):
        @abstractproperty
        def f(self) -> None:
            pass
      class B(A):
         pass
      def foo() -> None:
         B()
   |}
    ["Abstract class [38]: Class `B` does not implement abstract method `f`."];
  assert_type_errors
    {|
      from abc import abstractmethod, ABCMeta
      class A(metaclass=ABCMeta):
          @abstractmethod
          def h(self) -> None:
              pass
          @abstractmethod
          def g(self) -> None:
              pass
      class B(A):
          def g(self) -> None:
              pass
      class C(B):
          pass
    |}
    [ "Abstract class [38]: Class `B` does not implement abstract method `h`.";
      "Abstract class [38]: Class `C` does not implement abstract method `h`." ];
  assert_type_errors
    {|
      from abc import abstractmethod, ABCMeta
      class A(metaclass=ABCMeta):
          @abstractmethod
          def h(self) -> None:
              pass
          @abstractmethod
          def g(self) -> None:
              pass
      class B(A):
          def g(self) -> None:
              pass
      class C(B):
          pass
      def foo() -> None:
        B()
        C()
    |}
    [ "Abstract class [38]: Class `B` does not implement abstract method `h`.";
      "Abstract class [38]: Class `C` does not implement abstract method `h`." ];
  assert_type_errors
    {|
      from abc import ABCMeta, abstractmethod

      class A(ABCMeta):
          @property
          @abstractmethod
          def foo(self) -> int:
              pass

      class B(A):
          foo:int = 1
      B()
    |}
    [];
  assert_type_errors
    {|
      from abc import abstractmethod, ABC
      class Foo(ABC):
        @abstractmethod
        def bar(self) -> None:
          pass
      def foo() -> None:
        Foo()
      |}
    ["Invalid class instantiation [45]: Cannot instantiate abstract class `Foo`."];

  (* Explicit call. *)
  assert_type_errors
    {|
      class Foo:
        def __init__(self, i: int) -> None:
          pass
        def foo(self) -> None:
          Foo.__init__(self, 'asdf')
    |}
    [ "Incompatible parameter type [6]: Expected `int` for 2nd anonymous parameter to call \
       `Foo.__init__` but got `str`." ];

  (* Super calls. *)
  assert_type_errors
    {|
      class Super:
        def foo(self, i: int) -> None:
          pass
      class Foo(Super):
        def foo(self, i: int) -> None:
          super().foo('asdf')
    |}
    [ "Incompatible parameter type [6]: "
      ^ "Expected `int` for 1st anonymous parameter to call `Super.foo` but got `str`." ];
  assert_type_errors
    {|
      class Super:
        def __init__(self, i: int) -> None:
          pass
      class Foo(Super):
        def __init__(self, i: int) -> None:
          super().__init__('asdf')
    |}
    [ "Incompatible parameter type [6]: "
      ^ "Expected `int` for 1st anonymous parameter to call `Super.__init__` but got `str`." ];
  assert_type_errors
    {|
      from placeholder_stub import MadeUpClass
      class Foo(MadeUpClass):
        def __init__(self, i: int) -> None:
          super().__init__('asdf')
    |}
    [];
  assert_type_errors
    {|
      from placeholder_stub import MadeUpClass
      class Foo(MadeUpClass):
        pass
      def foo() -> None:
        Foo(7)
    |}
    [];

  (* The MRO of inheriting both a class and its direct parent will result in super() evaluating to
     the subclass, regardless of order. *)
  assert_type_errors
    {|
      class Subclass(A, B):
        def foo(self)->A:
          return super()
        def wrong(self)->B:
          return super()
    |}
    [];
  assert_type_errors
    {|
      class Class:
        def __init__(self, i: int) -> None: ...
      def foo(x: typing.Type[Class]) -> Class:
        return x(7)
    |}
    [];
  assert_type_errors
    {|
      class Class:
        def __init__(self, i: int) -> None: ...
      def foo(x: typing.Type[Clss]) -> Class:
        return x(7)
    |}
    [ "Undefined type [11]: Type `Clss` is not defined.";
      "Incompatible return type [7]: Expected `Class` but got `unknown`." ];
  assert_default_type_errors
    {|
      def foo(x: typing.Type[typing.Any]) -> typing.Any:
        return x()
    |}
    [];
  assert_default_type_errors
    {|
      def foo(x: typing.Type[typing.Any]) -> typing.Any:
        return x(42)
    |}
    [];
  assert_strict_type_errors
    {|
      class Class:
        def __init__(self, i: int) -> None: ...
      def foo(x: typing.Type[Clss]) -> Class:
        return x(7)
    |}
    ["Undefined type [11]: Type `Clss` is not defined."];
  assert_type_errors
    {|
      class Class:
        def __init__(self, i: int) -> None:
          ...
      def foo(x: typing.Callable[[int], Class]) -> None: ...
      foo(Class)
    |}
    [];
  assert_type_errors
    {|
      class Class:
        def __init__(self, i: int) -> None:
          ...
      def foo(x: typing.Callable[[str], Class]) -> None: ...
      foo(Class)
    |}
    [ "Incompatible parameter type [6]: Expected `typing.Callable[[str], Class]` for 1st \
       anonymous parameter to call `foo` but got `typing.Type[Class]`." ]


let test_infer_constructor_attributes context =
  (* We infer basic constructors. *)
  assert_type_errors
    ~context
    {|
      class C:
        pass
      class D:
        def __init__(self) -> None:
          self.x = C()
        def foo(self) -> int:
          return self.x
    |}
    ["Incompatible return type [7]: Expected `int` but got `C`."];
  assert_type_errors
    ~context
    {|
      class C:
        pass
      class D:
        def __init__(self) -> None:
          # We trust the callee blindly without examining the arguments for inference.
          self.x = C(1,2,3,4)
        def foo(self) -> int:
          return self.x
    |}
    [ "Too many arguments [19]: Call `object.__init__` expects 0 positional arguments, 4 were"
      ^ " provided.";
      "Incompatible return type [7]: Expected `int` but got `C`." ]


let test_newtype context =
  assert_type_errors
    ~context
    {|
      class C():
        def __init__(self, a: int, b: str) -> None: pass
      T = typing.NewType('T', C)
      def foo() -> T:
        return T(C(7, "A"))
    |}
    [];
  assert_type_errors
    ~context
    {|
      class C():
        def __init__(self, a: int, b: str) -> None: pass
      T = typing.NewType('T', C)
      def foo() -> T:
        return T(7, "A")
    |}
    ["Too many arguments [19]: Call `T.__init__` expects 1 positional argument, 2 were provided."];
  ()


let () =
  "constructor"
  >::: [ "check_invalid_constructor" >:: test_check_invalid_constructor;
         "check_init" >:: test_check_init;
         "check_constructors" >:: test_check_constructors;
         "check_infer_constructor_attributes" >:: test_infer_constructor_attributes;
         "newtype" >:: test_newtype ]
  |> Test.run
