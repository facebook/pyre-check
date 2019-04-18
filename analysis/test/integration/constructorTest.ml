(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


open OUnit2
open IntegrationTest


let test_check_invalid_constructor _ =
  assert_type_errors
    {|
      class C:
        def __init__(self) -> None:
          return
    |}
    [];

  assert_type_errors
    {|
      class C:
        def __init__(self) -> int:
          return 0
    |}
    [
      "Incompatible constructor annotation [17]: `__init__` is annotated as " ^
      "returning `int`, but it should return `None`.";
    ]


let test_check_init _ =
  assert_type_errors
    {|
      class Foo:
        attribute: int
        def __init__(self) -> None:
          pass
    |}
    [
      "Uninitialized attribute [13]: Attribute `attribute` is declared in class `Foo` to have " ^
      "type `int` but is never initialized.";
    ];
  assert_type_errors
    {|
      class Foo:
        attribute: int
    |}
    [
      "Uninitialized attribute [13]: Attribute `attribute` is declared in class `Foo` to have " ^
      "type `int` but is never initialized.";
    ];

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
    [
      "Incompatible attribute type [8]: Attribute `x` declared in class `Foo` has type `int` " ^
      "but is used as type `str`.";
      "Missing attribute annotation [4]: Attribute `y` of class `Foo` has type " ^
      "`int` but no type is specified.";
    ];

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
    [
      "Uninitialized attribute [13]: Attribute `attribute` is declared in class `Foo` to have " ^
      "type `int` but is never initialized.";
      "Uninitialized attribute [13]: Attribute `attribute_two` is declared in class `Foo` to " ^
      "have type `str` but is never initialized.";
    ];

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
    [
      "Incompatible attribute type [8]: Attribute `attribute` declared in class `Foo` " ^
      "has type `int` but is used as type `unknown`.";
      "Undefined name [18]: Global name `unknown` is not defined, or there is at least one \
       control flow path that doesn't define `unknown`.";
      "Undefined name [18]: Global name `unknown2` is not defined, or there is at least one \
       control flow path that doesn't define `unknown2`.";
    ];

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
    [
      "Uninitialized attribute [13]: Attribute `attribute` is declared in class `Foo` to have " ^
      "type `int` but is never initialized.";
    ];

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
    [
      "Uninitialized attribute [13]: Attribute `attribute` is declared in class `Foo` to have " ^
      "type `typing.Optional[int]` but is never initialized."
    ];
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
    [
      "Incompatible attribute type [8]: Attribute `attribute` declared in class `Foo` has type " ^
      "`int` but is used as type `str`.";
    ];

  assert_type_errors
    {|
      class Foo:
        def __init__(self, x:int) -> None:
          pass
      a = Foo("")
    |}
    [
      "Incompatible parameter type [6]: " ^
      "Expected `int` for 1st anonymous parameter to call `Foo.__init__` but got `str`.";
    ];

  assert_type_errors
    {|
      class C:
        def __init__(self, x: int) -> None:
          self.a = x
        def a(self) -> int:
          return self.a
    |}
    [
      "Missing attribute annotation [4]: Attribute `a` of class `C` has type `int` " ^
      "but no type is specified.";
      "Incompatible return type [7]: Expected `int` but got `unknown`.";
    ];

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
    [
      "Missing attribute annotation [4]: Attribute `attribute` of class `C` has type `int` " ^
      "but no type is specified.";
      "Missing attribute annotation [4]: Attribute `y` of class `C` has type `int` " ^
      "but no type is specified.";
    ];

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
    [
      "Missing attribute annotation [4]: Attribute `a`" ^
      " of class `C` has type `int` but no type is specified.";
      "Incompatible return type [7]: Expected `int` but got `unknown`."
    ];

  assert_type_errors
    {|
       alias = int
    |}
    [];
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
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 1st anonymous parameter to call `Foo.__new__` but got `str`."];

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
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 1st anonymous parameter to call `Super.__new__` but got `str`."];

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
    [
      "Incompatible parameter type [6]: Expected `int` for 1st anonymous parameter to call \
       `Super.__new__` but got `str`."];
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
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 1st anonymous parameter to call `Super.__init__` but got `str`."]


let test_check_constructors _ =
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
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 1st anonymous parameter to call `Foo.__init__` but got `str`."];
  assert_type_errors
    {|
      class Foo:
        def __init__(self, i: int, s: typing.Optional[str] = None) -> None:
          pass
      def foo() -> None:
        Foo('asdf')
        Foo(1, 2)
    |}
    [
      "Incompatible parameter type [6]: " ^
      "Expected `int` for 1st anonymous parameter to call `Foo.__init__` but got `str`.";
      "Incompatible parameter type [6]: " ^
      "Expected `typing.Optional[str]` for 2nd anonymous parameter to call `Foo.__init__` " ^
      "but got `int`.";
    ];

  (* Explicit call. *)
  assert_type_errors
    {|
      class Foo:
        def __init__(self, i: int) -> None:
          pass
        def foo(self) -> None:
          Foo.__init__(self, 'asdf')
    |}
    [
      "Incompatible parameter type [6]: Expected `int` for 2nd anonymous parameter to call \
       `Foo.__init__` but got `str`.";
    ];

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
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 1st anonymous parameter to call `Super.foo` but got `str`."];
  assert_type_errors
    {|
      class Super:
        def __init__(self, i: int) -> None:
          pass
      class Foo(Super):
        def __init__(self, i: int) -> None:
          super().__init__('asdf')
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 1st anonymous parameter to call `Super.__init__` but got `str`."];

  (* The MRO of inheriting both a class and its direct parent will result in super() evaluating
     to the subclass, regardless of order. *)
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
    [
      "Undefined type [11]: Type `Clss` is not defined.";
      "Incompatible return type [7]: Expected `Class` but got `unknown`.";
      "Call error [29]: `unknown` is not a function.";
    ];

  assert_type_errors ~debug:false
    {|
      def foo(x: typing.Type[typing.Any]) -> typing.Any:
        return x()
    |}
    [];

  assert_type_errors ~debug:false
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
    [
      "Incompatible parameter type [6]: Expected `typing.Callable[[str], Class]` for 1st anonymous \
       parameter to call `foo` but got `typing.Type[Class]`.";
    ]


let test_infer_constructor_attributes _ =
  (* We infer basic constructors. *)
  assert_type_errors
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
    [
      "Too many arguments [19]: Call `object.__init__` expects 0 positional arguments, 4 were" ^
      " provided.";
      "Incompatible return type [7]: Expected `int` but got `C`."]

let test_newtype _ =
  assert_type_errors
    {|
      class C():
        def __init__(self, a: int, b: str) -> None: pass
      T = typing.NewType('T', C)
      def foo() -> T:
        return T(C(7, "A"))
    |}
    [];
  assert_type_errors
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
  "constructor">:::[
    "check_invalid_constructor">::test_check_invalid_constructor;
    "check_init">::test_check_init;
    "check_constructors">::test_check_constructors;
    "check_infer_constructor_attributes">::test_infer_constructor_attributes;
    "newtype">::test_newtype;
  ]
  |> Test.run
