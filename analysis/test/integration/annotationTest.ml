(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open OUnit2
open IntegrationTest

let test_check_undefined_type context =
  let assert_type_errors = assert_type_errors ~context in
  let assert_default_type_errors = assert_default_type_errors ~context in
  let assert_strict_type_errors = assert_strict_type_errors ~context in
  assert_default_type_errors
    {|
      def foo(x: Derp) -> Herp:
        pass
    |}
    [ "Undefined type [11]: Type `Derp` is not defined.";
      "Undefined type [11]: Type `Herp` is not defined." ];

  (* Don't crash when returning a bad type. *)
  assert_default_type_errors
    {|
      def foo(a: gurbage) -> None:
        return a
    |}
    ["Undefined type [11]: Type `gurbage` is not defined."];
  assert_default_type_errors
    {|
      def foo(a: gurbage) -> int:
        a = 1
        return a
    |}
    ["Undefined type [11]: Type `gurbage` is not defined."];
  assert_default_type_errors
    {|
      def foo(x: Derp, y: Herp) -> None:
        pass
    |}
    [ "Undefined type [11]: Type `Derp` is not defined.";
      "Undefined type [11]: Type `Herp` is not defined." ];
  assert_default_type_errors
    {|
      def foo(x: int) -> Herp:
        return x
    |}
    ["Undefined type [11]: Type `Herp` is not defined."];

  (* TODO(T44482498): Fix our locations for types to also throw for Derp. *)
  assert_default_type_errors
    {|
      def foo(x: typing.Union[Derp, Herp]) -> typing.List[Herp]:
        pass
    |}
    [ "Undefined type [11]: Type `Herp` is not defined.";
      "Undefined type [11]: Type `Herp` is not defined." ];
  assert_default_type_errors
    {|
      def foo(x: Derp[int]) -> None:
        pass
    |}
    ["Undefined type [11]: Type `Derp` is not defined."];
  assert_default_type_errors
    {|
      def foo(x: Derp[int, str]) -> None:
        pass
    |}
    ["Undefined type [11]: Type `Derp` is not defined."];
  assert_default_type_errors
    {|
      def foo(x: typing.Optional[Derp[int]]) -> typing.List[Herp]:
        pass
    |}
    [ "Undefined type [11]: Type `Derp` is not defined.";
      "Undefined type [11]: Type `Herp` is not defined." ];
  assert_default_type_errors
    {|
      def foo(x: Optional) -> None:
        pass
    |}
    ["Undefined type [11]: Type `Optional` is not defined."];

  (* TODO(T44482498): We should error on both Optional and Any. *)
  assert_default_type_errors
    {|
      def foo(x: Optional[Any]) -> None:
        pass
    |}
    ["Undefined type [11]: Type `Optional` is not defined."];
  assert_default_type_errors
    {|
      def foo(x: Dict) -> None:
        pass
    |}
    ["Undefined type [11]: Type `Dict` is not defined."];
  assert_default_type_errors
    {|
      def foo() -> None:
        x: undefined = 1
        return
    |}
    ["Undefined type [11]: Type `undefined` is not defined."];
  assert_default_type_errors
    {|
      def foo(x: Derp) -> None:
        y: undefined = 1
        return
    |}
    [ "Undefined type [11]: Type `Derp` is not defined.";
      "Undefined type [11]: Type `undefined` is not defined." ];
  assert_type_errors
    {|
      T = typing.TypeVar('T')
      def foo(x: T) -> typing.Union[str, T]:
        return x
    |}
    [];

  (* Ensure other errors are not missed when undefined type is thrown. *)
  assert_strict_type_errors
    {|
      class Bar:
          async def undefined(self, x: Derp) -> Derp:
              return x
      class Foo(Bar):
          def error(self) -> int:
              return None
          async def undefined(self, x: Herp) -> Herp:
              return x
    |}
    [ "Undefined type [11]: Type `Derp` is not defined.";
      "Undefined type [11]: Type `Derp` is not defined.";
      "Incompatible return type [7]: Expected `int` but got `None`.";
      "Undefined type [11]: Type `Herp` is not defined.";
      "Undefined type [11]: Type `Herp` is not defined." ];
  assert_strict_type_errors
    {|
      def foo() -> typing.Optional["Herp"]:
        return None
    |}
    ["Undefined type [11]: Type `Herp` is not defined."];
  assert_strict_type_errors
    {|
      class Foo:
        def __getitem__(self, other) -> typing.Any: ...

      def foo() -> Foo["Herp"]:
        return 1
    |}
    [ "Missing return annotation [3]: Return type must be specified as type other than `Any`.";
      "Missing parameter annotation [2]: Parameter `other` has no type specified.";
      "Undefined type [11]: Type `Herp` is not defined." ];

  (* Attributes *)
  assert_type_errors
    {|
      class Foo:
        x: int = 1
        y: Derp = 1

        def __init__(self) -> None:
          self.z: Herp = 1
    |}
    [ "Undefined type [11]: Type `Derp` is not defined.";
      "Undefined type [11]: Type `Herp` is not defined." ];

  (* Class bases *)
  assert_type_errors
    {|
      class Foo(Bar): ...
    |}
    ["Undefined type [11]: Type `Bar` is not defined."];
  assert_type_errors
    {|
      _T = typing.TypeVar('_T')
      class Foo(Generic[_T]): ...
    |}
    [ "Invalid type variable [34]: The current class isn't generic with respect to the type \
       variable `Variable[_T]`.";
      "Undefined type [11]: Type `Generic` is not defined." ];
  assert_type_errors
    {|
      class AA: ...
      class CC: ...
      class Foo(AA, BB, CC, DD): ...
    |}
    [ "Undefined type [11]: Type `BB` is not defined.";
      "Undefined type [11]: Type `DD` is not defined." ];
  assert_type_errors
    {|
      class AA: ...
      class CC(BB): ...
      class Foo(AA, BB, CC, DD): ...
    |}
    [ "Undefined type [11]: Type `BB` is not defined.";
      "Undefined type [11]: Type `BB` is not defined.";
      "Undefined type [11]: Type `DD` is not defined." ];

  (* Globals *)
  assert_type_errors
    {|
      x: Derp = 1
      y: typing.List[Derp] = 1
      z: Derp
    |}
    [ "Undefined type [11]: Type `Derp` is not defined.";
      "Undefined type [11]: Type `Derp` is not defined.";
      "Undefined type [11]: Type `Derp` is not defined." ];

  (* Assigns *)
  assert_type_errors
    {|
      def foo() -> None:
        x: Derp = 1
        y: typing.List[Derp] = 1
        z: Derp
    |}
    [ "Undefined type [11]: Type `Derp` is not defined.";
      "Undefined type [11]: Type `Derp` is not defined.";
      "Undefined type [11]: Type `Derp` is not defined." ];

  (* cast, isinstance *)
  assert_type_errors
    {|
      def foo() -> None:
        x: int = 1
        typing.cast(Derp, x)
    |}
    ["Undefined type [11]: Type `Derp` is not defined."];
  assert_type_errors
    {|
      Derp = typing.Any
      Herp = typing.List[typing.Any]
      def foo() -> None:
        x: int = 1
        typing.cast(Derp, x)
        typing.cast(Herp, x)
    |}
    [ "Prohibited any [33]: `Derp` cannot alias to `Any`.";
      "Prohibited any [33]: `Herp` cannot alias to a type containing `Any`." ];
  assert_type_errors
    {|
      def foo() -> None:
        x: int = 1
        if isinstance(x, Derp):
          return x
        return

    |}
    [ "Undefined name [18]: Global name `Derp` is not defined, or there is at least one control \
       flow path that doesn't define `Derp`.";
      "Incompatible return type [7]: Expected `None` but got `int`." ]


let test_check_invalid_type context =
  let assert_type_errors = assert_type_errors ~context in
  let assert_strict_type_errors = assert_strict_type_errors ~context in
  assert_type_errors {|
      MyType = int
      x: MyType = 1
    |} [];
  assert_type_errors {|
      MyType: typing.TypeAlias = int
      x: MyType = 1
    |} [];
  assert_type_errors
    {|
      # Type aliases cannot be annotated
      MyType: typing.Type[int] = int
      x: MyType = 1
    |}
    ["Invalid type [31]: Expression `MyType` is not a valid type."];
  assert_type_errors
    {|
      x: MyType = 1
    |}
    ["Undefined type [11]: Type `MyType` is not defined."];
  assert_type_errors
    {|
      MyType: int
      x: MyType = 1
    |}
    ["Invalid type [31]: Expression `MyType` is not a valid type."];
  assert_strict_type_errors
    {|
      MyType = 1
      x: MyType = 1
    |}
    ["Invalid type [31]: Expression `MyType` is not a valid type."];

  (* Type aliases to Any *)
  assert_type_errors
    {|
      MyType: typing.Any
      x: MyType = 1
    |}
    [ "Missing global annotation [5]: Globally accessible variable `MyType` "
      ^ "must be specified as type other than `Any`.";
      "Invalid type [31]: Expression `MyType` is not a valid type." ];
  assert_type_errors
    {|
      MyType = typing.Any
      x: MyType = 1
    |}
    ["Prohibited any [33]: `MyType` cannot alias to `Any`."];
  assert_type_errors
    {|
      MyType = typing.Any
      x: typing.List[MyType] = [1]
    |}
    ["Prohibited any [33]: `MyType` cannot alias to `Any`."];

  (* Un-parseable expressions *)
  assert_type_errors
    {|
      def foo() -> (int, str):
        return 1
    |}
    ["Invalid type [31]: Expression `(int, str)` is not a valid type."];
  assert_type_errors
    {|
      def foo(x: int + str) -> None:
        return
    |}
    ["Invalid type [31]: Expression `int.__add__(str)` is not a valid type."];

  (* Using expressions of type meta-type: only OK in isinstance *)
  assert_type_errors
    {|
      def f(my_type: typing.Type[int]) -> None:
       x: my_type = ...
    |}
    ["Invalid type [31]: Expression `my_type` is not a valid type."];
  assert_type_errors
    {|
      def f(my_type: typing.Type[int]) -> None:
       y = typing.cast(my_type, "string")
    |}
    ["Invalid type [31]: Expression `my_type` is not a valid type."];
  assert_type_errors
    {|
      def f(my_type: typing.Type[int]) -> None:
       y = "string"
       assert isinstance(y, my_type)
       reveal_type(y)
    |}
    ["Revealed type [-1]: Revealed type for `y` is `int`."];
  assert_type_errors
    {|
      def takes_exception(x: Exception) -> None: ...
      def f(e: typing.Type[Exception]) -> None:
       try:
         pass
       except e as myexception:
         takes_exception(myexception)
    |}
    []


let test_check_illegal_annotation_target context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      class Bar:
        a: str = "string"
      class Foo:
        def foo(self) -> None:
          x = Bar()
          x.a: int = 1
          reveal_type(x.a)
    |}
    [ "Illegal annotation target [35]: Target `x.a` cannot be annotated.";
      "Revealed type [-1]: Revealed type for `x.a` is `str`." ];
  assert_type_errors
    {|
      class Bar: ...
      class Foo:
        def foo(self) -> None:
          Bar(): int = 1
    |}
    ["Illegal annotation target [35]: Target `Bar()` cannot be annotated."];
  assert_type_errors
    {|
      class Bar: ...
      class Foo:
        def foo(self, x: Bar) -> None:
          self.a: int = 1
          x.a: int = 1
    |}
    [ "Undefined attribute [16]: `Foo` has no attribute `a`.";
      "Illegal annotation target [35]: Target `x.a` cannot be annotated.";
      "Undefined attribute [16]: `Bar` has no attribute `a`." ];
  assert_type_errors
    {|
      class Foo:
        a: int = 1

      Foo.a: str = "string"
      reveal_type(Foo.a)
    |}
    [ "Illegal annotation target [35]: Target `Foo.a` cannot be annotated.";
      "Revealed type [-1]: Revealed type for `Foo.a` is `int`." ]


let test_check_missing_type_parameters context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      T = typing.TypeVar("_T")
      class C(typing.Generic[T]): ...
      def f(c: C) -> None:
        return None
    |}
    ["Invalid type parameters [24]: Generic type `C` expects 1 type parameter."];
  assert_type_errors
    {|
      T = typing.TypeVar("_T")
      class C(typing.Generic[T]): ...
      def f(c: typing.List[C]) -> None:
        return None
    |}
    ["Invalid type parameters [24]: Generic type `C` expects 1 type parameter."];
  assert_type_errors
    {|
      T = typing.TypeVar("_T")
      class C(typing.Generic[T]): ...
      def f() -> typing.List[C]:
        return []
    |}
    ["Invalid type parameters [24]: Generic type `C` expects 1 type parameter."];
  assert_type_errors
    {|
      T = typing.TypeVar("_T")
      S = typing.TypeVar("_S")
      class C(typing.Generic[T, S]): ...
      def f() -> typing.List[C]:
        return []
    |}
    ["Invalid type parameters [24]: Generic type `C` expects 2 type parameters."]


let test_check_analysis_failure context =
  assert_type_errors
    ~context
    {|
      def foo() -> Derp:
        pass

      def bar(x: int = foo()) -> int:
        return x
    |}
    [ "Undefined type [11]: Type `Derp` is not defined.";
      "Incompatible variable type [9]: x is declared to have type `int` "
      ^ "but is used as type `unknown`." ];
  assert_type_errors
    ~context
    {|
      def foo(x: int) -> None:
        pass

      def bar(x: Derp) -> None:
        test = foo( **x )
    |}
    [ "Undefined type [11]: Type `Derp` is not defined.";
      "Invalid argument [32]: Keyword argument `x` has type `unknown` "
      ^ "but must be a mapping with string keys." ]


let test_check_immutable_annotations context =
  let assert_type_errors = assert_type_errors ~context in
  let assert_default_type_errors = assert_default_type_errors ~context in
  let assert_strict_type_errors = assert_strict_type_errors ~context in
  assert_type_errors
    {|
      a: int = None
      def foobar() -> None:
          b: int = None
    |}
    [ "Incompatible variable type [9]: a is declared to have type `int` "
      ^ "but is used as type `None`.";
      "Incompatible variable type [9]: b is declared to have type `int` "
      ^ "but is used as type `None`." ];
  assert_type_errors
    {|
      def foo() -> None:
        x: int = 1
        x = 'string'
    |}
    ["Incompatible variable type [9]: x is declared to have type `int` but is used as type `str`."];
  assert_type_errors {|
      def f(x: int) -> None:
        x: str = int_to_str(x)
    |} [];
  assert_type_errors
    {|
    constant: int
    def foo() -> None:
      global constant
      constant = "hi"
    |}
    [ "Incompatible variable type [9]: constant is declared to have type `int` but is used as "
      ^ "type `str`." ];
  assert_default_type_errors
    {|
      def expects_str(x: str) -> None:
        pass

      def foo(x: int, y: typing.Any) -> None:
        x = y
        expects_str(x)
    |}
    [ "Incompatible parameter type [6]: "
      ^ "Expected `str` for 1st anonymous parameter to call `expects_str` but got `int`." ];
  assert_type_errors
    {|
      def foo(x: str = 1) -> str:
        return x
    |}
    [ "Incompatible variable type [9]: x is declared to have type `str` but is used as "
      ^ "type `int`." ];
  assert_type_errors
    {|
      T = typing.TypeVar('T')
      def foo(x: T = 1) -> T:
        return x
    |}
    [];
  assert_type_errors
    {|
      T = typing.TypeVar('T', int, float)
      def foo(x: T = 1) -> T:
        return x
    |}
    [];
  assert_type_errors
    {|
      T = typing.TypeVar('T', int, float)
      def foo(x: T = "str") -> T:
        return x
    |}
    [ "Incompatible variable type [9]: "
      ^ "x is declared to have type `Variable[T <: [int, float]]` but is used as type `str`." ];
  assert_type_errors
    {|
      class B: pass
      class C(B): pass
      T = typing.TypeVar('T', bound=B)
      def foo(x: T = C()) -> T:
        return x
    |}
    [];
  assert_type_errors
    {|
      class O: pass
      class B: pass
      class C(B): pass
      T = typing.TypeVar('T', bound=B)
      def foo(x: T = O()) -> T:
        return x
    |}
    [ "Incompatible variable type [9]: "
      ^ "x is declared to have type `Variable[T (bound to B)]` but is used as type `O`." ];
  assert_type_errors
    {|
      def bar() -> typing.Any:
        ...
      def foo(x: str = bar()) -> str:
        return x
    |}
    ["Missing return annotation [3]: Return type must be specified as type other than `Any`."];
  assert_type_errors
    {|
      constant: int
      def foo() -> None:
        constant = "hi"
    |}
    [];
  assert_type_errors
    {|
      constant: int
      def foo() -> None:
        global constant
        constant: str
        constant = "hi"
    |}
    [];
  assert_type_errors
    {|
      constant: typing.Union[int, str]
      def foo() -> None:
        global constant
        constant = 1
    |}
    [];
  assert_type_errors
    {|
      constant: typing.Optional[int]
      def foo() -> int:
        if constant is not None:
          return constant
        return 0
    |}
    [];
  assert_type_errors
    {|
      constant: typing.Optional[str]
      def foo() -> int:
        if constant is not None:
          return constant
        return 0
    |}
    ["Incompatible return type [7]: Expected `int` but got `str`."];
  assert_type_errors
    {|
      constant: typing.Optional[int]
      def foo() -> int:
        if constant is not None:
          return 0
        return constant
    |}
    ["Incompatible return type [7]: Expected `int` but got `None`."];
  assert_type_errors
    {|
      def foo() -> None:
        global constant
        constant = 1
    |}
    [ "Missing global annotation [5]: Globally accessible variable `constant` has type `int` but "
      ^ "no type is specified." ];

  assert_type_errors
    {|
      constant: typing.Any
      def foo() -> None:
        global constant
        constant = 1
    |}
    [ "Missing global annotation [5]: Globally accessible variable `constant` must be specified \
       as type other than `Any`.";
      "Missing global annotation [5]: Globally accessible variable `constant` has type `int` but \
       type `Any` is specified." ];
  assert_type_errors
    {|
      def foo() -> int:
        global constant
        constant = 1
        return constant
    |}
    [ "Missing global annotation [5]: Globally accessible variable `constant` has type `int` but "
      ^ "no type is specified." ];
  assert_type_errors
    {|
      constant: int
      def foo(x: int) -> str:
        if x > 10:
          global constant
          constant: str
        return constant
    |}
    ["Incompatible return type [7]: Expected `str` but got `typing.Union[int, str]`."];
  assert_type_errors
    {|
      def foo(x: int) -> None:
        x = "hi"
    |}
    [ "Incompatible variable type [9]: x is declared to have type `int` but is used as "
      ^ "type `str`." ];
  assert_type_errors {|
      def foo(x: typing.Optional[int]) -> None:
        x = 1
    |} [];
  assert_type_errors {|
      def foo(x: int) -> None:
        x: str
        x = "hi"
    |} [];
  assert_type_errors
    {|
      def foo() -> None:
        x = 1
        y: str
        y = x
        x = y
    |}
    [ "Incompatible variable type [9]: y is declared to have type `str` but is used as "
      ^ "type `int`." ];
  assert_type_errors
    {|
      def foo(any: typing.Any) -> None:
        x: int = any
    |}
    [ "Missing parameter annotation [2]: Parameter `any` must have a type other than `Any`.";
      "Incompatible variable type [9]: x is declared to have type `int` "
      ^ "but is used as type `typing.Any`." ];
  assert_strict_type_errors
    {|
      def foo(any: typing.Any) -> None:
        x: int = any
    |}
    ["Missing parameter annotation [2]: Parameter `any` must have a type other than `Any`."];
  assert_type_errors
    {|
      def foo(x: int) -> None:
        if x > 10:
          y: int
        else:
          y: str

        y = "hi"
    |}
    [];
  assert_type_errors
    {|
      def foo(x: int) -> None:
        if x > 10:
          y: int
        else:
          y: str
        y = 1
    |}
    [];
  assert_type_errors
    {|
      class Foo():
        attribute = ...
      def bar() -> int:
        foo = Foo()
        foo.attribute = 1
        return foo.attribute
    |}
    [ "Missing attribute annotation [4]: Attribute `attribute` of class `Foo` has no type specified.";
      "Missing attribute annotation [4]: Attribute `attribute` of class `Foo` has type `int` but \
       no type is specified." ];
  assert_type_errors
    {|
      def foo() -> None:
        x: typing.Dict[str, typing.Any] = {}
        x = { 'a': 'b' }
    |}
    [];
  assert_default_type_errors
    {|
      constant = 1
      def foo() -> None:
        global constant
        constant = 1
    |}
    [];
  assert_type_errors
    {|
      def foo() -> None:
        global constant
        constant = 1
      def bar() -> None:
        global constant
        constant = "hi"
    |}
    [ "Missing global annotation [5]: Globally accessible variable `constant` has type `int` but \
       no type is specified.";
      "Missing global annotation [5]: Globally accessible variable `constant` has type `str` but \
       no type is specified." ];
  assert_type_errors
    {|
      def foo() -> None:
        global constant
        constant = 1
      def bar() -> None:
        global constant
        constant = None
    |}
    [ "Missing global annotation [5]: Globally accessible variable `constant` has type `int` but \
       no type is specified.";
      "Missing global annotation [5]: Globally accessible variable `constant` has type `None` but \
       no type is specified." ];
  assert_type_errors
    {|
      def foo() -> None:
        global constant
        constant = 1
      def bar() -> None:
        global constant
        constant = 1.0
    |}
    [ "Missing global annotation [5]: Globally accessible variable `constant` has type `int` but \
       no type is specified.";
      "Missing global annotation [5]: Globally accessible variable `constant` has type `float` \
       but no type is specified." ];
  assert_type_errors
    {|
      def foo() -> None:
        global constant
        constant = A()
      def bar() -> None:
        global constant
        constant = B()
    |}
    [ "Missing global annotation [5]: Globally accessible variable `constant` has type `A` but no \
       type is specified.";
      "Missing global annotation [5]: Globally accessible variable `constant` has type `B` but no \
       type is specified." ];
  assert_type_errors
    {|
      class Foo():
        constant = ...
      def foo() -> None:
        foo = Foo()
        foo.constant = 1
      def bar() -> None:
        global constant
        constant = "hi"
    |}
    [ "Missing attribute annotation [4]: Attribute `constant` of class `Foo` has no type specified.";
      "Missing attribute annotation [4]: Attribute `constant` of class `Foo` has type `int` but \
       no type is specified.";
      "Missing global annotation [5]: Globally accessible variable `constant` has type `str` but "
      ^ "no type is specified." ];
  assert_type_errors
    {|
      x = 1
      y: typing.Any = 2
      z: typing.List[typing.Any] = [3]
      a: typing.Any

      def foo() -> int:
        global a
        a = 1
        return a
    |}
    [ "Missing global annotation [5]: Globally accessible variable `y` has type `int` "
      ^ "but type `Any` is specified.";
      "Missing global annotation [5]: Globally accessible variable `z` must be specified "
      ^ "as type that does not contain `Any`.";
      "Missing global annotation [5]: Globally accessible variable `a` must be specified as type \
       other than `Any`.";
      "Missing global annotation [5]: Globally accessible variable `a` has type `int` but type \
       `Any` is specified." ];
  assert_type_errors
    {|
      class Foo():
        __slots__: typing.List[str] = ['name']
        def foo(self) -> str:
          return self.name
    |}
    ["Incompatible return type [7]: Expected `str` but got `unknown`."];
  assert_type_errors
    {|
      class Foo():
        __slots__: typing.List[str] = ['name', 'attribute']
        def foo(self) -> str:
          return self.name + self.attribute + self.constant
    |}
    [ "Incompatible return type [7]: Expected `str` but got `unknown`.";
      "Undefined attribute [16]: `Foo` has no attribute `constant`." ];
  assert_type_errors
    {|
      class Foo():
        __slots__: typing.List[str] = ['name']
        def foo(self) -> str:
          return self.name
        def __init__(self) -> None:
          self.name: int = 1
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."]


let test_check_incomplete_annotations context =
  let assert_type_errors = assert_type_errors ~context in
  let assert_default_type_errors = assert_default_type_errors ~context in
  assert_type_errors
    {|
      def foo() -> None:
        x: typing.Any = 1
    |}
    [ "Prohibited any [33]: Expression `x` has type `int`; "
      ^ "given explicit type cannot be `Any`." ];
  assert_type_errors
    {|
      def foo() -> None:
        x: typing.List[typing.Any] = []
    |}
    ["Prohibited any [33]: Explicit annotation for `x` cannot contain `Any`."];
  assert_type_errors
    {|
      def foo() -> None:
        x = 1
        typing.cast(typing.Any, x)
    |}
    ["Prohibited any [33]: Explicit annotation for `typing.cast` cannot be `Any`."];
  assert_type_errors
    {|
      def foo() -> None:
        x = 1
        typing.cast(typing.List[typing.Any], x)
    |}
    ["Prohibited any [33]: Explicit annotation for `typing.cast` cannot contain `Any`."];
  assert_default_type_errors {|
      def foo() -> None:
        x: typing.Any = 1
    |} [];
  assert_type_errors
    {|
      def foo() -> None:
        x: typing.Dict[str, typing.Any] = {}
    |}
    [];
  assert_type_errors
    {|
      def foo() -> None:
        x: typing.List[typing.Dict[str, typing.Any]] = []
    |}
    [];
  assert_default_type_errors
    {|
      def foo() -> None:
        x = 1
        typing.cast(typing.Any, x)
    |}
    [];
  assert_type_errors {|
      MyDict = typing.Dict[str, typing.Any]
    |} []


let test_check_refinement context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      def takes_int(a: int) -> None: pass
      def foo() -> None:
        x: float
        x = 1
        takes_int(x)
        x = 1.0
    |}
    [];
  assert_type_errors
    {|
      def foo() -> None:
        l: typing.List[typing.Any] = []
        l = [1]
        l.append('asdf')
    |}
    ["Prohibited any [33]: Explicit annotation for `l` cannot contain `Any`."];
  assert_type_errors
    {|
      def foo() -> None:
        l: typing.List[int] = []
        l.append('a')
    |}
    [ "Incompatible parameter type [6]: "
      ^ "Expected `int` for 1st anonymous parameter to call `list.append` but got `str`." ];
  assert_type_errors
    {|
      def foo() -> None:
        l: typing.List[int] = None
        l.append('a')
    |}
    [ "Incompatible variable type [9]: l is declared to have type `typing.List[int]` "
      ^ "but is used as type `None`.";
      "Incompatible parameter type [6]: "
      ^ "Expected `int` for 1st anonymous parameter to call `list.append` but got `str`." ];
  assert_type_errors
    {|
      def foo(x: typing.Optional[int]) -> int:
        if not x:
          return 1
        return x
    |}
    [];
  assert_type_errors
    {|
      def foo(x: typing.Optional[int]) -> int:
        if not x:
          y = x
        return x
    |}
    ["Incompatible return type [7]: Expected `int` but got `typing.Optional[int]`."];
  assert_type_errors
    {|
      class A:
          a: typing.Optional[int] = None
          def foo(self) -> None:
              if self.a is None:
                  self.a = 5
    |}
    [];
  assert_type_errors
    {|
      class A:
          a: typing.Optional[int] = None
          def bar(self) -> int:
              if self.a is not None:
                  return self.a
              else:
                  return 1
    |}
    ["Incompatible return type [7]: Expected `int` but got `typing.Optional[int]`."];
  assert_type_errors
    {|
      def bar(x: typing.Optional[int]) -> None:
          if x and int_to_int(x) < 0:
              y = 1
    |}
    [];
  assert_type_errors
    {|
      def bar(input: typing.Optional[typing.Set[int]]) -> typing.Set[int]:
          if not input:
            input = set()
          return input
    |}
    [];
  assert_type_errors
    {|
      def bar(input: typing.Optional[int]) -> int:
          if not input:
            input = not_annotated()
          return input
    |}
    [ "Incompatible variable type [9]: input is declared to have type `typing.Optional[int]` "
      ^ "but is used as type `unknown`.";
      "Incompatible return type [7]: Expected `int` but got `unknown`." ]


let test_check_invalid_type_variables context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      T = typing.TypeVar("T")
      def f(x: T) -> T:
        return x
    |}
    [];
  assert_type_errors
    {|
      T = typing.TypeVar("T")
      def f() -> T:
        return T
    |}
    [ "Invalid type variable [34]: The type variable `Variable[T]` isn't present in the \
       function's parameters." ];
  assert_type_errors
    {|
      T = typing.TypeVar("T")
      class C:
        x: T = 1
    |}
    [ "Invalid type variable [34]: The current class isn't generic with respect to the type \
       variable `Variable[T]`." ];
  assert_type_errors
    {|
      T = typing.TypeVar("T")
      x: T = ...
    |}
    [ "Invalid type variable [34]: The type variable `Variable[T]` can only be used to annotate \
       generic classes or functions." ];

  (* We don't error for inferred generics. *)
  assert_type_errors
    {|
      T = typing.TypeVar("T")
      class C(typing.Generic[T]):
        pass
      class D(C[T]):
        pass
    |}
    [];

  (* This is fact valid, but not for the reason it looks like here, as the Ts are in different
     scopes. This means that changing the return value to Callable[[int], int] or anything else
     should work because of behavioral subtyping. *)
  assert_type_errors
    {|
      T = typing.TypeVar("T")
      def f() -> typing.Callable[[T], T]:
        def g(x: T) -> T:
          return x
        return g
    |}
    [];

  (* Check invalid type variables in parameters and returns. *)
  assert_type_errors
    {|
      T = typing.TypeVar("T", covariant=True)
      class Foo(typing.Generic[T]):
        def foo(self, x: T) -> T:
          return x
    |}
    [ "Invalid type variance [46]: The type variable `Variable[T](covariant)` is covariant "
      ^ "and cannot be a parameter type." ];
  assert_type_errors
    {|
      T = typing.TypeVar("T", covariant=True)
      class Foo(typing.Generic[T]):
        def __init__(self, x: T) -> None:
          return
    |}
    [];
  assert_type_errors
    {|
      T = typing.TypeVar("T", covariant=True)
      class Foo(typing.Generic[T]):
        def foo(self, x: typing.List[T]) -> T:
          return x[0]
    |}
    [];
  assert_type_errors
    {|
      T = typing.TypeVar("T", contravariant=True)
      class Foo(typing.Generic[T]):
        def foo(self, x: T) -> T:
          return x
    |}
    [ "Invalid type variance [46]: The type variable `Variable[T](contravariant)` is "
      ^ "contravariant and cannot be a return type." ];
  assert_type_errors
    {|
      T = typing.TypeVar("T", contravariant=True)
      class Foo(typing.Generic[T]):
        def foo(self, x: T) -> typing.List[T]:
          return [x]
    |}
    [];
  assert_type_errors
    {|
      T = typing.TypeVar("T", covariant=True)
      def foo(x: T) -> T:
        return x
    |}
    [ "Invalid type variance [46]: The type variable `Variable[T](covariant)` is covariant "
      ^ "and cannot be a parameter type." ]


let test_check_aliases context =
  assert_type_errors ~context {|
      class C(typing_extensions.Protocol):
        ...
    |} [];
  assert_type_errors
    ~context
    {|
      class C(typing_extensions.Protocol[int]):
        ...
    |}
    [];
  assert_type_errors
    ~context
    {|
      class FOO:
        x: int = 0
      class BAR:
        pass
      FOO = BAR
      def foo(a: FOO) -> int:
        return a.x
      foo(FOO())
    |}
    [ "Incompatible return type [7]: Expected `int` but got `unknown`.";
      "Undefined attribute [16]: `BAR` has no attribute `x`." ];

  (* Locals are not aliases *)
  assert_type_errors
    ~context
    {|
      def foo() -> None:
        x = int
        y: x = 1
    |}
    ["Invalid type [31]: Expression `foo.x` is not a valid type."];

  assert_type_errors ~context {|
      def foo(type: int) -> None:
        x = type
    |} [];

  (* Aliases to undefined types *)
  assert_type_errors
    ~context
    {|
      import typing
      MyAlias = typing.Union[int, UndefinedName]
    |}
    [ "Missing global annotation [5]: Globally accessible variable `MyAlias` has no type specified.";
      "Undefined attribute [16]: Module `typing` has no attribute `Union`." ];
  assert_type_errors
    ~context
    {|
      import typing
      MyAlias: typing.TypeAlias = typing.Union[int, UndefinedName]
    |}
    ["Undefined type [11]: Type `UndefinedName` is not defined."];
  assert_type_errors
    ~context
    {|
      import typing
      MyAlias: typing.TypeAlias = typing.Union[int, "UndefinedName"]
    |}
    ["Undefined type [11]: Type `UndefinedName` is not defined."];

  (* Aliases to invalid types *)
  assert_type_errors
    ~context
    {|
      import typing
      MyAlias = typing.Union[int, 3]
    |}
    [ "Missing global annotation [5]: Globally accessible variable `MyAlias` has no type specified.";
      "Undefined attribute [16]: Module `typing` has no attribute `Union`." ];
  assert_type_errors
    ~context
    {|
      import typing
      MyAlias: typing.TypeAlias = typing.Union[int, 3]
    |}
    []


let test_final_type context =
  assert_type_errors ~context {|
      from typing import Final
      x: Final[int] = 3
    |} [];
  assert_type_errors
    ~context
    {|
      from typing import Final
      x: Final[str] = 3
    |}
    ["Incompatible variable type [9]: x is declared to have type `str` but is used as type `int`."]


let test_check_invalid_inheritance context =
  assert_type_errors
    ~context
    {|
      from typing import Callable
      class MyCallable(Callable):
        pass
    |}
    ["Invalid inheritance [39]: `typing.Callable[..., typing.Any]` is not a valid parent class."];
  assert_type_errors
    ~context
    {|
      from typing import Any
      class MySpecialClass(Any, int):
        pass
    |}
    ["Invalid inheritance [39]: `typing.Any` is not a valid parent class."];
  ()


let test_check_safe_cast context =
  assert_type_errors
    ~context
    {|
      def foo(input: float) -> int:
        return pyre_extensions.safe_cast(int, input)
    |}
    [ "Unsafe cast [49]: `safe_cast` is only permitted to loosen the type of `input`. `float` is \
       not a super type of `input`." ];
  assert_type_errors
    ~context
    {|
        def foo(input: int) -> float:
          return pyre_extensions.safe_cast(float, input)
    |}
    []


let () =
  "annotation"
  >::: [ "check_undefined_type" >:: test_check_undefined_type;
         "check_invalid_type" >:: test_check_invalid_type;
         "check_illegal_annotation_target" >:: test_check_illegal_annotation_target;
         "check_invalid_type_variables" >:: test_check_invalid_type_variables;
         "check_missing_type_parameters" >:: test_check_missing_type_parameters;
         "check_analysis_failure" >:: test_check_analysis_failure;
         "check_immutable_annotations" >:: test_check_immutable_annotations;
         "check_incomplete_annotations" >:: test_check_incomplete_annotations;
         "check_refinement" >:: test_check_refinement;
         "check_aliases" >:: test_check_aliases;
         "check_final_type" >:: test_final_type;
         "check_invalid_inheritance" >:: test_check_invalid_inheritance;
         "check_safe_cast" >:: test_check_safe_cast ]
  |> Test.run
