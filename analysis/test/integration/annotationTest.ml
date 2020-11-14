(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

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
    [
      "Unbound name [10]: Name `Derp` is used but not defined in the current scope.";
      "Unbound name [10]: Name `Herp` is used but not defined in the current scope.";
    ];

  (* Don't crash when returning a bad type. *)
  assert_default_type_errors
    {|
      def foo(a: gurbage) -> None:
        return a
    |}
    ["Unbound name [10]: Name `gurbage` is used but not defined in the current scope."];
  assert_default_type_errors
    {|
      def foo(a: gurbage) -> int:
        a = 1
        return a
    |}
    ["Unbound name [10]: Name `gurbage` is used but not defined in the current scope."];
  assert_default_type_errors
    {|
      def foo(x: Derp, y: Herp) -> None:
        pass
    |}
    [
      "Unbound name [10]: Name `Derp` is used but not defined in the current scope.";
      "Unbound name [10]: Name `Herp` is used but not defined in the current scope.";
    ];
  assert_default_type_errors
    {|
      def foo(x: int) -> Herp:
        return x
    |}
    ["Unbound name [10]: Name `Herp` is used but not defined in the current scope."];
  assert_default_type_errors
    {|
      import typing
      def foo(x: typing.Union[Derp, Herp]) -> typing.List[Herp]:
        pass
    |}
    [
      "Unbound name [10]: Name `Derp` is used but not defined in the current scope.";
      "Unbound name [10]: Name `Herp` is used but not defined in the current scope.";
    ];
  assert_default_type_errors
    {|
      def foo(x: Derp[int]) -> None:
        pass
    |}
    ["Unbound name [10]: Name `Derp` is used but not defined in the current scope."];
  assert_default_type_errors
    {|
      def foo(x: Derp[int, str]) -> None:
        pass
    |}
    ["Unbound name [10]: Name `Derp` is used but not defined in the current scope."];
  assert_default_type_errors
    {|
      import typing
      def foo(x: typing.Optional[Derp[int]]) -> typing.List[Herp]:
        pass
    |}
    [
      "Unbound name [10]: Name `Derp` is used but not defined in the current scope.";
      "Unbound name [10]: Name `Herp` is used but not defined in the current scope.";
    ];
  assert_default_type_errors
    {|
      def foo(x: Optional) -> None:
        pass
    |}
    ["Unbound name [10]: Name `Optional` is used but not defined in the current scope."];
  assert_default_type_errors
    {|
      def foo(x: Optional[Any]) -> None:
        pass
    |}
    [
      "Unbound name [10]: Name `Optional` is used but not defined in the current scope.";
      "Unbound name [10]: Name `Any` is used but not defined in the current scope.";
    ];
  assert_default_type_errors
    {|
      def foo(x: Dict) -> None:
        pass
    |}
    ["Unbound name [10]: Name `Dict` is used but not defined in the current scope."];
  assert_default_type_errors
    {|
      def foo() -> None:
        x: undefined = 1
        return
    |}
    ["Unbound name [10]: Name `undefined` is used but not defined in the current scope."];
  assert_default_type_errors
    {|
      def foo(x: Derp) -> None:
        y: undefined = 1
        return
    |}
    [
      "Unbound name [10]: Name `Derp` is used but not defined in the current scope.";
      "Unbound name [10]: Name `undefined` is used but not defined in the current scope.";
    ];
  assert_type_errors
    {|
      import typing
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
    [
      "Unbound name [10]: Name `Derp` is used but not defined in the current scope.";
      "Incompatible return type [7]: Expected `int` but got `None`.";
      "Unbound name [10]: Name `Herp` is used but not defined in the current scope.";
    ];
  assert_strict_type_errors
    {|
      import typing
      def foo() -> typing.Optional["Herp"]:
        return None
    |}
    ["Unbound name [10]: Name `Herp` is used but not defined in the current scope."];
  assert_strict_type_errors
    {|
      import typing
      class Foo:
        def __getitem__(self, other) -> typing.Any: ...

      def foo() -> Foo["Herp"]:
        return 1
    |}
    [
      "Missing return annotation [3]: Return type must be specified as type other than `Any`.";
      "Missing parameter annotation [2]: Parameter `other` has no type specified.";
      "Unbound name [10]: Name `Herp` is used but not defined in the current scope.";
    ];

  assert_strict_type_errors
    {|
      import typing as t

      def foo() -> t.Set:
        return set()
    |}
    ["Invalid type parameters [24]: Generic type `set` expects 1 type parameter."];

  (* Attributes *)
  assert_type_errors
    {|
      class Foo:
        x: int = 1
        y: Derp = 1

        def __init__(self) -> None:
          self.z: Herp = 1
    |}
    [
      "Unbound name [10]: Name `Derp` is used but not defined in the current scope.";
      "Unbound name [10]: Name `Herp` is used but not defined in the current scope.";
    ];

  (* Class bases *)
  assert_type_errors
    {|
      class Foo(Bar): ...
    |}
    ["Unbound name [10]: Name `Bar` is used but not defined in the current scope."];
  assert_type_errors
    {|
      import typing
      _T = typing.TypeVar('_T')
      class Foo(Generic[_T]): ...
    |}
    [
      "Unbound name [10]: Name `Generic` is used but not defined in the current scope.";
      "Invalid type variable [34]: The current class isn't generic with respect to the type \
       variable `Variable[_T]`.";
    ];
  assert_type_errors
    {|
      class AA: ...
      class CC: ...
      class Foo(AA, BB, CC, DD): ...
    |}
    [
      "Unbound name [10]: Name `BB` is used but not defined in the current scope.";
      "Unbound name [10]: Name `DD` is used but not defined in the current scope.";
    ];
  assert_type_errors
    {|
      class AA: ...
      class CC(BB): ...
      class Foo(AA, BB, CC, DD): ...
    |}
    [
      "Unbound name [10]: Name `BB` is used but not defined in the current scope.";
      "Unbound name [10]: Name `DD` is used but not defined in the current scope.";
    ];

  (* Globals *)
  assert_type_errors
    {|
      import typing
      x: Derp = 1
      y: typing.List[Derp] = 1
      z: Derp
    |}
    ["Unbound name [10]: Name `Derp` is used but not defined in the current scope."];

  (* Assigns *)
  assert_type_errors
    {|
      import typing
      def foo() -> None:
        x: Derp = 1
        y: typing.List[Derp] = 1
        z: Derp
    |}
    ["Unbound name [10]: Name `Derp` is used but not defined in the current scope."];

  (* cast, isinstance *)
  assert_type_errors
    {|
      import typing
      def foo() -> None:
        x: int = 1
        typing.cast(Derp, x)
    |}
    ["Unbound name [10]: Name `Derp` is used but not defined in the current scope."];
  assert_type_errors
    {|
      import typing
      Derp = typing.Any
      Herp = typing.List[typing.Any]
      def foo() -> None:
        x: int = 1
        typing.cast(Derp, x)
        typing.cast(Herp, x)
    |}
    [
      "Prohibited any [33]: `Derp` cannot alias to `Any`.";
      "Prohibited any [33]: `Herp` cannot alias to a type containing `Any`.";
    ];
  assert_strict_type_errors
    {|
      def foo() -> None:
        x: int = 1
        if isinstance(x, Derp):
          return x
        return

    |}
    [
      "Unbound name [10]: Name `Derp` is used but not defined in the current scope.";
      "Incompatible return type [7]: Expected `None` but got `int`.";
    ]


let test_check_invalid_type context =
  let assert_type_errors = assert_type_errors ~context in
  let assert_strict_type_errors = assert_strict_type_errors ~context in
  assert_type_errors {|
      MyType = int
      x: MyType = 1
    |} [];
  assert_type_errors
    {|
      import typing
      MyType: typing.TypeAlias = int
      x: MyType = 1
    |}
    [];
  assert_type_errors
    {|
      import typing
      # Type aliases cannot be annotated
      MyType: typing.Type[int] = int
      x: MyType = 1
    |}
    ["Undefined or invalid type [11]: Annotation `MyType` is not defined as a type."];
  assert_type_errors
    {|
      x: MyType = 1
    |}
    ["Unbound name [10]: Name `MyType` is used but not defined in the current scope."];
  assert_type_errors
    {|
      MyType: int
      x: MyType = 1
    |}
    ["Undefined or invalid type [11]: Annotation `MyType` is not defined as a type."];
  assert_strict_type_errors
    {|
      MyType = 1
      x: MyType = 1
    |}
    ["Undefined or invalid type [11]: Annotation `MyType` is not defined as a type."];

  (* Type aliases cannot be nested *)
  assert_type_errors
    {|
        def foo() -> None:
          MyType = int
          x: MyType = 1
      |}
    ["Undefined or invalid type [11]: Annotation `foo.MyType` is not defined as a type."];
  assert_type_errors
    {|
      class Foo:
        X = int
      x: Foo.X = ...
    |}
    ["Undefined or invalid type [11]: Annotation `Foo.X` is not defined as a type."];
  assert_type_errors
    {|
        import typing
        def foo() -> None:
          MyType: typing.TypeAlias = int
          x: MyType = 1
      |}
    [
      "Invalid type [31]: Expression `MyType` is not a valid type. All type alias declarations \
       must be made in the module definition.";
      "Undefined or invalid type [11]: Annotation `foo.MyType` is not defined as a type.";
    ];

  (* Type aliases to Any *)
  assert_type_errors
    {|
      import typing
      MyType: typing.Any
      x: MyType = 1
    |}
    [
      "Missing global annotation [5]: Globally accessible variable `MyType` "
      ^ "must be specified as type other than `Any`.";
      "Undefined or invalid type [11]: Annotation `MyType` is not defined as a type.";
    ];
  assert_type_errors
    {|
      import typing
      MyType = typing.Any
      x: MyType = 1
    |}
    ["Prohibited any [33]: `MyType` cannot alias to `Any`."];
  assert_type_errors
    {|
      import typing
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
      import typing
      def f(my_type: typing.Type[int]) -> None:
       x: my_type = ...
    |}
    ["Undefined or invalid type [11]: Annotation `my_type` is not defined as a type."];
  assert_type_errors
    {|
      import typing
      def f(my_type: typing.Type[int]) -> None:
       y = typing.cast(my_type, "string")
    |}
    ["Undefined or invalid type [11]: Annotation `my_type` is not defined as a type."];
  assert_type_errors
    {|
      import typing
      def takes_exception(x: Exception) -> None: ...
      def f(e: typing.Type[Exception]) -> None:
       try:
         pass
       except e as myexception:
         takes_exception(myexception)
    |}
    [];
  assert_type_errors
    {|
      import typing
      x: typing.Dict[int, [str]]
    |}
    [
      "Invalid type parameters [24]: Single type parameter `Variable[_S]` expected, but a type \
       parameter group `[str]` was given for generic type dict.";
    ];
  assert_type_errors
    {|
      from typing import TypeVar, Generic

      TValue = TypeVar("TValue", bound=int)

      class Foo(Generic[TValue]): pass

      def foo() -> Foo[garbage]: ...
    |}
    ["Unbound name [10]: Name `garbage` is used but not defined in the current scope."];

  (* Malformed alias assignment *)
  assert_type_errors
    {|
      X, Y = int
      x: X = ...
    |}
    [
      "Missing global annotation [5]: Globally accessible variable `X` has no type specified.";
      "Unable to unpack [23]: Unable to unpack `typing.Type[int]` into 2 values.";
      "Missing global annotation [5]: Globally accessible variable `Y` has no type specified.";
      "Undefined or invalid type [11]: Annotation `X` is not defined as a type.";
    ];
  ()


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
    [
      "Illegal annotation target [35]: Target `x.a` cannot be annotated.";
      "Revealed type [-1]: Revealed type for `x.a` is `str`.";
    ];
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
    [
      "Undefined attribute [16]: `Foo` has no attribute `a`.";
      "Illegal annotation target [35]: Target `x.a` cannot be annotated.";
      "Undefined attribute [16]: `Bar` has no attribute `a`.";
    ];
  assert_type_errors
    {|
      class Foo:
        a: int = 1

      Foo.a: str = "string"
      reveal_type(Foo.a)
    |}
    [
      "Illegal annotation target [35]: Target `test.Foo.a` cannot be annotated.";
      "Revealed type [-1]: Revealed type for `test.Foo.a` is `int`.";
    ]


let test_check_missing_type_parameters context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar("_T")
      class C(typing.Generic[T]): ...
      def f(c: C) -> None:
        return None
    |}
    ["Invalid type parameters [24]: Generic type `C` expects 1 type parameter."];
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar("_T")
      class C(typing.Generic[T]): ...
      def f(c: typing.List[C]) -> None:
        return None
    |}
    ["Invalid type parameters [24]: Generic type `C` expects 1 type parameter."];
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar("_T")
      class C(typing.Generic[T]): ...
      def f() -> typing.List[C]:
        return []
    |}
    ["Invalid type parameters [24]: Generic type `C` expects 1 type parameter."];
  assert_type_errors
    {|
      import typing
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
    [
      "Unbound name [10]: Name `Derp` is used but not defined in the current scope.";
      "Incompatible variable type [9]: x is declared to have type `int` "
      ^ "but is used as type `unknown`.";
    ];
  assert_type_errors
    ~context
    {|
      def foo(x: int) -> None:
        pass

      def bar(x: Derp) -> None:
        test = foo( **x )
    |}
    [
      "Unbound name [10]: Name `Derp` is used but not defined in the current scope.";
      "Invalid argument [32]: Keyword argument `x` has type `unknown` "
      ^ "but must be a mapping with string keys.";
    ]


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
    [
      "Incompatible variable type [9]: a is declared to have type `int` "
      ^ "but is used as type `None`.";
      "Incompatible variable type [9]: b is declared to have type `int` "
      ^ "but is used as type `None`.";
    ];
  assert_type_errors
    {|
      def foo() -> None:
        x: int = 1
        x = 'string'
    |}
    ["Incompatible variable type [9]: x is declared to have type `int` but is used as type `str`."];
  assert_type_errors
    {|
      from builtins import int_to_str
      def f(x: int) -> None:
        x: str = int_to_str(x)
    |}
    ["Illegal annotation target [35]: Target `x` cannot be annotated after it is first declared."];
  assert_type_errors
    {|
    constant: int
    def foo() -> None:
      global constant
      constant = "hi"
    |}
    [
      "Incompatible variable type [9]: constant is declared to have type `int` but is used as "
      ^ "type `str`.";
    ];
  assert_default_type_errors
    {|
      import typing
      def expects_str(x: str) -> None:
        pass

      def foo(x: int, y: typing.Any) -> None:
        x = y
        expects_str(x)
    |}
    [];
  assert_type_errors
    {|
      def foo(x: str = 1) -> str:
        return x
    |}
    [
      "Incompatible variable type [9]: x is declared to have type `str` but is used as "
      ^ "type `int`.";
    ];
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar('T')
      def foo(x: T = 1) -> T:
        return x
    |}
    [];
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar('T', int, float)
      def foo(x: T = 1) -> T:
        return x
    |}
    [];
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar('T', int, float)
      def foo(x: T = "str") -> T:
        return x
    |}
    [
      "Incompatible variable type [9]: "
      ^ "x is declared to have type `Variable[T <: [int, float]]` but is used as type `str`.";
    ];
  assert_type_errors
    {|
      import typing
      class B: pass
      class C(B): pass
      T = typing.TypeVar('T', bound=B)
      def foo(x: T = C()) -> T:
        return x
    |}
    [];
  assert_type_errors
    {|
      import typing
      class O: pass
      class B: pass
      class C(B): pass
      T = typing.TypeVar('T', bound=B)
      def foo(x: T = O()) -> T:
        return x
    |}
    [
      "Incompatible variable type [9]: "
      ^ "x is declared to have type `Variable[T (bound to B)]` but is used as type `O`.";
    ];
  assert_type_errors
    {|
      import typing
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

  (* TODO (T56371223): Emit an invalid assignment error when trying to re-annotated a global like
     this *)
  assert_type_errors
    {|
      constant: int
      def foo() -> None:
        global constant
        constant: str
        constant = "hi"
    |}
    [
      "Incompatible variable type [9]: constant is declared to have type `int` but is used as type \
       `str`.";
    ];
  assert_type_errors
    {|
      import typing
      constant: typing.Union[int, str]
      def foo() -> None:
        global constant
        constant = 1
    |}
    [];
  assert_type_errors
    {|
      import typing
      constant: typing.Optional[int]
      def foo() -> int:
        if constant is not None:
          return constant
        return 0
    |}
    ["Incompatible return type [7]: Expected `int` but got `typing.Optional[int]`."];
  assert_type_errors
    {|
      import typing
      def foo() -> int:
        constant: typing.Optional[int]
        if constant is not None:
          return constant
        return 0
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo() -> int:
        constant: typing.Optional[str]
        if constant is not None:
          return constant
        return 0
    |}
    ["Incompatible return type [7]: Expected `int` but got `str`."];
  assert_type_errors
    {|
      import typing
      def foo() -> int:
        constant: typing.Optional[int]
        if constant is not None:
          return 0
        return constant
    |}
    ["Incompatible return type [7]: Expected `int` but got `None`."];

  assert_type_errors
    {|
      import typing
      constant: typing.Any
      def foo() -> None:
        global constant
        constant = 1
    |}
    [
      "Missing global annotation [5]: Globally accessible variable `constant` must be specified as \
       type other than `Any`.";
    ];
  assert_type_errors
    {|
      constant: int
      def foo(x: int) -> str:
        if x > 10:
          global constant
          constant: str
        return constant
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."];
  assert_type_errors
    {|
      def foo(x: int) -> None:
        x = "hi"
    |}
    [
      "Incompatible variable type [9]: x is declared to have type `int` but is used as "
      ^ "type `str`.";
    ];
  assert_type_errors
    {|
      import typing
      def foo(x: typing.Optional[int]) -> None:
        x = 1
    |}
    [];
  assert_type_errors
    {|
      def foo(x: int) -> None:
        x: str
        x = "hi"
    |}
    ["Illegal annotation target [35]: Target `x` cannot be annotated after it is first declared."];
  assert_type_errors
    {|
      def foo() -> None:
        x = 1
        y: str
        y = x
        x = y
    |}
    [
      "Incompatible variable type [9]: y is declared to have type `str` but is used as "
      ^ "type `int`.";
    ];
  assert_type_errors
    {|
      import typing
      def foo(any: typing.Any) -> None:
        x: int = any
    |}
    ["Missing parameter annotation [2]: Parameter `any` must have a type other than `Any`."];
  assert_strict_type_errors
    {|
      import typing
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
    [
      "Missing attribute annotation [4]: Attribute `attribute` of class `Foo` has no type specified.";
      "Incompatible return type [7]: Expected `int` but got `unknown`.";
    ];
  assert_type_errors
    {|
      import typing
      def foo() -> None:
        x: typing.Dict[str, typing.Any] = {}
        x = { 'a': 'b' }
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo() -> None:
        x: typing.Dict[str, typing.List[typing.Any]] = {}
    |}
    ["Prohibited any [33]: Explicit annotation for `x` cannot contain `Any`."];
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
      class Foo():
        constant = ...
      def foo() -> None:
        foo = Foo()
        foo.constant = 1
    |}
    ["Missing attribute annotation [4]: Attribute `constant` of class `Foo` has no type specified."];
  assert_type_errors
    {|
      import typing
      x = 1
      y: typing.Any = 2
      z: typing.List[typing.Any] = [3]
      a: typing.Any

      def foo() -> int:
        global a
        a = 1
        return a
    |}
    [
      "Missing global annotation [5]: Globally accessible variable `y` has type `int` "
      ^ "but type `Any` is specified.";
      "Missing global annotation [5]: Globally accessible variable `z` must be specified "
      ^ "as type that does not contain `Any`.";
      "Missing global annotation [5]: Globally accessible variable `a` must be specified as type \
       other than `Any`.";
    ];
  assert_type_errors
    {|
      import typing
      class Foo():
        __slots__: typing.List[str] = ['name']
        def foo(self) -> str:
          return self.name
    |}
    ["Incompatible return type [7]: Expected `str` but got `unknown`."];
  assert_type_errors
    {|
      import typing
      class Foo():
        __slots__: typing.List[str] = ['name', 'attribute']
        def foo(self) -> str:
          return self.name + self.attribute + self.constant
    |}
    ["Undefined attribute [16]: `Foo` has no attribute `constant`."];
  assert_type_errors
    {|
      import typing
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
      import typing
      def foo() -> None:
        x: typing.Any = 1
    |}
    [
      "Prohibited any [33]: Expression `x` has type `int`; " ^ "given explicit type cannot be `Any`.";
    ];
  assert_type_errors
    {|
      import typing
      def foo() -> None:
        x: typing.List[typing.Any] = []
    |}
    ["Prohibited any [33]: Explicit annotation for `x` cannot contain `Any`."];
  assert_type_errors
    {|
      import typing
      def foo() -> None:
        x = 1
        typing.cast(typing.Any, x)
    |}
    ["Prohibited any [33]: Explicit annotation for `typing.cast` cannot be `Any`."];
  assert_type_errors
    {|
      import typing
      def foo() -> None:
        x = 1
        typing.cast(typing.List[typing.Any], x)
    |}
    ["Prohibited any [33]: Explicit annotation for `typing.cast` cannot contain `Any`."];
  assert_default_type_errors
    {|
      import typing
      def foo() -> None:
        x: typing.Any = 1
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo() -> None:
        x: typing.Dict[str, typing.Any] = {}
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo() -> None:
        x: typing.List[typing.Dict[str, typing.Any]] = []
    |}
    [];
  assert_default_type_errors
    {|
      import typing
      def foo() -> None:
        x = 1
        typing.cast(typing.Any, x)
    |}
    [];
  assert_type_errors {|
      import typing
      MyDict = typing.Dict[str, typing.Any]
    |} []


let test_check_incomplete_callable context =
  assert_type_errors
    ~context
    {|
      import typing
      def foo(x: int) -> str:
        return "foo"
      bar: typing.Callable[[int], bool] = foo
    |}
    [
      "Incompatible variable type [9]: bar is declared to have type `typing.Callable[[int], bool]` \
       but is used as type `typing.Callable(foo)[[Named(x, int)], str]`.";
    ];
  assert_type_errors
    ~context
    {|
      import typing
      def foo(x: int) -> str:
        return "foo"
      bar: typing.Callable[[int]] = foo

      def baz(x: typing.Callable[[int]]) -> typing.Callable[[int]]: ...
    |}
    [
      "Invalid type [31]: Expression `typing.Callable[[int]]` is not a valid type.";
      "Invalid type [31]: Expression `typing.Callable[[int]]` is not a valid type.";
      "Invalid type [31]: Expression `typing.Callable[[int]]` is not a valid type.";
    ];
  assert_type_errors
    ~context
    ~show_error_traces:true
    {|
      from typing import Callable
      x: Callable[int]
      y: Callable[int, str]
    |}
    [
      "Invalid type [31]: Expression `typing.Callable[int]` is not a valid type. Expected \
       `Callable[[<parameters>], <return type>]`.";
      "Invalid type [31]: Expression `typing.Callable[(int, str)]` is not a valid type. Expected \
       `Callable[[<parameters>], <return type>]`.";
    ];
  ()


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

  (* List[Any] correctly can refine to List[int] *)
  assert_type_errors
    {|
      import typing
      def foo() -> None:
        l: typing.List[typing.Any] = []
        l = [1]
        l.append('asdf')
    |}
    ["Prohibited any [33]: Explicit annotation for `l` cannot contain `Any`."];
  assert_type_errors
    {|
      import typing
      def foo() -> None:
        l: typing.List[int] = []
        l.append('a')
    |}
    [
      "Incompatible parameter type [6]: "
      ^ "Expected `int` for 1st positional only parameter to call `list.append` but got `str`.";
    ];
  assert_type_errors
    {|
      import typing
      def foo() -> None:
        l: typing.List[int] = None
        l.append('a')
    |}
    [
      "Incompatible variable type [9]: l is declared to have type `typing.List[int]` "
      ^ "but is used as type `None`.";
      "Incompatible parameter type [6]: "
      ^ "Expected `int` for 1st positional only parameter to call `list.append` but got `str`.";
    ];
  assert_type_errors
    {|
      import typing
      def foo(x: typing.Optional[int]) -> int:
        if not x:
          return 1
        return x
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo(x: typing.Optional[int]) -> int:
        if not x:
          y = x
        return x
    |}
    ["Incompatible return type [7]: Expected `int` but got `typing.Optional[int]`."];
  assert_type_errors
    {|
      import typing
      class A:
          a: typing.Optional[int] = None
          def foo(self) -> None:
              if self.a is None:
                  self.a = 5
    |}
    [];
  assert_type_errors
    {|
      import typing
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
      from builtins import int_to_int
      import typing
      def bar(x: typing.Optional[int]) -> None:
          if x and int_to_int(x) < 0:
              y = 1
    |}
    [];
  assert_type_errors
    {|
      import typing
      def bar(input: typing.Optional[typing.Set[int]]) -> typing.Set[int]:
          if not input:
            input = set()
          return input
    |}
    [];
  ()


let test_check_invalid_type_variables context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar("T")
      def f(x: T) -> T:
        return x
    |}
    [];
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar("T")
      def f() -> T:
        return T
    |}
    [
      "Invalid type variable [34]: The type variable `Variable[T]` isn't present in the function's \
       parameters.";
    ];
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar("T")
      class C:
        x: T = 1
    |}
    [
      "Invalid type variable [34]: The current class isn't generic with respect to the type \
       variable `Variable[T]`.";
    ];
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar("T")
      x: T = ...
    |}
    [
      "Invalid type variable [34]: The type variable `Variable[T]` can only be used to annotate \
       generic classes or functions.";
    ];

  (* We don't error for inferred generics. *)
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar("T")
      class C(typing.Generic[T]):
        pass
      class D(C[T]):
        pass
    |}
    [];

  (* The inline Callable type does not actually make a new type variable scope *)
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar("T")
      def f() -> typing.Callable[[T], T]:
        def g(x: T) -> T:
          return x
        return g
    |}
    [
      "Invalid type variable [34]: The type variable `Variable[T]` isn't present in the function's \
       parameters.";
    ];

  (* Check invalid type variables in parameters and returns. *)
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar("T", covariant=True)
      class Foo(typing.Generic[T]):
        def foo(self, x: T) -> T:
          return x
    |}
    [
      "Invalid type variance [46]: The type variable `Variable[T](covariant)` is covariant "
      ^ "and cannot be a parameter type.";
    ];
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar("T", covariant=True)
      class Foo(typing.Generic[T]):
        def __init__(self, x: T) -> None:
          return
    |}
    [];
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar("T", covariant=True)
      class Foo(typing.Generic[T]):
        def foo(self, x: typing.List[T]) -> T:
          return x[0]
    |}
    [];
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar("T", contravariant=True)
      class Foo(typing.Generic[T]):
        def foo(self, x: T) -> T:
          return x
    |}
    [
      "Invalid type variance [46]: The type variable `Variable[T](contravariant)` is "
      ^ "contravariant and cannot be a return type.";
    ];
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar("T", contravariant=True)
      class Foo(typing.Generic[T]):
        def foo(self, x: T) -> typing.List[T]:
          return [x]
    |}
    [];
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar("T", covariant=True)
      def foo(x: T) -> T:
        return x
    |}
    [
      "Invalid type variance [46]: The type variable `Variable[T](covariant)` is covariant "
      ^ "and cannot be a parameter type.";
    ]


let test_check_aliases context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      import typing_extensions
      class C(typing_extensions.Protocol):
        ...
    |}
    [];
  assert_type_errors
    {|
      import typing_extensions
      class C(typing_extensions.Protocol[int]):
        ...
    |}
    [];
  assert_type_errors
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
    [
      "Incompatible return type [7]: Expected `int` but got `unknown`.";
      "Undefined attribute [16]: `BAR` has no attribute `x`.";
      "Incompatible parameter type [6]: Expected `BAR` for 1st positional only parameter to call \
       `foo` but got `FOO`.";
    ];

  (* Locals are not aliases *)
  assert_type_errors
    {|
      def foo() -> None:
        x = int
        y: x = 1
    |}
    ["Undefined or invalid type [11]: Annotation `foo.x` is not defined as a type."];

  assert_type_errors {|
      def foo(type: int) -> None:
        x = type
    |} [];

  (* Aliases to undefined types *)
  assert_type_errors
    {|
      import typing
      MyAlias = typing.Union[int, UndefinedName]
    |}
    [
      "Missing global annotation [5]: Globally accessible variable `MyAlias` has no type specified.";
      "Unbound name [10]: Name `UndefinedName` is used but not defined in the current scope.";
    ];
  (* TODO (T61917464): Surface explicit type aliases registeration failures as type errors *)
  assert_type_errors
    {|
      import typing
      MyAlias: typing.TypeAlias = typing.Union[int, UndefinedName]
    |}
    ["Unbound name [10]: Name `UndefinedName` is used but not defined in the current scope."];
  (* TODO (T61917464): Surface explicit type aliases registeration failures as type errors *)
  assert_type_errors
    {|
      import typing
      MyAlias: typing.TypeAlias = typing.Union[int, "UndefinedName"]
    |}
    [];

  (* Aliases to invalid types *)
  assert_type_errors
    {|
      import typing
      MyAlias = typing.Union[int, 3]
    |}
    ["Missing global annotation [5]: Globally accessible variable `MyAlias` has no type specified."];
  assert_type_errors
    {|
      import typing
      MyAlias: typing.TypeAlias = typing.Union[int, 3]
    |}
    []


let test_final_type context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors {|
      from typing import Final
      x: Final[int] = 3
    |} [];
  assert_type_errors
    {|
      from typing import Final
      x: Final[str] = 3
    |}
    ["Incompatible variable type [9]: x is declared to have type `str` but is used as type `int`."]


let test_check_invalid_inheritance context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from typing import Callable
      class MyCallable(Callable):
        pass
    |}
    [
      "Invalid type parameters [24]: Generic type `Callable` expects 2 type parameters.";
      "Invalid inheritance [39]: `typing.Callable[..., typing.Any]` is not a valid parent class.";
    ];
  assert_type_errors
    {|
      from typing import Any
      class MySpecialClass(Any, int):
        pass
    |}
    ["Invalid inheritance [39]: `typing.Any` is not a valid parent class."];
  ()


let test_check_invalid_generic_inheritance context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
        from typing import Generic, TypeVar

        T = TypeVar('T')
        class Base(Generic[T]):
          def __init__(self, foo: T) -> None:
            self.foo = foo
        class Child(Base[T]): pass
        class InstantiatedChild(Base[int]): pass

        y: Base[str] = Base(0)
        # Check __init__.
        InstantiatedChild("foo")

        x: Child[str] = Child(0)
        correct: Child[str] = Child("bar")
      |}
    [
      "Incompatible variable type [9]: y is declared to have type `Base[str]` but is used as type \
       `Base[int]`.";
      "Incompatible parameter type [6]: Expected `int` for 1st positional only parameter to call \
       `Base.__init__` but got `str`.";
      "Incompatible variable type [9]: x is declared to have type `Child[str]` but is used as type \
       `Child[int]`.";
      "Incompatible variable type [9]: correct is declared to have type `Child[str]` but is used \
       as type `Child[str]`.";
    ];
  (* Check __new__. *)
  assert_type_errors
    {|
        from typing import Generic, TypeVar

        T = TypeVar('T')
        T2 = TypeVar('T2')
        class Base(Generic[T, T2]):
          def __new__(cls, foo: T, bar: T2) -> Base[T, T2]:
            self = super(Base, cls).__new__(cls)
            self.foo = foo
            self.bar = bar
            return self
        class PartialChild(Base[int, T2], Generic[T2]): pass

        PartialChild("hello", "world")
      |}
    [
      "Incompatible parameter type [6]: Expected `int` for 1st positional only parameter to call \
       `Base.__new__` but got `str`.";
    ];
  assert_type_errors
    {|
        from typing import Generic, TypeVar

        T = TypeVar('T')
        T2 = TypeVar('T2')
        class Base(Generic[T, T2]):
          def __init__(self, foo: T, bar: T2) -> None:
            self.foo = foo
            self.bar = bar
        class PartialChild(Base[int, T2], Generic[T2]): pass

        PartialChild("hello", "world")
        y1: PartialChild[str] = PartialChild(0, "hello")
        y2: PartialChild[str] = PartialChild(0, 1)
        y3: PartialChild[str] = PartialChild("hello", 0)
        y4: PartialChild[int] = PartialChild(0, "hello")
      |}
    [
      "Incompatible parameter type [6]: Expected `int` for 1st positional only parameter to call \
       `Base.__init__` but got `str`.";
      "Incompatible variable type [9]: y1 is declared to have type `PartialChild[str]` but is used \
       as type `PartialChild[str]`.";
      "Incompatible variable type [9]: y2 is declared to have type `PartialChild[str]` but is used \
       as type `PartialChild[int]`.";
      "Incompatible variable type [9]: y3 is declared to have type `PartialChild[str]` but is used \
       as type `PartialChild[int]`.";
      "Incompatible parameter type [6]: Expected `int` for 1st positional only parameter to call \
       `Base.__init__` but got `str`.";
      "Incompatible variable type [9]: y4 is declared to have type `PartialChild[int]` but is used \
       as type `PartialChild[str]`.";
    ];
  assert_type_errors
    {|
        from typing import Generic, TypeVar

        T = TypeVar('T')
        T2 = TypeVar('T2')
        class Base(Generic[T, T2]):
          def __init__(self, foo: T, bar: T2) -> None:
            self.foo = foo
            self.bar = bar
        class PartialChildWithConstructor(Base[int, T2], Generic[T2]):
          def __init__(self, first: T2, second: int, third: str) -> None:
            self.foo: int = second
            self.bar: T2 = first
            self.third: str = third

        PartialChildWithConstructor("hello", 0, 0)
        y3: PartialChildWithConstructor[str] = PartialChildWithConstructor(0, 0, "world")
      |}
    [
      "Incompatible parameter type [6]: Expected `str` for 3rd positional only parameter to call \
       `PartialChildWithConstructor.__init__` but got `int`.";
      "Incompatible variable type [9]: y3 is declared to have type \
       `PartialChildWithConstructor[str]` but is used as type `PartialChildWithConstructor[int]`.";
    ];
  assert_type_errors
    {|
        from typing import Generic, TypeVar

        T = TypeVar('T')
        T2 = TypeVar('T2')
        T3 = TypeVar('T3')
        class Base(Generic[T, T2]):
          def __init__(self, foo: T, bar: T2) -> None:
            self.foo = foo
            self.bar = bar
        class TypeNotUsedInConstructor(Base[int, T2], Generic[T2, T3]):
          def __init__(self, first: T2, second: int, third: str) -> None:
            self.foo: int = second
            self.bar: T2 = first
            self.third: str = third

          def identity(self, x: T3) -> T3: ...

        y1: TypeNotUsedInConstructor[str, int]
        reveal_type(y1.identity(0))
        y1.identity("hello")
        reveal_type(y1.identity("hello"))
      |}
    [
      "Revealed type [-1]: Revealed type for `y1.identity(0)` is `int`.";
      "Incompatible parameter type [6]: Expected `int` for 1st positional only parameter to call \
       `TypeNotUsedInConstructor.identity` but got `str`.";
      "Revealed type [-1]: Revealed type for `y1.identity(\"hello\")` is `int`.";
    ];
  assert_type_errors
    {|
        from typing import Generic, TypeVar

        T = TypeVar('T')
        T2 = TypeVar('T2')
        class Base(Generic[T, T2]):
          def __init__(self, foo: T, bar: T2) -> None:
            self.foo = foo
            self.bar = bar
          def generic_method(self, x: T, y: T2) -> None: ...

        class Child(Base[T, T2]): pass
        class PartialChild(Base[int, T2], Generic[T2]): pass

        y1: Base[str, int] = Base("hello", 1)
        y2: Child[str, int] = Child("hello", 1)
        y3: PartialChild[str] = PartialChild(0, "hello")
        def call_base(x: Base[str, int]) -> None:
          x.generic_method("hello", 1)
          x.generic_method("hello", "world")
        def call_child(x: Child[str, int]) -> None:
          x.generic_method("hello", 1)
          x.generic_method("hello", "world")
        def call_partial_child(x: PartialChild[str]) -> None:
          x.generic_method(1, "world")
          x.generic_method("hello", "world")
      |}
    [
      "Incompatible variable type [9]: y1 is declared to have type `Base[str, int]` but is used as \
       type `Base[str, int]`.";
      "Incompatible variable type [9]: y2 is declared to have type `Child[str, int]` but is used \
       as type `Child[str, int]`.";
      "Incompatible variable type [9]: y3 is declared to have type `PartialChild[str]` but is used \
       as type `PartialChild[str]`.";
      "Incompatible parameter type [6]: Expected `int` for 2nd positional only parameter to call \
       `Base.generic_method` but got `str`.";
      "Incompatible parameter type [6]: Expected `int` for 2nd positional only parameter to call \
       `Base.generic_method` but got `str`.";
      "Incompatible parameter type [6]: Expected `int` for 1st positional only parameter to call \
       `Base.generic_method` but got `str`.";
    ];
  ()


let test_check_literal_assignment context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from typing_extensions import Literal
      x: Literal["on", "off"] = "on"
    |}
    [];
  assert_type_errors
    {|
      from typing import Generic, TypeVar

      T = TypeVar('T')
      class Foo(Generic[T]):
        foo: T
        def __init__(self, foo: T) -> None:
          self.foo = foo

      def foo(s: str) -> None:
        string: Foo[str] = Foo(s)
        literal_string: Foo[str] = Foo("bar")
    |}
    [
      "Incompatible variable type [9]: literal_string is declared to have type `Foo[str]` but is \
       used as type `Foo[str]`.";
    ];
  ()


let test_check_safe_cast context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      import pyre_extensions
      def foo(input: float) -> int:
        return pyre_extensions.safe_cast(int, input)
    |}
    [
      "Unsafe cast [49]: `safe_cast` is only permitted to loosen the type of `input`. `float` is \
       not a super type of `input`.";
    ];
  assert_type_errors
    {|
        import pyre_extensions
        def foo(input: int) -> float:
          return pyre_extensions.safe_cast(float, input)
    |}
    []


let test_check_annotation_with_any context =
  assert_type_errors
    ~context
    {|
      from typing import List, Any
      def foo(x: List[Any] = None) -> None:
        pass
    |}
    [
      "Incompatible variable type [9]: x is declared to have type `List[typing.Any]` but is used \
       as type `None`.";
      "Missing parameter annotation [2]: Parameter `x` is used as type `None` and must have a type \
       that does not contain `Any`.";
    ]


let test_check_typevar_arithmetic context =
  let assert_type_errors = assert_type_errors ~context in
  let assert_default_type_errors = assert_default_type_errors ~context in
  assert_type_errors
    {|
      from typing_extensions import Literal
      from pyre_extensions import Add
      x : Add[Literal[1],Literal[2]]
      reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `typing_extensions.Literal[3]`."];
  assert_type_errors
    {|
      from typing import TypeVar
      from typing_extensions import Literal
      from pyre_extensions import Add

      N = TypeVar("N", bound=int)
      def f1(a : N) -> Add[N,Literal[3]]: ...
      def f2(a : N) -> Add[Literal[3],N]: ...
      reveal_type(f1(2))
      reveal_type(f2(2))
    |}
    [
      "Revealed type [-1]: Revealed type for `test.f1(2)` is `typing_extensions.Literal[5]`.";
      "Revealed type [-1]: Revealed type for `test.f2(2)` is `typing_extensions.Literal[5]`.";
    ];
  assert_type_errors
    {|
      from typing import TypeVar
      from typing_extensions import Literal
      from pyre_extensions import Add

      N = TypeVar("N", bound=int)
      M = TypeVar("M", bound=int)
      def f(a : N, b : M) -> Add[N,M]: ...
      reveal_type(f(1,2))
    |}
    ["Revealed type [-1]: Revealed type for `test.f(1, 2)` is `typing_extensions.Literal[3]`."];
  assert_type_errors
    {|
      from typing import TypeVar
      from typing_extensions import Literal
      from pyre_extensions import Add

      N = TypeVar("N", bound=int)
      # 3N + 3
      def f(x : N) -> Add[Add[Add[N,N],Literal[3]],N]: ...
      reveal_type(f(1))
    |}
    ["Revealed type [-1]: Revealed type for `test.f(1)` is `typing_extensions.Literal[6]`."];
  assert_type_errors
    {|
      from typing import TypeVar
      from typing_extensions import Literal
      from pyre_extensions import Add

      N = TypeVar("N", bound=int)
      def f1(n : N) -> Add[N,N]: ...
      def f2(n : N) -> Add[N,N]:
        return f1(n)
    |}
    [];
  assert_type_errors
    {|
      from typing import TypeVar
      from typing_extensions import Literal
      from pyre_extensions import Add, Multiply

      N = TypeVar("N", bound=int)
      def f1(n : N) -> Multiply[N,Literal[2]]: ...
      def f2(n : N) -> Add[N,N]:
        return f1(n)
    |}
    [];
  assert_type_errors
    {|
      from typing import TypeVar, Generic
      from typing_extensions import Literal
      from pyre_extensions import Add

      N = TypeVar("N", bound=int)
      class Vec(Generic[N]): pass
      def push(a : Vec[N]) -> Vec[Add[N,Literal[1]]]: ...
      def pop(a : Vec[Add[N,Literal[1]]]) -> Vec[N] : ...

      v : Vec[Literal[10]]
      reveal_type(push(v))
      reveal_type(pop(v))
    |}
    [
      "Revealed type [-1]: Revealed type for `test.push(v)` is `Vec[typing_extensions.Literal[11]]`.";
      "Revealed type [-1]: Revealed type for `test.pop(v)` is `Vec[typing_extensions.Literal[9]]`.";
    ];
  assert_type_errors
    {|
      from typing import TypeVar
      from typing_extensions import Literal
      from pyre_extensions import Add, Multiply, Divide

      N = TypeVar("N", bound=int)
      A = TypeVar("A")
      def f1(a : A, n : N) -> Add[A,N]: ...
      def f2(a : A) -> Multiply[A,Literal[3]]: ...
      def f3(a : A) -> Divide[A,Literal[3]]: ...
    |}
    [
      "Invalid type parameters [24]: Type parameter `Variable[A]` violates constraints on \
       `pyre_extensions.Add`/`pyre_extensions.Multiply`/`pyre_extensions.Divide`. Add & Multiply & \
       Divide only accept type variables with a bound that's a subtype of int.";
      "Invalid type parameters [24]: Type parameter `Variable[A]` violates constraints on \
       `pyre_extensions.Add`/`pyre_extensions.Multiply`/`pyre_extensions.Divide`. Add & Multiply & \
       Divide only accept type variables with a bound that's a subtype of int.";
      "Invalid type parameters [24]: Type parameter `Variable[A]` violates constraints on \
       `pyre_extensions.Add`/`pyre_extensions.Multiply`/`pyre_extensions.Divide`. Add & Multiply & \
       Divide only accept type variables with a bound that's a subtype of int.";
    ];
  assert_type_errors
    {|
      from typing import TypeVar
      from typing_extensions import Literal
      from pyre_extensions import Add, Multiply, Divide

      N = TypeVar("N", bound=int)
      def f1(n : N) -> Add[N,Literal["foo"]]: ...
      def f2(n : N) -> Multiply[N,Literal["foo"]]: ...
      def f3(n : N) -> Divide[N,Literal["foo"]]: ...
    |}
    [
      "Invalid type parameters [24]: Type parameter `typing_extensions.Literal['foo']` violates \
       constraints on `Variable[pyre_extensions._B (bound to int)]` in generic type `Add`.";
      "Invalid type parameters [24]: Type parameter `typing_extensions.Literal['foo']` violates \
       constraints on `Variable[pyre_extensions._B (bound to int)]` in generic type `Multiply`.";
      "Invalid type parameters [24]: Type parameter `typing_extensions.Literal['foo']` violates \
       constraints on `Variable[pyre_extensions._B (bound to int)]` in generic type `Divide`.";
    ];
  assert_type_errors
    {|
      from typing import TypeVar, Generic
      from typing_extensions import Literal
      from pyre_extensions import Add

      N = TypeVar("N", bound=int)
      M = TypeVar("M", bound=int)

      class Vec(Generic[N]): pass
      def pop(a : Vec[Add[N,M]], b : M) -> Vec[N] : ...

      v : Vec[Literal[10]]
      pop(v,3)
    |}
    (* Currently this is a limitation of our system. We can only resolve subtyping relationships on
       IntExpressions of the form: factor * N + constant <: factor' * M + constant'. That is because
       to set bounds we need to isolate one type variable in one side of the expression what
       requires the use of some algebra that is not allowed due to the lack of inverse operators
       (subtraction and division)

       Eventually, we would like this to work at least for polynomials of degree 1. See T70449275 *)
    [
      "Incompatible parameter type [6]: Expected `Vec[pyre_extensions.IntExpression[M + N]]` for \
       1st positional only parameter to call `pop` but got `Vec[int]`.";
    ];
  assert_default_type_errors
    {|
      from typing import Any
      from pyre_extensions import Add
      from typing_extensions import Literal

      a : Add[Literal[3],int]
      b : Add[Literal[4],Any]
      c : Add[int,Any]

      reveal_type(a)
      reveal_type(b)
      reveal_type(c)
    |}
    [
      "Revealed type [-1]: Revealed type for `a` is `int`.";
      "Revealed type [-1]: Revealed type for `b` is `typing.Any`.";
      "Revealed type [-1]: Revealed type for `c` is `typing.Any`.";
    ];
  assert_default_type_errors
    {|
      from typing import Any
      from pyre_extensions import Multiply
      from typing_extensions import Literal

      a : Multiply[Literal[3],int]
      b : Multiply[Literal[4],Any]
      c : Multiply[int,Any]

      reveal_type(a)
      reveal_type(b)
      reveal_type(c)
    |}
    [
      "Revealed type [-1]: Revealed type for `a` is `int`.";
      "Revealed type [-1]: Revealed type for `b` is `typing.Any`.";
      "Revealed type [-1]: Revealed type for `c` is `typing.Any`.";
    ];
  assert_default_type_errors
    {|
      from typing import Any
      from pyre_extensions import Divide
      from typing_extensions import Literal

      a : Divide[Literal[3],int]
      b : Divide[Literal[4],Any]
      c : Divide[int,Any]

      reveal_type(a)
      reveal_type(b)
      reveal_type(c)
    |}
    [
      "Revealed type [-1]: Revealed type for `a` is `int`.";
      "Revealed type [-1]: Revealed type for `b` is `typing.Any`.";
      "Revealed type [-1]: Revealed type for `c` is `typing.Any`.";
    ];
  assert_default_type_errors
    {|
      from typing import Any, TypeVar, Generic
      from pyre_extensions import Add
      from typing_extensions import Literal

      A = TypeVar("A", bound=int)
      B = TypeVar("B", bound=int)

      class Vec(Generic[A]): ...

      def add(a : Vec[A], b : Vec[B]) -> Vec[Add[A,B]]: ...

      a : Vec[Literal[5]]
      b : Vec[int]
      c : Vec[Any]
      c1 = add(a,b)
      c2 = add(a,c)
      c3 = add(b,c)

      reveal_type(c1)
      reveal_type(c2)
      reveal_type(c3)
    |}
    [
      "Revealed type [-1]: Revealed type for `c1` is `Vec[int]`.";
      "Revealed type [-1]: Revealed type for `c2` is `Vec[typing.Any]`.";
      "Revealed type [-1]: Revealed type for `c3` is `Vec[typing.Any]`.";
    ];
  assert_default_type_errors
    {|
      from typing import Any, TypeVar, Generic
      from pyre_extensions import Multiply
      from typing_extensions import Literal

      A = TypeVar("A", bound=int)
      B = TypeVar("B", bound=int)

      class Vec(Generic[A]): ...

      def multiply(a : Vec[A], b : Vec[B]) -> Vec[Multiply[A,B]]: ...

      a : Vec[Literal[5]]
      b : Vec[int]
      c : Vec[Any]
      c1 = multiply(a,b)
      c2 = multiply(a,c)
      c3 = multiply(b,c)

      reveal_type(c1)
      reveal_type(c2)
      reveal_type(c3)
    |}
    [
      "Revealed type [-1]: Revealed type for `c1` is `Vec[int]`.";
      "Revealed type [-1]: Revealed type for `c2` is `Vec[typing.Any]`.";
      "Revealed type [-1]: Revealed type for `c3` is `Vec[typing.Any]`.";
    ];
  assert_default_type_errors
    {|
      from typing import Any, TypeVar, Generic
      from pyre_extensions import Divide
      from typing_extensions import Literal

      A = TypeVar("A", bound=int)
      B = TypeVar("B", bound=int)

      class Vec(Generic[A]): ...

      def divide(a : Vec[A], b : Vec[B]) -> Vec[Divide[A,B]]: ...

      a : Vec[Literal[5]]
      b : Vec[int]
      c : Vec[Any]
      c1 = divide(a,b)
      c2 = divide(a,c)
      c3 = divide(b,c)

      reveal_type(c1)
      reveal_type(c2)
      reveal_type(c3)
    |}
    [
      "Revealed type [-1]: Revealed type for `c1` is `Vec[int]`.";
      "Revealed type [-1]: Revealed type for `c2` is `Vec[typing.Any]`.";
      "Revealed type [-1]: Revealed type for `c3` is `Vec[typing.Any]`.";
    ]


let test_check_variadic_arithmetic context =
  let assert_type_errors = assert_type_errors ~context in
  let assert_default_type_errors = assert_default_type_errors ~context in
  assert_type_errors
    {|
      from pyre_extensions import Length

      x1 : Length[int,str]
      x2 : Length[[int,str]]

      reveal_type(x1)
      reveal_type(x2)
    |}
    [
      "Revealed type [-1]: Revealed type for `x1` is `typing_extensions.Literal[2]`.";
      "Revealed type [-1]: Revealed type for `x2` is `typing_extensions.Literal[2]`.";
    ];
  assert_default_type_errors
    {|
      from typing import TypeVar
      from typing_extensions import Literal
      from pyre_extensions import ListVariadic, Product, Add

      A = TypeVar("A", bound=int)

      def f(a : A) -> Product[Add[A,Literal[1]],Literal[3]]: ...
      x1 : Product[Literal[3],Literal[2]]
      x2 : Product[[Literal[3],Literal[2]]]
      x3 = f(2+3) # 2+3 : int

      reveal_type(x1)
      reveal_type(x2)
      reveal_type(x3)
      reveal_type(f)
    |}
    [
      "Revealed type [-1]: Revealed type for `x1` is `typing_extensions.Literal[6]`.";
      "Revealed type [-1]: Revealed type for `x2` is `typing_extensions.Literal[6]`.";
      "Revealed type [-1]: Revealed type for `x3` is `int`.";
      "Revealed type [-1]: Revealed type for `test.f` is `typing.Callable(f)[[Named(a, Variable[A \
       (bound to int)])], pyre_extensions.IntExpression[3 + 3A]]`.";
    ];
  assert_default_type_errors
    {|
      from pyre_extensions import ListVariadic, Product, Length

      Ts = ListVariadic("Ts")

      def f1( *ts : Ts) -> Length[Ts]: ...
      def f2( *ts : Ts) -> Product[Ts]: ...

      x1 = f1(2,3)
      x2 = f2(2,3)

      reveal_type(x1)
      reveal_type(x2)
    |}
    [
      "Revealed type [-1]: Revealed type for `x1` is `typing_extensions.Literal[2]`.";
      "Revealed type [-1]: Revealed type for `x2` is `typing_extensions.Literal[6]`.";
    ];
  assert_default_type_errors
    {|
      from typing import TypeVar, Generic
      from typing_extensions import Literal
      from pyre_extensions import ListVariadic, Product, Length

      Ts = ListVariadic("Ts")
      A = TypeVar("A", bound=int)

      class Vector(Generic[Ts]): pass

      def f1(v : Vector[Ts]) -> Length[Ts]: ...
      def f2(v : Vector[Ts]) -> Product[Ts]: ...

      v1 : Vector[int,str,float]
      v2 : Vector[Literal[2],Literal[3],Literal[4]]
      x1 = f1(v1)
      x2 = f2(v2)

      reveal_type(x1)
      reveal_type(x2)
    |}
    [
      "Revealed type [-1]: Revealed type for `x1` is `typing_extensions.Literal[3]`.";
      "Revealed type [-1]: Revealed type for `x2` is `typing_extensions.Literal[24]`.";
    ];
  assert_default_type_errors
    {|
      from typing import TypeVar, Generic
      from typing_extensions import Literal
      from pyre_extensions import ListVariadic, Product, Length

      Ts = ListVariadic("Ts")

      def f1(v : Length[Ts]): ...
      def f2(v : Product[Ts]) : ...

      x1 = f1(2)
      x2 = f2(3)
    |}
    [
      "Incompatible parameter type [6]: Expected `pyre_extensions.IntExpression[Length[test.Ts]]` \
       for 1st positional only parameter to call `f1` but got `int`.";
      "Incompatible parameter type [6]: Expected `pyre_extensions.IntExpression[Product[test.Ts]]` \
       for 1st positional only parameter to call `f2` but got `int`.";
    ];
  assert_default_type_errors
    {|
      from typing import TypeVar, Generic
      from typing_extensions import Literal
      from pyre_extensions import ListVariadic, Product, Length
      from pyre_extensions.type_variable_operators import Concatenate as Cat

      Ts = ListVariadic("Ts")
      A = TypeVar("A", bound=int)

      class Vector(Generic[Ts]): pass

      def f1(v : Vector[Ts], a : A) -> Length[Cat[A,Ts]]: ...
      def f2(v : Vector[Ts], a : A) -> Product[Cat[A,Ts]]: ...

      v1 : Vector[int,str,float]
      v2 : Vector[Literal[2],Literal[3],Literal[4]]
      x1 = f1(v1,5)
      x2 = f2(v2,2)

      reveal_type(x1)
      reveal_type(x2)
    |}
    [
      "Revealed type [-1]: Revealed type for `x1` is `typing_extensions.Literal[4]`.";
      "Revealed type [-1]: Revealed type for `x2` is `typing_extensions.Literal[48]`.";
    ];
  assert_default_type_errors
    {|
      from typing import TypeVar, Generic
      from typing_extensions import Literal
      from pyre_extensions import ListVariadic, Product, Length
      from pyre_extensions.type_variable_operators import Concatenate as Cat

      Ts = ListVariadic("Ts")
      A = TypeVar("A", bound=int)

      class Vector(Generic[Ts]): pass

      def f1(v : Vector[Ts]) -> Vector[Cat[Ts,Length[Ts]]]: ...
      def f2(v : Vector[Ts]) -> Vector[Cat[Product[Ts],Ts]]: ...

      v1 : Vector[int,str,float]
      v2 : Vector[Literal[2],Literal[3]]
      x1 = f1(v1)
      x2 = f2(v2)

      reveal_type(x1)
      reveal_type(x2)
    |}
    [
      "Revealed type [-1]: Revealed type for `x1` is `Vector[int, str, float, \
       typing_extensions.Literal[3]]`.";
      "Revealed type [-1]: Revealed type for `x2` is `Vector[typing_extensions.Literal[6], \
       typing_extensions.Literal[2], typing_extensions.Literal[3]]`.";
    ];
  assert_default_type_errors
    {|
      from typing import TypeVar, Generic
      from typing_extensions import Literal
      from pyre_extensions import ListVariadic, Product, Length
      from pyre_extensions.type_variable_operators import Concatenate as Cat

      Ts = ListVariadic("Ts")
      A = TypeVar("A", bound=int)

      class Vector(Generic[Ts]): pass

      def f1(v : Vector[Cat[A,Ts]]) -> Length[Ts]: ...
      def f2(v : Vector[Cat[A,Ts]]) -> Product[Ts]: ...

      v1 : Vector[int,str,float]
      v2 : Vector[Literal[2],Literal[3],Literal[4]]
      x1 = f1(v1)
      x2 = f2(v2)

      reveal_type(x1)
      reveal_type(x2)
    |}
    [
      "Revealed type [-1]: Revealed type for `x1` is `typing_extensions.Literal[2]`.";
      "Revealed type [-1]: Revealed type for `x2` is `typing_extensions.Literal[12]`.";
    ];
  assert_default_type_errors
    {|
      from typing import TypeVar, Generic
      from typing_extensions import Literal
      from pyre_extensions import ListVariadic, Product, Length

      Ts = ListVariadic("Ts")
      A = TypeVar("A", bound=int)

      class Vector(Generic[Ts]): pass

      def f1(v : Vector[Ts]) -> Product[Length[Ts],Length[Ts]] : ...
      def f2(v : Vector[Ts]) -> Length[Product[Ts],Product[Ts]] : ...

      v : Vector[Literal[2],Literal[3],Literal[4]]
      x1 = f1(v)
      x2 = f2(v)

      reveal_type(x1)
      reveal_type(x2)
    |}
    [
      "Revealed type [-1]: Revealed type for `x1` is `typing_extensions.Literal[9]`.";
      "Revealed type [-1]: Revealed type for `x2` is `typing_extensions.Literal[2]`.";
    ];
  assert_default_type_errors
    {|
      from typing import TypeVar, Generic
      from typing_extensions import Literal
      from pyre_extensions import ListVariadic, Add, Multiply, Product, Length
      from pyre_extensions.type_variable_operators import Concatenate as Cat

      Ts = ListVariadic("Ts")
      A = TypeVar("A", bound=int)
      B = TypeVar("B", bound=int)

      class Vector(Generic[Ts]): pass

      def f(v : Vector[Cat[A,B,Ts]]) -> Add[B,Multiply[Length[Ts],Product[Cat[A,Ts]]]] : ...

      v : Vector[Literal[2],Literal[3],Literal[4],Literal[5]]
      x = f(v) # 3 + (Length[4,5] * Prod[2,4,5]) -> 3 + (2*40) -> 83

      reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `typing_extensions.Literal[83]`."];
  assert_type_errors
    (* Currently ListVariadic cannot be bounded, so when Product is given a non int type it returns
       Bottom *)
    {|
      from pyre_extensions import Product

      x : Product[str,int]

      reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `undefined`."]


let test_check_typevar_division context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from typing import TypeVar
      from pyre_extensions import Divide

      A = TypeVar("A",bound=int)
      B = TypeVar("B",bound=int)
      # A + A/2
      def div(a: A, b: B) -> Divide[A,B]: ...

      def foo() -> None:
        x1 = div(2,2) # 2//2 = 1
        x2 = div(3,2) # 3//2 = 1
        x3 = div(-3,2) # -3//2 = -2
        reveal_type(x1)
        reveal_type(x2)
        reveal_type(x3)
    |}
    [
      "Revealed type [-1]: Revealed type for `x1` is `typing_extensions.Literal[1]`.";
      "Revealed type [-1]: Revealed type for `x2` is `typing_extensions.Literal[1]`.";
      "Revealed type [-1]: Revealed type for `x3` is `typing_extensions.Literal[-2]`.";
    ];
  assert_type_errors
    {|
      from typing import TypeVar
      from typing_extensions import Literal
      from pyre_extensions import Divide, Add

      A = TypeVar("A",bound=int)
      # A + A/2
      def f(a : A) -> Add[A, Divide[A,Literal[2]]]: ...

      def foo() -> None:
        x = f(3)
        reveal_type(x)
        reveal_type(f)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `typing_extensions.Literal[4]`.";
      "Revealed type [-1]: Revealed type for `test.f` is `typing.Callable(f)[[Named(a, Variable[A \
       (bound to int)])], pyre_extensions.IntExpression[A + (A//2)]]`.";
    ];
  assert_type_errors
    {|
      from typing import TypeVar
      from typing_extensions import Literal
      from pyre_extensions import Divide, Add

      A = TypeVar("A",bound=int)
      # A//2 + A//2
      def f(a : A) -> Add[Divide[A,Literal[2]], Divide[A,Literal[2]]]: ...

      def foo() -> None:
        x = f(3)
        reveal_type(x)
        reveal_type(f)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `typing_extensions.Literal[2]`.";
      "Revealed type [-1]: Revealed type for `test.f` is `typing.Callable(f)[[Named(a, Variable[A \
       (bound to int)])], pyre_extensions.IntExpression[2(A//2)]]`.";
    ];
  assert_type_errors
    {|
      from typing import TypeVar
      from typing_extensions import Literal
      from pyre_extensions import Divide, Add, Multiply

      A = TypeVar("A",bound=int)
      # A//2 - A//2
      def f(a : A) -> Add[Divide[A,Literal[2]], Multiply[Literal[-1],Divide[A,Literal[2]]]]: ...

      def foo() -> None:
        x = f(3)
        reveal_type(x)
        reveal_type(f)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `typing_extensions.Literal[0]`.";
      "Revealed type [-1]: Revealed type for `test.f` is `typing.Callable(f)[[Named(a, Variable[A \
       (bound to int)])], typing_extensions.Literal[0]]`.";
    ];
  assert_type_errors
    {|
      from typing import TypeVar
      from typing_extensions import Literal
      from pyre_extensions import Divide, Add

      A = TypeVar("A",bound=int)
      # 1 + 2//A + A//3
      def f(a : A) -> Add[Add[Literal[1], Divide[Literal[2],A]], Divide[A,Literal[3]]]: ...

      def foo() -> None:
        x = f(3)
        reveal_type(x)
        reveal_type(f)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `typing_extensions.Literal[2]`.";
      "Revealed type [-1]: Revealed type for `test.f` is `typing.Callable(f)[[Named(a, Variable[A \
       (bound to int)])], pyre_extensions.IntExpression[1 + (A//3) + (2//A)]]`.";
    ];
  assert_type_errors
    {|
      from typing import TypeVar
      from typing_extensions import Literal
      from pyre_extensions import Divide, Add, Multiply

      A = TypeVar("A",bound=int)
      B = TypeVar("B",bound=int)
      # (3 + (A+B)//A + AB//A) * (A+2)//B
      def f(a: A, b: B) -> Multiply[
                              Add[
                                  Add[
                                  Divide[Add[A, B],A],
                                  Divide[Multiply[A,B],A]
                                  ],
                                  Literal[3]
                              ],
                              Divide[Add[A,Literal[2]],B]
                          ]: ...

      def foo() -> None:
        x = f(2,3)
        reveal_type(x)
        reveal_type(f)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `typing_extensions.Literal[8]`.";
      "Revealed type [-1]: Revealed type for `test.f` is `typing.Callable(f)[[Named(a, Variable[A \
       (bound to int)]), Named(b, Variable[B (bound to int)])], pyre_extensions.IntExpression[3((2 \
       + A)//B) + B((2 + A)//B) + ((A + B)//A)((2 + A)//B)]]`.";
    ];
  assert_type_errors
    {|
      from typing import TypeVar
      from typing_extensions import Literal as L
      from pyre_extensions import Divide, Add

      A = TypeVar("A",bound=int)
      B = TypeVar("B",bound=int)
      # A//8 + A//8 + A//4 + A//2 + A
      def f(a: A) -> Add[Divide[A,L[8]],Add[Divide[A,L[8]], Add[Divide[A,L[4]],Add[Divide[A,L[2]],A]]]]: ...

      def foo() -> None:
        x = f(2)
        reveal_type(x)
        reveal_type(f)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `typing_extensions.Literal[3]`.";
      "Revealed type [-1]: Revealed type for `test.f` is `typing.Callable(f)[[Named(a, Variable[A \
       (bound to int)])], pyre_extensions.IntExpression[A + (A//2) + (A//4) + 2(A//8)]]`.";
    ];
  assert_type_errors
    {|
      from typing import TypeVar, Generic
      from typing_extensions import Literal as L
      from pyre_extensions import Divide, Multiply, Product as Prod, ListVariadic
      from pyre_extensions.type_variable_operators import Concatenate as Cat

      A = TypeVar("A",bound=int)
      Shape = ListVariadic("Shape")
      Ts = ListVariadic("Ts")

      class Tensor(Generic[Ts]): ...

      # Tensor[A,Prod(Shape)//(A*Prod(Ts)) ,Ts]
      def view(
          t : Tensor[Shape],
          a : A,
          b : L[-1],
          *ts : Ts) -> Tensor[Cat[A,Divide[Prod[Shape],Multiply[A,Prod[Ts]]],Ts]] : ...

      def foo() -> None:
        x : Tensor[L[5],L[3],L[12]]
        y1 = view(x,5,-1,3,6,1) # Tensor[5,2,3,6,1]
        y2 = view(x,3,-1,10) # Tensor[3,6,10]
        reveal_type(y1)
        reveal_type(y2)
    |}
    [
      "Revealed type [-1]: Revealed type for `y1` is `Tensor[typing_extensions.Literal[5], \
       typing_extensions.Literal[2], typing_extensions.Literal[3], typing_extensions.Literal[6], \
       typing_extensions.Literal[1]]`.";
      "Revealed type [-1]: Revealed type for `y2` is `Tensor[typing_extensions.Literal[3], \
       typing_extensions.Literal[6], typing_extensions.Literal[10]]`.";
    ];
  assert_type_errors
    {|
      from typing import TypeVar
      from typing_extensions import Literal
      from pyre_extensions import Divide

      A = TypeVar("A",bound=int)
      def f1() -> Divide[Literal[3],Literal[0]] : ...
      def f2(a : A) -> Divide[Literal[3],A] : ...

      def foo() -> None:
        x1 = f1()
        x2 = f2(0)
        reveal_type(x1)
        reveal_type(x2)
    |}
    [
      "Revealed type [-1]: Revealed type for `x1` is `undefined`.";
      "Revealed type [-1]: Revealed type for `x2` is `undefined`.";
    ]


let test_check_typevar_division_simplify context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from typing import TypeVar
      from typing_extensions import Literal
      from pyre_extensions import Divide, Add

      A = TypeVar("A",bound=int)
      # A + A//1
      def f(a : A) -> Add[A,Divide[A,Literal[1]]]: ...

      def foo() -> None:
        x = f(3)
        reveal_type(x)
        reveal_type(f)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `typing_extensions.Literal[6]`.";
      "Revealed type [-1]: Revealed type for `test.f` is `typing.Callable(f)[[Named(a, Variable[A \
       (bound to int)])], pyre_extensions.IntExpression[2A]]`.";
    ];
  assert_type_errors
    {|
      from typing import TypeVar
      from typing_extensions import Literal
      from pyre_extensions import Divide, Add, Multiply

      A = TypeVar("A",bound=int)
      # A/2 + 2A/4
      def f(a : A) -> Add[Divide[A,Literal[2]],Divide[Multiply[A,Literal[2]],Literal[4]]]: ...

      def foo() -> None:
        x = f(3)
        reveal_type(x)
        reveal_type(f)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `typing_extensions.Literal[2]`.";
      "Revealed type [-1]: Revealed type for `test.f` is `typing.Callable(f)[[Named(a, Variable[A \
       (bound to int)])], pyre_extensions.IntExpression[2(A//2)]]`.";
    ];
  assert_type_errors
    {|
      from typing import TypeVar
      from typing_extensions import Literal as L
      from pyre_extensions import Divide, Add, Multiply

      A = TypeVar("A",bound=int)
      B = TypeVar("B",bound=int)
      # (9B + 12A)/(6A + 15B)
      def f(a : A, b : B) -> Divide[Add[Multiply[L[9],B],Multiply[L[12],A]],Add[Multiply[L[6],A],Multiply[L[15],B]]]: ...

      def foo() -> None:
        # 26/20
        x = f(5,2)
        reveal_type(x)
        reveal_type(f)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `typing_extensions.Literal[1]`.";
      "Revealed type [-1]: Revealed type for `test.f` is `typing.Callable(f)[[Named(a, Variable[A \
       (bound to int)]), Named(b, Variable[B (bound to int)])], pyre_extensions.IntExpression[((4A \
       + 3B)//(2A + 5B))]]`.";
    ];
  assert_type_errors
    {|
      from typing import TypeVar
      from typing_extensions import Literal
      from pyre_extensions import Divide, Add, Multiply

      A = TypeVar("A",bound=int)
      B = TypeVar("B",bound=int)
      # (A+B)//A + AB//A
      def f(a : A, b : B) -> Add[Divide[Add[A,B],A], Divide[Multiply[A,B],A]]: ...

      def foo() -> None:
        # (2+3)//2 + 2*3//2 -> 5//2 + 6//2 -> 11//2 -> 5
        x = f(2,3)
        reveal_type(x)
        reveal_type(f)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `typing_extensions.Literal[5]`.";
      "Revealed type [-1]: Revealed type for `test.f` is `typing.Callable(f)[[Named(a, Variable[A \
       (bound to int)]), Named(b, Variable[B (bound to int)])], pyre_extensions.IntExpression[B + \
       ((A + B)//A)]]`.";
    ]


let test_check_broadcast_outside_concatenation context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from typing import Generic
      from typing_extensions import Literal as L
      from pyre_extensions import Broadcast as BC, ListVariadic

      Ts0 = ListVariadic("Ts0")
      Ts1 = ListVariadic("Ts1")

      class Tensor(Generic[Ts0]): ...

      def f(x : Tensor[Ts0], y : Tensor[Ts1]) -> Tensor[BC[Ts0,Ts1]]: ...

      def foo() -> None:
        x1 : Tensor[L[2],L[3]]
        y1 : Tensor[L[2],L[3]]
        z1 = f(x1,y1)

        x2 : Tensor[L[1],L[3]]
        y2 : Tensor[L[2],L[1]]
        z2 = f(x2,y2)

        x3 : Tensor[L[4],L[5],L[1],L[3]]
        y3 : Tensor[L[2],L[1]]
        z3 = f(x3,y3)

        reveal_type(z1)
        reveal_type(z2)
        reveal_type(z3)
    |}
    [
      "Revealed type [-1]: Revealed type for `z1` is `Tensor[typing_extensions.Literal[2], \
       typing_extensions.Literal[3]]`.";
      "Revealed type [-1]: Revealed type for `z2` is `Tensor[typing_extensions.Literal[2], \
       typing_extensions.Literal[3]]`.";
      "Revealed type [-1]: Revealed type for `z3` is `Tensor[typing_extensions.Literal[4], \
       typing_extensions.Literal[5], typing_extensions.Literal[2], typing_extensions.Literal[3]]`.";
    ];
  assert_type_errors
    {|
      from typing import Generic
      from typing_extensions import Literal as L
      from pyre_extensions import Broadcast as BC, ListVariadic

      Ts0 = ListVariadic("Ts0")
      Ts1 = ListVariadic("Ts1")

      class Tensor(Generic[Ts0]): ...

      def f(x : Tensor[Ts0], y : Tensor[Ts1]) -> Tensor[BC[Ts0,Ts1]]: ...

      def foo() -> None:
        x1 : Tensor[L[2],L[4]]
        y1 : Tensor[L[2],L[3]]
        z1 = f(x1,y1)

        x2 : Tensor[L[2],int]
        y2 : Tensor[L[2],L[3]]
        z2 = f(x2,y2)

        x3 : Tensor[L[2],str]
        y3 : Tensor[L[2],L[3]]
        z3 = f(x3,y3)

        reveal_type(z1)
        reveal_type(z2)
        reveal_type(z3)
    |}
    [
      "Revealed type [-1]: Revealed type for `z1` is `Tensor[typing_extensions.Literal[2], \
       typing.Any]`.";
      "Revealed type [-1]: Revealed type for `z2` is `Tensor[typing_extensions.Literal[2], int]`.";
      "Revealed type [-1]: Revealed type for `z3` is `Tensor[typing_extensions.Literal[2], \
       typing.Any]`.";
    ];
  assert_type_errors
    {|
      from typing import  Generic
      from typing_extensions import Literal as L
      from pyre_extensions import Broadcast as BC, ListVariadic

      Ts0 = ListVariadic("Ts0")
      Ts1 = ListVariadic("Ts1")
      Ts2 = ListVariadic("Ts2")

      class Tensor(Generic[Ts0]): ...

      def f(x : Tensor[Ts0], y : Tensor[Ts1], z : Tensor[Ts2]) -> Tensor[BC[Ts0,BC[Ts1,Ts2]]]: ...

      def foo() -> None:
        x1 : Tensor[L[2],L[3]]
        y1 : Tensor[L[2],L[3]]
        z1 : Tensor[L[2],L[3]]
        v1 = f(x1,y1,z1)

        x2 : Tensor[L[3],L[2],L[1]]
        y2 : Tensor[L[1],L[2],L[1]]
        z2 : Tensor[L[1],L[2],L[4]]
        v2 = f(x2,y2,z2)

        reveal_type(v1)
        reveal_type(v2)
    |}
    [
      "Revealed type [-1]: Revealed type for `v1` is `Tensor[typing_extensions.Literal[2], \
       typing_extensions.Literal[3]]`.";
      "Revealed type [-1]: Revealed type for `v2` is `Tensor[typing_extensions.Literal[3], \
       typing_extensions.Literal[2], typing_extensions.Literal[4]]`.";
    ];
  assert_type_errors
    {|
      from typing import  Generic
      from typing_extensions import Literal as L
      from pyre_extensions import Broadcast as BC, ListVariadic

      Ts = ListVariadic("Ts")

      class Tensor(Generic[Ts]): ...

      def f(x : Tensor[Ts]) -> Tensor[BC[[L[1],L[3]],Ts]]: ...

      def foo() -> None:
        x1 : Tensor[L[2],L[3]]
        y1 = f(x1)
        reveal_type(y1)

        x2 : Tensor[L[2],L[4]]
        y2 = f(x2)
        reveal_type(y2)
    |}
    [
      "Revealed type [-1]: Revealed type for `y1` is `Tensor[typing_extensions.Literal[2], \
       typing_extensions.Literal[3]]`.";
      "Revealed type [-1]: Revealed type for `y2` is `Tensor[typing_extensions.Literal[2], \
       typing.Any]`.";
    ];
  assert_type_errors
    {|
      from typing import TypeVar, Generic
      from typing_extensions import Literal as L
      from pyre_extensions import Broadcast as BC, ListVariadic
      from pyre_extensions.type_variable_operators import Concatenate as Cat

      A = TypeVar("A",bound=int)
      Ts = ListVariadic("Ts")
      Ts1 = ListVariadic("Ts1")

      class Tensor(Generic[Ts]): ...

      def f(x : Tensor[Cat[A,Ts]], y : Tensor[Ts1]) -> Tensor[BC[Cat[A,Ts],Ts1]]: ...

      def foo() -> None:
        x1 : Tensor[L[1],L[3]]
        x2 : Tensor[L[2],L[1]]
        y1 = f(x1,x2)
        reveal_type(y1)
    |}
    [
      "Revealed type [-1]: Revealed type for `y1` is `Tensor[typing_extensions.Literal[2], \
       typing_extensions.Literal[3]]`.";
    ]


let test_check_broadcast_inside_concatenation context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from typing import Generic, TypeVar
      from typing_extensions import Literal as L
      from pyre_extensions import Broadcast as BC, ListVariadic
      from pyre_extensions.type_variable_operators import Concatenate as Cat


      Ts = ListVariadic("Ts")
      Ts1 = ListVariadic("Ts1")
      Ts2 = ListVariadic("Ts2")
      A = TypeVar("A",bound=int)

      class Tensor(Generic[Ts]): ...

      def g(x : Tensor[Ts], a: A) -> Tensor[Cat[Ts,A]]: ...

      def h(x : Tensor[Ts1], y : Tensor[Ts2]) -> Tensor[BC[Ts1,Ts2]]: ...

      def f(t1 : Tensor[Ts1], t2 : Tensor[Ts2]) -> Tensor[Cat[BC[Ts1,Ts2],L[3]]]:
        x = h(t1,t2)
        y = g(x,3)
        return y

      def foo() -> None:
        t1 : Tensor[L[2],L[4]]
        t2 : Tensor[L[2],L[1]]
        result = f(t1,t2)
        reveal_type(result)
    |}
    [
      "Revealed type [-1]: Revealed type for `result` is `Tensor[typing_extensions.Literal[2], \
       typing_extensions.Literal[4], typing_extensions.Literal[3]]`.";
    ];
  assert_type_errors
    {|
      from typing import Generic, TypeVar
      from typing_extensions import Literal as L
      from pyre_extensions import Broadcast as BC, ListVariadic
      from pyre_extensions.type_variable_operators import Concatenate as Cat

      Ts1 = ListVariadic("Ts1")
      Ts2 = ListVariadic("Ts2")
      A = TypeVar("A",bound=int)
      B = TypeVar("B",bound=int)
      C = TypeVar("C",bound=int)

      class Tensor(Generic[Ts1]): ...

      def matmul(x: Tensor[Cat[Ts1,A,B]], y: Tensor[Cat[Ts2,B,C]]) -> Tensor[Cat[BC[Ts1,Ts2],A,C]]: ...

      def foo() -> None:
        t1 : Tensor[L[5],L[2],L[4]]
        t2 : Tensor[L[7],L[1],L[4],L[3]]
        x = matmul(t1,t2)
        reveal_type(x)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `Tensor[typing_extensions.Literal[7], \
       typing_extensions.Literal[5], typing_extensions.Literal[2], typing_extensions.Literal[3]]`.";
    ];
  assert_type_errors
    {|
      from typing import Generic
      from typing_extensions import Literal as L
      from pyre_extensions import Broadcast as BC, ListVariadic
      from pyre_extensions.type_variable_operators import Concatenate as Cat

      Ts1 = ListVariadic("Ts1")
      Ts2 = ListVariadic("Ts2")

      class Tensor(Generic[Ts1]): ...

      def f(x : Tensor[Ts1], y : Tensor[Ts2]) -> Tensor[
          BC[
              Cat[BC[Ts2,Ts1],L[1]],
              Cat[BC[Ts1,Ts2],L[1]]
            ]
          ]: ...

      def foo() -> None:
        t1 : Tensor[L[2],L[4]]
        t2 : Tensor[L[1],L[4]]
        x = f(t1,t2)
        reveal_type(x)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `Tensor[typing_extensions.Literal[2], \
       typing_extensions.Literal[4], typing_extensions.Literal[1]]`.";
    ];
  assert_type_errors
    {|
      from typing import Generic
      from typing_extensions import Literal as L
      from pyre_extensions import Broadcast as BC, ListVariadic
      from pyre_extensions.type_variable_operators import Concatenate as Cat

      Ts1 = ListVariadic("Ts1")
      Ts2 = ListVariadic("Ts2")

      class Tensor(Generic[Ts1]): ...

      def f(t: Tensor[BC[Ts1,Ts2]]) -> Tensor[Ts1]: ...

      def foo() -> None:
        t1 : Tensor[L[5],L[2],L[4]]
        x = f(t1)
        reveal_type(x)
    |}
    [
      "Incomplete type [37]: Type `Tensor[test.Ts1]` inferred for `x` is incomplete, add an \
       explicit annotation.";
      "Incompatible parameter type [6]: Expected \
       `Tensor[pyre_extensions.Broadcast[test.Ts1,test.Ts2]]` for 1st positional only parameter to \
       call `f` but got `Tensor[int, int, int]`.";
      "Revealed type [-1]: Revealed type for `x` is `Tensor[...]`.";
    ]


let () =
  "annotation"
  >::: [
         "check_undefined_type" >:: test_check_undefined_type;
         "check_invalid_type" >:: test_check_invalid_type;
         "check_illegal_annotation_target" >:: test_check_illegal_annotation_target;
         "check_invalid_type_variables" >:: test_check_invalid_type_variables;
         "check_missing_type_parameters" >:: test_check_missing_type_parameters;
         "check_analysis_failure" >:: test_check_analysis_failure;
         "check_immutable_annotations" >:: test_check_immutable_annotations;
         "check_incomplete_annotations" >:: test_check_incomplete_annotations;
         "check_incomplete_callable" >:: test_check_incomplete_callable;
         "check_refinement" >:: test_check_refinement;
         "check_aliases" >:: test_check_aliases;
         "check_final_type" >:: test_final_type;
         "check_invalid_inheritance" >:: test_check_invalid_inheritance;
         "check_invalid_generic_inheritance" >:: test_check_invalid_generic_inheritance;
         "check_literal_assignment" >:: test_check_literal_assignment;
         "check_safe_cast" >:: test_check_safe_cast;
         "check_annotation_with_any" >:: test_check_annotation_with_any;
         "check_typevar_arithmetic" >:: test_check_typevar_arithmetic;
         "check_variadic_arithmetic" >:: test_check_variadic_arithmetic;
         "check_typevar_division" >:: test_check_typevar_division;
         "check_typevar_division_simplify" >:: test_check_typevar_division_simplify;
         "check_broadcast_outside_concatenation" >:: test_check_broadcast_outside_concatenation;
         "check_broadcast_inside_concatenation" >:: test_check_broadcast_inside_concatenation;
       ]
  |> Test.run
