(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest

let test_check_callables context =
  let assert_type_errors = assert_type_errors ~context in
  let assert_default_type_errors = assert_default_type_errors ~context in
  (* Callable parameter checks. *)
  assert_type_errors
    {|
      import typing
      def foo(callable: typing.Callable[[str], None]) -> None:
        callable(1)
    |}
    [
      "Incompatible parameter type [6]: In anonymous call, for 1st positional only parameter \
       expected `str` but got `int`.";
    ];

  (* Type variables & callables. *)
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar('T')
      def foo(x: str) -> int:
        return 0
      def takes_parameter(f: typing.Callable[[T], int]) -> T:
        ...
      def takes_return(f: typing.Callable[[str], T]) -> T:
        ...
      def f() -> str:
        return takes_parameter(foo)
      def g() -> int:
        return takes_return(foo)
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo(f: typing.Callable[[int], int]) -> None:
        ...
      def i2i(x: int) -> int:
        return x
      foo(i2i)
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo(f: typing.Callable[[int], int]) -> None:
        ...
      def i2s(x: int) -> str:
        return ""
      foo(i2s)
    |}
    [
      "Incompatible parameter type [6]: In call `foo`, for 1st positional only parameter expected \
       `typing.Callable[[int], int]` but got `typing.Callable(i2s)[[Named(x, int)], str]`.";
    ];

  (* Classes with __call__ are callables. *)
  assert_type_errors
    {|
      import typing
      class CallMe:
        def __call__(self, x:int) -> str:
          ...
      class CallMeToo(CallMe):
        pass

      def particular_map(f: typing.Callable[[int], str], l: typing.List[int]) -> typing.List[str]:
        ...
      def apply(x: CallMe, y: CallMeToo) -> None:
        particular_map(x, [])
        particular_map(y, [])
    |}
    [];
  assert_type_errors
    {|
      import typing
      class CallMe:
        def __call__(self, x: str) -> str:
          ...
      class CallMeToo(CallMe):
        pass

      def particular_map(f: typing.Callable[[int], str], l: typing.List[int]) -> typing.List[str]:
        ...
      def apply(x: CallMe, y: CallMeToo) -> None:
        particular_map(x, [])
        particular_map(y, [])
    |}
    [
      "Incompatible parameter type [6]: In call `particular_map`, for 1st positional only \
       parameter expected `typing.Callable[[int], str]` but got `CallMe`.";
      "Incompatible parameter type [6]: In call `particular_map`, for 1st positional only \
       parameter expected `typing.Callable[[int], str]` but got `CallMeToo`.";
    ];

  (* Sanity check: Callables do not subclass classes. *)
  assert_type_errors
    {|
      import typing
      class CallMe:
        def __call__(self, x: int) -> str:
          ...
      def particular_map(callable_object: CallMe, x: int) -> None:
         callable_object(x)
      def apply(f: typing.Callable[[int], str]) -> None:
        particular_map(f, 1)
    |}
    [
      "Incompatible parameter type [6]: In call `particular_map`, for 1st positional only \
       parameter expected `CallMe` but got `typing.Callable[[int], str]`.";
    ];

  (* The annotation for callable gets expanded automatically. *)
  assert_type_errors
    {|
      import typing
      def i2i(x: int) -> int:
        return 0
      def hof(c: typing.Callable[[int], int]) -> None:
        return
      hof(i2i)
      hof(1)
    |}
    [
      "Incompatible parameter type [6]: In call `hof`, for 1st positional only parameter expected \
       `typing.Callable[[int], int]` but got `int`.";
    ];
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar("T")
      def foo(x: typing.Callable[[], T]) -> T:
        ...
      def f(x: int = 1) -> str:
        return ""
      reveal_type(foo(f))
    |}
    ["Revealed type [-1]: Revealed type for `test.foo(test.f)` is `str`."];

  (* Lambdas. *)
  assert_type_errors
    {|
      import typing
      def takes_callable(f: typing.Callable[[typing.Any], int]) -> int:
        return 0
      takes_callable(lambda y: 0)
    |}
    [
      "Missing parameter annotation [2]: Parameter `f` must have a type "
      ^ "that does not contain `Any`.";
    ];
  assert_type_errors
    {|
      import typing
      def takes_callable(f: typing.Callable[[typing.Any], int]) -> int:
        return 0
      takes_callable(lambda y: "")
    |}
    [
      "Missing parameter annotation [2]: Parameter `f` must have a type "
      ^ "that does not contain `Any`.";
      "Incompatible parameter type [6]: In call `takes_callable`, for 1st positional only \
       parameter expected `typing.Callable[[typing.Any], int]` but got `typing.Callable[[Named(y, \
       typing.Any)], str]`.";
    ];
  assert_default_type_errors
    {|
      import typing
      def exec(f: typing.Callable[[], int]) -> int:
        return f()
      def with_default(x: int = 0) -> int:
        return x
      def with_kwargs( **kwargs: int) -> int:
        return 0
      def with_varargs( *varargs: int) -> int:
        return 0
      def with_everything( *varargs: int, **kwargs: int) -> int:
        return 0
      exec(with_default)
      exec(with_kwargs)
      exec(with_varargs)
      exec(with_everything)
    |}
    []


let test_check_function_redirects context =
  assert_type_errors ~context {|
      def foo(a: float) -> float:
        return abs(a)
    |} []


let test_check_function_parameters_with_backups context =
  assert_type_errors ~context "(1).__add__(1)" [];
  assert_type_errors ~context "(1).__add__(1j)" [];
  assert_type_errors ~context "(1).__add__(1.0)" [];
  assert_type_errors ~context "(1).__iadd__(1.0)" []


let test_check_function_parameters context =
  let assert_type_errors = assert_type_errors ~context in
  let assert_default_type_errors = assert_default_type_errors ~context in
  assert_type_errors
    {|
      from builtins import int_to_int
      def foo() -> None:
        int_to_int(1)
    |}
    [];
  assert_type_errors
    {|
      from builtins import int_to_int
      int_to_int(1.0)
    |}
    [
      "Incompatible parameter type [6]: In call `int_to_int`, for 1st positional only parameter \
       expected `int` but got `float`.";
    ];
  assert_type_errors
    {|
      from builtins import int_to_int
      def foo() -> None:
        int_to_int(1.0)
    |}
    [
      "Incompatible parameter type [6]: In call `int_to_int`, for 1st positional only parameter \
       expected `int` but got `float`.";
    ];
  assert_type_errors
    {|
      def preprocessed(i: str) -> None:
        pass
      def foo() -> None:
        preprocessed(1.0)
    |}
    [
      "Incompatible parameter type [6]: In call `preprocessed`, for 1st positional only parameter \
       expected `str` but got `float`.";
    ];
  assert_type_errors
    {|
      from builtins import int_to_int
      def foo() -> int:
        return int_to_int(1.0)
    |}
    [
      "Incompatible parameter type [6]: In call `int_to_int`, for 1st positional only parameter \
       expected `int` but got `float`.";
    ];
  assert_type_errors
    {|
      from builtins import int_to_int
      def foo(i) -> None:
        int_to_int(i)
    |}
    ["Missing parameter annotation [2]: Parameter `i` has no type specified."];

  (* Type aliases in signatures are resolved. *)
  assert_type_errors
    "import hashlib; hashlib.md5(1.0)"
    [
      "Incompatible parameter type [6]: In call `hashlib.md5`, for 1st positional only parameter \
       expected `Union[int, str]` but got `float`.";
    ];
  assert_type_errors {|
      def foo(i: int, *, j: int) -> None:
        pass
    |} [];
  assert_type_errors
    {|
      def foo( *args, **kwargs) -> None:
        pass
    |}
    [
      "Missing parameter annotation [2]: Parameter `*args` has no type specified.";
      "Missing parameter annotation [2]: Parameter `**kwargs` has no type specified.";
    ];
  assert_type_errors
    {|
      from builtins import int_to_int
      class A:
        def foo(self) -> None:
          int_to_int(self.attribute)
    |}
    [
      "Incompatible parameter type [6]: In call `int_to_int`, for 1st positional only parameter \
       expected `int` but got `unknown`.";
      "Undefined attribute [16]: `A` has no attribute `attribute`.";
    ];
  assert_type_errors
    {|
      import typing
      def foo(a: typing.Union[str, None]) -> None: pass
      foo(None)
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo(a: typing.Union[str, None, typing.Tuple[int, str]]) -> None:
        pass
      foo(None)
    |}
    [];
  assert_type_errors
    {|
      import typing
      from builtins import to_int, int_to_str
      def foo(a: typing.Optional[int]) -> int:
        return to_int(a and int_to_str(a))
    |}
    [];
  assert_type_errors
    {|
      import typing
      from builtins import to_int, int_to_str
      def foo(a: typing.Optional[int]) -> int:
        return to_int(a or int_to_str(a))
    |}
    [
      "Incompatible parameter type [6]: In call `int_to_str`, for 1st positional only parameter \
       expected `int` but got `Optional[int]`.";
    ];
  assert_type_errors
    {|
      import typing
      def expect_type_float(meta: typing.Type[float]) -> None:
        pass
      expect_type_float(int)
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo(a: int) -> int:
        return a
      def bar() -> None:
        x: typing.Optional[int]
        foo(x if x else 1)
    |}
    [];
  assert_type_errors
    {|
      import typing
      from builtins import Attributes
      def bar(x: typing.Optional[Attributes]) -> None:
          baz(x.int_attribute if x is not None else None)

      def baz(x: typing.Optional[int]) -> None:
          pass
    |}
    [];
  assert_type_errors
    {|
      import typing
      from builtins import Attributes
      def bar(x: typing.Optional[Attributes]) -> None:
          baz(x.int_attribute if x else None)

      def baz(x: typing.Optional[int]) -> None:
          pass
    |}
    [];
  assert_type_errors
    {|
      from builtins import takes_iterable
      def foo(x) -> None:
        takes_iterable(x)
    |}
    ["Missing parameter annotation [2]: Parameter `x` has no type specified."];
  assert_type_errors
    {|
      import typing
      def foo(a: typing.Optional[int]) -> None:
        pass
      foo(None)
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo(a: typing.Optional[int]) -> None:
        pass
      foo("hello")
    |}
    [
      "Incompatible parameter type [6]: In call `foo`, for 1st positional only parameter expected \
       `Optional[int]` but got `str`.";
    ];
  assert_type_errors
    {|
      import typing
      def foo(a: typing.Optional[int], b: str) -> None:
        pass
      foo(1, "hello")
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo(a: typing.Optional[int], b: str) -> None:
        pass
      foo(1, 1)
    |}
    [
      "Incompatible parameter type [6]: In call `foo`, for 2nd positional only parameter expected \
       `str` but got `int`.";
    ];
  assert_default_type_errors
    {|
      import typing
      def f(d: typing.Dict[int, int], x) -> None:
        d.update({ 1: x })
    |}
    [];
  assert_default_type_errors
    {|
      import typing
      def f(d: typing.Dict[int, str], x) -> str:
        return d.get(x, "")
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo() -> None:
        a = {"key": set()}
        b = a.get("key", set())
    |}
    [
      "Incomplete type [37]: Type `typing.Dict[str, typing.Set[Variable[_T]]]` inferred for "
      ^ "`a` is incomplete, add an explicit annotation.";
    ];
  assert_type_errors
    {|
      import typing
      def foo() -> None:
        a: typing.Dict[str, typing.Set[int]] = {"key": set()}
        b = a.get("key", set())
    |}
    [];
  assert_type_errors
    {|
      def foo() -> int:
        for x in [1,2,3]:
          if x > 0:
            return x
          x = 15
        return 0
    |}
    [];
  assert_type_errors
    {|
      x: str
      def foo() -> str:
        return x.__getitem__(0)
    |}
    [];
  assert_type_errors
    {|
      import typing
      x: typing.List[int]
      def foo() -> int:
        return x.__getitem__(0)
    |}
    [];
  assert_type_errors
    {|
      import typing
      x: typing.List[int]
      def foo() -> typing.List[int]:
        return x.__getitem__(slice(0, 1, None))
    |}
    [];
  assert_type_errors
    {|
      def foo(x: int) -> str:
        return ""
      def f() -> None:
        a = foo(1,2)
    |}
    ["Too many arguments [19]: Call `foo` expects 1 positional argument, 2 were provided."];
  assert_type_errors
    {|
      def foo(x: int) -> str:
        return ""
      def f() -> None:
        a = foo()
    |}
    ["Missing argument [20]: Call `foo` expects argument `x`."];
  assert_type_errors
    {|
      class Foo:
        def __init__(self, bar: int, baz: str) -> None:
          pass
      Foo(baz="")
    |}
    ["Missing argument [20]: Call `Foo.__init__` expects argument `bar`."];
  assert_type_errors {|
      def foo(x: int) -> str:
        return str(x)
    |} [];
  assert_type_errors
    {|
      def foo(x: int) -> str:
        return ""
      def f() -> None:
        a = foo(y=4)
    |}
    ["Unexpected keyword [28]: Unexpected keyword argument `y` to call `foo`."];
  assert_type_errors
    {|
      class C:
        def f(self, x: str) -> None:
          ...
      def f(c: C) -> None:
        a = c.f()
    |}
    ["Missing argument [20]: Call `C.f` expects argument `x`."];
  assert_type_errors
    {|
      class C:
        def f(self, x: str) -> None:
          ...
      def f(c: C) -> None:
        a = c.f("", "")
    |}
    ["Too many arguments [19]: Call `C.f` expects 1 positional argument, 2 were provided."];
  assert_type_errors
    {|
      def foo(x: int, y: str) -> str:
        return ""
      def f() -> None:
        a = foo()
    |}
    ["Missing argument [20]: Call `foo` expects argument `x`."];
  assert_type_errors
    {|
      def foo() -> str:
        return ""
      def f() -> None:
        a = foo(1,2,3,4)
    |}
    ["Too many arguments [19]: Call `foo` expects 0 positional arguments, 4 were provided."];
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar("T", bound=int)
      S = typing.Callable[[], T]

      def foo(x: S) -> T: ...
      def foo_with_type_parameter(x: S[T]) -> T: ...

      def bar() -> None:
          reveal_type(foo)
          foo()

          reveal_type(foo_with_type_parameter)
          foo_with_type_parameter()
    |}
    [
      "Invalid type variable [34]: The type variable `Variable[T (bound to int)]` isn't present in \
       the function's parameters.";
      "Revealed type [-1]: Revealed type for `test.foo` is `typing.Callable(foo)[[Named(x, \
       typing.Callable[[], typing.Any])], Variable[T (bound to int)]]`.";
      "Missing argument [20]: Call `foo` expects argument `x`.";
      "Revealed type [-1]: Revealed type for `test.foo_with_type_parameter` is \
       `typing.Callable(foo_with_type_parameter)[[Named(x, typing.Callable[[], Variable[T (bound \
       to int)]])], Variable[T (bound to int)]]`.";
      "Missing argument [20]: Call `foo_with_type_parameter` expects argument `x`.";
    ]


let test_check_function_parameter_errors context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from builtins import str_float_to_int
      class Foo:
        attribute: str = ""
      def foo(input: Foo) -> None:
        str_float_to_int(input.attribute, input.undefined)
    |}
    [
      "Incompatible parameter type [6]: In call `str_float_to_int`, for 2nd positional only \
       parameter expected `float` but got `unknown`.";
      "Undefined attribute [16]: `Foo` has no attribute `undefined`.";
    ];
  assert_type_errors
    {|
      from builtins import str_float_to_int
      class Foo:
        attribute: str = ""
      def foo(input: Foo) -> None:
        str_float_to_int(input.undefined, input.undefined)
    |}
    [
      "Incompatible parameter type [6]: In call `str_float_to_int`, for 1st positional only \
       parameter expected `str` but got `unknown`.";
      "Undefined attribute [16]: `Foo` has no attribute `undefined`.";
      "Incompatible parameter type [6]: In call `str_float_to_int`, for 2nd positional only \
       parameter expected `float` but got `unknown`.";
    ];
  assert_type_errors
    {|
      import typing
      from builtins import optional_str_to_int
      class Foo:
        attribute: int = 1
      def foo(input: typing.Optional[Foo]) -> None:
        optional_str_to_int(input and input.attribute)
    |}
    [
      "Incompatible parameter type [6]: In call `optional_str_to_int`, for 1st positional only \
       parameter expected `Optional[str]` but got `Union[None, Foo, int]`.";
    ];
  assert_type_errors
    {|
      import typing
      from builtins import optional_str_to_int
      class Foo:
        attribute: int = 1
      def foo(input: typing.Optional[Foo]) -> None:
        optional_str_to_int(input and input.undefined)
    |}
    [
      "Incompatible parameter type [6]: In call `optional_str_to_int`, for 1st positional only \
       parameter expected `Optional[str]` but got `unknown`.";
      "Undefined attribute [16]: `Foo` has no attribute `undefined`.";
    ];
  assert_type_errors
    {|
      class attribute:
        ...
      class other:
        attribute: int = 1
      def foo(o: other) -> str:
        return o.attribute
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."];
  assert_type_errors
    {|
      import typing
      _T = typing.TypeVar("_T")
      def meta(x: typing.Type[_T]) -> None: ...
      meta(typing.Dict)
    |}
    []


let test_check_function_overloads context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from typing import overload, Union

      class Foo:
        @overload
        def derp(self, x: int) -> int:
          pass
        @overload
        def derp(self, x: str) -> str:
          pass
        def derp(self, x: Union[int, str]) -> Union[int, str]:
          if isinstance(x, int):
            return 0
          else:
            return ""

      def herp(x: Foo) -> int:
        return x.derp(5)
    |}
    [];

  (* Technically invalid; all @overload stubs must be followed by implementation *)
  assert_type_errors
    {|
      from typing import overload

      class Foo:
        @overload
        def derp(self, x: int) -> int:
          pass
        @overload
        def derp(self, x: str) -> str:
          pass

      def herp(x: Foo) -> int:
        return x.derp(5)
    |}
    [
      "Missing overload implementation [42]: Overloaded function `Foo.derp` must have an \
       implementation.";
    ];
  assert_type_errors
    {|
      from typing import overload

      class Foo:
        @overload
        def derp(self, x: int, y: int) -> int:
          pass
        @overload
        def derp(self, x: str, y: str) -> str:
          pass

      def herp(x: Foo) -> int:
        return x.derp(True)
    |}
    [
      "Missing overload implementation [42]: Overloaded function `Foo.derp` must have an \
       implementation.";
      "Missing argument [20]: Call `Foo.derp` expects argument `y`.";
    ];
  assert_type_errors
    {|
      import typing
      def herp(x: int) -> int:
        return typing.cast(x)
    |}
    ["Missing argument [20]: Call `typing.cast` expects argument `obj`."];

  (* Technically invalid; @overload stubs must comprehensively cover implementation *)
  assert_type_errors
    {|
      from typing import overload, Union

      class Foo:
        @overload
        def derp(self, x: int) -> int:
          pass
        def derp(self, x: Union[int, str]) -> Union[int, str]:
          if isinstance(x, int):
            return 0
          else:
            return ""

      def herp(x: Foo) -> int:
        return x.derp(5)
    |}
    [];
  assert_type_errors
    {|
      import typing
      @typing.overload
      def derp(x: int) -> int: ...
      @typing.overload
      def derp(x: str) -> str: ...

      reveal_type(derp)
    |}
    [
      "Missing overload implementation [42]: Overloaded function `derp` must have an implementation.";
      "Revealed type [-1]: Revealed type for `test.derp` is "
      ^ "`typing.Callable(derp)[..., unknown][[[Named(x, int)], int][[Named(x, str)], str]]`.";
    ];
  assert_type_errors
    {|
      from typing import overload

      @overload
      def derp(x: int) -> int: ...
      @overload
      def derp(x: str) -> str: ...

      reveal_type(derp)
    |}
    [
      "Missing overload implementation [42]: Overloaded function `derp` must have an implementation.";
      "Revealed type [-1]: Revealed type for `test.derp` is "
      ^ "`typing.Callable(derp)[..., unknown][[[Named(x, int)], int][[Named(x, str)], str]]`.";
    ];

  (* The overloaded stub will override the implementation *)
  assert_type_errors
    {|
      import typing

      @typing.overload
      def derp(x: int) -> int: ...
      @typing.overload
      def derp(x: str) -> str: ...

      reveal_type(derp)
    |}
    [
      "Missing overload implementation [42]: Overloaded function `derp` must have an implementation.";
      "Revealed type [-1]: Revealed type for `test.derp` is "
      ^ "`typing.Callable(derp)[..., unknown][[[Named(x, int)], int][[Named(x, str)], str]]`.";
    ];
  assert_type_errors
    {|
      import typing
      @typing.overload
      def derp(x: int) -> int: ...
      def derp(x: str) -> str: ...
      def derp(x: object): ...

      reveal_type(derp)
    |}
    [
      "Missing return annotation [3]: Return type is not specified.";
      "Revealed type [-1]: Revealed type for `test.derp` is "
      ^ "`typing.Callable(derp)[[Named(x, object)], typing.Any][[[Named(x, int)], int]]`.";
    ]


let test_check_constructor_overloads context =
  assert_type_errors
    ~context
    {|
      import typing

      class Class:
        @typing.overload
        def __init__(self, i: int) -> None: ...
        @typing.overload
        def __init__(self, s: str) -> None: ...
      def construct() -> None:
        Class(1)
        Class('asdf')
    |}
    [
      "Missing overload implementation [42]: Overloaded function `Class.__init__` must have an \
       implementation.";
    ]


let test_check_variable_arguments context =
  let assert_type_errors = assert_type_errors ~context in
  let assert_strict_type_errors = assert_strict_type_errors ~context in
  assert_type_errors
    {|
      import typing
      class C(typing.Iterable[int]):
        ...
      def f(a: int, b: int) -> None:
       pass
      def g(c: C) -> None:
        return f( *c)
    |}
    [];
  assert_type_errors
    {|
      import typing
      def f(a: int, b: int) -> None:
       pass
      def g(collection: typing.Collection[str]) -> None:
        return f( *collection)
    |}
    [
      "Incompatible parameter type [6]: In call `f`, for 1st positional only parameter expected \
       `int` but got `str`.";
    ];
  assert_type_errors
    {|
      def foo(a: int, b: int) -> int:
        return 1
      def bar(b) -> str:
        return foo ( *b )
    |}
    [
      "Missing parameter annotation [2]: Parameter `b` has no type specified.";
      "Incompatible return type [7]: Expected `str` but got `int`.";
    ];
  assert_type_errors
    {|
      import typing
      def foo(a: int, b: int) -> int:
        return 1
      def bar(b: typing.Any) -> int:
        return foo ( *b )
    |}
    ["Missing parameter annotation [2]: Parameter `b` must have a type other than `Any`."];
  assert_type_errors
    {|
      import typing
      def foo(a: int, b: int) -> int:
        return 1
      def bar(b: typing.List[typing.Any]) -> int:
        return foo ( *b )
    |}
    [
      "Missing parameter annotation [2]: Parameter `b` must have a type that "
      ^ "does not contain `Any`.";
    ];
  assert_strict_type_errors
    {|
      import typing
      def foo(a: int, b: int) -> int:
        return 1
      def bar(b: typing.Any) -> int:
        return foo ( *b )
    |}
    ["Missing parameter annotation [2]: Parameter `b` must have a type other than `Any`."];
  assert_type_errors
    {|
      import typing
      def foo(a: int, b: int) -> int:
        return 1
      def bar(b: typing.List[str]) -> int:
        return foo ( *b )
    |}
    [
      "Incompatible parameter type [6]: In call `foo`, for 1st positional only parameter expected \
       `int` but got `str`.";
    ];
  assert_type_errors
    {|
      import typing
      def foo(a: int, b: int) -> int:
        return 1
      def bar(b: typing.List[str]) -> None:
        foo('asdf', *b)
    |}
    [
      "Incompatible parameter type [6]: In call `foo`, for 1st positional only parameter expected \
       `int` but got `str`.";
      "Incompatible parameter type [6]: In call `foo`, for 2nd positional only parameter expected \
       `int` but got `str`.";
    ];
  assert_type_errors
    {|
      import typing
      def foo(a: int, b: int) -> int:
        return 1
      def bar(b: typing.List[str]) -> None:
        foo ( *b, 'asdf' )
    |}
    ["Too many arguments [19]: Call `foo` expects 2 positional arguments, 3 were provided."];
  assert_type_errors
    {|
      import typing
      def foo(a: int, b: str) -> int:
        return 1
      def bar(b: typing.List[str]) -> None:
        foo ( *b, 1, 'asdf' )
    |}
    ["Too many arguments [19]: Call `foo` expects 2 positional arguments, 4 were provided."];
  assert_type_errors
    {|
      import typing
      def foo(a: int, b: str) -> int:
        return 1
      def bar(b: typing.List[int]) -> None:
        foo ( *b, 'asdf' )
    |}
    ["Too many arguments [19]: Call `foo` expects 2 positional arguments, 3 were provided."];
  assert_type_errors
    {|
      import typing
      def durp(a: int, b: str) -> int:
        return 1
      def bar(b: typing.List[int]) -> None:
        durp( *b, 1.0 )
    |}
    ["Too many arguments [19]: Call `durp` expects 2 positional arguments, 3 were provided."];
  assert_type_errors
    {|
      import typing
      def foo(a: int, b: int) -> int:
        return 1
      def bar(b: typing.List[str]) -> int:
        return foo('asdf', *b)
    |}
    [
      "Incompatible parameter type [6]: In call `foo`, for 1st positional only parameter expected \
       `int` but got `str`.";
      "Incompatible parameter type [6]: In call `foo`, for 2nd positional only parameter expected \
       `int` but got `str`.";
    ];
  assert_type_errors
    {|
     import typing
     def foo(a: int, b: int) -> int:
       return 1
     def bar(b: typing.Tuple[int, int]) -> int:
       return foo( *b )
   |}
    [];
  assert_type_errors
    {|
     import typing
     def foo(a: typing.Tuple[int, str]) -> typing.Set[int]:
       return set(a)
   |}
    ["Incompatible return type [7]: Expected `Set[int]` but got `Set[Union[int, str]]`."];
  (* These two ways of annotating `*args` are equivalent. *)
  assert_type_errors
    {|
      from typing import Tuple
      from pyre_extensions import Unpack

      def simple_starred_args( *args: str) -> None: ...
      def unbounded_tuple_starred_args( *args: Unpack[Tuple[str, ...]]) -> None: ...

      def main() -> None:
        simple_starred_args(1, "hello")
        unbounded_tuple_starred_args(1, "hello")
   |}
    [
      "Incompatible parameter type [6]: In call `simple_starred_args`, for 1st positional only \
       parameter expected `str` but got `int`.";
      "Incompatible parameter type [6]: In call `unbounded_tuple_starred_args`, for 1st positional \
       only parameter expected `str` but got `int`.";
    ];
  ()


let test_check_variable_restrictions context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
       from builtins import variable_restricted_identity
       def f(x: str) -> int:
         return variable_restricted_identity(x)
    |}
    ["Incompatible return type [7]: Expected `int` but got `str`."];
  assert_type_errors
    {|
       from builtins import variable_restricted_identity
       def f(x: str) -> str:
         return variable_restricted_identity(x)
    |}
    [];
  assert_type_errors
    {|
       from builtins import variable_restricted_identity
       def f(x: float) -> str:
         return variable_restricted_identity(x)
    |}
    [
      "Incompatible parameter type [6]: In call `variable_restricted_identity`, for 1st positional \
       only parameter expected `Variable[_VR <: [str, int]]` but got `float`.";
    ];
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar('T', int, str)
      def foo(t: T) -> None: ...
      def bar(t: T) -> None:
        foo(t)
    |}
    [];
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar('T', 'C', 'X')
      class C():
        def baz(self) -> int:
          return 7
      class X():
        def baz(self) -> str:
          return "A"
      def foo(t: T) -> int:
        return t.baz()
    |}
    ["Incompatible return type [7]: Expected `int` but got `Union[int, str]`."]


let test_check_keyword_arguments context =
  let assert_type_errors = assert_type_errors ~context in
  let assert_default_type_errors = assert_default_type_errors ~context in
  assert_type_errors
    {|
      import typing

      def foo(x: int, y: str) -> None:
        pass

      def bar(x: typing.Dict[str, str]) -> None:
        test = foo( **x )
    |}
    [
      "Incompatible parameter type [6]: In call `foo`, for 1st positional only parameter expected \
       `int` but got `str`.";
    ];
  assert_type_errors
    {|
      import typing

      def foo(x: int, y: str) -> None:
        pass

      def bar(x: typing.Dict[int, str]) -> None:
        test = foo( **x )
    |}
    [
      "Invalid argument [32]: Keyword argument `x` has type `typing.Dict[int, str]` "
      ^ "but must be a mapping with string keys.";
    ];
  assert_type_errors
    {|
      def foo(x: int, y: str) -> None:
        pass

      def bar(x: int) -> None:
        test = foo( **x )
    |}
    [
      "Invalid argument [32]: Keyword argument `x` has type `int` "
      ^ "but must be a mapping with string keys.";
    ];
  assert_default_type_errors
    {|
      import typing

      def foo(x: int, y: str) -> None:
        pass

      def bar(x: typing.Dict[typing.Any, typing.Any]) -> None:
        test = foo( **x )
    |}
    [];
  assert_type_errors
    {|
      import typing

      def foo(x: int, y: int) -> None:
        pass

      def bar(x: typing.Dict[str, typing.Union[int, str]]) -> None:
        test = foo( **x )
    |}
    [
      "Incompatible parameter type [6]: In call `foo`, for 1st positional only parameter expected \
       `int` but got `Union[int, str]`.";
    ];
  assert_type_errors
    {|
      import typing

      def foo(x: float, y: float) -> None:
        pass

      def bar(x: typing.Union[typing.Dict[str, int], typing.Dict[str, float]]) -> None:
        test = foo( **x )
    |}
    [];
  assert_type_errors
    {|
      import typing

      def foo(x: float, y: int) -> None:
        pass

      def bar(x: typing.Union[typing.Dict[str, int], typing.Dict[str, float]]) -> None:
        test = foo( **x )
    |}
    [
      "Incompatible parameter type [6]: In call `foo`, for 1st positional only parameter expected \
       `int` but got `float`.";
    ];
  ()


let test_check_named_arguments context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from builtins import str_float_to_int
      def bar()->int:
        return str_float_to_int(i="",f=2.0) + str_float_to_int(f=1.0,i="bar")
    |}
    [];
  assert_type_errors
    {|
      class Bar:
        @classmethod
        def bar(cls, a: str, b: int) -> None: ...

      Bar.bar("asdf", 10)
    |}
    [];
  assert_type_errors
    {|
      class Bar:
        @classmethod
        def bar(cls, a: str, b: int = 10) -> None: ...

      Bar.bar("asdf", 10)
    |}
    [];
  assert_type_errors
    {|
      from builtins import str_float_to_int
      def bar() -> int:
        return str_float_to_int(i="")
    |}
    ["Missing argument [20]: Call `str_float_to_int` expects argument `f`."];
  assert_type_errors
    {|
      from builtins import str_float_to_int
      def bar() -> int:
        return 1 + str_float_to_int(i=2.0,f=1)
      def foo() -> int:
        return str_float_to_int(f="No",i="Hi")
    |}
    [
      "Incompatible parameter type [6]: In call `str_float_to_int`, for 1st parameter `i` expected \
       `str` but got `float`.";
      "Incompatible parameter type [6]: In call `str_float_to_int`, for 1st parameter `f` expected \
       `float` but got `str`.";
    ]


let test_check_literals context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      import typing
      from typing import overload
      from typing_extensions import Literal
      import typing_extensions
      @typing.overload
      def foo(x: typing_extensions.Literal["give_me_int", "also_give_me_int"]) -> int: ...
      @typing.overload
      def foo(x: Literal["give_me_str"]) -> str: ...
      def foo(x: str) -> typing.Union[int, str, bool]:
        if x == "give_me_int":
          return 7
        if x == "give_me_str":
          return "seven"
        return False
      def bar() -> None:
        a = foo("give_me_str")
        reveal_type(a)
        b = foo("give_me_int")
        reveal_type(b)
        c = foo("something_else")
        reveal_type(c)
        d = foo("also_give_me_int")
        reveal_type(d)
    |}
    [
      "Revealed type [-1]: Revealed type for `a` is `str`.";
      "Revealed type [-1]: Revealed type for `b` is `int`.";
      "Incompatible parameter type [6]: In call `foo`, for 1st positional only parameter expected \
       `Union[typing_extensions.Literal['also_give_me_int'], \
       typing_extensions.Literal['give_me_int']]` but got \
       `typing_extensions.Literal['something_else']`.";
      "Revealed type [-1]: Revealed type for `c` is `int`.";
      "Revealed type [-1]: Revealed type for `d` is `int`.";
    ];
  ()


let () =
  "signatureSelection"
  >::: [
         "check_callables" >:: test_check_callables;
         "check_function_redirects" >:: test_check_function_redirects;
         "check_function_parameters_with_backups" >:: test_check_function_parameters_with_backups;
         "check_function_parameters" >:: test_check_function_parameters;
         "check_function_parameter_errors" >:: test_check_function_parameter_errors;
         "check_function_overloads" >:: test_check_function_overloads;
         "check_constructor_overloads" >:: test_check_constructor_overloads;
         "check_variable_arguments" >:: test_check_variable_arguments;
         "check_variable_restrictions" >:: test_check_variable_restrictions;
         "check_keyword_arguments" >:: test_check_keyword_arguments;
         "check_named_arguments" >:: test_check_named_arguments;
         "check_literals" >:: test_check_literals;
       ]
  |> Test.run
