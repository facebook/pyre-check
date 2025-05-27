(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest

let assert_strict_any_errors
    ?other_sources
    (src : string)
    (expected : string list)
    (context : test_ctxt)
    : unit
  =
  assert_strict_type_errors ?other_sources ~enable_strict_any_check:true src expected context;
  assert_strict_type_errors ?other_sources ~enable_strict_any_check:false src [] context;
  ()


let test_any =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
              import typing
              class Foo:
                def __getitem__(self) -> typing.Any: ...
            |}
           [
             "Missing return annotation [3]: Return type must be specified as type other than `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
              import typing
              Derp = typing.Any
              Herp = typing.List[typing.Any]
            |}
           [
             "Prohibited any [33]: `Derp` cannot alias to `Any`.";
             "Prohibited any [33]: `Herp` cannot alias to a type containing `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
              import typing
              MyType: typing.Any
            |}
           [
             "Missing global annotation [5]: Globally accessible variable `MyType` must be \
              specified as type other than `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
              import typing
              MyType = typing.Any
              x: MyType = 1
            |}
           ["Prohibited any [33]: `MyType` cannot alias to `Any`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
              import typing
              y: typing.Any = 2
              z: typing.List[typing.Any] = [3]
              a: typing.Any
            |}
           [
             "Missing global annotation [5]: Globally accessible variable `y` has type `int` but \
              type `Any` is specified.";
             "Missing global annotation [5]: Globally accessible variable `z` must be specified as \
              type that does not contain `Any`.";
             "Missing global annotation [5]: Globally accessible variable `a` must be specified as \
              type other than `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
              import typing
              def foo() -> None:
                x: typing.Any = 1
            |}
           [
             "Prohibited any [33]: Expression `x` has type `int`; given explicit type cannot be \
              `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
              import typing
              def foo() -> None:
                x: typing.List[typing.Any] = []
            |}
           ["Prohibited any [33]: Explicit annotation for `x` cannot contain `Any`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
              import typing
              def foo() -> None:
                x = 1
                typing.cast(typing.Any, x)
            |}
           ["Prohibited any [33]: Explicit annotation for `typing.cast` cannot be `Any`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
              import typing
              def foo() -> None:
                x = 1
                typing.cast(typing.List[typing.Any], x)
            |}
           ["Prohibited any [33]: Explicit annotation for `typing.cast` cannot contain `Any`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
              import typing
              class Foo:
                bar: typing.Dict[str, typing.Any] = {}
                baz: typing.Dict[typing.Any, typing.Any] = {}
            |}
           [
             "Missing attribute annotation [4]: Attribute `baz` of class `Foo` must have a type \
              that does not contain `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
              import typing
              __property__: typing.Any = ...
              x: typing.Optional[int]
              class Foo:
                @__property__
                def x(self) -> int: ...
                @x.setter
                def x(self, value: typing.Optional[int]) -> None: ...
                @__property__
                def y(self) -> int: ...
              def bar() -> int:
                foo = Foo()
                return foo.x
              def baz() -> int:
                foo = Foo()
                return foo.y
            |}
           [
             "Missing global annotation [5]: Globally accessible variable `__property__` "
             ^ "must be specified as type other than `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
              import typing
              class Foo:
                bar: typing.Any
                def __init__(self) -> None:
                  self.bar = 'foo'
            |}
           [
             "Missing attribute annotation [4]: Attribute `bar` of class `Foo` must have a type \
              other than `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
              import typing
              def foo(any: typing.Any) -> int:
                return any.attribute
            |}
           ["Missing parameter annotation [2]: Parameter `any` must have a type other than `Any`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
              import typing
              MyType = typing.Any
              class Foo:
                def __init__(self, a: MyType) -> None:
                  self.a = a
                  self.b: MyType = 1
            |}
           ["Prohibited any [33]: `MyType` cannot alias to `Any`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
                import typing
                class Foo:
                  def __init__(self, a: typing.Any) -> None:
                    self.a = a
                    self.b : typing.Any = a
            |}
           [
             "Missing parameter annotation [2]: Parameter `a` must have a type other than `Any`.";
             "Missing attribute annotation [4]: Attribute `b` of class `Foo` must have a type \
              other than `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
              import typing
              class Foo:
                x: typing.Any = 1
                y: typing.List[typing.Any] = [1]
                def __init__(self, test: bool, y: str) -> None:
                  self.a: typing.Any = 1
                  self.b: typing.List[typing.Any] = [1]
            |}
           [
             "Missing attribute annotation [4]: Attribute `x` of class `Foo` has type `int` "
             ^ "but type `Any` is specified.";
             "Missing attribute annotation [4]: Attribute `y` of class `Foo` must have a type "
             ^ "that does not contain `Any`.";
             "Missing attribute annotation [4]: Attribute `a` of class `Foo` has type `int` "
             ^ "but type `Any` is specified.";
             "Missing attribute annotation [4]: Attribute `b` of class `Foo` must have a type "
             ^ "that does not contain `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
              import typing
              def baz(x: typing.Union[typing.Callable[[int], typing.Any], typing.Callable[..., typing.Any]]) -> None:
                  x(1)
            |}
           [
             "Missing parameter annotation [2]: Parameter `x` must have a type that does not \
              contain `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           ~other_sources:
             [
               {
                 handle = "reexports_callable.pyi";
                 source =
                   {|
                from typing import Callable as Callable

              |};
               };
             ]
           {|
              from typing import Any, TypeVar
              from reexports_callable import Callable

              _FuncT = TypeVar("_FuncT", bound=Callable[..., Any])

              def identity_for_function(funcobj: _FuncT) -> _FuncT: ...

              class Foo:
                @identity_for_function
                def my_method(self) -> int: ...

              def main() -> None:
                f1: Callable[[int], str]
            |}
           ["Prohibited any [33]: Type variable `_FuncT` cannot have a bound containing `Any`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
      import collections
      class Derp:
          Word = collections.namedtuple("word", ("verb", "noun"))
    |}
           [
             "Missing attribute annotation [4]: Attribute `noun` of class `Derp.Word` must have a \
              type other than `Any`.";
             "Missing attribute annotation [4]: Attribute `verb` of class `Derp.Word` must have a \
              type other than `Any`.";
             "Missing parameter annotation [2]: Parameter `noun` must have a type other than `Any`.";
             "Missing parameter annotation [2]: Parameter `verb` must have a type other than `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
              from typing import Any
              def my_decorator(x: int) -> Any: ...
              @my_decorator(1)
              def f(x: int) -> int:
                return x
            |}
           [
             "Missing return annotation [3]: Return type must be specified as type other than `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
              import typing
              meta_type = typing.Callable[[typing.Callable[[int], str]], typing.Callable[[str], str]]
              def meta_decorate(f: typing.Any) -> meta_type:
                ...
              @meta_decorate
              def decorate(f: typing.Callable[[int], str]) -> typing.Callable[[str], int]:
                ...

              @decorate
              def f(x: int) -> str:
                return str(x)
            |}
           ["Missing parameter annotation [2]: Parameter `f` must have a type other than `Any`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
              import typing
              T = typing.TypeVar("T")
              def synchronize(
                coroutine: typing.Callable[..., typing.Coroutine[typing.Any, typing.Any, T]]
              ) -> typing.Callable[..., T]: ...

              @synchronize
              async def am_i_async(x: int) -> str:
                return str(x)

            |}
           [
             "Missing parameter annotation [2]: Parameter `coroutine` must have a type that does \
              not contain `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
              import typing
              T = typing.TypeVar("T")
              class synchronize:
                @typing.overload
                def __call__(
                   self,
                   coroutine: typing.Callable[..., typing.Coroutine[typing.Any, typing.Any, T]]
                ) -> typing.Callable[..., T]: ...
                @typing.overload
                def __call__(self, coroutine: int) -> int: ...
                def __call__(self, coroutine: typing.Any) -> typing.Any: ...

              s: synchronize = synchronize()
              @s
              async def am_i_async(x: int) -> str:
                return str(x)
            |}
           [
             "Missing parameter annotation [2]: Parameter `coroutine` must have a type other than \
              `Any`.";
             "Missing return annotation [3]: Return type must be specified as type other than \
              `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
              import typing
              T = typing.TypeVar("T")
              class synchronize:
                @typing.overload
                def __call__(
                   self,
                   coroutine: typing.Callable[..., typing.Coroutine[typing.Any, typing.Any, T]]
                ) -> typing.Callable[..., T]: ...
                @typing.overload
                def __call__(self, coroutine: int) -> int: ...
                def __call__(self, coroutine: typing.Any) -> typing.Any: ...

              @synchronize()
              async def am_i_async(x: int) -> str:
                return str(x)
            |}
           [
             "Missing parameter annotation [2]: Parameter `coroutine` must have a type other than \
              `Any`.";
             "Missing return annotation [3]: Return type must be specified as type other than \
              `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
              import typing
              T = typing.TypeVar("T")
              class m:
                def __call__(
                   self,
                   coroutine: typing.Callable[..., typing.Coroutine[typing.Any, typing.Any, T]]
                ) -> typing.Callable[..., T]: ...

              class synchronize(metaclass=m):
                pass

              @synchronize
              async def am_i_async(x: int) -> str:
                return str(x)
            |}
           [
             "Missing parameter annotation [2]: Parameter `coroutine` must have a type that does \
              not contain `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
              from typing import Any

              maybe_a_factory: Any

              @maybe_a_factory(1, 2)
              def foo(x: int) -> None:
                pass
            |}
           [
             "Missing global annotation [5]: Globally accessible variable `maybe_a_factory` must \
              be specified as type other than `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
              from typing import Any

              def my_decorator() -> Any: ...

              @my_decorator()
              def f(x: int) -> int: ...
            |}
           [
             "Missing return annotation [3]: Return type must be specified as type other than `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
              from typing import Any, Callable

              LooselyTypedCallable = Callable[..., int]

              def good_decorator() -> Callable[
                [Callable[[int], int]],
                Callable[[int], int]
              ]: ...

              def bad_decorator() -> Any: ...

              @bad_decorator()
              @good_decorator()
              def f(x: int) -> int: ...

              @good_decorator()
              @bad_decorator()
              def g(x: int) -> int: ...
            |}
           [
             "Missing return annotation [3]: Return type must be specified as type other than `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
      from typing import overload, Union, TypeVar, List, StaticMethod, Callable, Type, Any

      def maker() -> Any: ...

      class Host:
        sm: StaticMethod[Callable[[int, str], bool]] = maker()

      def f() -> None:
        x = Host().sm
        y = Host.sm
    |}
           [
             "Missing return annotation [3]: Return type must be specified as type other than `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
      import typing
      def foo(l: typing.Iterable[typing.Any])->typing.Generator[typing.Any, None, None]:
        return (x for x in l)
    |}
           [
             "Missing return annotation [3]: Return type must be specified as type "
             ^ "that does not contain `Any`.";
             "Missing parameter annotation [2]: Parameter `l` must have a type "
             ^ "that does not contain `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
              import typing
              def foo(xs: typing.Iterable[typing.Any]) -> None:
                a, b = xs
                z = a * b
            |}
           [
             "Missing parameter annotation [2]: Parameter `xs` must have a type that does not \
              contain `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
              import typing
              def foo(xs: typing.Iterable[typing.Any]) -> None:
                a, b = xs
            |}
           [
             "Missing parameter annotation [2]: Parameter `xs` must have a type that does not \
              contain `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
              import typing
              class Foo:
                def __getattr__(self, name: str) -> typing.Any: ...
              def foo(x: Foo) -> None:
                x.attribute + 1
            |}
           [
             "Missing return annotation [3]: Return type must be specified as type other than `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
              from typing import Any
              def foo(x: BoundMethod[Any, int]) -> None:
                y = x()
            |}
           [
             "Missing parameter annotation [2]: Parameter `x` must have a type that does not \
              contain `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
      import typing
      def foo() -> typing.Dict[typing.Any, typing.Any]:
        return {}
    |}
           [
             "Missing return annotation [3]: Return type must be specified as type that does "
             ^ "not contain `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
      import typing
      x: typing.List[typing.Any]
    |}
           [
             "Missing global annotation [5]: Globally accessible variable `x` must be specified "
             ^ "as type that does not contain `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
      import typing
      def takes_callable(f: typing.Callable[[typing.Any], int]) -> int:
        return 0
    |}
           [
             "Missing parameter annotation [2]: Parameter `f` must have a type "
             ^ "that does not contain `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
      import typing
      def foo(a: int, b: int) -> int:
        return 1
      def bar(b: typing.Any) -> int:
        return foo ( *b )
    |}
           ["Missing parameter annotation [2]: Parameter `b` must have a type other than `Any`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
      import typing
      def foo(a: int, b: int) -> int:
        return 1
      def bar(b: typing.Any) -> int:
        return foo ( *b )
    |}
           ["Missing parameter annotation [2]: Parameter `b` must have a type other than `Any`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           ~other_sources:
             [
               {
                 handle = "mypy_extensions.pyi";
                 source =
                   {|
                    import typing
                    def TypedDict(
                        typename: str,
                        fields: typing.Dict[str, typing.Type[_T]],
                        total: bool = ...,
                    ) -> typing.Type[dict]: ...
                  |};
               };
             ]
           {|
              import typing
              import mypy_extensions
              Movie = mypy_extensions.TypedDict('Movie', {'name': typing.Any, 'year': 'int'})
              class Bar(mypy_extensions.TypedDict):
                x: typing.Any
            |}
           [
             "Prohibited any [33]: Explicit annotation for `name` cannot be `Any`.";
             "Prohibited any [33]: Explicit annotation for `x` cannot be `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
    from typing import Any
    def foo(x: Any) -> None:
      if callable(x):
        y = x
  |}
           ["Missing parameter annotation [2]: Parameter `x` must have a type other than `Any`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
    from typing import Any, Union
    def foo(x: Union[int, Any]) -> None:
      if callable(x):
        y = x
  |}
           ["Missing parameter annotation [2]: Parameter `x` must have a type other than `Any`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
  from typing import is_typeddict, TypedDict, Any

  def foo(x: Any) -> None:
    pass
  |}
           ["Missing parameter annotation [2]: Parameter `x` must have a type other than `Any`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
  from typing import is_typeddict, TypedDict, Type, Any

  def foo(x: Type[Any]) -> None:
    pass
  |}
           [
             "Missing parameter annotation [2]: Parameter `x` must have a type that does not \
              contain `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
    from typing import Any, Optional

    class Foo1:
      bar: Optional[str] = None

    class Foo2:
      bar: Optional[str] = None

    def main(x: Any) -> None:
        y = x.bar.capitalize() if x.bar else ""
  |}
           ["Missing parameter annotation [2]: Parameter `x` must have a type other than `Any`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
              from typing import Any, List, TypeVar
              T = TypeVar("T", bound=List[Any])
              |}
           ["Prohibited any [33]: Type variable `T` cannot have a bound containing `Any`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
      import typing
      def foo(d: typing.Dict[int, typing.Any]) -> None:
        d.update({ 1: 1 })
    |}
           [
             "Missing parameter annotation [2]: Parameter `d` must have a type "
             ^ "that does not contain `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
      import collections
      T = collections.namedtuple('T', 'a b c')
      def b(d: T) -> None:
        a = d.a + d.b
    |}
           [
             "Missing attribute annotation [4]: Attribute `a` of class `T` must have a type other \
              than `Any`.";
             "Missing attribute annotation [4]: Attribute `b` of class `T` must have a type other \
              than `Any`.";
             "Missing attribute annotation [4]: Attribute `c` of class `T` must have a type other \
              than `Any`.";
             "Missing parameter annotation [2]: Parameter `a` must have a type other than `Any`.";
             "Missing parameter annotation [2]: Parameter `b` must have a type other than `Any`.";
             "Missing parameter annotation [2]: Parameter `c` must have a type other than `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
      import collections
      T = collections.namedtuple('T', 'a b c')
      def foo(t: T) -> None:
        x, y, z = t
    |}
           [
             "Missing attribute annotation [4]: Attribute `a` of class `T` must have a type other \
              than `Any`.";
             "Missing attribute annotation [4]: Attribute `b` of class `T` must have a type other \
              than `Any`.";
             "Missing attribute annotation [4]: Attribute `c` of class `T` must have a type other \
              than `Any`.";
             "Missing parameter annotation [2]: Parameter `a` must have a type other than `Any`.";
             "Missing parameter annotation [2]: Parameter `b` must have a type other than `Any`.";
             "Missing parameter annotation [2]: Parameter `c` must have a type other than `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
      import collections
      T = collections.namedtuple('T', 'a')
      T(a=1)
      def foo() -> None:
        T(a=2)
    |}
           [
             "Missing attribute annotation [4]: Attribute `a` of class `T` must have a type other \
              than `Any`.";
             "Missing parameter annotation [2]: Parameter `a` must have a type other than `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
      import typing
      class FooNotNamedTuple:
        bar: typing.Optional[str] = None
        baz: typing.Dict[int, typing.Any] = {}
        hello: typing.Dict[str, typing.Any] = {}
    |}
           [
             "Missing attribute annotation [4]: Attribute `baz` of class `FooNotNamedTuple` must \
              have a type that does not contain `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
      import typing
      class Foo(typing.NamedTuple):
        bar: typing.Optional[str] = None
        baz: typing.Dict[int, typing.Any] = {}
        hello: typing.Dict[str, typing.Any] = {}
    |}
           [
             "Missing parameter annotation [2]: Parameter `baz` must have a type that does not \
              contain `Any`.";
             "Missing attribute annotation [4]: Attribute `baz` of class `Foo` must have a type \
              that does not contain `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
      import typing
      class Foo:
        def __new__(cls, foo: typing.Dict[int, typing.Any] = {}) -> Foo:
            return super(Foo, cls).__new__(cls)
    |}
           [
             "Missing parameter annotation [2]: Parameter `foo` must have a type that does not \
              contain `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
      import typing
      def __new__(foo: typing.Dict[int, typing.Any] = {}) -> None:
        pass
    |}
           [
             "Missing parameter annotation [2]: Parameter `foo` must have a type that does not \
              contain `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
      import typing
      def foo(x: typing.Any) -> int:
        return 1
    |}
           ["Missing parameter annotation [2]: Parameter `x` must have a type other than `Any`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
      import typing
      def foo(x: typing.Dict[str, typing.Any], y: typing.Dict[int, typing.Any]) -> int:
        return 1
    |}
           [
             "Missing parameter annotation [2]: Parameter `y` must have a type "
             ^ "that does not contain `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
      import typing
      MyType = typing.Any
      def foo(x: MyType) -> int:
        return 1
    |}
           ["Prohibited any [33]: `MyType` cannot alias to `Any`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
      import typing
      d: typing.Any
      def foo(x: typing.Any = d) -> int:
        return 1
    |}
           [
             "Missing global annotation [5]: Globally accessible variable `d` must be specified as \
              type other than `Any`.";
             "Missing parameter annotation [2]: Parameter `x` must have a type other than `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
      import typing
      def foo(x: typing.Dict[typing.Any, str]) -> int:
        return 1
    |}
           [
             "Missing parameter annotation [2]: Parameter `x` must have a type "
             ^ "that does not contain `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
      import typing
      def foo(x: typing.Any, *args: typing.Any, **kwargs: typing.Any) -> int:
        return 1
    |}
           ["Missing parameter annotation [2]: Parameter `x` must have a type other than `Any`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
      import typing
      def foo() -> typing.Any:
        return 1
    |}
           ["Missing return annotation [3]: Returning `int` but type `Any` is specified."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
      import typing
      def foo() -> typing.Dict[str, typing.Any]:
        return {}

      def bar() -> typing.Dict[typing.Any, typing.Any]:
        return {}
    |}
           [
             "Missing return annotation [3]: Return type must be specified as type "
             ^ "that does not contain `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
      import typing
      MyType = typing.Any
      def foo() -> MyType:
        return 1
    |}
           ["Prohibited any [33]: `MyType` cannot alias to `Any`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           ~other_sources:[{ handle = "export.py"; source = "MyType = typing.List[typing.Any]" }]
           {|
      import typing
      from export import MyType
      MyTypeLocal = typing.List[typing.Any]
      def foo() -> MyType:
        return []
      def bar() -> MyTypeLocal:
        return []
    |}
           ["Prohibited any [33]: `MyTypeLocal` cannot alias to a type containing `Any`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
       import typing
       def foo(x: typing.Any) -> typing.Any:
         return x
     |}
           [
             "Missing return annotation [3]: Return type must be specified as type other than `Any`.";
             "Missing parameter annotation [2]: Parameter `x` must have a type other than `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
      import typing
      def foo() -> typing.Tuple[int, typing.Any]:
        return (1, 2)
    |}
           [
             "Missing return annotation [3]: Return type must be specified as type "
             ^ "that does not contain `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
      from builtins import condition
      import typing
      def foo() -> typing.Any:
        if condition():
          return 1
    |}
           [
             "Missing return annotation [3]: Returning `typing.Optional[int]` but "
             ^ "type `Any` is specified.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_any_errors
           {|
      from builtins import int_to_bool
      import typing
      def foo(optional: typing.Optional[int]) -> typing.Any:
        return optional and int_to_bool(optional)
    |}
           [
             "Missing return annotation [3]: Returning `typing.Optional[int]` but type `Any` is \
              specified.";
           ];
    ]


let () = "any" >::: [test_any] |> Test.run
