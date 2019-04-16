(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open OUnit2
open IntegrationTest

let test_check_protocol _ =
  assert_type_errors
    {|
      class P(typing.Protocol):
        def foo(self) -> int: ...

      class A:
        def foo(self) -> int:
          return 9

      def foo(p: P) -> int:
        return p.foo()

      def bar(a: A) -> None:
        foo(a)

    |}
    [];
  assert_type_errors
    {|
      class B: pass
      class C(B): pass

      class P(typing.Protocol):
        def foo(self) -> B: ...

      class A:
        def foo(self) -> C:
          return C()

      def foo(p: P) -> B:
        return p.foo()

      def bar(a: A) -> None:
        foo(a)

    |}
    [];
  assert_type_errors
    {|
      class P(typing.Protocol):
        def foo(self) -> int: ...

      class Base:
        def foo(self) -> int:
          return 9
      class Child(Base):
        def baz(self) -> int:
          return 8

      def foo(p: P) -> int:
        return p.foo()

      def bar(a: Child) -> None:
        foo(a)

    |}
    [];
  assert_type_errors
    {|
      class FooProtocol(typing.Protocol):
        def foo(self) -> int: ...

      class FooBarProtocol(typing.Protocol, FooProtocol):
        def bar(self) -> int: ...


      class FooClass():
        def foo(self) -> int:
          return 7
      class FooBarClass():
        def foo(self) -> int:
          return 7
        def bar(self) -> int:
          return 7
      def takesFooBarProtocol(x: FooBarProtocol) -> None: pass

      def f() -> None:
        takesFooBarProtocol(FooClass())
        takesFooBarProtocol(FooBarClass())
    |}
    [
      "Incompatible parameter type [6]: Expected `FooBarProtocol` for 1st anonymous parameter " ^
      "to call `takesFooBarProtocol` but got `FooClass`."
    ];
  (* TODO:(T40727281) This doesn't work because we only identify even possible implementations
     based on what a class directly contains, for perf reasons *)
  assert_type_errors
    {|
      class ParentFoo():
        def foo(self) -> int:
          return 7
      class ParentBar():
        def bar(self) -> int:
          return 9
      class Child(ParentFoo, ParentBar):
        def other(self) -> int:
          return 11
      class FooBarProtocol(typing.Protocol):
        def foo(self) -> int: ...
        def bar(self) -> int: ...
      def takesFooBar(x: FooBarProtocol) -> int:
        return x.bar() + x.foo()
      def fun() -> int:
        return takesFooBar(Child())
    |}
    [
      "Incompatible parameter type [6]: Expected `FooBarProtocol` for 1st anonymous parameter " ^
      "to call `takesFooBar` but got `Child`."
    ];
  (* TODO(T40726328): Allow protocols to implement other protocols *)
  assert_type_errors
    {|
      class P(typing.Protocol):
        def foo(self) -> int: ...

      class ProtocolBase(typing.Protocol):
        def foo(self) -> int:
          return 9
      class Child(ProtocolBase):
        def baz(self) -> int:
          return 8

      def foo(p: P) -> int:
        return p.foo()

      def bar(a: Child) -> None:
        foo(a)

    |}
    [
      "Incompatible parameter type [6]: Expected `P` for 1st anonymous parameter " ^
      "to call `foo` but got `Child`."
    ];
  (* TODO(T40726328): Allow protocols to implement other protocols *)
  assert_type_errors
    {|
      class FooProtocol(typing.Protocol):
        def foo(self) -> int: ...
      class FooBarProtocol(typing.Protocol):
        def foo(self) -> int: ...
        def bar(self) -> int: ...
      def takesFoo(x: FooProtocol) -> int:
        return x.foo()
      def takesFooBar(x: FooBarProtocol) -> int:
        y = takesFoo(x)
        return x.bar() + y
    |}
    [
      "Incompatible parameter type [6]: Expected `FooProtocol` for 1st anonymous parameter " ^
      "to call `takesFoo` but got `FooBarProtocol`."
    ];
  (* Collection -> Sized is special cased for now *)
  assert_type_errors
    {|
      def foo(
        a: typing.Sequence[int],
        b: typing.Mapping[int, int],
        c: typing.AbstractSet[int],
        d: typing.Collection[int]
        ) -> int:
        return len(a) + len(b) + len(c) + len(d)
    |}
    [];
  assert_type_errors
    {|
      T = typing.TypeVar("T")
      def foo(d: typing.Collection[T]) -> T: ...
      def bar(x: typing.List[int]) -> int:
        reveal_type(foo(x))
        return foo(x)
    |}
    ["Revealed type [-1]: Revealed type for `foo.(...)` is `int`."];
  assert_type_errors
    {|
      class A:
        def __hash__(self) -> int:
          return 9

      def foo(p: typing.Hashable) -> int:
        return p.__hash__()

      def bar(a: A) -> None:
        foo(a)

    |}
    [];
  (* TODO(T41436690): end to end support for attribute protocols *)
  assert_type_errors
    {|
      class P(typing.Protocol):
        foo: int

      class A:
        foo: int = 7

      def foo(p: P) -> int:
        return p.foo

      def bar(a: A) -> None:
        foo(a)

    |}
    [
      "Uninitialized attribute [13]: Attribute `foo` is declared in class `P` to have type `int` " ^
      "but is never initialized.";
      "Incompatible parameter type [6]: Expected `P` for 1st anonymous parameter to call `foo` " ^
      "but got `A`.";
    ];
  ()


let test_check_generic_protocols _ =
  assert_type_errors
    {|
      T = typing.TypeVar("T", int, str)
      class P(typing.Protocol[T]):
        def foo() -> T: ...

      class A():
        def foo(self) -> int:
          return 7

      def foo(p: P[int]) -> int:
        return p.foo()

      def bar(a: A) -> None:
        foo(a)

    |}
    [];
  assert_type_errors
    {|
      T = typing.TypeVar("T", int, str)
      class P(typing.Protocol[T]):
        def foo() -> T: ...

      class A():
        def foo(self) -> int:
          return 7

      def foo(p: P[str]) -> str:
        return p.foo()

      def bar(a: A) -> None:
        foo(a)

    |}
    [
      "Incompatible parameter type [6]: " ^
      "Expected `P[str]` for 1st anonymous parameter to call `foo` but got `A`.";
    ];
  assert_type_errors
    {|
      T = typing.TypeVar("T", int, str)
      class P(typing.Protocol[T]):
        def foo() -> T: ...

      class A():
        def foo(self) -> int:
          return 7

      T2 = typing.TypeVar("T2", int, str)
      def foo(p: P[T2]) -> T2:
        return p.foo()

      def bar(a: A) -> int:
        v = foo(a)
        reveal_type(v)
        return v

    |}
    ["Revealed type [-1]: Revealed type for `v` is `int`."];
  assert_type_errors
    {|
      T = typing.TypeVar("T", int, str)
      class P(typing.Protocol[T]):
        def foo() -> T: ...

      class A():
        def foo(self) -> bool:
          return True

      T2 = typing.TypeVar("T2", int, str)
      def foo(p: P[T2]) -> T2:
        return p.foo()

      def bar(a: A) -> None:
        foo(a)

    |}
    [
      "Incompatible parameter type [6]: Expected `P[Variable[T2 <: [int, str]]]` for 1st " ^
      "anonymous parameter to call `foo` but got `A`.";
    ];
  ()

let test_check_generic_implementors _ =
  assert_type_errors
    {|
      def foo(l: typing.List[int]) -> int:
         return len(l)
    |}
    [];
  assert_type_errors
    {|
      class P(typing.Protocol):
        def foo() -> typing.Union[int, str]: ...

      T = typing.TypeVar("T", bound=typing.Union[int, str])
      class A(typing.Generic[T]):
        x: T
        def __init__(self, x: T) -> None:
          self.x = x
        def foo(self) -> T:
          return self.x

      def foo(p: P) -> typing.Union[int, str]:
        return p.foo()

      def bar(a: A[int]) -> None:
        foo(a)

    |}
    [];
  assert_type_errors
    {|
    T1 = typing.TypeVar("T1")
    class P(typing.Protocol[T1]):
      def foo() -> T1: ...

    T = typing.TypeVar("T", bound=typing.Union[int, str])
    class A(typing.Generic[T]):
      x: T
      def __init__(self, x: T) -> None:
        self.x = x
      def foo(self) -> T:
        return self.x

    def foo(p: P[int]) -> int:
      return p.foo()

    def bar(a: A[int]) -> None:
      foo(a)

    |}
    [];
  assert_type_errors
    {|
    T1 = typing.TypeVar("T1")
    class P(typing.Protocol[T1]):
      def foo() -> T1: ...

    T = typing.TypeVar("T", bound=typing.Union[int, str])
    class A(typing.Generic[T]):
      x: T
      def __init__(self, x: T) -> None:
        self.x = x
      def foo(self) -> T:
        return self.x

    def foo(p: P[bool]) -> bool:
      return p.foo()

    def bar(a: A[int]) -> None:
      foo(a)

    |}
    [
      "Incompatible parameter type [6]: " ^
      "Expected `P[bool]` for 1st anonymous parameter to call `foo` but got `A[int]`."
    ];
  assert_type_errors
    {|
    T1 = typing.TypeVar("T1")
    class P(typing.Protocol[T1]):
      def foo() -> T1: ...

    T = typing.TypeVar("T", bound=typing.Union[int, str])
    class A(typing.Generic[T]):
      x: T
      def __init__(self, x: T) -> None:
        self.x = x
      def foo(self) -> T:
        return self.x

    T3 = typing.TypeVar("T", bound=typing.Union[int, str])
    def foo(p: P[T3]) -> T3:
      return p.foo()

    def bar(a: A[int]) -> int:
      return foo(a)

    |}
    [];
  (* This should work but doesn't because we don't currently support back-edge parameters, i.e.
     the situation where A is only a subclass of P when it has int as a parameter *)
  assert_type_errors
    {|
      class P(typing.Protocol):
        def foo() -> int: ...

      T = typing.TypeVar("T")
      class A(typing.Generic[T]):
        x: T
        def __init__(self, x: T) -> None:
          self.x = x
        def foo(self) -> T:
          return self.x

      def foo(p: P) -> int:
        return p.foo()

      def bar(a: A[int]) -> None:
        foo(a)

    |}
    [
      "Incompatible parameter type [6]: " ^
      "Expected `P` for 1st anonymous parameter to call `foo` but got `A[int]`."
    ];
  ()


let test_callback_protocols _ =
  assert_type_errors
    {|
      class P(typing.Protocol):
        def __call__(self, x: int, y:str) -> bool: ...
      def takesP(f: P) -> bool:
        return f(x = 1, y = "one")
      def exactMatch(x: int, y: str) -> bool:
        return True
      def doesNotMatch(x: int, y: str) -> str:
        return "True"
      def foo() -> None:
        takesP(exactMatch)
        takesP(doesNotMatch)
    |}
    [
      "Incompatible parameter type [6]: Expected `P` for 1st anonymous parameter to call " ^
      "`takesP` but got `typing.Callable(doesNotMatch)[[Named(x, int), Named(y, str)], str]`.";
    ];

  assert_type_errors
    {|
      class NotAProtocol():
        def __call__(self, x: int, y:str) -> bool: ...
      def exactMatch(x: int, y: str) -> bool:
        return True
      def foo() -> NotAProtocol:
        return exactMatch
    |}
    [
      "Incompatible return type [7]: Expected `NotAProtocol` but got " ^
      "`typing.Callable(exactMatch)[[Named(x, int), Named(y, str)], bool]`.";
    ];

  assert_type_errors
    {|
      T = typing.TypeVar("T")
      class P(typing.Protocol[T]):
        def __call__(self, x: int, y:str) -> T: ...
      def takesPInt(f: P[int]) -> int:
        return f(x = 1, y = "one")
      def exactMatch(x: int, y: str) -> int:
        return 7
      def doesNotMatch(x: int, y: str) -> str:
        return "True"
      def foo() -> None:
        takesPInt(exactMatch)
        takesPInt(doesNotMatch)
    |}
    [
      "Incompatible parameter type [6]: Expected `P[int]` for 1st anonymous parameter to call " ^
      "`takesPInt` but got `typing.Callable(doesNotMatch)[[Named(x, int), Named(y, str)], str]`.";
    ];

  assert_type_errors
    {|
      T = typing.TypeVar("T")
      class P(typing.Protocol[T]):
        def __call__(self, x: int, y:str) -> T: ...
      T2 = typing.TypeVar("T")
      def takesPGeneric(f: P[T2]) -> T2:
        return f(x = 1, y = "one")
      def intMatch(x: int, y: str) -> int:
        return 7
      def strMatch(x: int, y: str) -> str:
        return "True"
      def doesNotMatch(x: str, y: int) -> int:
        return 17
      def foo() -> None:
        v = takesPGeneric(intMatch)
        reveal_type(v)
        v = takesPGeneric(strMatch)
        reveal_type(v)
        takesPGeneric(doesNotMatch)
    |}
    [
      "Revealed type [-1]: Revealed type for `v` is `int`.";
      "Revealed type [-1]: Revealed type for `v` is `str`.";
      "Incompatible parameter type [6]: Expected `P[Variable[T2]]` for 1st anonymous parameter " ^
      "to call `takesPGeneric` but got " ^
      "`typing.Callable(doesNotMatch)[[Named(x, str), Named(y, int)], int]`.";
    ];
  ()


let () =
  "protocol">:::[
    "check_protocols">::test_check_protocol;
    "check_generic_implementors">::test_check_generic_implementors;
    "check_generic_protocols">::test_check_generic_protocols;
    "callback_protocols">::test_callback_protocols;
  ]
  |> Test.run
