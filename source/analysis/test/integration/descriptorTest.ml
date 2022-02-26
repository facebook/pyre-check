(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest

let test_non_data_descriptors context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from typing import overload, Union
      class Descriptor:
        def __get__(self, o: object, t: object = None) -> int:
          return 1

      class Host:
        d: Descriptor = Descriptor()

      def f() -> None:
        x = Host().d
        reveal_type(x)
        y = Host.d
        reveal_type(y)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `int`.";
      "Revealed type [-1]: Revealed type for `y` is `int`.";
    ];
  (* Distinguishing being called from instance/from class *)
  assert_type_errors
    {|
      from typing import overload, Union
      class Descriptor:
        @overload
        def __get__(self, o: None, t: object = None) -> int: ...
        @overload
        def __get__(self, o: object, t: object = None) -> str: ...
        def __get__(self, o: object, t: object = None) -> Union[int, str]:
          if o:
           return "A"
          else:
           return 1

      class Host:
        d: Descriptor = Descriptor()

      def f() -> None:
        x = Host().d
        reveal_type(x)
        y = Host.d
        reveal_type(y)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `str`.";
      "Revealed type [-1]: Revealed type for `y` is `int`.";
    ];
  (* Overloading based on host class *)
  assert_type_errors
    {|
      from typing import overload, Union, NoReturn
      class BaseA:
        a_prop: int = 1
      class BaseB:
        b_prop: str = "A"
      class Descriptor:
        @overload
        def __get__(self, o: BaseA, t: object = None) -> int: ...
        @overload
        def __get__(self, o: BaseB, t: object = None) -> str: ...
        @overload
        def __get__(self, o: object, t: object = None) -> bool: ...
        def __get__(self, o: object, t: object = None) -> Union[int, str, bool]:
          if isinstance(o, BaseA):
           return o.a_prop
          elif isinstance(o, BaseB):
           return o.b_prop
          else:
           return True

      class HostA(BaseA):
        d: Descriptor = Descriptor()
      class HostB(BaseB):
        d: Descriptor = Descriptor()
      class HostC:
        d: Descriptor = Descriptor()

      def f() -> None:
        x = HostA().d
        reveal_type(x)
        y = HostB().d
        reveal_type(y)
        z = HostC().d
        reveal_type(z)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `int`.";
      "Revealed type [-1]: Revealed type for `y` is `str`.";
      "Revealed type [-1]: Revealed type for `z` is `bool`.";
    ];
  (* Generic descriptors *)
  assert_type_errors
    {|
      from typing import overload, Union, Generic, TypeVar, Callable
      T = TypeVar("T")
      THost = TypeVar("THost")
      class MyCallable(Generic[T]):
        @overload
        def __get__(self, o: None, t: object = None) -> T: ...
        @overload
        def __get__(self, o: THost, t: object = None) -> BoundMethod[T, THost]: ...

      class Host:
        d: MyCallable[Callable[[Host, int], str]] = MyCallable()

      def f() -> None:
        x = Host().d
        reveal_type(x)
        y = Host.d
        reveal_type(y)
        z = Host().d(1)
        reveal_type(z)
        Host.d(1)
    |}
    [
      "Missing overload implementation [42]: Overloaded function `MyCallable.__get__` must have an \
       implementation.";
      "Revealed type [-1]: Revealed type for `x` is `BoundMethod[typing.Callable[[Host, int], \
       str], Host]`.";
      "Revealed type [-1]: Revealed type for `y` is `typing.Callable[[Host, int], str]`.";
      "Revealed type [-1]: Revealed type for `z` is `str`.";
      "Missing argument [20]: PositionalOnly call expects argument in position 1.";
    ];
  assert_type_errors
    {|
      from typing import overload, Union
      class Descriptor:
        def __get__(self, o: object, t: object = None) -> str:
          return "A"

      def producer() -> Union[Descriptor, int]: ...

      class Host:
        d: Union[Descriptor, int] = producer()

      def f() -> None:
        x = Host().d
        reveal_type(x)
        y = Host.d
        reveal_type(y)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `Union[int, str]`.";
      "Revealed type [-1]: Revealed type for `y` is `Union[int, str]`.";
    ];
  assert_type_errors
    {|
      from typing import overload, Union
      from dataclasses import dataclass

      class Descriptor:
        def __get__(self, o: object, t: object = None) -> str:
          return "A"

      @dataclass
      class DC:
        d: Descriptor = Descriptor()

      def f(d: DC) -> None:
        x = DC.d
        reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `str`."];
  assert_type_errors
    {|
      from typing import overload, Union
      class Descriptor:
        # TODO(T65806273): should error here
        __get__: int = 1

      class Host:
        d: Descriptor = Descriptor()

      def f() -> None:
        x = Host().d
        reveal_type(x)
        y = Host.d
        reveal_type(y)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `typing.Any`.";
      "Revealed type [-1]: Revealed type for `y` is `typing.Any`.";
    ];
  assert_type_errors
    {|
      from typing import overload, Union
      class Inner:
        def __call__(self, descriptor: object, host: object, host_type: object = None) -> int:
          return 42

        def __get__(self, host: object, host_type: object = None) -> str:
          # This should not be relevant to anything since `__get__`s are accessed
          # magically without applying description
          return "irrelevant"

      class Descriptor:
        __get__: Inner = Inner()

      class Host:
        d: Descriptor = Descriptor()

      def f() -> None:
        x = Host().d
        reveal_type(x)
        y = Host.d
        reveal_type(y)
    |}
    [
      (* TODO(T65807186): This should be supported (should be `int`) *)
      "Revealed type [-1]: Revealed type for `x` is `typing.Any`.";
      "Revealed type [-1]: Revealed type for `y` is `typing.Any`.";
    ];
  assert_type_errors
    {|
      from typing import overload, Union, TypeVar, List

      T = TypeVar("T")

      class OnlyCanBeHostedOnLists:
        # TODO(T65807232): should error here, because not less than "virtual" object.__get__
        def __get__(self, host: List[T], host_type: object = None) -> T:
          return host[0]

      class ListHost(List[int]):
        first = OnlyCanBeHostedOnLists()

      class NonListHost():
        first = OnlyCanBeHostedOnLists()

      def f(l: ListHost, n: NonListHost) -> None:
        x = l.first
        reveal_type(x)
        y = n.first
        reveal_type(y)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `int`.";
      "Revealed type [-1]: Revealed type for `y` is `typing.Any`.";
    ];
  assert_type_errors
    {|
      from typing import overload, Union, TypeVar, List

      class Parent:
        pass

      class EvilChild(Parent):
        # TODO(T65807232): should error here, because not less than "virtual" Parent.__get__
        def __get__(self, host: object, host_type: object = None) -> int:
          return 42

      def producer() -> Parent:
        return EvilChild()

      class Host:
        a: Parent = producer()

      def f() -> None:
        x = Host().a
        reveal_type(x)
        y = Host.a
        reveal_type(y)
    |}
    [
      (* These are technically wrong, but its not our fault *)
      "Revealed type [-1]: Revealed type for `x` is `Parent`.";
      "Revealed type [-1]: Revealed type for `y` is `Parent`.";
    ];
  assert_type_errors
    {|
      from typing import overload, Union, TypeVar, List

      class Descriptor:
        def __get__(self, o: object, t: object = None) -> int:
          return 1

      class Host:
        a = Descriptor()

      def f() -> None:
        h = Host()
        x = Host.a
        reveal_type(x)
        h.a = 5
        h.a = Descriptor()
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `int`.";
      "Incompatible attribute type [8]: Attribute `a` declared in class `Host` has type `int` but \
       is used as type `Descriptor`.";
    ];
  assert_type_errors
    {|
      from typing import overload, Union
      class Descriptor:
        @overload
        def __get__(self, o: None, t: object = None) -> int: ...
        @overload
        def __get__(self, o: object, t: object = None) -> str: ...
        def __get__(self, o: object, t: object = None) -> Union[int, str]:
          if o:
           return "A"
          else:
           return 1

      class MetaclassHost(type):
        d: Descriptor = Descriptor()

      class C(metaclass=MetaclassHost):
        pass

      def f() -> None:
        # This is str because Type[C] is an instance of MetaclassHost
        x = C.d
        reveal_type(x)
        y = MetaclassHost("A", (), {}).d
        reveal_type(y)
        z = MetaclassHost.d
        reveal_type(z)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `str`.";
      "Revealed type [-1]: Revealed type for `y` is `str`.";
      "Revealed type [-1]: Revealed type for `z` is `int`.";
    ];

  assert_type_errors
    {|
      from typing import overload, Union, TypeVar, List, ClassMethod, Callable, Type, Any

      def maker() -> Any: ...

      class Host:
        cm: ClassMethod[Callable[[Type[Host], int, str], bool]] = maker()

      def f() -> None:
        x = Host().cm
        reveal_type(x)
        y = Host.cm
        reveal_type(y)
        z = Host().cm(1, "A")
        reveal_type(z)
        z = Host().cm(1, 2)
    |}
    [
      "Undefined import [21]: Could not find a name `ClassMethod` defined in module `typing`.";
      "Missing return annotation [3]: Return type must be specified as type other than `Any`.";
      "Revealed type [-1]: Revealed type for `x` is `BoundMethod[typing.Callable[[Type[Host], int, \
       str], bool], Type[Host]]`.";
      "Revealed type [-1]: Revealed type for `y` is `BoundMethod[typing.Callable[[Type[Host], int, \
       str], bool], Type[Host]]`.";
      "Revealed type [-1]: Revealed type for `z` is `bool`.";
      "Incompatible parameter type [6]: In anonymous call, for 2nd positional only parameter \
       expected `str` but got `int`.";
    ];
  assert_type_errors
    {|
     from typing import TypeVar, Type, Optional
     T = TypeVar("T")
     class X:
       @classmethod
       def x(cls, x: T) -> Optional[T]: ...

       @classmethod
       def foo(cls, y: int) -> None:
         z = cls.x(y)
         reveal_type(z)

    |}
    ["Revealed type [-1]: Revealed type for `z` is `Optional[int]`."];
  assert_type_errors
    {|
      from typing import overload, Union, TypeVar, List, StaticMethod, Callable, Type, Any

      def maker() -> Any: ...

      class Host:
        sm: StaticMethod[Callable[[int, str], bool]] = maker()

      def f() -> None:
        x = Host().sm
        reveal_type(x)
        y = Host.sm
        reveal_type(y)
    |}
    [
      "Undefined import [21]: Could not find a name `StaticMethod` defined in module `typing`.";
      "Missing return annotation [3]: Return type must be specified as type other than `Any`.";
      "Revealed type [-1]: Revealed type for `x` is `typing.Callable[[int, str], bool]`.";
      "Revealed type [-1]: Revealed type for `y` is `typing.Callable[[int, str], bool]`.";
    ];
  assert_type_errors
    {|
      from typing import overload, Union, TypeVar, List, StaticMethod, Callable, Type, Any

      def free_function(h: object, x: int) -> str:
        return "A"

      class Host:
        m: Callable[[object, int], str] = free_function

      def f() -> None:
        x = Host().m
        reveal_type(x)
        y = Host.m
        reveal_type(y)
    |}
    [
      "Undefined import [21]: Could not find a name `StaticMethod` defined in module `typing`.";
      "Revealed type [-1]: Revealed type for `x` is `BoundMethod[typing.Callable[[object, int], \
       str], Host]`.";
      "Revealed type [-1]: Revealed type for `y` is `typing.Callable[[object, int], str]`.";
    ];
  assert_type_errors
    {|
      from typing import overload, Union, TypeVar, List, StaticMethod, Callable, Type, Any

      class CallableClass:
        def __call__(self, h: object, x: int) -> str:
          return "A"

      class Host:
        direct: CallableClass = CallableClass()
        as_callable: Callable[[object, int], str] = CallableClass()

      def f() -> None:
        x = Host().direct
        reveal_type(x)
        y = Host.direct
        reveal_type(y)

        a = Host().as_callable
        reveal_type(a)
        b = Host.as_callable
        reveal_type(b)
    |}
    [
      "Undefined import [21]: Could not find a name `StaticMethod` defined in module `typing`.";
      "Revealed type [-1]: Revealed type for `x` is `CallableClass`.";
      "Revealed type [-1]: Revealed type for `y` is `CallableClass`.";
      (* This is wrong. Unfortunately its currently avoidable as long as we resolve defs to
         Callables *)
      "Revealed type [-1]: Revealed type for `a` is `BoundMethod[typing.Callable[[object, int], \
       str], Host]`.";
      "Revealed type [-1]: Revealed type for `b` is `typing.Callable[[object, int], str]`.";
    ];
  assert_type_errors
    {|
      from typing import overload, Union, TypeVar, List, StaticMethod, Callable, Type, Any

      def free_function(h: object, x: int) -> str:
        return "A"

      class Host:
       m: Union[Callable[[object, int], str], int] = free_function

      def f() -> None:
        x = Host().m
        reveal_type(x)
        y = Host.m
        reveal_type(y)
    |}
    [
      "Undefined import [21]: Could not find a name `StaticMethod` defined in module `typing`.";
      "Revealed type [-1]: Revealed type for `x` is `Union[BoundMethod[typing.Callable[[object, \
       int], str], Host], int]`.";
      "Revealed type [-1]: Revealed type for `y` is `Union[typing.Callable[[object, int], str], \
       int]`.";
    ];
  assert_type_errors
    {|
      from typing import NamedTuple

      class Descriptor:
        def __get__(self, o: object, t: object = None) -> int:
          return 1

      class N(NamedTuple):
          value: Descriptor = Descriptor()

      def f() -> None:
          foo = N()
          x = foo.value
          reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `Descriptor`."];
  ()


let test_data_descriptors context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from typing import overload, Union
      class Descriptor:
        def __get__(self, o: object, t: object = None) -> int:
          return 1
        def __set__(self, o: object, v: str) -> None:
          pass

      class Host:
        d: Descriptor = Descriptor()

      def f() -> None:
        x = Host().d
        reveal_type(x)
        y = Host.d
        reveal_type(y)

        Host().d = "A"
        # assignments to the class always ignore __set__
        Host.d = Descriptor()

        Host().d = Descriptor()
        Host.d = "A"

        reveal_type(Host().d)
        reveal_type(Host.d)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `int`.";
      "Revealed type [-1]: Revealed type for `y` is `int`.";
      (* This is not a great error message but it is correct *)
      "Incompatible attribute type [8]: Attribute `d` declared in class `Host` has type `str` but \
       is used as type `Descriptor`.";
      "Incompatible attribute type [8]: Attribute `d` declared in class `Host` has type \
       `Descriptor` but is used as type `str`.";
      (* This is an even more confusing message, but is also correct *)
      "Revealed type [-1]: Revealed type for `test.Host().d` is `str` (inferred: `int`).";
      "Revealed type [-1]: Revealed type for `test.Host.d` is `Descriptor` (inferred: `int`).";
    ];
  (* Overloading based on host class *)
  assert_type_errors
    {|
      from typing import overload, Union, NoReturn
      class BaseA:
        a_prop: int = 1
      class BaseB:
        b_prop: str = "A"
      class Descriptor:
        @overload
        def __set__(self, o: BaseA, v: int) -> None: ...
        @overload
        def __set__(self, o: BaseB, v: str) -> None: ...
        @overload
        def __set__(self, o: object, v: bool) -> None: ...
        def __set__(self, o: object, v: object) -> None:
          pass

      class HostA(BaseA):
        d: Descriptor = Descriptor()
      class HostB(BaseB):
        d: Descriptor = Descriptor()
      class HostC:
        d: Descriptor = Descriptor()

      def f() -> None:
        reveal_type(HostA().d)
        reveal_type(HostB().d)
        reveal_type(HostC().d)
    |}
    [
      "Revealed type [-1]: Revealed type for `test.HostA().d` is `int` (inferred: `Descriptor`).";
      "Revealed type [-1]: Revealed type for `test.HostB().d` is `str` (inferred: `Descriptor`).";
      "Revealed type [-1]: Revealed type for `test.HostC().d` is `bool` (inferred: `Descriptor`).";
    ];
  assert_type_errors
    {|
    from typing import overload, Union
    from dataclasses import dataclass

    class Descriptor:
      x: str = ""
      def __get__(self, o: object, t: object = None) -> str:
        return "A" + self.x
      def __set__(self, o: object, value: str) -> None:
        self.x = value

    @dataclass
    class DC:
      d: Descriptor = Descriptor()

    def f() -> None:
      DC("A")
    |}
    [
      (* TODO(T65806273): This should be accepted, but we're currently ignoring descriptors when
         building dataclass constructors for perf reasons *)
      "Incompatible parameter type [6]: In call `DC.__init__`, for 1st positional only parameter \
       expected `Descriptor` but got `str`.";
    ];
  assert_type_errors
    {|
      from typing import overload, Union
      class Descriptor:
          def __set__(self, h: object, v: int) -> None:
            pass

      class MetaclassHost(type):
          d: Descriptor = Descriptor()

      class C(metaclass=MetaclassHost):
          pass

      def f() -> None:
          # This is correct because Type[C] is an instance of MetaclassHost
          C.d = 1
          C.d = Descriptor()
          reveal_type("separator")
          MetaclassHost("A", (), {}).d = 2
          MetaclassHost("A", (), {}).d = Descriptor()
          reveal_type("separator")
          MetaclassHost.d = 3
          MetaclassHost.d = Descriptor()
    |}
    [
      "Incompatible attribute type [8]: Attribute `d` declared in class `MetaclassHost` has type \
       `int` but is used as type `Descriptor`.";
      "Revealed type [-1]: Revealed type for `\"separator\"` is \
       `typing_extensions.Literal['separator']`.";
      "Incompatible attribute type [8]: Attribute `d` declared in class `MetaclassHost` has type \
       `int` but is used as type `Descriptor`.";
      "Revealed type [-1]: Revealed type for `\"separator\"` is \
       `typing_extensions.Literal['separator']`.";
      "Incompatible attribute type [8]: Attribute `d` declared in class `MetaclassHost` has type \
       `Descriptor` but is used as type `int`.";
    ];
  ()


let () =
  "descriptors"
  >::: [
         "check_non_data_descriptors" >:: test_non_data_descriptors;
         "check_data_descriptors" >:: test_data_descriptors;
       ]
  |> Test.run
