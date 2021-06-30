(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Core
open Pyre
open Analysis
open Ast
open Interprocedural
open Test

let setup ?(additional_sources = []) ~context ~handle source =
  let project =
    ScratchProject.setup ~context ~external_sources:[] ([handle, source] @ additional_sources)
  in
  let { ScratchProject.BuiltTypeEnvironment.sources; type_environment; _ } =
    ScratchProject.build_type_environment project
  in
  let source =
    List.find_exn sources ~f:(fun { Source.source_path = { SourcePath.relative; _ }; _ } ->
        String.equal relative handle)
  in
  source, TypeEnvironment.read_only type_environment


let test_all_decorators context =
  let assert_decorators source expected =
    let additional_sources =
      [
        ( "file1.py",
          {|
            from typing import Callable
            def decorator1(callable: Callable[[str], None]) -> Callable[[str], None]: ...
      |}
        );
        ( "some_module/file2.py",
          {|
            from typing import Callable
            def decorator2(callable: Callable[[str], None]) -> Callable[[str], None]: ...
      |}
        );
      ]
    in
    let _, environment = setup ~additional_sources ~context ~handle:"test.py" source in
    assert_equal
      ~cmp:[%equal: DecoratorHelper.decorator_reference_and_module list]
      ~printer:[%show: DecoratorHelper.decorator_reference_and_module list]
      (List.map expected ~f:(fun (decorator, module_reference) ->
           { DecoratorHelper.decorator; module_reference }))
      ( DecoratorHelper.all_decorators environment
      |> List.sort ~compare:[%compare: DecoratorHelper.decorator_reference_and_module] )
  in
  assert_decorators
    {|
    @file1.decorator1
    def foo(z: str) -> None:
      print(z)

    @some_module.file2.decorator2
    @decorator_with_no_module(1, 2)
    def bar(z: str) -> None:
      print(z)
  |}
    [
      !&"decorator_with_no_module", None;
      !&"file1.decorator1", Some !&"file1";
      !&"some_module.file2.decorator2", Some !&"some_module.file2";
    ];
  assert_decorators
    {|
    def outer(z: str) -> None:
      @file1.decorator1
      def inner(z: str) -> None:
        print(z)
  |}
    [!&"file1.decorator1", Some !&"file1"];
  assert_decorators
    {|
    class Foo:
      @file1.decorator1
      def some_method(self, z: str) -> None:
        print(z)
  |}
    [!&"file1.decorator1", Some !&"file1"];
  ()


let test_inline_decorators context =
  let assert_inlined ?(additional_sources = []) ?(handle = "test.py") source expected =
    let source, environment = setup ~additional_sources ~context ~handle source in
    let decorator_bodies = DecoratorHelper.all_decorator_bodies environment in
    let actual = DecoratorHelper.inline_decorators ~decorator_bodies source in
    (* Using the same setup code instead of `parse` because the SourcePath `priority` is different
       otherwise. *)
    let expected =
      setup ~additional_sources ~context ~handle expected
      |> fst
      |> DecoratorHelper.sanitize_defines ~strip_decorators:false
    in
    assert_source_equal ~location_insensitive:true expected actual
  in
  assert_inlined
    {|
    from builtins import __test_sink
    from typing import Callable

    def with_logging(callable: Callable[[str], None]) -> Callable[[str], None]:

      def inner(y: str) -> None:
        __test_sink(y)
        callable(y)

      return inner

    @with_logging
    def foo(z: str) -> None:
      print(z)
  |}
    {|
    from builtins import __test_sink
    from typing import Callable

    def with_logging(callable: Callable[[str], None]) -> Callable[[str], None]:

      def inner(y: str) -> None:
        __test_sink(y)
        callable(y)

      return inner

    def foo(y: str) -> None:
      def __original_function(z: str) -> None:
        print(z)

      def __inlined_with_logging(y: str) -> None:
        __test_sink(y)
        __original_function(y)

      return __inlined_with_logging(y)
  |};
  (* Leave decorators as such if none can be inlined. *)
  assert_inlined
    {|
    from builtins import __test_sink, __test_source
    from typing import Callable

    def fails_to_apply(f):
      return f

    @fails_to_apply
    def foo(z: str) -> None:
      print(z)

    class Foo:
      @classmethod
      @fails_to_apply
      def some_method(cls, z: str) -> None:
        print(z)
  |}
    {|
    from builtins import __test_sink, __test_source
    from typing import Callable

    def fails_to_apply(f):
      return f

    @fails_to_apply
    def foo(z: str) -> None:
      print(z)

    class Foo:
      @classmethod
      @fails_to_apply
      def some_method(cls, z: str) -> None:
        print(z)
  |};
  assert_inlined
    {|
    from builtins import __test_sink, __test_source
    from typing import Callable

    class Foo:
      _some_property: str = "hello"

      @classmethod
      def some_method(cls, z: str) -> None:
        print(z)

      @property
      def some_property(self) -> str:
        return self._some_property

      @some_property.setter
      def some_property(self, value: str) -> None:
        self._some_property = value
  |}
    {|
    from builtins import __test_sink, __test_source
    from typing import Callable

    class Foo:
      _some_property: str = "hello"

      @classmethod
      def some_method(cls, z: str) -> None:
        print(z)

      @property
      def some_property(self) -> str:
        return self._some_property

      @some_property.setter
      def some_property(self, value: str) -> None:
        self._some_property = value
  |};
  (* Ignore decorator if not all calls are identical. *)
  assert_inlined
    {|
    from typing import Callable
    from builtins import __test_sink

    def with_logging(f: Callable) -> Callable:

      def inner( *args, **kwargs) -> None:
        __test_sink(args)
        f( **kwargs)
        f( *args)
        f(1, 2)

      return inner

    @with_logging
    def foo(x: str) -> None:
      print(x)
  |}
    {|
    from typing import Callable
    from builtins import __test_sink

    def with_logging(f: Callable) -> Callable:

      def inner( *args, **kwargs) -> None:
        __test_sink(args)
        f( **kwargs)
        f( *args)
        f(1, 2)

      return inner

    @with_logging
    def foo(x: str) -> None:
      print(x)
  |};
  (* Inline a decorator even if it has no calls to the original function. *)
  assert_inlined
    {|
    from typing import Callable
    from builtins import __test_sink

    def no_calls_to_original_function(f: Callable) -> Callable:

      def inner( *args, **kwargs) -> None:
        __test_sink(args)

      return inner

    @no_calls_to_original_function
    def foo(x: str) -> None:
      print(x)
  |}
    {|
    from typing import Callable
    from builtins import __test_sink

    def no_calls_to_original_function(f: Callable) -> Callable:

      def inner( *args, **kwargs) -> None:
        __test_sink(args)

      return inner

    def foo( *args, **kwargs) -> None:

      def __original_function(x: str) -> None:
        print(x)

      def __inlined_no_calls_to_original_function( *args, **kwargs) -> None:
        __test_sink(args)

      return __inlined_no_calls_to_original_function( *args, **kwargs)
  |};
  assert_inlined
    {|
    from builtins import __test_sink
    from typing import Callable

    def with_logging(callable: Callable[[str], None]) -> Callable[[str], None]:

      def inner(y: str) -> None:
        __test_sink(y)
        result = callable(y)
        return result

      return inner

    @with_logging
    def foo(z: str) -> None:
      print(z)
      result = None
      return result
  |}
    {|
    from builtins import __test_sink
    from typing import Callable

    def with_logging(callable: Callable[[str], None]) -> Callable[[str], None]:

      def inner(y: str) -> None:
        __test_sink(y)
        result = callable(y)
        return result

      return inner

    def foo(y: str) -> None:
      def __original_function(z: str) -> None:
        print(z)
        result = None
        return result

      def __inlined_with_logging(y: str) -> None:
        __test_sink(y)
        result = __original_function(y)
        return result

      return __inlined_with_logging(y)
  |};
  (* `async` decorator. *)
  assert_inlined
    {|
    from typing import Awaitable, Callable

    def with_logging_async(f: Callable[[str], Awaitable[int]]) -> Callable[[str], Awaitable[int]]:

      async def inner(y: str) -> int:
        try:
          result = await f(y)
          return result
        except Exception:
          return 42

      return inner

    @with_logging_async
    async def foo(x: str) -> int:
      print(x)
  |}
    {|
    from typing import Awaitable, Callable

    def with_logging_async(f: Callable[[str], Awaitable[int]]) -> Callable[[str], Awaitable[int]]:

      async def inner(y: str) -> int:
        try:
          result = await f(y)
          return result
        except Exception:
          return 42

      return inner

    async def foo(y: str) -> int:

      async def __original_function(x: str) -> int:
        print(x)

      async def __inlined_with_logging_async(y: str) -> int:
        try:
          result = await __original_function(y)
          return result
        except Exception:
          return 42

      return await __inlined_with_logging_async(y)
  |};
  (* Decorator that types the function parameter as `f: Callable`. *)
  assert_inlined
    {|
    from typing import Callable
    from builtins import __test_sink

    def with_logging(f: Callable) -> Callable:

      def inner(y: str) -> int:
        __test_sink(y)
        f(y)

      return inner

    @with_logging
    def foo(x: str) -> int:
      print(x)
  |}
    {|
    from typing import Callable
    from builtins import __test_sink

    def with_logging(f: Callable) -> Callable:

      def inner(y: str) -> int:
        __test_sink(y)
        f(y)

      return inner

    def foo(y: str) -> int:

      def __original_function(x: str) -> int:
        print(x)

      def __inlined_with_logging(y: str) -> int:
        __test_sink(y)
        __original_function(y)

      return __inlined_with_logging(y)
  |};
  (* Wrapper function with default values for parameters. *)
  assert_inlined
    {|
    from builtins import __test_sink
    from typing import Callable

    def with_logging(callable: Callable[[str], None]) -> Callable[[str], None]:

      def inner(y: str, z: int = 4) -> None:
        __test_sink(y)
        callable(y + z)

      return inner

    @with_logging
    def foo(z: str) -> None:
      print(z)
  |}
    {|
    from builtins import __test_sink
    from typing import Callable

    def with_logging(callable: Callable[[str], None]) -> Callable[[str], None]:

      def inner(y: str, z: int = 4) -> None:
        __test_sink(y)
        callable(y + z)

      return inner

    def foo(y: str, z: int = 4) -> None:
      def __original_function(z: str) -> None:
        print(z)

      def __inlined_with_logging(y: str, z: int = 4) -> None:
        __test_sink(y)
        __original_function(y + z)

      return __inlined_with_logging(y, z)
  |};
  (* Wrapper function with `*args` and `**kwargs`. *)
  assert_inlined
    {|
    from typing import Callable
    from builtins import __test_sink

    def with_logging(f: Callable) -> Callable:

      def inner( *args, **kwargs) -> None:
        __test_sink(args)
        f( *args, **kwargs)

      return inner

    @with_logging
    def foo(x: str) -> None:
      print(x)
  |}
    {|
    from typing import Callable
    from builtins import __test_sink

    def with_logging(f: Callable) -> Callable:

      def inner( *args, **kwargs) -> None:
        __test_sink(args)
        f( *args, **kwargs)

      return inner

    def foo(x: str) -> None:

      def __original_function(x: str) -> None:
        print(x)

      def __inlined_with_logging(x: str) -> None:
        __args = (x,)
        __kwargs = {"x": x}
        __test_sink(__args)
        __original_function(x)

      return __inlined_with_logging(x)
  |};
  (* ParamSpec. *)
  assert_inlined
    {|
    from typing import Callable
    from pyre_extensions import ParameterSpecification
    from builtins import __test_sink

    P = ParameterSpecification("P")

    def with_logging(f: Callable[P, None]) -> Callable[P, None]:

      def inner( *args: P.args, **kwargs: P.kwargs) -> None:
        f( *args, **kwargs)

      return inner

    @with_logging
    def foo(x: str, y: int) -> None:
      print(x, y)
  |}
    {|
    from typing import Callable
    from pyre_extensions import ParameterSpecification
    from builtins import __test_sink

    P = ParameterSpecification("P")

    def with_logging(f: Callable[P, None]) -> Callable[P, None]:

      def inner( *args: P.args, **kwargs: P.kwargs) -> None:
        f( *args, **kwargs)

      return inner

    def foo(x: str, y: int) -> None:

      def __original_function(x: str, y: int) -> None:
        print(x, y)

      def __inlined_with_logging(x: str, y: int) -> None:
        __args = (x, y)
        __kwargs = {"x": x, "y": y}
        __original_function(x, y)

      return __inlined_with_logging(x, y)
  |};
  assert_inlined
    {|
    from typing import Callable
    from builtins import __test_sink

    def change_return_type(f: Callable) -> Callable:

      def inner( *args, **kwargs) -> int:
        f( *args, **kwargs)
        return 1

      return inner

    @change_return_type
    def foo(x: str) -> None:
      print(x)
  |}
    {|
    from typing import Callable
    from builtins import __test_sink

    def change_return_type(f: Callable) -> Callable:

      def inner( *args, **kwargs) -> int:
        f( *args, **kwargs)
        return 1

      return inner

    def foo(x: str) -> int:

      def __original_function(x: str) -> None:
        print(x)

      def __inlined_change_return_type(x: str) -> int:
        __args = (x,)
        __kwargs = {"x": x}
        __original_function(x)
        return 1

      return __inlined_change_return_type(x)
  |};
  (* Multiple decorators. *)
  assert_inlined
    {|
    from builtins import __test_sink, __test_source
    from typing import Callable

    def with_logging_sink(callable: Callable[[str], None]) -> Callable[[str], None]:

      def inner(y: str) -> None:
        __test_sink(y)
        callable(y)

      return inner

    def with_logging_source(callable: Callable[[str], None]) -> Callable[[str], None]:

      def inner(y: str) -> None:
        callable(y + __test_source())

      return inner

    @with_logging_source
    @with_logging_sink
    def foo(z: str) -> None:
      print(z)
  |}
    {|
    from builtins import __test_sink, __test_source
    from typing import Callable

    def with_logging_sink(callable: Callable[[str], None]) -> Callable[[str], None]:

      def inner(y: str) -> None:
        __test_sink(y)
        callable(y)

      return inner

    def with_logging_source(callable: Callable[[str], None]) -> Callable[[str], None]:

      def inner(y: str) -> None:
        callable(y + __test_source())

      return inner

    def foo(y: str) -> None:

      def __original_function(z: str) -> None:
        print(z)

      def __inlined_with_logging_sink(y: str) -> None:
        __test_sink(y)
        __original_function(y)

      def __inlined_with_logging_source(y: str) -> None:
        __inlined_with_logging_sink(y + __test_source())

      return __inlined_with_logging_source(y)
  |};
  (* Multiple decorators where one decorator fails to apply. *)
  assert_inlined
    {|
    from builtins import __test_sink, __test_source
    from typing import Callable

    def with_logging_sink(callable: Callable[[str], None]) -> Callable[[str], None]:

      def inner(y: str) -> None:
        __test_sink(y)
        callable(y)

      return inner

    def with_logging_source(callable: Callable[[str], None]) -> Callable[[str], None]:

      def inner(y: str) -> None:
        callable(y + __test_source())

      return inner

    def fails_to_apply(f):
      return f

    @fails_to_apply
    @with_logging_source
    @fails_to_apply
    @with_logging_sink
    @fails_to_apply
    def foo(z: str) -> None:
      print(z)
  |}
    {|
    from builtins import __test_sink, __test_source
    from typing import Callable

    def with_logging_sink(callable: Callable[[str], None]) -> Callable[[str], None]:

      def inner(y: str) -> None:
        __test_sink(y)
        callable(y)

      return inner

    def with_logging_source(callable: Callable[[str], None]) -> Callable[[str], None]:

      def inner(y: str) -> None:
        callable(y + __test_source())

      return inner

    def fails_to_apply(f):
      return f

    def foo(y: str) -> None:

      def __original_function(z: str) -> None:
        print(z)

      def __inlined_with_logging_sink(y: str) -> None:
        __test_sink(y)
        __original_function(y)

      def __inlined_with_logging_source(y: str) -> None:
        __inlined_with_logging_sink(y + __test_source())

      return __inlined_with_logging_source(y)
  |};
  (* Decorator factory. *)
  assert_inlined
    {|
    from typing import Callable
    from builtins import __test_sink

    def with_named_logger(logger_name: str) -> Callable[[Callable], Callable]:

      def _inner_decorator(f: Callable) -> Callable:

        def inner( *args: object, **kwargs: object) -> None:
          print(logger_name)
          __test_sink(args)
          f( *args, **kwargs)

        return inner

      return _inner_decorator

    @with_named_logger("foo_logger")
    def foo(x: str) -> None:
      print(x)
  |}
    {|
    from typing import Callable
    from builtins import __test_sink

    def with_named_logger(logger_name: str) -> Callable[[Callable], Callable]:

      def _inner_decorator(f: Callable) -> Callable:

        def inner( *args: object, **kwargs: object) -> None:
          print(logger_name)
          __test_sink(args)
          f( *args, **kwargs)

        return inner

      return _inner_decorator

    def foo(x: str) -> None:
      def __original_function(x: str) -> None:
        print(x)

      def __inlined_with_named_logger(x: str) -> None:
        __args = (x, )
        __kwargs = {"x": x}

        print($parameter$logger_name)
        __test_sink(__args)
        __original_function(x)

      return __inlined_with_named_logger(x)
  |};
  (* Decorator that uses helper functions. *)
  assert_inlined
    {|
    from builtins import __test_sink
    from typing import Callable

    def with_logging(callable: Callable[[str], None]) -> Callable[[str], None]:

      def inner(y: str) -> None:
        __test_sink(y)
        before(y)
        callable(y)
        after(y)

      def my_print(y: str) -> None:
        print("before", y)

      def before(y: str) -> None:
        message = "before"
        my_print(message, y)

      def after(y: str) -> None:
        message = "after"
        my_print(message, y)

      return inner

    @with_logging
    def foo(z: str) -> None:
      print(z)
  |}
    {|
    from builtins import __test_sink
    from typing import Callable

    def with_logging(callable: Callable[[str], None]) -> Callable[[str], None]:

      def inner(y: str) -> None:
        __test_sink(y)
        before(y)
        callable(y)
        after(y)

      def my_print(y: str) -> None:
        print("before", y)

      def before(y: str) -> None:
        message = "before"
        my_print(message, y)

      def after(y: str) -> None:
        message = "after"
        my_print(message, y)

      return inner

    def foo(y: str) -> None:
      def __original_function(z: str) -> None:
        print(z)

      def __inlined_with_logging(y: str) -> None:

        def my_print(y: str) -> None:
          print("before", y)

        def before(y: str) -> None:
          message = "before"
          my_print(message, y)

        def after(y: str) -> None:
          message = "after"
          my_print(message, y)

        __test_sink(y)
        before(y)
        __original_function(y)
        after(y)

      return __inlined_with_logging(y)
  |};
  (* Decorator factory with helper functions. *)
  assert_inlined
    {|
    from builtins import __test_sink
    from typing import Callable

    def with_named_logger(logger_name: str) -> Callable[[Callable], Callable]:
      def with_logging(callable: Callable[[str], None]) -> Callable[[str], None]:

        def inner(y: str) -> None:
          __test_sink(y)
          before(y)
          callable(y)
          after(y)

        def my_print(y: str) -> None:
          print("before", y)

        def before(y: str) -> None:
          message = "before"
          my_print(message, y)

        def after(y: str) -> None:
          message = "after"
          callable(y)
          my_print(message, y)

        return inner

      return with_logging

    @with_named_logger("hello")
    def foo(z: str) -> None:
      print(z)
  |}
    {|
    from builtins import __test_sink
    from typing import Callable

    def with_named_logger(logger_name: str) -> Callable[[Callable], Callable]:
      def with_logging(callable: Callable[[str], None]) -> Callable[[str], None]:

        def inner(y: str) -> None:
          __test_sink(y)
          before(y)
          callable(y)
          after(y)

        def my_print(y: str) -> None:
          print("before", y)

        def before(y: str) -> None:
          message = "before"
          my_print(message, y)

        def after(y: str) -> None:
          message = "after"
          callable(y)
          my_print(message, y)

        return inner

      return with_logging

    def foo(y: str) -> None:
      def __original_function(z: str) -> None:
        print(z)

      def __inlined_with_named_logger(y: str) -> None:

        def my_print(y: str) -> None:
          print("before", y)

        def before(y: str) -> None:
          message = "before"
          my_print(message, y)

        def after(y: str) -> None:
          message = "after"
          __original_function(y)
          my_print(message, y)

        __test_sink(y)
        before(y)
        __original_function(y)
        after(y)

      return __inlined_with_named_logger(y)
  |};
  assert_inlined
    {|
    from typing import Callable
    from pyre_extensions import ParameterSpecification
    from pyre_extensions.type_variable_operators import Concatenate

    from builtins import __test_sink

    P = ParameterSpecification("P")

    def with_logging(f: Callable[Concatenate[int, P], None]) -> Callable[Concatenate[int, P], None]:

      def inner(first_parameter: int, *args: P.args, **kwargs: P.kwargs) -> None:
        f(first_parameter, *args, **kwargs)
        print(first_parameter)
        print(args, kwargs)

      return inner

    @with_logging
    def foo(x: int, y: str, z: bool) -> None:
      print(x, y, z)
  |}
    {|
    from typing import Callable
    from pyre_extensions import ParameterSpecification
    from pyre_extensions.type_variable_operators import Concatenate

    from builtins import __test_sink

    P = ParameterSpecification("P")

    def with_logging(f: Callable[Concatenate[int, P], None]) -> Callable[Concatenate[int, P], None]:

      def inner(first_parameter: int, *args: P.args, **kwargs: P.kwargs) -> None:
        f(first_parameter, *args, **kwargs)
        print(first_parameter)
        print(args, kwargs)

      return inner

    def foo(x: int, y: str, z: bool) -> None:

      def __original_function(x: int, y: str, z: bool) -> None:
        print(x, y, z)

      def __inlined_with_logging(x: int, y: str, z: bool) -> None:
        __args = (y, z)
        __kwargs = {"y": y, "z": z}
        __original_function(x, y, z)
        print(x)
        print(__args, __kwargs)

      return __inlined_with_logging(x, y, z)
  |};
  (* Decorator used on a method. *)
  assert_inlined
    {|
    from typing import Callable, TypeVar
    from builtins import __test_sink

    T = TypeVar("T", bound="Foo")

    def with_logging(f: Callable) -> Callable:
      def helper(args) -> None:
        __test_sink(args)

      def inner( *args, **kwargs) -> None:
        helper(args)
        f( *args, **kwargs)

      return inner

    class Base: ...

    class Foo(Base):
      def bar(self, x: str) -> None:
        print(x)

      @with_logging
      def foo(self, x: str) -> None:
        self.bar(x)

      @with_logging
      def self_has_type(self: Base, x: str) -> None:
        self.bar(x)

      @with_logging
      def self_has_generic_type(self: T, other: T, x: str) -> None:
        self.bar(x)
        other.bar(x)
  |}
    {|
    from typing import Callable, TypeVar
    from builtins import __test_sink

    T = TypeVar("T", bound="Foo")

    def with_logging(f: Callable) -> Callable:
      def helper(args) -> None:
        __test_sink(args)

      def inner( *args, **kwargs) -> None:
        helper(args)
        f( *args, **kwargs)

      return inner

    class Base: ...

    class Foo(Base):
      def bar(self, x: str) -> None:
        print(x)

      def foo(self, x: str) -> None:
        def __original_function(self: Foo, x: str) -> None:
          self.bar(x)

        def __inlined_with_logging(self: Foo, x: str) -> None:

          def helper(args) -> None:
            __test_sink(args)

          __args = (self, x)
          __kwargs = {"self": self, "x": x}
          helper(__args)
          __original_function(self, x)

        return __inlined_with_logging(self, x)

      def self_has_type(self: Base, x: str) -> None:
        def __original_function(self: Base, x: str) -> None:
          self.bar(x)

        def __inlined_with_logging(self: Base, x: str) -> None:

          def helper(args) -> None:
            __test_sink(args)

          __args = (self, x)
          __kwargs = {"self": self, "x": x}
          helper(__args)
          __original_function(self, x)

        return __inlined_with_logging(self, x)

      def self_has_generic_type(self: T, other: T, x: str) -> None:
        def __original_function(self: T, other: T, x: str) -> None:
          self.bar(x)
          other.bar(x)

        def __inlined_with_logging(self: T, other: T, x: str) -> None:

          def helper(args) -> None:
            __test_sink(args)

          __args = (self, other, x)
          __kwargs = {"self": self, "other": other, "x": x}
          helper(__args)
          __original_function(self, other, x)

        return __inlined_with_logging(self, other, x)
  |};
  (* Decorator used on a classmethod. *)
  assert_inlined
    {|
    from typing import Callable
    from builtins import __test_sink

    def with_logging(f: Callable) -> Callable:

      def inner( *args, **kwargs) -> None:
        __test_sink(args)
        f( *args, **kwargs)

      return inner

    def fails_to_apply(f):
      return f

    class Foo:
      def some_method(self, x: int) -> None:
        print(self, x)

      @classmethod
      def some_class_method(cls, x: int) -> None:
        print(cls, x)

      @classmethod
      @with_logging
      @fails_to_apply
      def foo(cls, x: int) -> None:
        cls.some_class_method(x)
        cls().some_method(x)
  |}
    {|
    from typing import Callable
    from builtins import __test_sink

    def with_logging(f: Callable) -> Callable:

      def inner( *args, **kwargs) -> None:
        __test_sink(args)
        f( *args, **kwargs)

      return inner

    def fails_to_apply(f):
      return f

    class Foo:
      def some_method(self, x: int) -> None:
        print(self, x)

      @classmethod
      def some_class_method(cls, x: int) -> None:
        print(cls, x)

      @classmethod
      def foo(cls, x: int) -> None:

        def __original_function(cls: typing.Type[Foo], x: int) -> None:
          cls.some_class_method(x)
          cls().some_method(x)

        def __inlined_with_logging(cls: typing.Type[Foo], x: int) -> None:
          __args = (cls, x)
          __kwargs = {"cls": cls, "x": x}
          __test_sink(__args)
          __original_function(cls, x)

        return __inlined_with_logging(cls, x)
  |};
  (* TODO(T69755379): Correctly inline decorator used on a staticmethod. Right now, we're missing
     the @staticmethod decorator. *)
  assert_inlined
    {|
    from typing import Callable
    from builtins import __test_sink

    def with_logging(f: Callable) -> Callable:

      def inner( *args, **kwargs) -> None:
        __test_sink(args)
        f( *args, **kwargs)

      return inner

    class Foo:
      def some_method(self, x: int) -> None:
        print(self, x)

      @staticmethod
      @with_logging
      def foo(x: int) -> None:
        print(x)
  |}
    {|
    from typing import Callable
    from builtins import __test_sink

    def with_logging(f: Callable) -> Callable:

      def inner( *args, **kwargs) -> None:
        __test_sink(args)
        f( *args, **kwargs)

      return inner

    class Foo:
      def some_method(self, x: int) -> None:
        print(self, x)

      def foo(x: int) -> None:

        def __original_function(x: int) -> None:
          print(x)

        def __inlined_with_logging(x: int) -> None:
          __args = (x,)
          __kwargs = {"x": x}
          __test_sink(__args)
          __original_function(x)

        return __inlined_with_logging(x)
  |};
  (* Same decorator applied multiple times. *)
  assert_inlined
    {|
    from builtins import __test_sink
    from typing import Callable

    def with_logging(callable: Callable[[str], None]) -> Callable[[str], None]:

      def inner(y: str) -> None:
        __test_sink(y)
        callable(y)

      return inner

    def identity(f):
      def inner(y: str) -> None:
        f(y)

      return inner

    @with_logging
    @identity
    @with_logging
    def foo(z: str) -> None:
      print(z)
  |}
    {|
    from builtins import __test_sink
    from typing import Callable

    def with_logging(callable: Callable[[str], None]) -> Callable[[str], None]:

      def inner(y: str) -> None:
        __test_sink(y)
        callable(y)

      return inner


    def identity(f):
      def inner(y: str) -> None:
        f(y)

      return inner

    def foo(y: str) -> None:
      def __original_function(z: str) -> None:
        print(z)

      def __inlined_with_logging(y: str) -> None:
        __test_sink(y)
        __original_function(y)

      def __inlined_identity(y: str) -> None:
        __inlined_with_logging(y)

      def __inlined_with_logging2(y: str) -> None:
        __test_sink(y)
        __inlined_identity(y)

      return __inlined_with_logging2(y)
  |};
  (* Decorator that passes in a local variable to the original function.

     Note: This is a bit of a weird edge case because the `@wraps` says that the signature is the
     same as the original function, but in reality it takes in one less parameter. I'm reconciling
     this by keeping the original signature (for the sake of model-writing and typechecking) but
     only storing the remaining parameters in `__args` and `__kwargs`. *)
  assert_inlined
    {|
    from typing import Callable
    from builtins import __test_sink
    from functools import wraps

    def with_logging(f: Callable) -> Callable:

      @wraps(f)
      def inner(request: str, *args, **kwargs) -> None:
        __test_sink(args)
        x = 42
        f(request, x, *args, **kwargs)

      return inner

    @with_logging
    def foo(request: str, x: int, y: int) -> None:
      print(x)
  |}
    {|
    from typing import Callable
    from builtins import __test_sink
    from functools import wraps

    def with_logging(f: Callable) -> Callable:

      @wraps(f)
      def inner(request: str, *args, **kwargs) -> None:
        __test_sink(args)
        x = 42
        f(request, x, *args, **kwargs)

      return inner

    def foo(request: str, x: int, y: int) -> None:

      def __original_function(request: str, x: int, y: int) -> None:
        print(x)

      def __inlined_with_logging(request: str, x: int, y: int) -> None:
        __args = (y, )
        __kwargs = {"y": y}
        __test_sink(__args)

        # Need to explicitly qualify this local variable because `x` is also a parameter.
        $local_test?foo?__inlined_with_logging$x = 42
        __original_function(request, $local_test?foo?__inlined_with_logging$x, y)

      return __inlined_with_logging(request, x, y)
  |};
  (* Decorator that passes in a local variable but doesn't use @wraps. We fall back to having *args,
     **kwargs in the outer signature. *)
  assert_inlined
    {|
    from typing import Callable
    from builtins import __test_sink

    def with_logging(f: Callable) -> Callable:

      def inner(request: str, *args, **kwargs) -> None:
        __test_sink(args)
        x = 42
        f(request, x, *args, **kwargs)

      return inner

    @with_logging
    def foo(request: str, x: int, y: int) -> None:
      print(x)
  |}
    {|
    from typing import Callable
    from builtins import __test_sink

    def with_logging(f: Callable) -> Callable:

      def inner(request: str, *args, **kwargs) -> None:
        __test_sink(args)
        x = 42
        f(request, x, *args, **kwargs)

      return inner

    def foo(request: str, *args, **kwargs) -> None:

      def __original_function(request: str, x: int, y: int) -> None:
        print(x)

      def __inlined_with_logging(request: str, *args, **kwargs) -> None:
        __test_sink(args)
        x = 42
        __original_function(request, x, *args, **kwargs)

      return __inlined_with_logging(request, *args, **kwargs)
  |};
  (* Preserve the return type if the decorator uses @wraps. *)
  assert_inlined
    {|
    from builtins import __test_sink
    from typing import Any, Callable
    from functools import wraps

    def decorator_using_wraps(func: Callable) -> Any:
        @wraps(func)
        def wrap( *args: Any, **kwargs: Any) -> Any:
            return func( *args, **kwargs)

        return wrap

    @decorator_using_wraps
    def foo(x: str) -> str:
      return x
  |}
    {|
    from builtins import __test_sink
    from typing import Any, Callable
    from functools import wraps

    def decorator_using_wraps(func: Callable) -> Any:
        @wraps(func)
        def wrap( *args: Any, **kwargs: Any) -> Any:
            return func( *args, **kwargs)

        return wrap

    def foo(x: str) -> str:
      def __original_function(x: str) -> str:
        return x

      def __inlined_decorator_using_wraps(x: str) -> str:
        __args = (x,)
        __kwargs = {"x": x}
        return __original_function(x)

      return __inlined_decorator_using_wraps(x)
  |};
  assert_inlined
    {|
    from builtins import __test_sink
    from typing import Any, Callable
    from functools import wraps

    def decorator_using_wraps(callable: Callable) -> Callable:

      @wraps(callable)
      def inner(y: str) -> Any:
        return callable(y)

      return inner

    @decorator_using_wraps
    def foo(z: str) -> str:
      return z
  |}
    {|
    from builtins import __test_sink
    from typing import Any, Callable
    from functools import wraps

    def decorator_using_wraps(callable: Callable) -> Callable:

      @wraps(callable)
      def inner(y: str) -> Any:
        return callable(y)

      return inner

    def foo(y: str) -> str:
      def __original_function(z: str) -> str:
        return z

      def __inlined_decorator_using_wraps(y: str) -> str:
        return __original_function(y)

      return __inlined_decorator_using_wraps(y)
  |};
  assert_inlined
    {|
    from typing import Any, Callable, TypeVar
    from pyre_extensions import ParameterSpecification
    from functools import wraps

    P = ParameterSpecification("P")
    R = TypeVar("R")

    def decorator_using_wraps(callable: Callable[P, R]) -> Callable[P, R]:

      @wraps(callable)
      def inner( *args: P.args, **kwargs: P.kwargs) -> R:
        return callable( *args, **kwargs)

      return inner

    @decorator_using_wraps
    def foo(z: str) -> str:
      return z
  |}
    {|
    from typing import Any, Callable, TypeVar
    from pyre_extensions import ParameterSpecification
    from functools import wraps

    P = ParameterSpecification("P")
    R = TypeVar("R")

    def decorator_using_wraps(callable: Callable[P, R]) -> Callable[P, R]:

      @wraps(callable)
      def inner( *args: P.args, **kwargs: P.kwargs) -> R:
        return callable( *args, **kwargs)

      return inner

    def foo(z: str) -> str:
      def __original_function(z: str) -> str:
        return z

      def __inlined_decorator_using_wraps(z: str) -> str:
        __args = (z, )
        __kwargs = {"z": z}
        return __original_function(z)

      return __inlined_decorator_using_wraps(z)
  |};
  (* Don't preserve the return type if the decorator doesn't use @wraps. *)
  assert_inlined
    {|
    from builtins import __test_sink
    from typing import Any, Callable

    def decorator_not_using_wraps(func: Callable) -> Any:
        def wrap( *args: Any, **kwargs: Any) -> int:
            func( *args, **kwargs)
            return 1

        return wrap

    @decorator_not_using_wraps
    def foo(x: str) -> str:
      return x
  |}
    {|
    from builtins import __test_sink
    from typing import Any, Callable

    def decorator_not_using_wraps(func: Callable) -> Any:
        def wrap( *args: Any, **kwargs: Any) -> int:
            func( *args, **kwargs)
            return 1

        return wrap

    def foo(x: str) -> int:
      def __original_function(x: str) -> str:
        return x

      def __inlined_decorator_not_using_wraps(x: str) -> int:
        __args = (x,)
        __kwargs = {"x": x}
        __original_function(x)
        return 1

      return __inlined_decorator_not_using_wraps(x)
  |};
  ()


let test_decorator_location context =
  let assert_inlined
      ?(additional_sources = [])
      ?(handle = "test.py")
      ~expected_function_module_pairs
      source
      expected
    =
    let source, environment = setup ~additional_sources ~context ~handle source in
    let decorator_bodies = DecoratorHelper.all_decorator_bodies environment in
    let actual = DecoratorHelper.inline_decorators ~decorator_bodies source in
    (* Using the same setup code instead of `parse` because the SourcePath `priority` is different
       otherwise. *)
    let expected =
      setup ~additional_sources ~context ~handle expected
      |> fst
      |> DecoratorHelper.sanitize_defines ~strip_decorators:false
    in
    assert_source_equal ~location_insensitive:true expected actual;
    List.iter expected_function_module_pairs ~f:(fun (function_reference, decorator_module) ->
        assert_equal
          ~printer:(fun module_reference ->
            Format.asprintf
              "function: %s\tmodule: %s"
              ([%show: Reference.t] function_reference)
              ([%show: Reference.t option] module_reference))
          ~cmp:[%equal: Reference.t option]
          decorator_module
          (DecoratorHelper.DecoratorModule.get function_reference))
  in
  let additional_sources =
    [
      ( "logging_decorator.py",
        {|
            from typing import Callable
            def with_logging(callable: Callable[[str], None]) -> Callable[[str], None]:
              def helper(y: str) -> None:
                print(y)

              def inner(y: str) -> None:
                __test_sink(y)
                callable(y)
                helper(y)

              return inner

            def fails_to_apply(f):
              return f
      |}
      );
      ( "some_module/identity_decorator.py",
        {|
            from typing import Callable
            def identity(callable: Callable[[str], None]) -> Callable[[str], None]:
              def inner(y: str) -> None:
                callable(y)

              return inner
      |}
      );
    ]
  in
  Memory.reset_shared_memory ();
  assert_inlined
    ~additional_sources
    {|
    from builtins import __test_sink
    from typing import Callable
    from logging_decorator import with_logging, fails_to_apply
    from some_module.identity_decorator import identity

    def same_module_decorator(callable: Callable[[str], None]) -> Callable[[str], None]:
      def inner(y: str) -> None:
        callable(y)

      return inner

    @with_logging
    def foo(z: str) -> None:
      print(z)

    @with_logging
    @fails_to_apply
    @identity
    def bar(z: str) -> None:
      print(z)

    @same_module_decorator
    def baz(z: str) -> None:
      print(z)

    @identity
    @identity
    def same_decorator_twice(z: str) -> None:
      print(z)
  |}
    ~expected_function_module_pairs:
      [
        !&"test.baz.__inlined_same_module_decorator", Some !&"test";
        !&"test.baz.__original_function", None;
        !&"test.baz", None;
        !&"test.bar.__inlined_with_logging", Some !&"logging_decorator";
        !&"test.bar.__inlined_with_logging.helper", Some !&"logging_decorator";
        !&"test.bar", None;
        !&"test.bar.__original_function", None;
        !&"test.bar.__inlined_identity", Some !&"some_module.identity_decorator";
        !&"test.foo.__inlined_with_logging", Some !&"logging_decorator";
        !&"test.foo.__inlined_with_logging.helper", Some !&"logging_decorator";
        !&"test.foo", None;
        !&"test.foo.__original_function", None;
        !&"test.same_decorator_twice.__inlined_identity", Some !&"some_module.identity_decorator";
        !&"test.same_decorator_twice.__inlined_identity2", Some !&"some_module.identity_decorator";
        !&"test.same_decorator_twice.__original_function", None;
        !&"test.same_decorator_twice", None;
      ]
    {|
    from builtins import __test_sink
    from typing import Callable
    from logging_decorator import with_logging, fails_to_apply
    from some_module.identity_decorator import identity

    def same_module_decorator(callable: Callable[[str], None]) -> Callable[[str], None]:
      def inner(y: str) -> None:
        callable(y)

      return inner

    def foo(y: str) -> None:
      def __original_function(z: str) -> None:
        print(z)

      def __inlined_with_logging(y: str) -> None:

        def helper(y: str) -> None:
          print(y)

        __test_sink(y)
        __original_function(y)
        helper(y)

      return __inlined_with_logging(y)

    def bar(y: str) -> None:

      def __original_function(z: str) -> None:
        print(z)

      def __inlined_identity(y: str) -> None:
        __original_function(y)

      def __inlined_with_logging(y: str) -> None:

        def helper(y: str) -> None:
          print(y)

        __test_sink(y)
        __inlined_identity(y)
        helper(y)

      return __inlined_with_logging(y)


    def baz(y: str) -> None:
      def __original_function(z: str) -> None:
        print(z)

      def __inlined_same_module_decorator(y: str) -> None:
        __original_function(y)

      return __inlined_same_module_decorator(y)


    def same_decorator_twice(y: str) -> None:

      def __original_function(z: str) -> None:
        print(z)

      def __inlined_identity(y: str) -> None:
        __original_function(y)

      def __inlined_identity2(y: str) -> None:
        __inlined_identity(y)

      return __inlined_identity2(y)
  |};
  ()


let test_requalify_name _ =
  let open Expression in
  let assert_requalified ~old_qualifier ~new_qualifier name expected =
    assert_equal
      ~cmp:[%equal: Name.t]
      ~printer:[%show: Name.t]
      expected
      (DecoratorHelper.requalify_name ~old_qualifier ~new_qualifier name)
  in
  assert_requalified
    ~old_qualifier:!&"test.foo"
    ~new_qualifier:!&"test.foo.bar"
    (Name.Identifier "$local_test?foo$hello")
    (Name.Identifier "$local_test?foo?bar$hello");
  assert_requalified
    ~old_qualifier:!&"test.foo"
    ~new_qualifier:!&"test.foo.bar"
    (Name.Identifier "$local_test$hello")
    (Name.Identifier "$local_test$hello");
  assert_requalified
    ~old_qualifier:!&"test.foo"
    ~new_qualifier:!&"test.foo.bar"
    (Name.Identifier "hello")
    (Name.Identifier "hello");
  ()


let test_replace_signature _ =
  let assert_signature_replaced ~new_signature given expected =
    match
      ( (expected >>| fun expected -> parse expected |> Source.statements),
        parse given |> Source.statements,
        parse new_signature |> Source.statements )
    with
    | ( expected,
        [{ Node.value = Define given; _ }],
        [
          {
            Node.value =
              Define
                { signature = { name = { Node.value = callee_name; _ }; _ } as new_signature; _ };
            _;
          };
        ] ) ->
        let actual =
          DecoratorHelper.replace_signature_if_always_passing_on_arguments
            ~callee_name:(Reference.show callee_name)
            ~new_signature
            given
        in
        let expected =
          match expected with
          | Some [{ Node.value = Define expected; _ }] -> Some expected
          | _ -> None
        in
        let printer = [%show: Statement.statement option] in
        let equal_optional_statements left right =
          match left, right with
          | Some left, Some right ->
              Statement.location_insensitive_compare
                (Node.create_with_default_location left)
                (Node.create_with_default_location right)
              = 0
          | None, None -> true
          | _ -> false
        in
        assert_equal
          ~cmp:equal_optional_statements
          ~printer
          ~pp_diff:(diff ~print:(fun format x -> Format.fprintf format "%s" (printer x)))
          (expected >>| fun x -> Statement.Statement.Define x)
          (actual >>| fun x -> Statement.Statement.Define x)
    | _ -> failwith "expected one statement each"
  in
  assert_signature_replaced
    ~new_signature:"def foo(y: str, z: int = 7) -> None: ..."
    {|
      def wrapper( *args, **kwargs) -> None:
        foo( *args, **kwargs)
        bar(args)
        baz(kwargs)
  |}
    (Some
       {|
      def wrapper(y: str, z: int = 7) -> None:
        __args = (y, z)
        __kwargs = {"y": y, "z": z}
        foo(y, z)
        bar(__args)
        baz(__kwargs)
  |});
  assert_signature_replaced
    ~new_signature:"def foo(y: str) -> None: ..."
    {|
      def wrapper( *args, **kwargs) -> None:
        foo("extra argument", *args, **kwargs)
        bar(args)
  |}
    None;
  (* Give up if the wrapper has anything more complex than `*args, **kwargs`. *)
  assert_signature_replaced
    ~new_signature:"def foo(y: str) -> None: ..."
    {|
      def wrapper(extra: int, *args, **kwargs) -> None:
        foo( *args, **kwargs)
  |}
    None;
  (* The wrapper still has `*args, **kwargs` but also gives them a type annotation. If it always
     passes on both to the callee, we use the callee's signature. *)
  assert_signature_replaced
    ~new_signature:"def foo(y: str) -> None: ..."
    {|
      def wrapper( *args: int, **kwargs: str) -> None:
        foo( *args, **kwargs)
        bar(args)
  |}
    (Some
       {|
      def wrapper(y: str) -> None:
        __args = (y,)
        __kwargs = {"y": y}
        foo(y)
        bar(__args)
  |});
  (* If the callee expects `args` and `kwargs`, make sure not to confuse them with our synthetic
     locals `__args` and `__kwargs`.

     Note that we conservatively store all the arguments to both `__args` and `__kwargs` so that we
     catch any flows to sinks within the decorator. We are more precise about what we pass to the
     callee since false positives there might be more annoying. *)
  assert_signature_replaced
    ~new_signature:"def foo(y: str, *args: int, **kwargs: str) -> None: ..."
    {|
      def wrapper( *args, **kwargs) -> None:
        foo( *args, **kwargs)
        baz(kwargs)
  |}
    (Some
       {|
      def wrapper(y: str, *args: int, **kwargs: str) -> None:
        __args = (y, *args)
        __kwargs = {"y": y, **kwargs}
        foo(y, *args, **kwargs)
        baz(__kwargs)
  |});
  (* Ensure that the return type is unchanged regardless of the new signature. *)
  assert_signature_replaced
    ~new_signature:"def foo(y: str) -> None: ..."
    {|
      def wrapper( *args, **kwargs) -> int:
        foo( *args, **kwargs)
        return 1
  |}
    (Some
       {|
      def wrapper(y: str) -> int:
        __args = (y,)
        __kwargs = {"y": y}
        foo(y)
        return 1
  |});
  assert_signature_replaced
    ~new_signature:"def foo(some_parameter: int, x: str, y: bool) -> None: ..."
    {|
      def wrapper(some_parameter: int, *args: object, **kwargs: object) -> int:
        foo(some_parameter, *args, **kwargs)
        print(some_parameter)
        print(args, kwargs)
  |}
    (Some
       {|
      def wrapper(some_parameter: int, x: str, y: bool) -> int:
        __args = (x, y)
        __kwargs = {"x": x, "y": y}
        foo(some_parameter, x, y)
        print(some_parameter)
        print(__args, __kwargs)
  |});
  assert_signature_replaced
    ~new_signature:"def foo(prefix1: int, prefix2: str, x: str, y: bool) -> None: ..."
    {|
      def wrapper(some_parameter1: int, some_parameter2: str, *args: object, **kwargs: object) -> int:
        foo(some_parameter1, some_parameter2, *args, **kwargs)
        print(some_parameter1, some_parameter2)
        print(args, kwargs)
  |}
    (Some
       {|
      def wrapper(prefix1: int, prefix2: str, x: str, y: bool) -> int:
        __args = (x, y)
        __kwargs = {"x": x, "y": y}
        foo(prefix1, prefix2, x, y)
        print(prefix1, prefix2)
        print(__args, __kwargs)
  |});
  (* Don't get confused if the original function uses `x` as a parameter name. *)
  assert_signature_replaced
    ~new_signature:"def foo(some_parameter: int, x: str, y: bool) -> None: ..."
    {|
      def wrapper(x: int, *args: object, **kwargs: object) -> int:
        foo(x, *args, **kwargs)
        print(x)
        print(args, kwargs)
  |}
    (Some
       {|
      def wrapper(some_parameter: int, x: str, y: bool) -> int:
        __args = (x, y)
        __kwargs = {"x": x, "y": y}
        foo(some_parameter, x, y)
        print(some_parameter)
        print(__args, __kwargs)
  |});
  assert_signature_replaced
    ~new_signature:"def foo(some_parameter: int, x: str, y: bool) -> None: ..."
    {|
      def wrapper(x: int, *args: object, **kwargs: object) -> int:
        foo(x, "extra argument", *args, **kwargs)
        print(x)
        print(args, kwargs)
  |}
    None;
  assert_signature_replaced
    ~new_signature:"def foo(some_parameter: int, x: str, y: bool) -> None: ..."
    {|
      def wrapper(x: int, *args: object, **kwargs: object) -> int:
        foo("not x", *args, **kwargs)
        print(x)
        print(args, kwargs)
  |}
    None;
  assert_signature_replaced
    ~new_signature:"def foo() -> None: ..."
    {|
      def wrapper(x: int, *args: object, **kwargs: object) -> int:
        foo(x, *args, **kwargs)
        print(x)
        print(args, kwargs)
  |}
    None;
  assert_signature_replaced
    ~new_signature:"def foo(some_parameter: int, *args: int, **kwargs: str) -> None: ..."
    {|
      def wrapper(some_parameter: int, *args: object, **kwargs: object) -> int:
        foo(some_parameter, *args, **kwargs)
        print(some_parameter)
        print(args, kwargs)
  |}
    (Some
       {|
      def wrapper(some_parameter: int, *args: int, **kwargs: str) -> int:
        __args = ( *args,)
        __kwargs = { **kwargs}
        foo(some_parameter, *args, **kwargs)
        print(some_parameter)
        print(__args, __kwargs)
  |});
  ()


let test_rename_local_variables _ =
  let assert_renamed ~pairs given expected =
    match parse expected |> Source.statements, parse given |> Source.statements with
    | [{ Node.value = Define expected; _ }], [{ Node.value = Define given; _ }] ->
        let actual = DecoratorHelper.rename_local_variables ~pairs given in
        let printer = [%show: Statement.statement] in
        assert_equal
          ~cmp:(fun left right ->
            Statement.location_insensitive_compare
              (Node.create_with_default_location left)
              (Node.create_with_default_location right)
            = 0)
          ~printer
          ~pp_diff:(diff ~print:(fun format x -> Format.fprintf format "%s" (printer x)))
          (Statement.Statement.Define expected)
          (Statement.Statement.Define actual)
    | _ -> failwith "expected one statement each"
  in
  assert_renamed
    ~pairs:["y", "y_renamed"; "z", "z_renamed"; "not_found", "not_found_renamed"]
    {|
    def wrapper(y: str, z: int) -> None:
      x = y + z
      foo(y, z)
  |}
    {|
    def wrapper(y: str, z: int) -> None:
      x = y_renamed + z_renamed
      foo(y_renamed, z_renamed)
  |};
  assert_renamed
    ~pairs:["y", "y_renamed"; "y", "y_duplicate"]
    {|
    def wrapper(y: str, z: int) -> None:
      x = y + z
      foo(y, z)
  |}
    {|
    def wrapper(y: str, z: int) -> None:
      x = y + z
      foo(y, z)
  |};
  ()


let test_uniquify_names _ =
  let assert_uniquified given expected =
    assert_equal
      ~cmp:[%equal: Reference.t list]
      ~printer:[%show: Reference.t list]
      (List.map expected ~f:Reference.create)
      ( List.map given ~f:Reference.create
      |> DecoratorHelper.uniquify_names ~get_reference:Fn.id ~set_reference:(fun reference _ ->
             reference) )
  in
  assert_uniquified
    ["a.b"; "a.c"; "a.b"; "a.b"; "a.c"; "foo"]
    ["a.b3"; "a.c2"; "a.b2"; "a.b"; "a.c"; "foo"];
  assert_uniquified [] [];
  ()


let () =
  "decoratorHelper"
  >::: [
         "all_decorators" >:: test_all_decorators;
         "inline_decorators" >:: test_inline_decorators;
         "decorator_location" >:: test_decorator_location;
         "requalify_name" >:: test_requalify_name;
         "replace_signature" >:: test_replace_signature;
         "rename_local_variables" >:: test_rename_local_variables;
         "uniquify_names" >:: test_uniquify_names;
       ]
  |> Test.run
