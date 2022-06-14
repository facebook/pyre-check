(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Core
open Pyre
open Analysis
open Ast
open Test

let test_decorators_to_skip _ =
  let assert_decorators_to_skip source expected =
    assert_equal
      ~cmp:[%equal: Reference.t list]
      ~printer:[%show: Reference.t list]
      expected
      (trim_extra_indentation source
      |> InlineDecorator.decorators_to_skip ~path:(PyrePath.create_absolute "/root/test.py")
      |> List.sort ~compare:[%compare: Reference.t])
  in
  assert_decorators_to_skip
    {|
    @SkipDecoratorWhenInlining
    def foo.skip_this_decorator(f): ...

    @SkipObscure
    @SkipDecoratorWhenInlining
    @SkipOverrides
    def bar.skip_this_decorator2(f): ...

    @SkipObscure
    @SkipOverrides
    def bar.dont_skip(self: TaintInTaintOut[LocalReturn]): ...

    @Sanitize
    def bar.dont_skip2(self: TaintInTaintOut[LocalReturn]): ...

    def baz.dont_skip3(): ...
  |}
    [!&"bar.skip_this_decorator2"; !&"foo.skip_this_decorator"];
  assert_decorators_to_skip {|
    @CouldNotParse
  |} [];
  ()


let test_decorator_body context =
  Memory.reset_shared_memory ();
  let assert_decorator_body
      ?(should_skip_decorator = fun _ -> false)
      ~expected_handle
      decorator_reference
      expected
    =
    let additional_sources =
      [
        ( "file1.py",
          {|
            from typing import Callable
            def decorator1(callable: Callable[[str], None]) -> Callable[[str], None]:

              def inner(x: str) -> None:
                return None

              return inner

            def no_body(callable: Callable[[str], None]) -> Callable[[str], None]: ...

            def no_inner_function(callable: Callable[[str], None]) -> Callable[[str], None]:
              return callable
      |}
        );
        ( "some_module/file2.py",
          {|
            from typing import Callable

            def decorator2(callable: Callable[[str], None]) -> Callable[[str], None]:
              def inner(x: str) -> None:
                return None

              return inner
      |}
        );
      ]
    in
    let handle = "test.py" in
    let ast_environment =
      ScratchProject.setup ~context ~external_sources:additional_sources [handle, ""]
      |> ScratchProject.build_ast_environment
    in
    let get_source = AstEnvironment.ReadOnly.get_processed_source ast_environment in
    let get_expected_define expected =
      let { ScratchProject.BuiltTypeEnvironment.sources; _ } =
        ScratchProject.setup ~context ~external_sources:[] [expected_handle, expected]
        |> ScratchProject.build_type_environment
      in
      List.find_exn sources ~f:(fun { Source.module_path = { ModulePath.relative; _ }; _ } ->
          String.equal relative expected_handle)
      |> InlineDecorator.sanitize_defines ~strip_decorators:true
      |> Source.statements
      |> List.last_exn
      |> function
      | { Node.value = Define define; _ } -> define
      | _ -> failwith "expected define as the last statement"
    in
    let open Statement in
    assert_equal
      ~cmp:(Option.equal (fun left right -> Define.location_insensitive_compare left right = 0))
      ~printer:[%show: Define.t option]
      (expected >>| get_expected_define)
      (InlineDecorator.decorator_body ~should_skip_decorator ~get_source decorator_reference)
  in
  assert_decorator_body
    ~expected_handle:"file1.py"
    !&"file1.decorator1"
    (Some
       {|
      from typing import Callable

      def decorator1(callable: Callable[[str], None]) -> Callable[[str], None]:

        def inner(x: str) -> None:
          return None

        return inner
  |});
  assert_decorator_body
    ~expected_handle:"file1.py"
    ~should_skip_decorator:(Reference.equal !&"file1.decorator1")
    !&"file1.decorator1"
    None;
  assert_decorator_body
    ~expected_handle:"some_module/file2.py"
    !&"some_module.file2.decorator2"
    (Some
       {|
      from typing import Callable

      def decorator2(callable: Callable[[str], None]) -> Callable[[str], None]:

        def inner(x: str) -> None:
          return None

        return inner
  |});
  assert_decorator_body ~expected_handle:"file1.py" !&"file1.no_body" None;
  assert_decorator_body ~expected_handle:"file1.py" !&"file1.no_inner_function" None;
  ()


let get_expected_actual_sources ~context ~additional_sources ~handle source expected =
  Memory.reset_shared_memory ();
  InlineDecorator.set_should_inline_decorators true;
  let ast_environment =
    ScratchProject.setup ~context ~external_sources:additional_sources [handle, source]
    |> ScratchProject.build_ast_environment
  in
  let get_source = AstEnvironment.ReadOnly.get_processed_source ast_environment in
  let actual =
    get_source !&"test"
    >>| (fun source -> InlineDecorator.inline_decorators ~get_source source)
    >>| InlineDecorator.sanitize_defines ~strip_decorators:false
    |> fun optional -> Option.value_exn optional
  in
  let expected =
    let { ScratchProject.BuiltTypeEnvironment.sources; _ } =
      ScratchProject.setup ~context ~external_sources:[] [handle, expected]
      |> ScratchProject.build_type_environment
    in
    List.find_exn sources ~f:(fun { Source.module_path = { ModulePath.relative; _ }; _ } ->
        String.equal relative handle)
    |> InlineDecorator.sanitize_defines ~strip_decorators:false
  in
  expected, actual


let test_inline_decorators context =
  let assert_inlined ?(additional_sources = []) ?(handle = "test.py") source expected =
    let expected, actual =
      get_expected_actual_sources ~context ~additional_sources ~handle source expected
    in
    assert_source_equal ~location_insensitive:true expected actual
  in
  assert_inlined
    {|
    from builtins import _test_sink
    from typing import Callable

    def with_logging(callable: Callable[[str], None]) -> Callable[[str], None]:

      def inner(y: str) -> None:
        _test_sink(y)
        callable(y)

      return inner

    @with_logging
    def foo(z: str) -> None:
      print(z)
  |}
    {|
    from builtins import _test_sink
    from typing import Callable

    def with_logging(callable: Callable[[str], None]) -> Callable[[str], None]:

      def inner(y: str) -> None:
        _test_sink(y)
        callable(y)

      return inner

    def foo(y: str) -> None:
      def _original_function(z: str) -> None:
        print(z)

      def _inlined_with_logging(y: str) -> None:
        _test_sink(y)
        _original_function(y)

      return _inlined_with_logging(y)
  |};
  (* Leave decorators as such if none can be inlined. *)
  assert_inlined
    {|
    from builtins import _test_sink, _test_source
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
    from builtins import _test_sink, _test_source
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
    from builtins import _test_sink, _test_source
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
    from builtins import _test_sink, _test_source
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
    from builtins import _test_sink

    def with_logging(f: Callable) -> Callable:

      def inner( *args, **kwargs) -> None:
        _test_sink(args)
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
    from builtins import _test_sink

    def with_logging(f: Callable) -> Callable:

      def inner( *args, **kwargs) -> None:
        _test_sink(args)
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
    from builtins import _test_sink

    def no_calls_to_original_function(f: Callable) -> Callable:

      def inner( *args, **kwargs) -> None:
        _test_sink(args)

      return inner

    @no_calls_to_original_function
    def foo(x: str) -> None:
      print(x)
  |}
    {|
    from typing import Callable
    from builtins import _test_sink

    def no_calls_to_original_function(f: Callable) -> Callable:

      def inner( *args, **kwargs) -> None:
        _test_sink(args)

      return inner

    def foo( *args, **kwargs) -> None:

      def _original_function(x: str) -> None:
        print(x)

      def _inlined_no_calls_to_original_function( *args, **kwargs) -> None:
        _test_sink(args)

      return _inlined_no_calls_to_original_function( *args, **kwargs)
  |};
  assert_inlined
    {|
    from builtins import _test_sink
    from typing import Callable

    def with_logging(callable: Callable[[str], None]) -> Callable[[str], None]:

      def inner(y: str) -> None:
        _test_sink(y)
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
    from builtins import _test_sink
    from typing import Callable

    def with_logging(callable: Callable[[str], None]) -> Callable[[str], None]:

      def inner(y: str) -> None:
        _test_sink(y)
        result = callable(y)
        return result

      return inner

    def foo(y: str) -> None:
      def _original_function(z: str) -> None:
        print(z)
        result = None
        return result

      def _inlined_with_logging(y: str) -> None:
        _test_sink(y)
        result = _original_function(y)
        return result

      return _inlined_with_logging(y)
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

      async def _original_function(x: str) -> int:
        print(x)

      async def _inlined_with_logging_async(y: str) -> int:
        try:
          result = await _original_function(y)
          return result
        except Exception:
          return 42

      return await _inlined_with_logging_async(y)
  |};
  (* Decorator that types the function parameter as `f: Callable`. *)
  assert_inlined
    {|
    from typing import Callable
    from builtins import _test_sink

    def with_logging(f: Callable) -> Callable:

      def inner(y: str) -> int:
        _test_sink(y)
        f(y)

      return inner

    @with_logging
    def foo(x: str) -> int:
      print(x)
  |}
    {|
    from typing import Callable
    from builtins import _test_sink

    def with_logging(f: Callable) -> Callable:

      def inner(y: str) -> int:
        _test_sink(y)
        f(y)

      return inner

    def foo(y: str) -> int:

      def _original_function(x: str) -> int:
        print(x)

      def _inlined_with_logging(y: str) -> int:
        _test_sink(y)
        _original_function(y)

      return _inlined_with_logging(y)
  |};
  (* Wrapper function with default values for parameters. *)
  assert_inlined
    {|
    from builtins import _test_sink
    from typing import Callable

    def with_logging(callable: Callable[[str], None]) -> Callable[[str], None]:

      def inner(y: str, z: int = 4) -> None:
        _test_sink(y)
        callable(y + z)

      return inner

    @with_logging
    def foo(z: str) -> None:
      print(z)
  |}
    {|
    from builtins import _test_sink
    from typing import Callable

    def with_logging(callable: Callable[[str], None]) -> Callable[[str], None]:

      def inner(y: str, z: int = 4) -> None:
        _test_sink(y)
        callable(y + z)

      return inner

    def foo(y: str, z: int = 4) -> None:
      def _original_function(z: str) -> None:
        print(z)

      def _inlined_with_logging(y: str, z: int = 4) -> None:
        _test_sink(y)
        _original_function(y + z)

      return _inlined_with_logging(y, z)
  |};
  (* Wrapper function with `*args` and `**kwargs`. *)
  assert_inlined
    {|
    from typing import Callable
    from builtins import _test_sink

    def with_logging(f: Callable) -> Callable:

      def inner( *args, **kwargs) -> None:
        _test_sink(args)
        f( *args, **kwargs)

      return inner

    @with_logging
    def foo(x: str) -> None:
      print(x)
  |}
    {|
    from typing import Callable
    from builtins import _test_sink

    def with_logging(f: Callable) -> Callable:

      def inner( *args, **kwargs) -> None:
        _test_sink(args)
        f( *args, **kwargs)

      return inner

    def foo(x: str) -> None:

      def _original_function(x: str) -> None:
        print(x)

      def _inlined_with_logging(x: str) -> None:
        _args = (x,)
        _kwargs = {"x": x}
        _test_sink(_args)
        _original_function(x)

      return _inlined_with_logging(x)
  |};
  (* ParamSpec. *)
  assert_inlined
    {|
    from typing import Callable
    from pyre_extensions import ParameterSpecification
    from builtins import _test_sink

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
    from builtins import _test_sink

    P = ParameterSpecification("P")

    def with_logging(f: Callable[P, None]) -> Callable[P, None]:

      def inner( *args: P.args, **kwargs: P.kwargs) -> None:
        f( *args, **kwargs)

      return inner

    def foo(x: str, y: int) -> None:

      def _original_function(x: str, y: int) -> None:
        print(x, y)

      def _inlined_with_logging(x: str, y: int) -> None:
        _args = (x, y)
        _kwargs = {"x": x, "y": y}
        _original_function(x, y)

      return _inlined_with_logging(x, y)
  |};
  assert_inlined
    {|
    from typing import Callable
    from builtins import _test_sink

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
    from builtins import _test_sink

    def change_return_type(f: Callable) -> Callable:

      def inner( *args, **kwargs) -> int:
        f( *args, **kwargs)
        return 1

      return inner

    def foo(x: str) -> int:

      def _original_function(x: str) -> None:
        print(x)

      def _inlined_change_return_type(x: str) -> int:
        _args = (x,)
        _kwargs = {"x": x}
        _original_function(x)
        return 1

      return _inlined_change_return_type(x)
  |};
  (* Multiple decorators. *)
  assert_inlined
    {|
    from builtins import _test_sink, _test_source
    from typing import Callable

    def with_logging_sink(callable: Callable[[str], None]) -> Callable[[str], None]:

      def inner(y: str) -> None:
        _test_sink(y)
        callable(y)

      return inner

    def with_logging_source(callable: Callable[[str], None]) -> Callable[[str], None]:

      def inner(y: str) -> None:
        callable(y + _test_source())

      return inner

    @with_logging_source
    @with_logging_sink
    def foo(z: str) -> None:
      print(z)
  |}
    {|
    from builtins import _test_sink, _test_source
    from typing import Callable

    def with_logging_sink(callable: Callable[[str], None]) -> Callable[[str], None]:

      def inner(y: str) -> None:
        _test_sink(y)
        callable(y)

      return inner

    def with_logging_source(callable: Callable[[str], None]) -> Callable[[str], None]:

      def inner(y: str) -> None:
        callable(y + _test_source())

      return inner

    def foo(y: str) -> None:

      def _original_function(z: str) -> None:
        print(z)

      def _inlined_with_logging_sink(y: str) -> None:
        _test_sink(y)
        _original_function(y)

      def _inlined_with_logging_source(y: str) -> None:
        _inlined_with_logging_sink(y + _test_source())

      return _inlined_with_logging_source(y)
  |};
  (* Multiple decorators where one decorator fails to apply. *)
  assert_inlined
    {|
    from builtins import _test_sink, _test_source
    from typing import Callable

    def with_logging_sink(callable: Callable[[str], None]) -> Callable[[str], None]:

      def inner(y: str) -> None:
        _test_sink(y)
        callable(y)

      return inner

    def with_logging_source(callable: Callable[[str], None]) -> Callable[[str], None]:

      def inner(y: str) -> None:
        callable(y + _test_source())

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
    from builtins import _test_sink, _test_source
    from typing import Callable

    def with_logging_sink(callable: Callable[[str], None]) -> Callable[[str], None]:

      def inner(y: str) -> None:
        _test_sink(y)
        callable(y)

      return inner

    def with_logging_source(callable: Callable[[str], None]) -> Callable[[str], None]:

      def inner(y: str) -> None:
        callable(y + _test_source())

      return inner

    def fails_to_apply(f):
      return f

    def foo(y: str) -> None:

      def _original_function(z: str) -> None:
        print(z)

      def _inlined_with_logging_sink(y: str) -> None:
        _test_sink(y)
        _original_function(y)

      def _inlined_with_logging_source(y: str) -> None:
        _inlined_with_logging_sink(y + _test_source())

      return _inlined_with_logging_source(y)
  |};
  (* Decorator factory.

     Note: Commenting out `logger_name` because the new CPython parser cannot handle
     `$parameter$logger_name`, which is invalid Python syntax. Leaving it here for historical
     reasons. *)
  assert_inlined
    {|
    from typing import Callable
    from builtins import _test_sink

    def with_named_logger(logger_name: str) -> Callable[[Callable], Callable]:

      def _inner_decorator(f: Callable) -> Callable:

        def inner( *args: object, **kwargs: object) -> None:
          # print(logger_name)
          _test_sink(args)
          f( *args, **kwargs)

        return inner

      return _inner_decorator

    @with_named_logger("foo_logger")
    def foo(x: str) -> None:
      print(x)
  |}
    {|
    from typing import Callable
    from builtins import _test_sink

    def with_named_logger(logger_name: str) -> Callable[[Callable], Callable]:

      def _inner_decorator(f: Callable) -> Callable:

        def inner( *args: object, **kwargs: object) -> None:
          # print(logger_name)
          _test_sink(args)
          f( *args, **kwargs)

        return inner

      return _inner_decorator

    def foo(x: str) -> None:
      def _original_function(x: str) -> None:
        print(x)

      def _inlined_with_named_logger(x: str) -> None:
        _args = (x, )
        _kwargs = {"x": x}

        # print(logger_name)
        _test_sink(_args)
        _original_function(x)

      return _inlined_with_named_logger(x)
  |};
  (* Decorator that uses helper functions. *)
  assert_inlined
    {|
    from builtins import _test_sink
    from typing import Callable

    def with_logging(callable: Callable[[str], None]) -> Callable[[str], None]:

      def inner(y: str) -> None:
        _test_sink(y)
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
    from builtins import _test_sink
    from typing import Callable

    def with_logging(callable: Callable[[str], None]) -> Callable[[str], None]:

      def inner(y: str) -> None:
        _test_sink(y)
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
      def _original_function(z: str) -> None:
        print(z)

      def _inlined_with_logging(y: str) -> None:

        def my_print(y: str) -> None:
          print("before", y)

        def before(y: str) -> None:
          message = "before"
          my_print(message, y)

        def after(y: str) -> None:
          message = "after"
          my_print(message, y)

        _test_sink(y)
        before(y)
        _original_function(y)
        after(y)

      return _inlined_with_logging(y)
  |};
  (* Decorator factory with helper functions. *)
  assert_inlined
    {|
    from builtins import _test_sink
    from typing import Callable

    def with_named_logger(logger_name: str) -> Callable[[Callable], Callable]:
      def with_logging(callable: Callable[[str], None]) -> Callable[[str], None]:

        def inner(y: str) -> None:
          _test_sink(y)
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
    from builtins import _test_sink
    from typing import Callable

    def with_named_logger(logger_name: str) -> Callable[[Callable], Callable]:
      def with_logging(callable: Callable[[str], None]) -> Callable[[str], None]:

        def inner(y: str) -> None:
          _test_sink(y)
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
      def _original_function(z: str) -> None:
        print(z)

      def _inlined_with_named_logger(y: str) -> None:

        def my_print(y: str) -> None:
          print("before", y)

        def before(y: str) -> None:
          message = "before"
          my_print(message, y)

        def after(y: str) -> None:
          message = "after"
          _original_function(y)
          my_print(message, y)

        _test_sink(y)
        before(y)
        _original_function(y)
        after(y)

      return _inlined_with_named_logger(y)
  |};
  assert_inlined
    {|
    from typing import Callable
    from pyre_extensions import ParameterSpecification
    from pyre_extensions.type_variable_operators import Concatenate

    from builtins import _test_sink

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

    from builtins import _test_sink

    P = ParameterSpecification("P")

    def with_logging(f: Callable[Concatenate[int, P], None]) -> Callable[Concatenate[int, P], None]:

      def inner(first_parameter: int, *args: P.args, **kwargs: P.kwargs) -> None:
        f(first_parameter, *args, **kwargs)
        print(first_parameter)
        print(args, kwargs)

      return inner

    def foo(x: int, y: str, z: bool) -> None:

      def _original_function(x: int, y: str, z: bool) -> None:
        print(x, y, z)

      def _inlined_with_logging(x: int, y: str, z: bool) -> None:
        _args = (y, z)
        _kwargs = {"y": y, "z": z}
        _original_function(x, y, z)
        print(x)
        print(_args, _kwargs)

      return _inlined_with_logging(x, y, z)
  |};
  (* Decorator used on a method. *)
  assert_inlined
    {|
    from typing import Callable, TypeVar
    from builtins import _test_sink

    T = TypeVar("T", bound="Foo")

    def with_logging(f: Callable) -> Callable:
      def helper(args) -> None:
        _test_sink(args)

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
    from builtins import _test_sink

    T = TypeVar("T", bound="Foo")

    def with_logging(f: Callable) -> Callable:
      def helper(args) -> None:
        _test_sink(args)

      def inner( *args, **kwargs) -> None:
        helper(args)
        f( *args, **kwargs)

      return inner

    class Base: ...

    class Foo(Base):
      def bar(self, x: str) -> None:
        print(x)

      def foo(self, x: str) -> None:
        def _original_function(self: Foo, x: str) -> None:
          self.bar(x)

        def _inlined_with_logging(self: Foo, x: str) -> None:

          def helper(args) -> None:
            _test_sink(args)

          _args = (self, x)
          _kwargs = {"self": self, "x": x}
          helper(_args)
          _original_function(self, x)

        return _inlined_with_logging(self, x)

      def self_has_type(self: Base, x: str) -> None:
        def _original_function(self: Base, x: str) -> None:
          self.bar(x)

        def _inlined_with_logging(self: Base, x: str) -> None:

          def helper(args) -> None:
            _test_sink(args)

          _args = (self, x)
          _kwargs = {"self": self, "x": x}
          helper(_args)
          _original_function(self, x)

        return _inlined_with_logging(self, x)

      def self_has_generic_type(self: T, other: T, x: str) -> None:
        def _original_function(self: T, other: T, x: str) -> None:
          self.bar(x)
          other.bar(x)

        def _inlined_with_logging(self: T, other: T, x: str) -> None:

          def helper(args) -> None:
            _test_sink(args)

          _args = (self, other, x)
          _kwargs = {"self": self, "other": other, "x": x}
          helper(_args)
          _original_function(self, other, x)

        return _inlined_with_logging(self, other, x)
  |};
  (* Decorator used on a classmethod. *)
  assert_inlined
    {|
    from typing import Callable
    from builtins import _test_sink

    def with_logging(f: Callable) -> Callable:

      def inner( *args, **kwargs) -> None:
        _test_sink(args)
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
    from builtins import _test_sink

    def with_logging(f: Callable) -> Callable:

      def inner( *args, **kwargs) -> None:
        _test_sink(args)
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

        def _original_function(cls: typing.Type[Foo], x: int) -> None:
          cls.some_class_method(x)
          cls().some_method(x)

        def _inlined_with_logging(cls: typing.Type[Foo], x: int) -> None:
          _args = (cls, x)
          _kwargs = {"cls": cls, "x": x}
          _test_sink(_args)
          _original_function(cls, x)

        return _inlined_with_logging(cls, x)
  |};
  (* TODO(T69755379): Correctly inline decorator used on a staticmethod. Right now, we're missing
     the @staticmethod decorator. *)
  assert_inlined
    {|
    from typing import Callable
    from builtins import _test_sink

    def with_logging(f: Callable) -> Callable:

      def inner( *args, **kwargs) -> None:
        _test_sink(args)
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
    from builtins import _test_sink

    def with_logging(f: Callable) -> Callable:

      def inner( *args, **kwargs) -> None:
        _test_sink(args)
        f( *args, **kwargs)

      return inner

    class Foo:
      def some_method(self, x: int) -> None:
        print(self, x)

      def foo(x: int) -> None:

        def _original_function(x: int) -> None:
          print(x)

        def _inlined_with_logging(x: int) -> None:
          _args = (x,)
          _kwargs = {"x": x}
          _test_sink(_args)
          _original_function(x)

        return _inlined_with_logging(x)
  |};
  (* Same decorator applied multiple times. *)
  assert_inlined
    {|
    from builtins import _test_sink
    from typing import Callable

    def with_logging(callable: Callable[[str], None]) -> Callable[[str], None]:

      def inner(y: str) -> None:
        _test_sink(y)
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
    from builtins import _test_sink
    from typing import Callable

    def with_logging(callable: Callable[[str], None]) -> Callable[[str], None]:

      def inner(y: str) -> None:
        _test_sink(y)
        callable(y)

      return inner


    def identity(f):
      def inner(y: str) -> None:
        f(y)

      return inner

    def foo(y: str) -> None:
      def _original_function(z: str) -> None:
        print(z)

      def _inlined_with_logging(y: str) -> None:
        _test_sink(y)
        _original_function(y)

      def _inlined_identity(y: str) -> None:
        _inlined_with_logging(y)

      def _inlined_with_logging2(y: str) -> None:
        _test_sink(y)
        _inlined_identity(y)

      return _inlined_with_logging2(y)
  |};
  (* Decorator that passes in a local variable to the original function.

     Note: This is a bit of a weird edge case because the `@wraps` says that the signature is the
     same as the original function, but in reality it takes in one less parameter. I'm reconciling
     this by keeping the original signature (for the sake of model-writing and typechecking) but
     only storing the remaining parameters in `_args` and `_kwargs`.

     Note 2: Commenting out `x = 42` because the new CPython parser cannot handle
     `$local_test...$x`, which is invalid Python syntax. Leaving it commented for historical
     reasons. *)
  assert_inlined
    {|
    from typing import Callable
    from builtins import _test_sink
    from functools import wraps

    def with_logging(f: Callable) -> Callable:

      @wraps(f)
      def inner(request: str, *args, **kwargs) -> None:
        _test_sink(args)
        f(request, 42, *args, **kwargs)

      return inner

    @with_logging
    def foo(request: str, x: int, y: int) -> None:
      print(x)
  |}
    {|
    from typing import Callable
    from builtins import _test_sink
    from functools import wraps

    def with_logging(f: Callable) -> Callable:

      @wraps(f)
      def inner(request: str, *args, **kwargs) -> None:
        _test_sink(args)
        f(request, 42, *args, **kwargs)

      return inner

    def foo(request: str, x: int, y: int) -> None:

      def _original_function(request: str, x: int, y: int) -> None:
        print(x)

      def _inlined_with_logging(request: str, x: int, y: int) -> None:
        _args = (y, )
        _kwargs = {"y": y}
        _test_sink(_args)

        _original_function(request, 42, y)

      return _inlined_with_logging(request, x, y)
  |};
  (* Decorator that passes in a local variable but doesn't use @wraps. We fall back to having *args,
     **kwargs in the outer signature. *)
  assert_inlined
    {|
    from typing import Callable
    from builtins import _test_sink

    def with_logging(f: Callable) -> Callable:

      def inner(request: str, *args, **kwargs) -> None:
        _test_sink(args)
        x = 42
        f(request, x, *args, **kwargs)

      return inner

    @with_logging
    def foo(request: str, x: int, y: int) -> None:
      print(x)
  |}
    {|
    from typing import Callable
    from builtins import _test_sink

    def with_logging(f: Callable) -> Callable:

      def inner(request: str, *args, **kwargs) -> None:
        _test_sink(args)
        x = 42
        f(request, x, *args, **kwargs)

      return inner

    def foo(request: str, *args, **kwargs) -> None:

      def _original_function(request: str, x: int, y: int) -> None:
        print(x)

      def _inlined_with_logging(request: str, *args, **kwargs) -> None:
        _test_sink(args)
        x = 42
        _original_function(request, x, *args, **kwargs)

      return _inlined_with_logging(request, *args, **kwargs)
  |};
  (* Preserve the return type if the decorator uses @wraps. *)
  assert_inlined
    {|
    from builtins import _test_sink
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
    from builtins import _test_sink
    from typing import Any, Callable
    from functools import wraps

    def decorator_using_wraps(func: Callable) -> Any:
        @wraps(func)
        def wrap( *args: Any, **kwargs: Any) -> Any:
            return func( *args, **kwargs)

        return wrap

    def foo(x: str) -> str:
      def _original_function(x: str) -> str:
        return x

      def _inlined_decorator_using_wraps(x: str) -> str:
        _args = (x,)
        _kwargs = {"x": x}
        return _original_function(x)

      return _inlined_decorator_using_wraps(x)
  |};
  assert_inlined
    {|
    from builtins import _test_sink
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
    from builtins import _test_sink
    from typing import Any, Callable
    from functools import wraps

    def decorator_using_wraps(callable: Callable) -> Callable:

      @wraps(callable)
      def inner(y: str) -> Any:
        return callable(y)

      return inner

    def foo(y: str) -> str:
      def _original_function(z: str) -> str:
        return z

      def _inlined_decorator_using_wraps(y: str) -> str:
        return _original_function(y)

      return _inlined_decorator_using_wraps(y)
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
      def _original_function(z: str) -> str:
        return z

      def _inlined_decorator_using_wraps(z: str) -> str:
        _args = (z, )
        _kwargs = {"z": z}
        return _original_function(z)

      return _inlined_decorator_using_wraps(z)
  |};
  (* Don't preserve the return type if the decorator doesn't use @wraps. *)
  assert_inlined
    {|
    from builtins import _test_sink
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
    from builtins import _test_sink
    from typing import Any, Callable

    def decorator_not_using_wraps(func: Callable) -> Any:
        def wrap( *args: Any, **kwargs: Any) -> int:
            func( *args, **kwargs)
            return 1

        return wrap

    def foo(x: str) -> int:
      def _original_function(x: str) -> str:
        return x

      def _inlined_decorator_not_using_wraps(x: str) -> int:
        _args = (x,)
        _kwargs = {"x": x}
        _original_function(x)
        return 1

      return _inlined_decorator_not_using_wraps(x)
  |};
  ()


let test_decorator_location context =
  let assert_inlined
      ?(additional_sources = [])
      ?(handle = "test.py")
      ~expected_inlined_original_pairs
      source
      expected
    =
    let expected, actual =
      get_expected_actual_sources ~context ~additional_sources ~handle source expected
    in
    assert_source_equal ~location_insensitive:true expected actual;
    List.iter expected_inlined_original_pairs ~f:(fun (inlined_function_reference, expected) ->
        assert_equal
          ~printer:(fun outer_decorator_reference ->
            Format.asprintf
              "inlined_function: %s\toriginal_decorator: %s"
              ([%show: Reference.t] inlined_function_reference)
              ([%show: Reference.t option] outer_decorator_reference))
          ~cmp:[%equal: Reference.t option]
          expected
          (InlineDecorator.InlinedNameToOriginalName.get inlined_function_reference))
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
                _test_sink(y)
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
    from builtins import _test_sink
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
    ~expected_inlined_original_pairs:
      [
        !&"test.baz._inlined_same_module_decorator", Some !&"test.same_module_decorator";
        !&"test.baz._original_function", None;
        !&"test.baz", None;
        !&"test.bar._inlined_with_logging", Some !&"logging_decorator.with_logging";
        !&"test.bar._inlined_with_logging.helper", Some !&"logging_decorator.with_logging";
        !&"test.bar", None;
        !&"test.bar._original_function", None;
        !&"test.bar._inlined_identity", Some !&"some_module.identity_decorator.identity";
        !&"test.foo._inlined_with_logging", Some !&"logging_decorator.with_logging";
        !&"test.foo._inlined_with_logging.helper", Some !&"logging_decorator.with_logging";
        !&"test.foo", None;
        !&"test.foo._original_function", None;
        ( !&"test.same_decorator_twice._inlined_identity",
          Some !&"some_module.identity_decorator.identity" );
        ( !&"test.same_decorator_twice._inlined_identity2",
          (* TODO(T105998389): The original name should be `identity`, not `identity2`. *)
          Some !&"some_module.identity_decorator.identity2" );
        !&"test.same_decorator_twice._original_function", None;
        !&"test.same_decorator_twice", None;
      ]
    {|
    from builtins import _test_sink
    from typing import Callable
    from logging_decorator import with_logging, fails_to_apply
    from some_module.identity_decorator import identity

    def same_module_decorator(callable: Callable[[str], None]) -> Callable[[str], None]:
      def inner(y: str) -> None:
        callable(y)

      return inner

    def foo(y: str) -> None:
      def _original_function(z: str) -> None:
        print(z)

      def _inlined_with_logging(y: str) -> None:

        def helper(y: str) -> None:
          print(y)

        _test_sink(y)
        _original_function(y)
        helper(y)

      return _inlined_with_logging(y)

    def bar(y: str) -> None:

      def _original_function(z: str) -> None:
        print(z)

      def _inlined_identity(y: str) -> None:
        _original_function(y)

      def _inlined_with_logging(y: str) -> None:

        def helper(y: str) -> None:
          print(y)

        _test_sink(y)
        _inlined_identity(y)
        helper(y)

      return _inlined_with_logging(y)


    def baz(y: str) -> None:
      def _original_function(z: str) -> None:
        print(z)

      def _inlined_same_module_decorator(y: str) -> None:
        _original_function(y)

      return _inlined_same_module_decorator(y)


    def same_decorator_twice(y: str) -> None:

      def _original_function(z: str) -> None:
        print(z)

      def _inlined_identity(y: str) -> None:
        _original_function(y)

      def _inlined_identity2(y: str) -> None:
        _inlined_identity(y)

      return _inlined_identity2(y)
  |};
  ()


let test_requalify_name _ =
  let open Expression in
  let assert_requalified ~old_qualifier ~new_qualifier name expected =
    assert_equal
      ~cmp:[%compare.equal: Name.t]
      ~printer:[%show: Name.t]
      expected
      (InlineDecorator.requalify_name ~old_qualifier ~new_qualifier name)
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
        [{ Node.value = Define { signature = { name = callee_name; _ } as new_signature; _ }; _ }] )
      ->
        let actual =
          InlineDecorator.replace_signature_if_always_passing_on_arguments
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
        _args = (y, z)
        _kwargs = {"y": y, "z": z}
        foo(y, z)
        bar(_args)
        baz(_kwargs)
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
        _args = (y,)
        _kwargs = {"y": y}
        foo(y)
        bar(_args)
  |});
  (* If the callee expects `args` and `kwargs`, make sure not to confuse them with our synthetic
     locals `_args` and `_kwargs`.

     Note that we conservatively store all the arguments to both `_args` and `_kwargs` so that we
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
        _args = (y, *args)
        _kwargs = {"y": y, **kwargs}
        foo(y, *args, **kwargs)
        baz(_kwargs)
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
        _args = (y,)
        _kwargs = {"y": y}
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
        _args = (x, y)
        _kwargs = {"x": x, "y": y}
        foo(some_parameter, x, y)
        print(some_parameter)
        print(_args, _kwargs)
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
        _args = (x, y)
        _kwargs = {"x": x, "y": y}
        foo(prefix1, prefix2, x, y)
        print(prefix1, prefix2)
        print(_args, _kwargs)
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
        _args = (x, y)
        _kwargs = {"x": x, "y": y}
        foo(some_parameter, x, y)
        print(some_parameter)
        print(_args, _kwargs)
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
        _args = ( *args,)
        _kwargs = { **kwargs}
        foo(some_parameter, *args, **kwargs)
        print(some_parameter)
        print(_args, _kwargs)
  |});
  assert_signature_replaced
    ~new_signature:"async def foo(y: str, z: int) -> None: ..."
    {|
      async def wrapper( *args, **kwargs) -> None:
        await foo( *args, **kwargs)
  |}
    (Some
       {|
      async def wrapper(y: str, z: int) -> None:
        _args = (y, z)
        _kwargs = {"y": y, "z": z}
        await foo(y, z)
  |});
  ()


let test_rename_local_variables _ =
  let assert_renamed ~pairs given expected =
    match parse expected |> Source.statements, parse given |> Source.statements with
    | [{ Node.value = Define expected; _ }], [{ Node.value = Define given; _ }] ->
        let actual = InlineDecorator.rename_local_variables ~pairs given in
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
      (List.map given ~f:Reference.create
      |> InlineDecorator.uniquify_names ~get_reference:Fn.id ~set_reference:(fun reference _ ->
             reference))
  in
  assert_uniquified
    ["a.b"; "a.c"; "a.b"; "a.b"; "a.c"; "foo"]
    ["a.b3"; "a.c2"; "a.b2"; "a.b"; "a.c"; "foo"];
  assert_uniquified [] [];
  ()


let () =
  "inline"
  >::: [
         "decorators_to_skip" >:: test_decorators_to_skip;
         "decorator_body" >:: test_decorator_body;
         "inline_decorators" >:: test_inline_decorators;
         "decorator_location" >:: test_decorator_location;
         "requalify_name" >:: test_requalify_name;
         "replace_signature" >:: test_replace_signature;
         "rename_local_variables" >:: test_rename_local_variables;
         "uniquify_names" >:: test_uniquify_names;
       ]
  |> Test.run
