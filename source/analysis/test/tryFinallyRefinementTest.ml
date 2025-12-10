(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Test

let type_check ~environment ~source =
  let { Ast.Source.module_path = { Ast.ModulePath.qualifier; _ }; _ } = source in
  let scheduler = Scheduler.create_sequential () in
  Analysis.TypeEnvironment.populate_for_modules
    ~scheduler
    ~scheduler_policies:Configuration.SchedulerPolicies.empty
    environment
    [qualifier];
  Analysis.Postprocessing.run
    ~scheduler
    ~environment:(Analysis.TypeEnvironment.read_only environment)
    [qualifier]


let assert_type_errors = assert_errors ~check:type_check ~debug:true

let test_assignment_in_try_with_finally context =
  assert_type_errors
    {|
      def use_int(x: int) -> None:
        pass

      def may_raise(foo: int) -> int:
        if True:
          raise ValueError("Test error")
        return foo + 1

      def test() -> None:
        x: int | None = None
        try:
          x = may_raise(123)
        finally:
          use_int(x)
    |}
    (* TODO: this is incorrect, should infer that x is `int | None` in finally *)
    []
    context


let test_assignment_in_try_with_except context =
  assert_type_errors
    {|
      def use_int(x: int) -> None:
        pass

      def may_raise(foo: int) -> int:
        if True:
          raise ValueError("Test error")
        return foo + 1

      def test() -> None:
        x: int | None = None
        try:
          x = may_raise(123)
        except ValueError:
          use_int(x)
    |}
    [
      "Incompatible parameter type [6]: In call `use_int`, for 1st positional argument, expected \
       `int` but got `None`.";
    ]
    context


let test_multiple_assignments_in_try context =
  assert_type_errors
    {|
      def may_raise() -> int:
        if True:
          raise ValueError()
        return 1

      def test() -> None:
        a: int | None = None
        b: int | None = None
        c: str | None = None
        try:
          a = 1
          b = may_raise()
          c = "success"
        finally:
          reveal_type(a)
          reveal_type(b)
          reveal_type(c)
    |}
    [
      (* TODO: should infer that b and c are still optional *)
      "Revealed type [-1]: Revealed type for `a` is `typing.Optional[int]` (inferred: \
       `typing_extensions.Literal[1]`).";
      "Revealed type [-1]: Revealed type for `b` is `typing.Optional[int]` (inferred: `int`).";
      "Revealed type [-1]: Revealed type for `c` is `typing.Optional[str]` (inferred: \
       `typing_extensions.Literal['success']`).";
    ]
    context


let test_try_except_finally_union context =
  assert_type_errors
    {|
      def may_raise() -> int:
        if True:
          raise ValueError()
        return 1

      def test() -> None:
        x: int | None = None
        try:
          _ = may_raise()
          x = 1
        except ValueError:
          x = 2
        finally:
          reveal_type(x)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `typing.Optional[int]` (inferred: \
       `typing.Union[typing_extensions.Literal[1], typing_extensions.Literal[2]]`).";
    ]
    context


let test_modified_in_try_with_empty_finally context =
  assert_type_errors
    {|
      def may_raise() -> int:
        if True:
          raise ValueError()
        return 1

      def use_int(x: int) -> None:
        pass

      def test() -> None:
        x: int | None = None
        try:
          x = may_raise()
        finally:
          pass

        reveal_type(x)
        use_int(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `typing.Optional[int]` (inferred: `int`)."]
    context


let test_modified_in_try_conditional_with_empty_finally context =
  assert_type_errors
    {|
      def may_raise() -> int:
        if True:
          raise ValueError()
        return 1

      def use_int(x: int) -> None:
        pass

      def test() -> None:
        x = 123
        try:
          if may_raise() > 10:
            x = "hello"
        finally:
          pass

        reveal_type(x)
        use_int(x)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `typing.Union[typing_extensions.Literal[123], \
       typing_extensions.Literal['hello']]`.";
      "Incompatible parameter type [6]: In call `use_int`, for 1st positional argument, expected \
       `int` but got `Union[int, str]`.";
    ]
    context


let test_isinstance_refinement_not_in_finally context =
  assert_type_errors
    {|
      def use_int(x: int) -> None:
        pass

      def test(x: int | str) -> None:
        try:
          assert isinstance(x, int)
        finally:
          reveal_type(x)
          use_int(x)
    |}
    [
      (* TODO: this is incorrect, should be int | str *)
      "Revealed type [-1]: Revealed type for `x` is `int`.";
    ]
    context


let test_assert_usage_in_finally context =
  assert_type_errors
    {|
      def use_int(x: int) -> None:
        pass

      def test(x: int | None) -> None:
        try:
          assert x is not None
        finally:
          reveal_type(x)
          use_int(x)
    |}
    [
      (* TODO: this is incorrect, should be int | None *)
      "Revealed type [-1]: Revealed type for `x` is `typing.Optional[int]` (inferred: `int`).";
    ]
    context


let test_try_finally_no_exception context =
  assert_type_errors
    {|
      def use_int(x: int) -> None:
        pass

      def test() -> None:
        x: int | None = None
        try:
          x = 1
        finally:
          reveal_type(x)
          use_int(x)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `typing.Optional[int]` (inferred: \
       `typing_extensions.Literal[1]`).";
    ]
    context


let test_not_modified_in_try_with_finally context =
  assert_type_errors
    {|
      def use_int(x: int) -> None:
        pass

      def get_int() -> int:
        return 123

      def may_raise(foo: int) -> int:
        if True:
          raise ValueError("Test error")
        return foo + 1

      def test() -> None:
        x = get_int()
        try:
          _ = may_raise(x)
        finally:
          reveal_type(x)
          use_int(x)

        reveal_type(x)
        use_int(x)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `int`.";
      "Revealed type [-1]: Revealed type for `x` is `int`.";
    ]
    context


let test_not_modified_in_try_then_except_uses_pre_try_type context =
  assert_type_errors
    {|
      def use_int(x: int) -> None:
        pass

      def get_int() -> int:
        return 123

      def may_raise(foo: int) -> int:
        if True:
          raise ValueError("Test error")
        return foo + 1

      def test() -> None:
        x = get_int()
        try:
          _ = may_raise(x)
        except ValueError:
          reveal_type(x)
          use_int(x)

        reveal_type(x)
        use_int(x)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `int`.";
      "Revealed type [-1]: Revealed type for `x` is `int`.";
    ]
    context


let test_not_modified_in_try_with_except_else context =
  assert_type_errors
    {|
      def use_int(x: int) -> None:
        pass

      def get_int() -> int:
        return 123

      def may_raise(foo: int) -> int:
        if True:
          raise ValueError("Test error")
        return foo + 1

      def test() -> None:
        x = get_int()

        try:
          _ = may_raise(x)
        except ValueError:
          reveal_type(x)
          use_int(x)
        else:
          reveal_type(x)
          use_int(x)

        reveal_type(x)
        use_int(x)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `int`.";
      "Revealed type [-1]: Revealed type for `x` is `int`.";
      "Revealed type [-1]: Revealed type for `x` is `int`.";
    ]
    context


let test_not_modified_in_try_with_except_finally context =
  assert_type_errors
    {|
      def use_int(x: int) -> None:
        pass

      def get_int() -> int:
        return 123

      def may_raise(foo: int) -> int:
        if True:
          raise ValueError("Test error")
        return foo + 1

      def test() -> None:
        x = get_int()
        try:
          _ = may_raise(x)
        except ValueError:
          use_int(x)
        finally:
          reveal_type(x)
          use_int(x)

        reveal_type(x)
        use_int(x)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `int`.";
      "Revealed type [-1]: Revealed type for `x` is `int`.";
    ]
    context


let test_not_modified_in_try_with_except_else_finally context =
  assert_type_errors
    {|
      def use_int(x: int) -> None:
        pass

      def get_int() -> int:
        return 123

      def may_raise(foo: int) -> int:
        if True:
          raise ValueError("Test error")
        return foo + 1

      def test() -> None:
        x = get_int()
        try:
          _ = may_raise(x)
        except ValueError:
          use_int(x)
        else:
          reveal_type(x)
          use_int(x)
        finally:
          reveal_type(x)
          use_int(x)

        reveal_type(x)
        use_int(x)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `int`.";
      "Revealed type [-1]: Revealed type for `x` is `int`.";
      "Revealed type [-1]: Revealed type for `x` is `int`.";
    ]
    context


let test_modified_in_try_after_raise_with_finally context =
  assert_type_errors
    {|
      def use_int(x: int) -> None:
        pass

      def may_raise(foo: int) -> int:
        if True:
          raise ValueError("Test error")
        return foo + 1

      def test() -> None:
        x = 123
        try:
          _ = may_raise(x)
          x = "hello"
        finally:
          reveal_type(x)
          use_int(x)

        reveal_type(x)
        use_int(x)
    |}
    [
      (* Inside finally *)
      (* TODO: this is incorrect, should be `int | str` because `try` body may or may not run to
         completion *)
      "Revealed type [-1]: Revealed type for `x` is `typing_extensions.Literal['hello']`.";
      "Incompatible parameter type [6]: In call `use_int`, for 1st positional argument, expected \
       `int` but got `str`.";
      (* After try-finally: ideally should be `str` only because try definitely ran to completion *)
      "Revealed type [-1]: Revealed type for `x` is `typing_extensions.Literal['hello']`.";
      "Incompatible parameter type [6]: In call `use_int`, for 1st positional argument, expected \
       `int` but got `str`.";
    ]
    context


let test_modified_in_try_after_raise_with_finally_reassign context =
  assert_type_errors
    {|
      def use_int(x: int) -> None:
        pass

      def may_raise(foo: int) -> int:
        if True:
          raise ValueError("Test error")
        return foo + 1

      def test() -> None:
        x = 123
        try:
          _ = may_raise(x)
          x = "hello"
        finally:
          reveal_type(x)
          x = True

        reveal_type(x)
    |}
    [
      (* Inside finally *)
      (* TODO: this is incorrect, should be `int | str` because `try` body may or may not run to
         completion *)
      "Revealed type [-1]: Revealed type for `x` is `typing_extensions.Literal['hello']`.";
      (* After try-finally: must be `Literal[True]` *)
      "Revealed type [-1]: Revealed type for `x` is `typing_extensions.Literal[True]`.";
    ]
    context


let test_modified_in_try_after_raise_with_finally_return context =
  assert_type_errors
    {|
      def use_int(x: int) -> None:
        pass

      def may_raise(foo: int) -> int:
        if True:
          raise ValueError("Test error")
        return foo + 1

      def test() -> None:
        x = 123
        try:
          _ = may_raise(x)
          x = "hello"
        finally:
          reveal_type(x)
          return

        reveal_type(x)
        use_int(x)
    |}
    [
      (* Inside finally *)
      (* TODO: this is incorrect, should be `int | str` because `try` body may or may not run to
         completion *)
      "Revealed type [-1]: Revealed type for `x` is `typing_extensions.Literal['hello']`.";
      (* After try-finally: unreachable, no error messages *)
    ]
    context


let test_modified_in_try_after_raise_conditional_with_finally context =
  assert_type_errors
    {|
      def use_int(x: int) -> None:
        pass

      def may_raise(foo: int) -> int:
        if True:
          raise ValueError("Test error")
        return foo + 1

      def test() -> None:
        x = 123
        try:
          if may_raise(x) > 10:
            x = "hello"
        finally:
          reveal_type(x)
          use_int(x)

        reveal_type(x)
        use_int(x)
    |}
    [
      (* Inside finally *)
      "Revealed type [-1]: Revealed type for `x` is `typing.Union[typing_extensions.Literal[123], \
       typing_extensions.Literal['hello']]`.";
      "Incompatible parameter type [6]: In call `use_int`, for 1st positional argument, expected \
       `int` but got `Union[int, str]`.";
      (* After try-finally *)
      "Revealed type [-1]: Revealed type for `x` is `typing.Union[typing_extensions.Literal[123], \
       typing_extensions.Literal['hello']]`.";
      "Incompatible parameter type [6]: In call `use_int`, for 1st positional argument, expected \
       `int` but got `Union[int, str]`.";
    ]
    context


let test_modified_in_try_before_raise_with_finally context =
  assert_type_errors
    {|
      def use_int(x: int) -> None:
        pass

      def may_raise(foo: int) -> int:
        if True:
          raise ValueError("Test error")
        return foo + 1

      def test() -> None:
        x = 123
        try:
          x = "hello"
          _ = may_raise(1)
        finally:
          reveal_type(x)
          use_int(x)

        reveal_type(x)
        use_int(x)
    |}
    [
      (* Inside finally *)
      "Revealed type [-1]: Revealed type for `x` is `typing_extensions.Literal['hello']`.";
      "Incompatible parameter type [6]: In call `use_int`, for 1st positional argument, expected \
       `int` but got `str`.";
      (* After try-finally *)
      "Revealed type [-1]: Revealed type for `x` is `typing_extensions.Literal['hello']`.";
      "Incompatible parameter type [6]: In call `use_int`, for 1st positional argument, expected \
       `int` but got `str`.";
    ]
    context


let test_modified_in_try_after_raise_with_except context =
  assert_type_errors
    {|
      def use_int(x: int) -> None:
        pass

      def may_raise(foo: int) -> int:
        if True:
          raise ValueError("Test error")
        return foo + 1

      def test() -> None:
        x = 123
        try:
          _ = may_raise(x)
          x = "hello"
        except ValueError:
          reveal_type(x)
          use_int(x)

        reveal_type(x)
        use_int(x)
    |}
    [
      (* From except: ideal strict behavior to infer that x = "hello" is discarded. May be
         acceptable to change to int | str *)
      "Revealed type [-1]: Revealed type for `x` is `typing_extensions.Literal[123]`.";
      (* After try-except *)
      "Revealed type [-1]: Revealed type for `x` is `typing.Union[typing_extensions.Literal[123], \
       typing_extensions.Literal['hello']]`.";
      "Incompatible parameter type [6]: In call `use_int`, for 1st positional argument, expected \
       `int` but got `Union[int, str]`.";
    ]
    context


let test_modified_in_try_after_raise_conditional_with_except context =
  assert_type_errors
    {|
      def use_int(x: int) -> None:
        pass

      def may_raise(foo: int) -> int:
        if True:
          raise ValueError("Test error")
        return foo + 1

      def test() -> None:
        x = 123
        try:
          if may_raise(x) > 10:
            x = "hello"
        except ValueError:
          reveal_type(x)
          use_int(x)

        reveal_type(x)
        use_int(x)
    |}
    [
      (* From except: ideal strict behavior to infer that x = "hello" is discarded. May be
         acceptable to change to int | str *)
      "Revealed type [-1]: Revealed type for `x` is `typing_extensions.Literal[123]`.";
      (* After try-except: either success or except path taken, must be `int | str` *)
      "Revealed type [-1]: Revealed type for `x` is `typing.Union[typing_extensions.Literal[123], \
       typing_extensions.Literal['hello']]`.";
      "Incompatible parameter type [6]: In call `use_int`, for 1st positional argument, expected \
       `int` but got `Union[int, str]`.";
    ]
    context


let test_modified_in_try_before_raise_with_except context =
  assert_type_errors
    {|
      def use_int(x: int) -> None:
        pass

      def may_raise(foo: int) -> int:
        if True:
          raise ValueError("Test error")
        return foo + 1

      def test() -> None:
        x = 123
        try:
          x = "hello"
          _ = may_raise(1)
        except ValueError:
          reveal_type(x)
          use_int(x)

        reveal_type(x)
        use_int(x)
    |}
    [
      (* Inside except: not ideal, but already existing behavior to discard effects within try. OK
         to change this to reveal `int` or `int | str` in the future. *)
      "Revealed type [-1]: Revealed type for `x` is `typing_extensions.Literal[123]`.";
      (* After try-except *)
      "Revealed type [-1]: Revealed type for `x` is `typing.Union[typing_extensions.Literal[123], \
       typing_extensions.Literal['hello']]`.";
      "Incompatible parameter type [6]: In call `use_int`, for 1st positional argument, expected \
       `int` but got `Union[int, str]`.";
    ]
    context


let test_modified_in_try_after_raise_with_except_else context =
  assert_type_errors
    {|
      def use_int(x: int) -> None:
        pass

      def may_raise(foo: int) -> int:
        if True:
          raise ValueError("Test error")
        return foo + 1

      def test() -> None:
        x = 123
        try:
          _ = may_raise(x)
          x = "hello"
        except ValueError:
          reveal_type(x)
          use_int(x)
        else:
          reveal_type(x)
          use_int(x)

        reveal_type(x)
        use_int(x)
    |}
    [
      (* From except *)
      "Revealed type [-1]: Revealed type for `x` is `typing_extensions.Literal[123]`.";
      (* From else *)
      "Revealed type [-1]: Revealed type for `x` is `typing_extensions.Literal['hello']`.";
      "Incompatible parameter type [6]: In call `use_int`, for 1st positional argument, expected \
       `int` but got `str`.";
      (* After try-except-else *)
      "Revealed type [-1]: Revealed type for `x` is `typing.Union[typing_extensions.Literal[123], \
       typing_extensions.Literal['hello']]`.";
      "Incompatible parameter type [6]: In call `use_int`, for 1st positional argument, expected \
       `int` but got `Union[int, str]`.";
    ]
    context


let test_modified_in_try_after_raise_conditional_with_except_else context =
  assert_type_errors
    {|
      def use_int(x: int) -> None:
        pass

      def may_raise(foo: int) -> int:
        if True:
          raise ValueError("Test error")
        return foo + 1

      def test() -> None:
        x = 123
        try:
          if may_raise(x) > 10:
            x = "hello"
        except ValueError:
          reveal_type(x)
          use_int(x)
        else:
          reveal_type(x)
          use_int(x)

        reveal_type(x)
        use_int(x)
    |}
    [
      (* From except *)
      "Revealed type [-1]: Revealed type for `x` is `typing_extensions.Literal[123]`.";
      (* From else: success path, must be `int | str` *)
      "Revealed type [-1]: Revealed type for `x` is `typing.Union[typing_extensions.Literal[123], \
       typing_extensions.Literal['hello']]`.";
      "Incompatible parameter type [6]: In call `use_int`, for 1st positional argument, expected \
       `int` but got `Union[int, str]`.";
      (* After try-except-else: either success or except, must be `int | str` *)
      "Revealed type [-1]: Revealed type for `x` is `typing.Union[typing_extensions.Literal[123], \
       typing_extensions.Literal['hello']]`.";
      "Incompatible parameter type [6]: In call `use_int`, for 1st positional argument, expected \
       `int` but got `Union[int, str]`.";
    ]
    context


let test_modified_in_try_after_raise_conditional_then_raise_with_except_else context =
  assert_type_errors
    {|
      def use_int(x: int) -> None:
        pass

      def may_raise(foo: int) -> int:
        if True:
          raise ValueError("Test error")
        return foo + 1

      def test() -> None:
        x = 123
        try:
          if may_raise(x) > 10:
            x = "hello"
          raise ValueError("Test error")
        except ValueError:
          reveal_type(x)
          use_int(x)
        else:
          reveal_type(x)
          use_int(x)

        reveal_type(x)
        use_int(x)
    |}
    [
      (* From except: must be `int | str` *)
      "Revealed type [-1]: Revealed type for `x` is `typing.Union[typing_extensions.Literal[123], \
       typing_extensions.Literal['hello']]`.";
      "Incompatible parameter type [6]: In call `use_int`, for 1st positional argument, expected \
       `int` but got `Union[int, str]`.";
      (* Nothing from else (unreachable) *)
      (* After try-except-else: either success or except, must be `int | str` *)
      "Revealed type [-1]: Revealed type for `x` is `typing.Union[typing_extensions.Literal[123], \
       typing_extensions.Literal['hello']]`.";
      "Incompatible parameter type [6]: In call `use_int`, for 1st positional argument, expected \
       `int` but got `Union[int, str]`.";
    ]
    context


let test_modified_in_try_after_raise_conditional_then_raise_conditional_with_except_else context =
  assert_type_errors
    {|
      def use_int(x: int) -> None:
        pass

      def may_raise(foo: int) -> int:
        if True:
          raise ValueError("Test error")
        return foo + 1

      def test() -> None:
        x = 123
        try:
          if may_raise(x) > 10:
            x = "hello"
          else:
            raise ValueError("Test error")
        except ValueError:
          reveal_type(x)
          use_int(x)
        else:
          reveal_type(x)
          use_int(x)

        reveal_type(x)
        use_int(x)
    |}
    [
      (* From except *)
      "Revealed type [-1]: Revealed type for `x` is `typing_extensions.Literal[123]`.";
      (* From else: no error, `try` body ran to completion, so `x` must be `str` *)
      "Revealed type [-1]: Revealed type for `x` is `typing_extensions.Literal['hello']`.";
      "Incompatible parameter type [6]: In call `use_int`, for 1st positional argument, expected \
       `int` but got `str`.";
      (* After try-except-else: either success or except, must be `int | str` *)
      "Revealed type [-1]: Revealed type for `x` is `typing.Union[typing_extensions.Literal[123], \
       typing_extensions.Literal['hello']]`.";
      "Incompatible parameter type [6]: In call `use_int`, for 1st positional argument, expected \
       `int` but got `Union[int, str]`.";
    ]
    context


let test_modified_in_try_before_raise_with_except_else context =
  assert_type_errors
    {|
      def use_int(x: int) -> None:
        pass

      def may_raise(foo: int) -> int:
        if True:
          raise ValueError("Test error")
        return foo + 1

      def test() -> None:
        x = 123
        try:
          x = "hello"
          _ = may_raise(123)
        except ValueError:
          reveal_type(x)
          use_int(x)
        else:
          reveal_type(x)
          use_int(x)

        reveal_type(x)
        use_int(x)
    |}
    [
      (* Inside except: not ideal, but already existing behavior to discard effects within try. OK
         to change this to reveal `int` or `int | str` in the future. *)
      "Revealed type [-1]: Revealed type for `x` is `typing_extensions.Literal[123]`.";
      (* From else *)
      "Revealed type [-1]: Revealed type for `x` is `typing_extensions.Literal['hello']`.";
      "Incompatible parameter type [6]: In call `use_int`, for 1st positional argument, expected \
       `int` but got `str`.";
      (* After try-except-else *)
      "Revealed type [-1]: Revealed type for `x` is `typing.Union[typing_extensions.Literal[123], \
       typing_extensions.Literal['hello']]`.";
      "Incompatible parameter type [6]: In call `use_int`, for 1st positional argument, expected \
       `int` but got `Union[int, str]`.";
    ]
    context


let test_modified_in_try_before_raise_with_except_finally context =
  assert_type_errors
    {|
      def use_int(x: int) -> None:
        pass

      def may_raise(foo: int) -> int:
        if True:
          raise ValueError("Test error")
        return foo + 1

      def test() -> None:
        x = 123
        try:
          x = "hello"
          _ = may_raise(1)
        except ValueError:
          reveal_type(x)
          use_int(x)
        finally:
          reveal_type(x)
          use_int(x)

        reveal_type(x)
        use_int(x)
    |}
    [
      (* Inside except: not ideal, but already existing behavior to discard effects within try. OK
         to change this to reveal `int` or `int | str` in the future. *)
      "Revealed type [-1]: Revealed type for `x` is `typing_extensions.Literal[123]`.";
      (* From finally *)
      "Revealed type [-1]: Revealed type for `x` is `typing.Union[typing_extensions.Literal[123], \
       typing_extensions.Literal['hello']]`.";
      "Incompatible parameter type [6]: In call `use_int`, for 1st positional argument, expected \
       `int` but got `Union[int, str]`.";
      (* After try-except-finally *)
      "Revealed type [-1]: Revealed type for `x` is `typing.Union[typing_extensions.Literal[123], \
       typing_extensions.Literal['hello']]`.";
      "Incompatible parameter type [6]: In call `use_int`, for 1st positional argument, expected \
       `int` but got `Union[int, str]`.";
    ]
    context


let test_modified_in_try_after_raise_with_except_finally context =
  assert_type_errors
    {|
      def use_int(x: int) -> None:
        pass

      def may_raise(foo: int) -> int:
        if True:
          raise ValueError("Test error")
        return foo + 1

      def test() -> None:
        x = 123
        try:
          _ = may_raise(x)
          x = "hello"
        except ValueError:
          reveal_type(x)
          use_int(x)
        finally:
          reveal_type(x)
          use_int(x)

        reveal_type(x)
        use_int(x)
    |}
    [
      (* From except *)
      "Revealed type [-1]: Revealed type for `x` is `typing_extensions.Literal[123]`.";
      (* From finally *)
      "Revealed type [-1]: Revealed type for `x` is `typing.Union[typing_extensions.Literal[123], \
       typing_extensions.Literal['hello']]`.";
      "Incompatible parameter type [6]: In call `use_int`, for 1st positional argument, expected \
       `int` but got `Union[int, str]`.";
      (* After finally *)
      "Revealed type [-1]: Revealed type for `x` is `typing.Union[typing_extensions.Literal[123], \
       typing_extensions.Literal['hello']]`.";
      "Incompatible parameter type [6]: In call `use_int`, for 1st positional argument, expected \
       `int` but got `Union[int, str]`.";
    ]
    context


let test_modified_in_try_after_raise_conditional_with_except_finally context =
  assert_type_errors
    {|
      def use_int(x: int) -> None:
        pass

      def may_raise(foo: int) -> int:
        if True:
          raise ValueError("Test error")
        return foo + 1

      def test() -> None:
        x = 123
        try:
          if may_raise(x) > 10:
            x = "hello"
        except ValueError:
          reveal_type(x)
          use_int(x)
        finally:
          reveal_type(x)
          use_int(x)

        reveal_type(x)
        use_int(x)
    |}
    [
      (* From except *)
      "Revealed type [-1]: Revealed type for `x` is `typing_extensions.Literal[123]`.";
      (* From finally *)
      "Revealed type [-1]: Revealed type for `x` is `typing.Union[typing_extensions.Literal[123], \
       typing_extensions.Literal['hello']]`.";
      "Incompatible parameter type [6]: In call `use_int`, for 1st positional argument, expected \
       `int` but got `Union[int, str]`.";
      (* After finally *)
      "Revealed type [-1]: Revealed type for `x` is `typing.Union[typing_extensions.Literal[123], \
       typing_extensions.Literal['hello']]`.";
      "Incompatible parameter type [6]: In call `use_int`, for 1st positional argument, expected \
       `int` but got `Union[int, str]`.";
    ]
    context


let test_modified_in_try_after_raise_conditional_then_raise_with_except_finally context =
  assert_type_errors
    {|
      def use_int(x: int) -> None:
        pass

      def may_raise(foo: int) -> int:
        if True:
          raise ValueError("Test error")
        return foo + 1

      def test() -> None:
        x = 123
        try:
          if may_raise(x) > 10:
            x = "hello"
          raise ValueError("Test error")
        except ValueError:
          reveal_type(x)
          use_int(x)
        finally:
          reveal_type(x)
          use_int(x)

        reveal_type(x)
        use_int(x)
    |}
    [
      (* From except: can get there after x becomes str, so must infer `int | str` *)
      "Revealed type [-1]: Revealed type for `x` is `typing.Union[typing_extensions.Literal[123], \
       typing_extensions.Literal['hello']]`.";
      "Incompatible parameter type [6]: In call `use_int`, for 1st positional argument, expected \
       `int` but got `Union[int, str]`.";
      (* From finally *)
      "Revealed type [-1]: Revealed type for `x` is `typing.Union[typing_extensions.Literal[123], \
       typing_extensions.Literal['hello']]`.";
      "Incompatible parameter type [6]: In call `use_int`, for 1st positional argument, expected \
       `int` but got `Union[int, str]`.";
      (* After finally *)
      "Revealed type [-1]: Revealed type for `x` is `typing.Union[typing_extensions.Literal[123], \
       typing_extensions.Literal['hello']]`.";
      "Incompatible parameter type [6]: In call `use_int`, for 1st positional argument, expected \
       `int` but got `Union[int, str]`.";
    ]
    context


let test_try_except_sees_pre_try_type context =
  assert_type_errors
    {|
      def may_raise() -> int:
        if True:
          raise ValueError()
        return 1

      def use_int(x: int) -> None:
        pass

      def test() -> None:
        x: int | None = None
        try:
          x = may_raise()
          x = x + 1
        except ValueError:
          reveal_type(x)
          use_int(x)

        reveal_type(x)
        use_int(x)
    |}
    [
      (* Inside except *)
      "Revealed type [-1]: Revealed type for `x` is `typing.Optional[int]` (inferred: `None`).";
      "Incompatible parameter type [6]: In call `use_int`, for 1st positional argument, expected \
       `int` but got `None`.";
      (* After try-except *)
      "Revealed type [-1]: Revealed type for `x` is `typing.Optional[int]`.";
      "Incompatible parameter type [6]: In call `use_int`, for 1st positional argument, expected \
       `int` but got `Optional[int]`.";
    ]
    context


let test_return_from_try_with_finally context =
  assert_type_errors
    {|
      def may_raise() -> int:
        if True:
          raise ValueError()
        return 1

      def test() -> int:
        try:
          x = may_raise()
          return x
        finally:
          print("finally")
    |}
    [ (* No type errors expected *) ]
    context


let test_assign_in_try_and_in_except context =
  assert_type_errors
    {|
      def may_raise() -> int:
        if True:
          raise ValueError()
        return 1

      def test() -> int:
        try:
          x = may_raise()
        except Exception:
          x = 1

        reveal_type(x)
        return x
    |}
    ["Revealed type [-1]: Revealed type for `x` is `int`."]
    context


let test_define_in_try_and_use_after_finally context =
  assert_type_errors
    {|
      def may_raise() -> int:
        if True:
          raise ValueError()
        return 1

      def use_int(x: int) -> None:
        pass

      def test() -> None:
        try:
          x = may_raise()
        finally:
          print("finally")

        reveal_type(x)
        use_int(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `int`."]
    context


let () =
  "tryFinallyRefinement"
  >::: [
         "test_assignment_in_try_with_finally" >:: test_assignment_in_try_with_finally;
         "test_assignment_in_try_with_except" >:: test_assignment_in_try_with_except;
         "test_multiple_assignments_in_try" >:: test_multiple_assignments_in_try;
         "test_try_except_finally_union" >:: test_try_except_finally_union;
         "test_modified_in_try_with_empty_finally" >:: test_modified_in_try_with_empty_finally;
         "test_modified_in_try_conditional_with_empty_finally"
         >:: test_modified_in_try_conditional_with_empty_finally;
         "test_isinstance_refinement_not_in_finally" >:: test_isinstance_refinement_not_in_finally;
         "test_assert_usage_in_finally" >:: test_assert_usage_in_finally;
         "test_try_finally_no_exception" >:: test_try_finally_no_exception;
         "test_not_modified_in_try_with_finally" >:: test_not_modified_in_try_with_finally;
         "test_not_modified_in_try_then_except_uses_pre_try_type"
         >:: test_not_modified_in_try_then_except_uses_pre_try_type;
         "test_not_modified_in_try_with_except_finally"
         >:: test_not_modified_in_try_with_except_finally;
         "test_not_modified_in_try_with_except_else" >:: test_not_modified_in_try_with_except_else;
         "test_not_modified_in_try_with_except_else_finally"
         >:: test_not_modified_in_try_with_except_else_finally;
         "test_modified_in_try_after_raise_with_finally"
         >:: test_modified_in_try_after_raise_with_finally;
         "test_modified_in_try_after_raise_conditional_with_finally"
         >:: test_modified_in_try_after_raise_conditional_with_finally;
         "test_modified_in_try_after_raise_with_finally_reassign"
         >:: test_modified_in_try_after_raise_with_finally_reassign;
         "test_modified_in_try_after_raise_with_finally_return"
         >:: test_modified_in_try_after_raise_with_finally_return;
         "test_modified_in_try_before_raise_with_finally"
         >:: test_modified_in_try_before_raise_with_finally;
         "test_modified_in_try_after_raise_with_except"
         >:: test_modified_in_try_after_raise_with_except;
         "test_modified_in_try_after_raise_conditional_with_except"
         >:: test_modified_in_try_after_raise_conditional_with_except;
         "test_modified_in_try_before_raise_with_except"
         >:: test_modified_in_try_before_raise_with_except;
         "test_modified_in_try_after_raise_with_except_else"
         >:: test_modified_in_try_after_raise_with_except_else;
         "test_modified_in_try_after_raise_conditional_with_except_else"
         >:: test_modified_in_try_after_raise_conditional_with_except_else;
         "test_modified_in_try_after_raise_conditional_then_raise_with_except_else"
         >:: test_modified_in_try_after_raise_conditional_then_raise_with_except_else;
         "test_modified_in_try_after_raise_conditional_then_raise_conditional_with_except_else"
         >:: test_modified_in_try_after_raise_conditional_then_raise_conditional_with_except_else;
         "test_modified_in_try_before_raise_with_except_else"
         >:: test_modified_in_try_before_raise_with_except_else;
         "test_modified_in_try_before_raise_with_except_finally"
         >:: test_modified_in_try_before_raise_with_except_finally;
         "test_modified_in_try_after_raise_with_except_finally"
         >:: test_modified_in_try_after_raise_with_except_finally;
         "test_modified_in_try_after_raise_conditional_with_except_finally"
         >:: test_modified_in_try_after_raise_conditional_with_except_finally;
         "test_modified_in_try_after_raise_conditional_then_raise_with_except_finally"
         >:: test_modified_in_try_after_raise_conditional_then_raise_with_except_finally;
         "test_try_except_sees_pre_try_type" >:: test_try_except_sees_pre_try_type;
         "test_return_from_try_with_finally" >:: test_return_from_try_with_finally;
         "test_assign_in_try_and_in_except" >:: test_assign_in_try_and_in_except;
         "test_define_in_try_and_use_after_finally" >:: test_define_in_try_and_use_after_finally;
       ]
  |> Test.run
