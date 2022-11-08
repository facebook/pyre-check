(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest

let test_ignore_readonly context =
  let assert_type_errors = assert_type_errors ~context ~enable_readonly_analysis:false in
  (* The type checking analysis ignores `ReadOnly`. *)
  assert_type_errors
    {|
      from pyre_extensions import ReadOnly

      def foo(x: ReadOnly[int]) -> int:
        y: int = x
        z: ReadOnly[int] = y
        return z
    |}
    [];
  (* The type checking analysis will check compatibility for the type wrapped by `ReadOnly`. *)
  assert_type_errors
    {|
      from pyre_extensions import ReadOnly

      def foo(x: ReadOnly[int]) -> None:
        y: str = x
    |}
    [
      (* TODO(T130377746): Avoid mixing `ReadOnly` in type errors. *)
      "Incompatible variable type [9]: y is declared to have type `str` but is used as type \
       `pyre_extensions.ReadOnly[int]`.";
    ];
  assert_type_errors
    {|
      from pyre_extensions import ReadOnly
      from typing_extensions import Literal

      def foo(
        always_true: ReadOnly[Literal[True]],
        always_false: ReadOnly[Literal[False]]
      ) -> None:
        x: Literal[True] = always_true or False
        y: Literal[False] = always_false and True
    |}
    [];
  assert_type_errors
    {|
      from pyre_extensions import ReadOnly

      def foo(s: ReadOnly[str]) -> None:
        y: str = s.capitalize()
    |}
    [];
  ()


let test_readonly_configuration_flag context =
  let assert_type_errors_including_readonly =
    assert_type_errors ~context ~enable_readonly_analysis:true
  in
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly

      def main() -> None:
        x: ReadOnly[int] = 42
        y = x
        z: int = y
    |}
    [
      "ReadOnly violation - Incompatible variable type [3001]: z is declared to have readonlyness \
       `Mutable` but is used as readonlyness `ReadOnly`.";
    ];
  (* Test readonly violations at the top level. *)
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly

      x: ReadOnly[int] = 42
      y: int = x
    |}
    [
      "ReadOnly violation - Incompatible variable type [3001]: y is declared to have readonlyness \
       `Mutable` but is used as readonlyness `ReadOnly`.";
    ];
  ()


let test_assignment context =
  let assert_type_errors_including_readonly =
    assert_type_errors ~context ~enable_readonly_analysis:true
  in
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly

      def main() -> None:
        x: ReadOnly[int] = 42
        y = x
        z: int = y
    |}
    [
      "ReadOnly violation - Incompatible variable type [3001]: z is declared to have readonlyness \
       `Mutable` but is used as readonlyness `ReadOnly`.";
    ];
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly

      def main() -> None:
        x = 42
        y = x
        z: ReadOnly[int] = y
    |}
    [];
  (* Treat constants, such as `42` or `...`, as assignable to mutable types. *)
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly

      class Foo:
        # This is treated as `some_attribute: int = ...`.
        some_attribute: int

      def main() -> None:
        x: int = 42
    |}
    [
      "Uninitialized attribute [13]: Attribute `some_attribute` is declared in class `Foo` to have \
       type `int` but is never initialized.";
    ];
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly

      class Foo:
        mutable_attribute: int

      def main() -> None:
        readonly_foo: ReadOnly[Foo]
        mutable_foo: Foo

        x1: int = readonly_foo.mutable_attribute
        x2: int = mutable_foo.mutable_attribute
    |}
    [
      "Uninitialized attribute [13]: Attribute `mutable_attribute` is declared in class `Foo` to \
       have type `int` but is never initialized.";
      "ReadOnly violation - Incompatible variable type [3001]: x1 is declared to have readonlyness \
       `Mutable` but is used as readonlyness `ReadOnly`.";
    ];
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly

      class Bar:
        bar_attribute: int = 42

      class Foo:
        foo_attribute: Bar = Bar()

      def main() -> None:
        readonly_foo: ReadOnly[Foo]
        mutable_foo: Foo

        x1: int = readonly_foo.foo_attribute.bar_attribute
        x2: int = mutable_foo.foo_attribute.bar_attribute
    |}
    [
      "ReadOnly violation - Incompatible variable type [3001]: x1 is declared to have readonlyness \
       `Mutable` but is used as readonlyness `ReadOnly`.";
    ];
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly

      class Foo:
        readonly_attribute: ReadOnly[int] = 42

      def main() -> None:
        readonly_foo: ReadOnly[Foo]
        mutable_foo: Foo

        x1: int = readonly_foo.readonly_attribute
        x2: int = mutable_foo.readonly_attribute
    |}
    [
      "ReadOnly violation - Incompatible variable type [3001]: x1 is declared to have readonlyness \
       `Mutable` but is used as readonlyness `ReadOnly`.";
      "ReadOnly violation - Incompatible variable type [3001]: x2 is declared to have readonlyness \
       `Mutable` but is used as readonlyness `ReadOnly`.";
    ];
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly

      class Foo:
        some_attribute: int = 42

      def main() -> None:
        readonly_foo: ReadOnly[Foo]
        readonly_foo.some_attribute = 99
    |}
    [
      "ReadOnly violation - Assigning to readonly attribute [3003]: Cannot assign to attribute \
       `some_attribute` since it is readonly";
    ];
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly

      class Foo:
        readonly_attribute: ReadOnly[int] = 42
        mutable_attribute: int = 42

      def main() -> None:
        mutable_foo: Foo
        mutable_foo.readonly_attribute = 99
        mutable_foo.mutable_attribute = 99
    |}
    [
      "ReadOnly violation - Assigning to readonly attribute [3003]: Cannot assign to attribute \
       `readonly_attribute` since it is readonly";
    ];
  (* Technically, reassigning to a variable doesn't mutate it. So, we don't emit an error in this
     case. *)
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly

      def main() -> None:
        x: ReadOnly[int] = 42
        x = 99
    |}
    [];
  ()


let test_function_call context =
  let assert_type_errors_including_readonly =
    assert_type_errors ~context ~enable_readonly_analysis:true
  in
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly

      def expect_mutable_and_readonly(x: int, y: ReadOnly[int]) -> ReadOnly[int]: ...

      def main() -> None:
        x: ReadOnly[int] = 42
        y: int = expect_mutable_and_readonly(x, x)
    |}
    [
      "ReadOnly violation - Incompatible variable type [3001]: y is declared to have readonlyness \
       `Mutable` but is used as readonlyness `ReadOnly`.";
      "ReadOnly violation - Incompatible parameter type [3002]: In call \
       `test.expect_mutable_and_readonly`, for 1st positional argument, expected `Mutable` but got \
       `ReadOnly`.";
    ];
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly

      def foo(x: int) -> ReadOnly[int]: ...

      def main(x: int) -> None:
        y: int = foo(foo(x))
    |}
    [
      "ReadOnly violation - Incompatible variable type [3001]: y is declared to have readonlyness \
       `Mutable` but is used as readonlyness `ReadOnly`.";
      "ReadOnly violation - Incompatible parameter type [3002]: In call `test.foo`, for 1st \
       positional argument, expected `Mutable` but got `ReadOnly`.";
    ];
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly

      def expect_mutable(x: int) -> ReadOnly[int]: ...

      def main() -> None:
        x: ReadOnly[int] = 42
        expect_mutable(x)
    |}
    [
      "ReadOnly violation - Incompatible parameter type [3002]: In call `test.expect_mutable`, for \
       1st positional argument, expected `Mutable` but got `ReadOnly`.";
    ];
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly

      class Foo:
        def return_readonly(self, x: int) -> ReadOnly[int]: ...

      def main() -> None:
        foo: Foo
        x: ReadOnly[int]
        y: int = foo.return_readonly(x)
    |}
    [
      "ReadOnly violation - Incompatible variable type [3001]: y is declared to have readonlyness \
       `Mutable` but is used as readonlyness `ReadOnly`.";
      "ReadOnly violation - Incompatible parameter type [3002]: In call \
       `test.Foo.return_readonly`, for 1st positional argument, expected `Mutable` but got \
       `ReadOnly`.";
    ];
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly

      class Foo:
        def return_readonly(self, x: int) -> ReadOnly[int]: ...

      def return_foo(x: int) -> Foo: ...

      def main() -> None:
        foo: Foo
        x: ReadOnly[int]
        y: int = return_foo(x).return_readonly(x)
    |}
    [
      "ReadOnly violation - Incompatible variable type [3001]: y is declared to have readonlyness \
       `Mutable` but is used as readonlyness `ReadOnly`.";
      "ReadOnly violation - Incompatible parameter type [3002]: In call `test.return_foo`, for 1st \
       positional argument, expected `Mutable` but got `ReadOnly`.";
      "ReadOnly violation - Incompatible parameter type [3002]: In call \
       `test.Foo.return_readonly`, for 1st positional argument, expected `Mutable` but got \
       `ReadOnly`.";
    ];
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly

      class Foo:
        def expect_readonly_self(self: ReadOnly[Foo], x: int) -> None: ...

      def main() -> None:
        readonly_foo: ReadOnly[Foo]
        mutable_foo: Foo
        x: int
        readonly_foo.expect_readonly_self(x)
        mutable_foo.expect_readonly_self(x)
    |}
    [];
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly

      class Foo:
        def expect_mutable_self(self, x: int) -> None: ...

      def main() -> None:
        readonly_foo: ReadOnly[Foo]
        mutable_foo: Foo
        x: int
        readonly_foo.expect_mutable_self(x)
        mutable_foo.expect_mutable_self(x)
    |}
    [
      "ReadOnly violation - Incompatible parameter type [3002]: In call \
       `test.Foo.expect_mutable_self`, for 0th positional argument, expected `Mutable` but got \
       `ReadOnly`.";
    ];
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly

      def expect_positional_mutable_and_readonly(x: int, y: ReadOnly[int], /) -> None:
        reveal_type(x)
        reveal_type(y)

      def main(my_readonly: ReadOnly[int]) -> None:
        expect_positional_mutable_and_readonly(my_readonly, my_readonly)
    |}
    [
      "ReadOnly - Revealed type [3004]: Revealed type for `x` is Mutable.";
      "Revealed type [-1]: Revealed type for `x` is `int`.";
      "ReadOnly - Revealed type [3004]: Revealed type for `y` is ReadOnly.";
      "Revealed type [-1]: Revealed type for `y` is `pyre_extensions.ReadOnly[int]`.";
      "ReadOnly violation - Incompatible parameter type [3002]: In call \
       `test.expect_positional_mutable_and_readonly`, for 1st positional argument, expected \
       `Mutable` but got `ReadOnly`.";
    ];
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly

      def expect_keyword_only( *, x: int, y: ReadOnly[int]) -> None:
        reveal_type(x)
        reveal_type(y)

      def main(my_readonly: ReadOnly[int], my_mutable: int) -> None:
        expect_keyword_only(y=my_mutable, x=my_readonly)
    |}
    [
      "ReadOnly - Revealed type [3004]: Revealed type for `x` is Mutable.";
      "Revealed type [-1]: Revealed type for `x` is `int`.";
      "ReadOnly - Revealed type [3004]: Revealed type for `y` is ReadOnly.";
      "Revealed type [-1]: Revealed type for `y` is `pyre_extensions.ReadOnly[int]`.";
      "ReadOnly violation - Incompatible parameter type [3002]: In call \
       `test.expect_keyword_only`, for argument `x`, expected `Mutable` but got `ReadOnly`.";
    ];
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly

      def expect_kwargs( **kwargs: int) -> None: ...

      def main(my_readonly: ReadOnly[int], my_mutable: int, **kwargs: ReadOnly[int]) -> None:
        expect_kwargs(y=my_mutable, x=my_readonly)
        expect_kwargs( **kwargs)
    |}
    [
      "ReadOnly violation - Incompatible parameter type [3002]: In call `test.expect_kwargs`, for \
       argument `x`, expected `Mutable` but got `ReadOnly`.";
      "ReadOnly violation - Incompatible parameter type [3002]: In call `test.expect_kwargs`, for \
       1st positional argument, expected `Mutable` but got `ReadOnly`.";
    ];
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly
      from typing import Callable

      def main(my_readonly: ReadOnly[int], undefined: Callable[..., ReadOnly[int]]) -> None:
        x: int = undefined(my_readonly)
    |}
    [
      "ReadOnly violation - Incompatible variable type [3001]: x is declared to have readonlyness \
       `Mutable` but is used as readonlyness `ReadOnly`.";
    ];
  ()


let test_await context =
  let assert_type_errors_including_readonly =
    assert_type_errors ~context ~enable_readonly_analysis:true
  in
  (* TODO(T130377746): This should have a readonly violation error. *)
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly

      async def return_readonly() -> ReadOnly[int]: ...

      async def main() -> None:
        y: int = await return_readonly()
    |}
    [];
  ()


let test_parameters context =
  let assert_type_errors_including_readonly =
    assert_type_errors ~context ~enable_readonly_analysis:true
  in
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly

      def main(my_mutable: int, my_readonly: ReadOnly[int]) -> None:
        y: int = my_readonly
        y2: ReadOnly[int] = my_mutable
    |}
    [
      "ReadOnly violation - Incompatible variable type [3001]: y is declared to have readonlyness \
       `Mutable` but is used as readonlyness `ReadOnly`.";
    ];
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly

      def main(unannotated) -> None:
        y: int = unannotated
    |}
    ["Missing parameter annotation [2]: Parameter `unannotated` has no type specified."];
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly

      def main(x: ReadOnly[int], /, y: ReadOnly[int], *, z: ReadOnly[int]) -> None:
        y1: int = x
        y2: int = y
        y3: int = z
    |}
    [
      "ReadOnly violation - Incompatible variable type [3001]: y1 is declared to have readonlyness \
       `Mutable` but is used as readonlyness `ReadOnly`.";
      "ReadOnly violation - Incompatible variable type [3001]: y2 is declared to have readonlyness \
       `Mutable` but is used as readonlyness `ReadOnly`.";
      "ReadOnly violation - Incompatible variable type [3001]: y3 is declared to have readonlyness \
       `Mutable` but is used as readonlyness `ReadOnly`.";
    ];
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly

      def main(*args: ReadOnly[int], **kwargs: ReadOnly[int]) -> None:
        reveal_type(args)
        reveal_type(kwargs)
    |}
    [
      "ReadOnly - Revealed type [3004]: Revealed type for `args` is ReadOnly.";
      "Revealed type [-1]: Revealed type for `args` is \
       `typing.Tuple[pyre_extensions.ReadOnly[int], ...]`.";
      "ReadOnly - Revealed type [3004]: Revealed type for `kwargs` is ReadOnly.";
      "Revealed type [-1]: Revealed type for `kwargs` is `typing.Dict[str, \
       pyre_extensions.ReadOnly[int]]`.";
    ];
  (* Check for errors in constructing the default value of a parameter. *)
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly

      def return_readonly() -> ReadOnly[int]: ...

      def expect_mutable(x: int) -> int: ...

      def main(x: int = expect_mutable(return_readonly())) -> None:
        pass
    |}
    [
      "ReadOnly violation - Incompatible parameter type [3002]: In call `test.expect_mutable`, for \
       1st positional argument, expected `Mutable` but got `ReadOnly`.";
    ];
  (* Don't consider `x` in scope for the default value of parameter `y`. We don't need to emit an
     error here, because the type checking analysis will. *)
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly

      def foo(x: ReadOnly[int], y: int = x) -> None:
        pass
    |}
    [
      "Incompatible variable type [9]: y is declared to have type `int` but is used as type \
       `unknown`.";
      "Unbound name [10]: Name `x` is used but not defined in the current scope.";
    ];
  ()


let test_reveal_type context =
  let assert_type_errors_including_readonly =
    assert_type_errors ~context ~enable_readonly_analysis:true
  in
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly

      def main(my_mutable: int, my_readonly: ReadOnly[int]) -> None:
        y1 = my_readonly
        y2 = my_mutable
        reveal_type(y1)
        reveal_type(y2)
    |}
    [
      "ReadOnly - Revealed type [3004]: Revealed type for `y1` is ReadOnly.";
      "Revealed type [-1]: Revealed type for `y1` is `pyre_extensions.ReadOnly[int]`.";
      "ReadOnly - Revealed type [3004]: Revealed type for `y2` is Mutable.";
      "Revealed type [-1]: Revealed type for `y2` is `int`.";
    ];
  ()


let test_format_string context =
  let assert_type_errors_including_readonly =
    assert_type_errors ~context ~enable_readonly_analysis:true
  in
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly

      def expect_mutable(x: int) -> ReadOnly[int]: ...

      def main(my_readonly: ReadOnly[int], my_mutable: int) -> None:
        s = f"hello, {expect_mutable(my_readonly)}, {expect_mutable(my_mutable)}"
    |}
    [
      "ReadOnly violation - Incompatible parameter type [3002]: In call `test.expect_mutable`, for \
       1st positional argument, expected `Mutable` but got `ReadOnly`.";
    ];
  ()


let () =
  "readOnly"
  >::: [
         "ignore" >:: test_ignore_readonly;
         "readonly_configuration_flag" >:: test_readonly_configuration_flag;
         "assignment" >:: test_assignment;
         "function_call" >:: test_function_call;
         "await" >:: test_await;
         "parameters" >:: test_parameters;
         "reveal_type" >:: test_reveal_type;
         "format_string" >:: test_format_string;
       ]
  |> Test.run
