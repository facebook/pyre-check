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
  assert_type_errors
    {|
      from pyre_extensions import ReadOnly

      def foo(x: ReadOnly[int]) -> int:
        y: int = x
        z: ReadOnly[int] = y
        return z
    |}
    [
      "ReadOnly violation - Incompatible variable type [3001]: y is declared to have type `int` \
       but is used as type `pyre_extensions.ReadOnly[int]`.";
    ];
  (* The type checking analysis will check compatibility for the type wrapped by `ReadOnly`. *)
  assert_type_errors
    {|
      from pyre_extensions import ReadOnly

      def foo(x: ReadOnly[int]) -> None:
        y: str = x
    |}
    [
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
    [
      "ReadOnly violation - Incompatible variable type [3001]: x is declared to have type \
       `typing_extensions.Literal[True]` but is used as type \
       `pyre_extensions.ReadOnly[typing_extensions.Literal[True]]`.";
      "ReadOnly violation - Incompatible variable type [3001]: y is declared to have type \
       `typing_extensions.Literal[False]` but is used as type \
       `pyre_extensions.ReadOnly[typing_extensions.Literal[False]]`.";
    ];
  assert_type_errors
    {|
      from pyre_extensions import ReadOnly

      def foo(s: ReadOnly[str]) -> None:
        y: str = s.capitalize()
    |}
    [
      "ReadOnly violation - Calling mutating method on readonly type [3005]: Method \
       `str.capitalize` may modify its object. Cannot call it on readonly expression `s` of type \
       `pyre_extensions.ReadOnly[str]`.";
    ];
  ()


let test_readonly_configuration_flag context =
  let assert_type_errors_including_readonly =
    assert_type_errors ~context ~enable_readonly_analysis:false
  in
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly

      def main() -> None:
        x: ReadOnly[int]
        y = x
        z: int = y
    |}
    [
      "ReadOnly violation - Incompatible variable type [3001]: z is declared to have type `int` \
       but is used as type `pyre_extensions.ReadOnly[int]`.";
    ];
  (* Test readonly violations at the top level. *)
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly

      x: ReadOnly[int]
      y: int = x
    |}
    [
      "ReadOnly violation - Incompatible variable type [3001]: y is declared to have type `int` \
       but is used as type `pyre_extensions.ReadOnly[int]`.";
    ];
  ()


let test_assignment context =
  let assert_type_errors_including_readonly =
    assert_type_errors ~context ~enable_readonly_analysis:false
  in
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly

      def main() -> None:
        x: ReadOnly[int]
        y = x
        z: int = y
    |}
    [
      "ReadOnly violation - Incompatible variable type [3001]: z is declared to have type `int` \
       but is used as type `pyre_extensions.ReadOnly[int]`.";
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
        mutable_attribute: int = 42

      def main() -> None:
        readonly_foo: ReadOnly[Foo]
        mutable_foo: Foo

        x1: int = readonly_foo.mutable_attribute
        x2: int = mutable_foo.mutable_attribute
    |}
    [
      "ReadOnly violation - Incompatible variable type [3001]: x1 is declared to have type `int` \
       but is used as type `pyre_extensions.ReadOnly[int]`.";
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
      "ReadOnly violation - Incompatible variable type [3001]: x1 is declared to have type `int` \
       but is used as type `pyre_extensions.ReadOnly[int]`.";
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
      "ReadOnly violation - Incompatible variable type [3001]: x1 is declared to have type `int` \
       but is used as type `pyre_extensions.ReadOnly[int]`.";
      "ReadOnly violation - Incompatible variable type [3001]: x2 is declared to have type `int` \
       but is used as type `pyre_extensions.ReadOnly[int]`.";
    ];
  (* Handle attribute types that have both `ReadOnly[...]` and `Type[...]`. *)
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly
      from typing import Type

      class Foo:
        some_attribute: int = 42

      def main() -> None:
        readonly_type_foo: ReadOnly[Type[Foo]]
        type_readonly_foo: Type[ReadOnly[Foo]]
        x: int = readonly_type_foo.some_attribute
        y: int = type_readonly_foo.some_attribute
    |}
    [
      "ReadOnly violation - Incompatible variable type [3001]: x is declared to have type `int` \
       but is used as type `pyre_extensions.ReadOnly[int]`.";
      "ReadOnly violation - Incompatible variable type [3001]: y is declared to have type `int` \
       but is used as type `pyre_extensions.ReadOnly[int]`.";
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
       `some_attribute` since it is readonly.";
    ];
  (* It is ok to reassign to a readonly attribute of a mutable object, since we aren't actually
     modifying the value that has been marked as readonly. *)
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
    [];
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly

      class Foo:
        readonly_attribute: ReadOnly[int] = 42
        mutable_attribute: int = 42

        def some_method(self) -> None:
          self.readonly_attribute = 99
          self.mutable_attribute = 99
    |}
    [];
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly

      class Foo:
        def __init__(self, x: ReadOnly[int], some_bool: bool) -> None:
          self.x = x

          if some_bool:
            self.x = 99
    |}
    [];
  (* TODO(T130377746): Recognize attribute for `ReadOnly[Type[Foo]]` when it is the target of an
     assignment. *)
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly
      from typing import Type

      class Foo:
        some_attribute: int = 42

      def main() -> None:
        readonly_type_foo: ReadOnly[Type[Foo]]
        readonly_type_foo.some_attribute = 99

        type_readonly_foo: Type[ReadOnly[Foo]]
        type_readonly_foo.some_attribute = 99
    |}
    [
      "Undefined attribute [16]: `pyre_extensions.ReadOnly[Type[Foo]]` has no attribute \
       `some_attribute`.";
    ];
  (* TODO(T130377746): Emit readonly violation error when assigning to attribute of
     `Type[ReadOnly[Foo]]`. *)
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly
      from typing import Type

      class Foo:
        some_attribute: int = 42

      def main() -> None:
        type_readonly_foo: Type[ReadOnly[Foo]]
        type_readonly_foo.some_attribute = 99
    |}
    [];
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
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly
      from typing import List

      def main(xs: List[ReadOnly[int]]) -> None:
        y: List[int] = xs
    |}
    [
      "ReadOnly violation - Incompatible variable type [3001]: y is declared to have type \
       `typing.List[int]` but is used as type `typing.List[pyre_extensions.ReadOnly[int]]`.";
    ];
  (* We cannot assign to any attribute of a readonly object. *)
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly

      class Foo:
        mutable_attribute: int = 42

      def main(readonly_foo: ReadOnly[Foo]) -> None:
        readonly_foo.mutable_attribute = 99
    |}
    [
      "ReadOnly violation - Assigning to readonly attribute [3003]: Cannot assign to attribute \
       `mutable_attribute` since it is readonly.";
    ];
  ()


let test_function_call context =
  let assert_type_errors_including_readonly =
    assert_type_errors ~context ~enable_readonly_analysis:false
  in
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly

      def expect_mutable_and_readonly(x: int, y: ReadOnly[int]) -> ReadOnly[int]: ...

      def main() -> None:
        x: ReadOnly[int]
        y: int = expect_mutable_and_readonly(x, x)
    |}
    [
      "ReadOnly violation - Incompatible variable type [3001]: y is declared to have type `int` \
       but is used as type `pyre_extensions.ReadOnly[int]`.";
      "ReadOnly violation - Incompatible parameter type [3002]: In call \
       `test.expect_mutable_and_readonly`, for 1st positional argument, expected `int` but got \
       `pyre_extensions.ReadOnly[int]`.";
    ];
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly

      def foo(x: int) -> ReadOnly[int]: ...

      def main(x: int) -> None:
        y: int = foo(foo(x))
    |}
    [
      "ReadOnly violation - Incompatible variable type [3001]: y is declared to have type `int` \
       but is used as type `pyre_extensions.ReadOnly[int]`.";
      "ReadOnly violation - Incompatible parameter type [3002]: In call `test.foo`, for 1st \
       positional argument, expected `int` but got `pyre_extensions.ReadOnly[int]`.";
    ];
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly

      def expect_mutable(x: int) -> ReadOnly[int]: ...

      def main() -> None:
        x: ReadOnly[int]
        expect_mutable(x)
    |}
    [
      "ReadOnly violation - Incompatible parameter type [3002]: In call `test.expect_mutable`, for \
       1st positional argument, expected `int` but got `pyre_extensions.ReadOnly[int]`.";
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
      "ReadOnly violation - Incompatible variable type [3001]: y is declared to have type `int` \
       but is used as type `pyre_extensions.ReadOnly[int]`.";
      "ReadOnly violation - Incompatible parameter type [3002]: In call \
       `test.Foo.return_readonly`, for 1st positional argument, expected `int` but got \
       `pyre_extensions.ReadOnly[int]`.";
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
      "ReadOnly violation - Incompatible variable type [3001]: y is declared to have type `int` \
       but is used as type `pyre_extensions.ReadOnly[int]`.";
      "ReadOnly violation - Incompatible parameter type [3002]: In call `test.return_foo`, for 1st \
       positional argument, expected `int` but got `pyre_extensions.ReadOnly[int]`.";
      "ReadOnly violation - Incompatible parameter type [3002]: In call \
       `test.Foo.return_readonly`, for 1st positional argument, expected `int` but got \
       `pyre_extensions.ReadOnly[int]`.";
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
      "ReadOnly violation - Calling mutating method on readonly type [3005]: Method \
       `test.Foo.expect_mutable_self` may modify its object. Cannot call it on readonly expression \
       `readonly_foo` of type `pyre_extensions.ReadOnly[Foo]`.";
    ];
  (* A method with readonly `self` can be called on either mutable or readonly objects. However, the
     method itself cannot call other mutating methods. *)
  assert_type_errors_including_readonly
    ~include_line_numbers:true
    {|
      from pyre_extensions import ReadOnly

      class Foo:
        def expect_mutable_self(self, x: int) -> None: ...

        def expect_readonly_self(self: ReadOnly["Foo"], x: int) -> None:
          self.expect_mutable_self(x)


      def main() -> None:
        readonly_foo: ReadOnly[Foo]
        mutable_foo: Foo
        x: int
        readonly_foo.expect_readonly_self(x)
        mutable_foo.expect_readonly_self(x)
    |}
    [
      "8: ReadOnly violation - Calling mutating method on readonly type [3005]: Method \
       `test.Foo.expect_mutable_self` may modify its object. Cannot call it on readonly expression \
       `self` of type `pyre_extensions.ReadOnly[Foo]`.";
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
      "Revealed type [-1]: Revealed type for `x` is `int`.";
      "Revealed type [-1]: Revealed type for `y` is `pyre_extensions.ReadOnly[int]`.";
      "ReadOnly violation - Incompatible parameter type [3002]: In call \
       `test.expect_positional_mutable_and_readonly`, for 1st positional argument, expected `int` \
       but got `pyre_extensions.ReadOnly[int]`.";
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
      "Revealed type [-1]: Revealed type for `x` is `int`.";
      "Revealed type [-1]: Revealed type for `y` is `pyre_extensions.ReadOnly[int]`.";
      "ReadOnly violation - Incompatible parameter type [3002]: In call \
       `test.expect_keyword_only`, for argument `x`, expected `int` but got \
       `pyre_extensions.ReadOnly[int]`.";
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
       argument `x`, expected `int` but got `pyre_extensions.ReadOnly[int]`.";
      "ReadOnly violation - Incompatible parameter type [3002]: In call `test.expect_kwargs`, for \
       1st positional argument, expected `int` but got `pyre_extensions.ReadOnly[int]`.";
    ];
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly
      from typing import Callable

      def main(my_readonly: ReadOnly[int], undefined: Callable[..., ReadOnly[int]]) -> None:
        x: int = undefined(my_readonly)
    |}
    [
      "ReadOnly violation - Incompatible variable type [3001]: x is declared to have type `int` \
       but is used as type `pyre_extensions.ReadOnly[int]`.";
    ];
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly

      def expect_mutable(x: object) -> None:
        # pyre-ignore[16]: `object` has no attribute `some_attribute`.
        x.some_attribute = 42

      def main(readonly: ReadOnly[int]) -> None:
        expect_mutable(readonly)
    |}
    [
      "ReadOnly violation - Incompatible parameter type [3002]: In call `test.expect_mutable`, for \
       1st positional argument, expected `object` but got `pyre_extensions.ReadOnly[int]`.";
    ];
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly

      def expect_union(x: int | ReadOnly[str]) -> None: ...

      def main(readonly_string: ReadOnly[str]) -> None:
        expect_union(readonly_string)
    |}
    [];
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly
      from typing import List

      def expect_list_mutable(xs: List[ReadOnly[int]]) -> None: ...

      def main(xs: List[int]) -> None:
        expect_list_mutable(xs)
    |}
    [
      "ReadOnly violation - Incompatible parameter type [3002]: In call \
       `test.expect_list_mutable`, for 1st positional argument, expected \
       `typing.List[pyre_extensions.ReadOnly[int]]` but got `typing.List[int]`.";
    ];
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly
      from typing import List

      def expect_list_list_mutable(xs: List[List[ReadOnly[int]]]) -> None: ...

      def main(xs: List[List[int]]) -> None:
        expect_list_list_mutable(xs)
    |}
    [
      "ReadOnly violation - Incompatible parameter type [3002]: In call \
       `test.expect_list_list_mutable`, for 1st positional argument, expected \
       `typing.List[typing.List[pyre_extensions.ReadOnly[int]]]` but got \
       `typing.List[typing.List[int]]`.";
    ];
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly
      from typing import Union

      def expect_union(xs: Union[int, str]) -> None: ...

      def main(readonly_int: ReadOnly[int] | ReadOnly[str]) -> None:
        expect_union(readonly_int)
    |}
    [
      "ReadOnly violation - Incompatible parameter type [3002]: In call `test.expect_union`, for \
       1st positional argument, expected `typing.Union[int, str]` but got \
       `typing.Union[pyre_extensions.ReadOnly[int], pyre_extensions.ReadOnly[str]]`.";
    ];
  assert_type_errors_including_readonly
    {|
      from typing import TypeVar
      from pyre_extensions import ReadOnly
      from typing_extensions import Self

      class Foo:
          def mutable_method(self) -> None: ...

          def readonly_method(self: ReadOnly[Self]) -> None:
            self.mutable_method()
      |}
    [
      "ReadOnly violation - Calling mutating method on readonly type [3005]: Method \
       `test.Foo.mutable_method` may modify its object. Cannot call it on readonly expression \
       `self` of type `pyre_extensions.ReadOnly[Variable[_Self_test_Foo__ (bound to Foo)]]`.";
    ];
  ()


let test_await context =
  let assert_type_errors_including_readonly =
    assert_type_errors ~context ~enable_readonly_analysis:false
  in
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly

      async def return_readonly() -> ReadOnly[int]: ...

      async def main() -> None:
        y: int = await return_readonly()
    |}
    [
      "ReadOnly violation - Incompatible variable type [3001]: y is declared to have type `int` \
       but is used as type `pyre_extensions.ReadOnly[int]`.";
    ];
  ()


let test_parameters context =
  let assert_type_errors_including_readonly =
    assert_type_errors ~context ~enable_readonly_analysis:false
  in
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly

      def main(my_mutable: int, my_readonly: ReadOnly[int]) -> None:
        y: int = my_readonly
        y2: ReadOnly[int] = my_mutable
    |}
    [
      "ReadOnly violation - Incompatible variable type [3001]: y is declared to have type `int` \
       but is used as type `pyre_extensions.ReadOnly[int]`.";
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
      "ReadOnly violation - Incompatible variable type [3001]: y1 is declared to have type `int` \
       but is used as type `pyre_extensions.ReadOnly[int]`.";
      "ReadOnly violation - Incompatible variable type [3001]: y2 is declared to have type `int` \
       but is used as type `pyre_extensions.ReadOnly[int]`.";
      "ReadOnly violation - Incompatible variable type [3001]: y3 is declared to have type `int` \
       but is used as type `pyre_extensions.ReadOnly[int]`.";
    ];
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly

      def main(*args: ReadOnly[int], **kwargs: ReadOnly[int]) -> None:
        reveal_type(args)
        reveal_type(kwargs)
    |}
    [
      "Revealed type [-1]: Revealed type for `args` is \
       `typing.Tuple[pyre_extensions.ReadOnly[int], ...]`.";
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
       1st positional argument, expected `int` but got `pyre_extensions.ReadOnly[int]`.";
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
    assert_type_errors ~context ~enable_readonly_analysis:false
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
      "Revealed type [-1]: Revealed type for `y1` is `pyre_extensions.ReadOnly[int]`.";
      "Revealed type [-1]: Revealed type for `y2` is `int`.";
    ];
  ()


let test_format_string context =
  let assert_type_errors_including_readonly =
    assert_type_errors ~context ~enable_readonly_analysis:false
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
       1st positional argument, expected `int` but got `pyre_extensions.ReadOnly[int]`.";
    ];
  ()


let test_generic_types context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from pyre_extensions import ReadOnly
      from typing import Generic, List, TypeVar

      T = TypeVar("T")

      def identity(x: T) -> T: ...

      def main(readonly: ReadOnly[int]) -> None:
        y = identity(readonly)
        reveal_type(y)
    |}
    ["Revealed type [-1]: Revealed type for `y` is `pyre_extensions.ReadOnly[int]`."];
  assert_type_errors
    {|
      from pyre_extensions import ReadOnly
      from typing import Generic, List, TypeVar

      T = TypeVar("T")

      class Foo(Generic[T]):
        def get_element(self) -> T: ...

      def main(foo: Foo[ReadOnly[int]]) -> None:
        x = foo.get_element()
        reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `pyre_extensions.ReadOnly[int]`."];
  assert_type_errors
    {|
      from pyre_extensions import ReadOnly
      from typing import List

      def main(xs: List[ReadOnly[int]]) -> None:
        x = xs[0]
        reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `pyre_extensions.ReadOnly[int]`."];
  assert_type_errors
    {|
      from pyre_extensions import ReadOnly
      from typing import List

      def main(xs: List[ReadOnly[int]]) -> None:
        x = xs[0]
        reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `pyre_extensions.ReadOnly[int]`."];
  ()


let test_refinement context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from pyre_extensions import ReadOnly
      from typing import Optional

      def main(optional_readonly: ReadOnly[Optional[int]]) -> None:
        if optional_readonly:
          reveal_type(optional_readonly)
        else:
          reveal_type(optional_readonly)
    |}
    [
      "Revealed type [-1]: Revealed type for `optional_readonly` is \
       `Optional[pyre_extensions.ReadOnly[int]]` (inferred: `pyre_extensions.ReadOnly[int]`).";
      "Revealed type [-1]: Revealed type for `optional_readonly` is \
       `Optional[pyre_extensions.ReadOnly[int]]`.";
    ];
  assert_type_errors
    {|
      from pyre_extensions import ReadOnly
      from typing import Optional

      def main(optional_readonly: ReadOnly[Optional[int]]) -> None:
        x = optional_readonly if optional_readonly else 99
        reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `pyre_extensions.ReadOnly[int]`."];
  assert_type_errors
    {|
      from pyre_extensions import ReadOnly

      class Base: ...

      class Child(Base): ...

      def main(x: ReadOnly[Base | int]) -> None:
        if isinstance(x, Child):
          reveal_type(x)
        else:
          reveal_type(x)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `pyre_extensions.ReadOnly[Child]`.";
      "Revealed type [-1]: Revealed type for `x` is `typing.Union[pyre_extensions.ReadOnly[Base], \
       pyre_extensions.ReadOnly[int]]`.";
    ];
  (* If the variable has type `Top`, don't pollute the output with `ReadOnly` type. *)
  assert_type_errors
    {|
      from unknown_module import Foo

      variable_with_type_top: Foo

      def main() -> None:
        reveal_type(variable_with_type_top)

        if isinstance(variable_with_type_top, str):
          reveal_type(variable_with_type_top)
    |}
    [
      (* This is merely one way to get a variable of type Top. *)
      "Undefined import [21]: Could not find a module corresponding to import `unknown_module`.";
      "Undefined or invalid type [11]: Annotation `Foo` is not defined as a type.";
      "Revealed type [-1]: Revealed type for `variable_with_type_top` is `unknown`.";
      "Revealed type [-1]: Revealed type for `variable_with_type_top` is `str`.";
    ];
  assert_type_errors
    {|
      from pyre_extensions import ReadOnly

      class Foo: ...

      def main(x: ReadOnly[Foo] | int) -> None:
        if isinstance(x, Foo):
          reveal_type(x)
        else:
          reveal_type(x)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `pyre_extensions.ReadOnly[Foo]`.";
      "Revealed type [-1]: Revealed type for `x` is `int`.";
    ];
  assert_type_errors
    {|
      from pyre_extensions import ReadOnly

      class Foo: ...

      def main(x: ReadOnly[Foo] | int) -> None:
        if not isinstance(x, Foo):
          reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `int`."];
  ()


let test_captured_variable_for_specially_decorated_functions context =
  let assert_type_errors = assert_type_errors ~context in
  (* Parameter captured in a nested zone entrypoint should be marked as readonly. *)
  assert_type_errors
    {|
      from pyre_extensions import ReadOnly
      from readonly_stubs_for_testing import readonly_entrypoint

      class Foo:
        x: int = 42

      def main(parameter: Foo) -> None:

        @readonly_entrypoint
        def nested() -> None:
          reveal_type(parameter)
          parameter.x = 99

        not_captured = parameter
        reveal_type(not_captured)
    |}
    [
      "Revealed type [-1]: Revealed type for `parameter` is `pyre_extensions.ReadOnly[Foo]`.";
      "ReadOnly violation - Assigning to readonly attribute [3003]: Cannot assign to attribute `x` \
       since it is readonly.\n\
       Note that this is a zone entrypoint and any captured variables are treated as readonly";
      "Revealed type [-1]: Revealed type for `not_captured` is `Foo`.";
    ];
  (* Outer local variable should be marked as readonly within the entrypoint. *)
  assert_type_errors
    {|
      from pyre_extensions import ReadOnly
      from readonly_stubs_for_testing import readonly_entrypoint

      class Foo:
        x: int = 42

      def main() -> None:
        local_variable: Foo = Foo()

        @readonly_entrypoint
        def nested() -> None:
          reveal_type(local_variable)
          local_variable.x = 99
    |}
    [
      "Revealed type [-1]: Revealed type for `local_variable` is `pyre_extensions.ReadOnly[Foo]`.";
      "ReadOnly violation - Assigning to readonly attribute [3003]: Cannot assign to attribute `x` \
       since it is readonly.\n\
       Note that this is a zone entrypoint and any captured variables are treated as readonly";
    ];
  assert_type_errors
    {|
      from pyre_extensions import ReadOnly
      from readonly_stubs_for_testing import readonly_entrypoint

      class Foo:
        x: int = 42

      def main() -> None:
        local_variable = Foo()

        @readonly_entrypoint
        def nested() -> None:
          reveal_type(local_variable)
          local_variable.x = 99
    |}
    [
      "Missing annotation for captured variable [53]: Captured variable `local_variable` is not \
       annotated.";
      "Revealed type [-1]: Revealed type for `local_variable` is \
       `pyre_extensions.ReadOnly[typing.Any]`.";
    ];
  (* `self` captured in a nested entrypoint should be marked as readonly. *)
  assert_type_errors
    {|
      from pyre_extensions import ReadOnly
      from readonly_stubs_for_testing import readonly_entrypoint

      class Foo:
        x: int = 99

        def some_method(self) -> None:

          @readonly_entrypoint
          def nested() -> None:
            reveal_type(self)
            self.x = 99
    |}
    [
      "Revealed type [-1]: Revealed type for `self` is `pyre_extensions.ReadOnly[Foo]`.";
      "ReadOnly violation - Assigning to readonly attribute [3003]: Cannot assign to attribute `x` \
       since it is readonly.\n\
       Note that this is a zone entrypoint and any captured variables are treated as readonly";
    ];
  (* `cls` captured in a nested entrypoint should be marked as readonly. *)
  assert_type_errors
    {|
      from pyre_extensions import ReadOnly
      from readonly_stubs_for_testing import readonly_entrypoint

      class Foo:
        x: int = 99

        @classmethod
        def some_classmethod(cls) -> None:

          @readonly_entrypoint
          def nested() -> None:
            reveal_type(cls)
            cls.x = 99
    |}
    [
      "Revealed type [-1]: Revealed type for `cls` is `pyre_extensions.ReadOnly[typing.Type[Foo]]`.";
      (* TODO(T130377746): Recognize attribute for `ReadOnly[Type[Foo]]` when it is the target of an
         assignment. *)
      "Undefined attribute [16]: `pyre_extensions.ReadOnly[typing.Type[Foo]]` has no attribute `x`.";
    ];
  (* TODO(T130377746): We should error when calling a readonly callable. *)
  assert_type_errors
    {|
      from pyre_extensions import ReadOnly
      from readonly_stubs_for_testing import readonly_entrypoint

      some_global: str = ""

      def main() -> None:
        def other_nested() -> None:
          some_global += "foo"

        @readonly_entrypoint
        def nested() -> None:
          reveal_type(other_nested)
          other_nested()
    |}
    [
      "Revealed type [-1]: Revealed type for `other_nested` is \
       `pyre_extensions.ReadOnly[typing.Callable[[], None]]`.";
    ];
  ()


let test_return_type context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from pyre_extensions import ReadOnly

      def main(x: ReadOnly[int]) -> int:
        return x
    |}
    [
      "ReadOnly violation - Incompatible return type [3004]: Expected `int` but got \
       `pyre_extensions.ReadOnly[int]`.";
    ];
  assert_type_errors
    {|
      from pyre_extensions import ReadOnly
      from typing import List

      def main(x: List[ReadOnly[int]]) -> List[int]:
        return x
    |}
    [
      "ReadOnly violation - Incompatible return type [3004]: Expected `typing.List[int]` but got \
       `typing.List[pyre_extensions.ReadOnly[int]]`.";
    ];
  ()


let test_ignored_module context =
  (* Get errors with unnecessary errors filtered out, which is what end-users see. *)
  let assert_filtered_type_errors = assert_default_type_errors ~context in
  (* Don't error when calling a "mutating" method on a readonly object from an ignored module. This
     allows us to gradually annotate the methods in that module as `ReadOnly`. *)
  assert_filtered_type_errors
    {|
      from pyre_extensions import ReadOnly
      from readonly_module_to_ignore import Foo

      def main(foo: ReadOnly[Foo]) -> None:
        foo.some_method(42)
    |}
    [];
  (* Don't error when passing a readonly argument to a method or function defined in an ignored
     module. *)
  assert_filtered_type_errors
    {|
      from pyre_extensions import ReadOnly
      from readonly_module_to_ignore import Foo

      def main(readonly_int: ReadOnly[int]) -> None:
        Foo().some_method(readonly_int)
    |}
    [];
  assert_filtered_type_errors
    {|
      from pyre_extensions import ReadOnly
      from readonly_module_to_ignore import some_function

      def main(readonly_int: ReadOnly[int]) -> None:
        some_function(readonly_int)
    |}
    [];
  (* Show errors in user-defined code that uses types from the ignored module. *)
  assert_filtered_type_errors
    {|
      from pyre_extensions import ReadOnly
      from readonly_module_to_ignore import Foo

      def user_function_expects_mutable_foo(foo: Foo) -> None: ...

      def main(readonly_foo: ReadOnly[Foo]) -> None:
        user_function_expects_mutable_foo(readonly_foo)
    |}
    [
      "ReadOnly violation - Incompatible parameter type [3002]: In call \
       `test.user_function_expects_mutable_foo`, for 1st positional argument, expected \
       `readonly_module_to_ignore.Foo` but got \
       `pyre_extensions.ReadOnly[readonly_module_to_ignore.Foo]`.";
    ];
  ()


let test_typechecking_errors_are_prioritized context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from pyre_extensions import ReadOnly

      def expect_mutable_int(x: ReadOnly[int]) -> None: ...

      def main(s: str) -> None:
        expect_mutable_int(s)
    |}
    [
      "Incompatible parameter type [6]: In call `expect_mutable_int`, for 1st positional argument, \
       expected `pyre_extensions.ReadOnly[int]` but got `str`.";
    ];
  assert_type_errors
    {|
      from pyre_extensions import ReadOnly

      class Foo:
        def expect_mutable_self(self, x: int) -> None: ...

      def main(readonly_foo: ReadOnly[Foo]) -> None:
        readonly_foo.expect_mutable_self("hello")
        readonly_foo.non_existent_method("hello")
    |}
    [
      "ReadOnly violation - Calling mutating method on readonly type [3005]: Method \
       `test.Foo.expect_mutable_self` may modify its object. Cannot call it on readonly expression \
       `readonly_foo` of type `pyre_extensions.ReadOnly[Foo]`.";
      "Incompatible parameter type [6]: In call `Foo.expect_mutable_self`, for 1st positional \
       argument, expected `int` but got `str`.";
      "Undefined attribute [16]: `Foo` has no attribute `non_existent_method`.";
    ];
  assert_type_errors
    {|
      from pyre_extensions import ReadOnly

      class Foo:
        x: int = 42

      def main(readonly_foo: ReadOnly[Foo]) -> None:
        readonly_foo.x = "wrong type"
    |}
    [
      "Incompatible attribute type [8]: Attribute `x` declared in class `Foo` has type \
       `pyre_extensions.ReadOnly[int]` but is used as type `str`.";
      "ReadOnly violation - Assigning to readonly attribute [3003]: Cannot assign to attribute `x` \
       since it is readonly.";
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
         "generic_types" >:: test_generic_types;
         "refinement" >:: test_refinement;
         "captured_variables_for_specially_decorated_functions"
         >:: test_captured_variable_for_specially_decorated_functions;
         "return_type" >:: test_return_type;
         "ignored_module" >:: test_ignored_module;
         "typechecking_errors_are_prioritized" >:: test_typechecking_errors_are_prioritized;
       ]
  |> Test.run
