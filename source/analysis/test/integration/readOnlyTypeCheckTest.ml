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
      "Incompatible variable type [9]: y is declared to have type `int` but is used as type \
       `pyre_extensions.ReadOnly[int]`.";
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
      "Incompatible variable type [9]: x is declared to have type \
       `typing_extensions.Literal[True]` but is used as type \
       `pyre_extensions.ReadOnly[typing_extensions.Literal[True]]`.";
      "Incompatible variable type [9]: y is declared to have type \
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
      "Incompatible variable type [9]: z is declared to have type `int` but is used as type \
       `pyre_extensions.ReadOnly[int]`.";
    ];
  (* Test readonly violations at the top level. *)
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly

      x: ReadOnly[int]
      y: int = x
    |}
    [
      "Incompatible variable type [9]: y is declared to have type `int` but is used as type \
       `pyre_extensions.ReadOnly[int]`.";
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
      "Incompatible variable type [9]: z is declared to have type `int` but is used as type \
       `pyre_extensions.ReadOnly[int]`.";
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
      "Incompatible variable type [9]: x1 is declared to have type `int` but is used as type \
       `pyre_extensions.ReadOnly[int]`.";
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
      "Incompatible variable type [9]: x1 is declared to have type `int` but is used as type \
       `pyre_extensions.ReadOnly[int]`.";
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
      "Incompatible variable type [9]: x1 is declared to have type `int` but is used as type \
       `pyre_extensions.ReadOnly[int]`.";
      "Incompatible variable type [9]: x2 is declared to have type `int` but is used as type \
       `pyre_extensions.ReadOnly[int]`.";
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
      "Incompatible variable type [9]: x is declared to have type `int` but is used as type \
       `pyre_extensions.ReadOnly[int]`.";
      "Incompatible variable type [9]: y is declared to have type `int` but is used as type \
       `pyre_extensions.ReadOnly[int]`.";
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
    [
      "ReadOnly violation - Assigning to readonly attribute [3003]: Cannot assign to attribute \
       `readonly_attribute` since it is readonly";
    ];
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
      "Incompatible variable type [9]: y is declared to have type `int` but is used as type \
       `pyre_extensions.ReadOnly[int]`.";
      "Incompatible parameter type [6]: In call `expect_mutable_and_readonly`, for 1st positional \
       argument, expected `int` but got `pyre_extensions.ReadOnly[int]`.";
    ];
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly

      def foo(x: int) -> ReadOnly[int]: ...

      def main(x: int) -> None:
        y: int = foo(foo(x))
    |}
    [
      "Incompatible variable type [9]: y is declared to have type `int` but is used as type \
       `pyre_extensions.ReadOnly[int]`.";
      "Incompatible parameter type [6]: In call `foo`, for 1st positional argument, expected `int` \
       but got `pyre_extensions.ReadOnly[int]`.";
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
      "Incompatible parameter type [6]: In call `expect_mutable`, for 1st positional argument, \
       expected `int` but got `pyre_extensions.ReadOnly[int]`.";
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
      "Incompatible variable type [9]: y is declared to have type `int` but is used as type \
       `pyre_extensions.ReadOnly[int]`.";
      "Incompatible parameter type [6]: In call `Foo.return_readonly`, for 1st positional \
       argument, expected `int` but got `pyre_extensions.ReadOnly[int]`.";
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
      "Incompatible variable type [9]: y is declared to have type `int` but is used as type \
       `pyre_extensions.ReadOnly[int]`.";
      "Incompatible parameter type [6]: In call `return_foo`, for 1st positional argument, \
       expected `int` but got `pyre_extensions.ReadOnly[int]`.";
      "Incompatible parameter type [6]: In call `Foo.return_readonly`, for 1st positional \
       argument, expected `int` but got `pyre_extensions.ReadOnly[int]`.";
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
       `readonly_foo` of type `pyre_extensions.ReadOnly[test.Foo]`.";
    ];
  (* A method with readonly `self` can be called on either mutable or readonly objects. However, the
     method itself cannot call other mutating methods. *)
  assert_type_errors_including_readonly
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
      "ReadOnly violation - Calling mutating method on readonly type [3005]: Method \
       `test.Foo.expect_mutable_self` may modify its object. Cannot call it on readonly expression \
       `self` of type `pyre_extensions.ReadOnly[test.Foo]`.";
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
      "Incompatible parameter type [6]: In call `expect_positional_mutable_and_readonly`, for 1st \
       positional argument, expected `int` but got `pyre_extensions.ReadOnly[int]`.";
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
      "Incompatible parameter type [6]: In call `expect_keyword_only`, for argument `x`, expected \
       `int` but got `pyre_extensions.ReadOnly[int]`.";
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
      "Incompatible parameter type [6]: In call `expect_kwargs`, for argument `x`, expected `int` \
       but got `pyre_extensions.ReadOnly[int]`.";
      "Incompatible parameter type [6]: In call `expect_kwargs`, for 1st positional argument, \
       expected `int` but got `pyre_extensions.ReadOnly[int]`.";
    ];
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly
      from typing import Callable

      def main(my_readonly: ReadOnly[int], undefined: Callable[..., ReadOnly[int]]) -> None:
        x: int = undefined(my_readonly)
    |}
    [
      "Incompatible variable type [9]: x is declared to have type `int` but is used as type \
       `pyre_extensions.ReadOnly[int]`.";
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
      "Incompatible parameter type [6]: In call `expect_mutable`, for 1st positional argument, \
       expected `object` but got `pyre_extensions.ReadOnly[int]`.";
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
      "Incompatible variable type [9]: y is declared to have type `int` but is used as type \
       `pyre_extensions.ReadOnly[int]`.";
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
      "Incompatible variable type [9]: y is declared to have type `int` but is used as type \
       `pyre_extensions.ReadOnly[int]`.";
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
      "Incompatible variable type [9]: y1 is declared to have type `int` but is used as type \
       `pyre_extensions.ReadOnly[int]`.";
      "Incompatible variable type [9]: y2 is declared to have type `int` but is used as type \
       `pyre_extensions.ReadOnly[int]`.";
      "Incompatible variable type [9]: y3 is declared to have type `int` but is used as type \
       `pyre_extensions.ReadOnly[int]`.";
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
      "Incompatible parameter type [6]: In call `expect_mutable`, for 1st positional argument, \
       expected `int` but got `pyre_extensions.ReadOnly[int]`.";
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
      "Incompatible parameter type [6]: In call `expect_mutable`, for 1st positional argument, \
       expected `int` but got `pyre_extensions.ReadOnly[int]`.";
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
       ]
  |> Test.run
