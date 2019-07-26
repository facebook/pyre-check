(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open OUnit2
open IntegrationTest

let test_higher_order_callables context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      def foo(f: typing.Callable[[int], str]) -> str:
        return f(1)

      def callme(x: int) -> str:
        return ""

      foo(callme)
    |}
    [];
  assert_type_errors
    {|
      def foo(f: typing.Callable[..., str]) -> str:
        return f(1)

      def callme(x: int) -> str:
        return ""

      foo(callme)
    |}
    [];
  assert_type_errors
    {|
      T = typing.TypeVar("T")
      def foo(f: typing.Callable[[int], T]) -> typing.Callable[[str], T]:
        def takes_str(s: str) -> T:
          return f(int(s))
        return takes_str

      def callme(x: int) -> str:
        return ""

      reveal_type(foo(callme))
    |}
    ["Revealed type [-1]: Revealed type for `foo(callme)` is `typing.Callable[[str], str]`."];
  assert_type_errors
    {|
      T = typing.TypeVar("T")
      def foo(f: typing.Callable[..., T]) -> typing.Callable[..., T]:
        def takes_str(s: str) -> T:
          return f(int(s))
        return takes_str

      def callme(x: int) -> str:
        return ""

      reveal_type(foo(callme))
    |}
    ["Revealed type [-1]: Revealed type for `foo(callme)` is `typing.Callable[..., str]`."]


let test_union_of_callables context =
  assert_type_errors
    ~context
    {|
      def baz(x: typing.Union[typing.Callable[[int], typing.Any], typing.Callable[..., typing.Any]]) -> None:
          reveal_type(x)
          x(1)
    |}
    [ "Missing parameter annotation [2]: Parameter `x` must have a type that does not contain `Any`.";
      "Revealed type [-1]: Revealed type for `x` is `typing.Union[typing.Callable[[int], \
       typing.Any], typing.Callable[..., typing.Any]]`." ]


let () =
  "callable"
  >::: [ "higher_order_callables" >:: test_higher_order_callables;
         "union_of_callables" >:: test_union_of_callables ]
  |> Test.run
