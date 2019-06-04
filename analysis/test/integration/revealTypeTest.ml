(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open OUnit2
open IntegrationTest

let test_reveal_type _ =
  assert_type_errors
    {|
      def foo(x: str) -> None:
        reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `str`."];
  assert_default_type_errors
    {|
      def foo(x) -> None:
        reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `typing.Any`."];
  assert_type_errors
    {|
      def foo(x: int, y: int) -> None:
        reveal_type(x + y)
    |}
    ["Revealed type [-1]: Revealed type for `x.__add__(y)` is `int`."];
  assert_type_errors
    {|
      def foo(x: int) -> None:

        reveal_type(int_to_str(x))
    |}
    ["Revealed type [-1]: Revealed type for `int_to_str(x)` is `str`."];
  assert_type_errors
    {|
      def foo() -> int:
        bar, baz = list(range(2))
        reveal_type(bar)
        return bar
    |}
    ["Revealed type [-1]: Revealed type for `bar` is `int`."];
  assert_type_errors
    {|
      def foo(s: typing.Sequence[float]) -> list[float]:
        l = list(s)
        bar, baz = l
        reveal_type(bar)
        return l
    |}
    ["Revealed type [-1]: Revealed type for `bar` is `float`."];
  assert_type_errors
    {|
      def foo() -> dict[str, int]:
        d = dict(a = 1, b = 2)
        reveal_type(d)
        bar = d['a']
        reveal_type(bar)
        return d
    |}
    [ "Revealed type [-1]: Revealed type for `d` is `typing.Dict[str, int]`.";
      "Revealed type [-1]: Revealed type for `bar` is `int`." ];
  assert_type_errors
    {|
      def foo(map: typing.Mapping[str, int]) -> dict[str, int]:
        d = dict(map)
        bar = d['a']
        reveal_type(bar)
        return d
    |}
    ["Revealed type [-1]: Revealed type for `bar` is `int`."];
  assert_type_errors
    {|
      def foo(t: typing.Iterable[typing.Tuple[str, int]]) -> dict[str, int]:
        d = dict(t)
        bar = d['a']
        reveal_type(bar)
        return d
    |}
    ["Revealed type [-1]: Revealed type for `bar` is `int`."];
  assert_type_errors
    {|
      def foo(bar: typing.Union[int, str]) -> None:
        if type(bar) is int:
          reveal_type(bar)
        else:
          reveal_type(bar)
    |}
    [ "Revealed type [-1]: Revealed type for `bar` is `int`.";
      "Revealed type [-1]: Revealed type for `bar` is `str`." ]


let () = "revealType" >::: ["reveal_type" >:: test_reveal_type] |> Test.run
