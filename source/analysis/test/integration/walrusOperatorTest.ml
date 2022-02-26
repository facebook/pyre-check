(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest

let test_check_walrus_operator context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      (x := True)
      reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `typing_extensions.Literal[True]`."];
  assert_type_errors
    {|
      if (b := True):
        reveal_type(b)
    |}
    ["Revealed type [-1]: Revealed type for `b` is `typing_extensions.Literal[True]`."];
  assert_type_errors
    {|
      a = [1, 2, 3]
      if (d := len(a)):
        reveal_type(d)
    |}
    ["Revealed type [-1]: Revealed type for `d` is `int`."];
  assert_type_errors
    {|
      x = (y := 0)
      reveal_type(x)
      reveal_type(y)
    |}
    [
      "Missing global annotation [5]: Globally accessible variable `x` has type `int` but no type \
       is specified.";
      "Revealed type [-1]: Revealed type for `x` is `typing_extensions.Literal[0]`.";
      "Revealed type [-1]: Revealed type for `y` is `typing_extensions.Literal[0]`.";
    ];
  assert_type_errors
    {|
      def foo(x: int, cat: str) -> None:
        pass
      foo(x := 3, cat='vector')
      reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `typing_extensions.Literal[3]`."];
  assert_type_errors
    {|
      def foo(cat: str) -> None:
        pass
      foo(cat=(category := 'vector'))
      reveal_type(category)
    |}
    ["Revealed type [-1]: Revealed type for `category` is `typing_extensions.Literal['vector']`."];
  assert_type_errors
    {|
      a = [1, 2, 3]
      if (b := len(a)) > 4:
        reveal_type(b)
    |}
    ["Revealed type [-1]: Revealed type for `b` is `int`."];
  assert_type_errors
    {|
      a = [1, 2, 3]
      if (b := 3) in a:
        reveal_type(b)
    |}
    ["Revealed type [-1]: Revealed type for `b` is `typing_extensions.Literal[3]`."];
  assert_type_errors
    {|
      if (b := 42) and True:
        reveal_type(b)
    |}
    ["Revealed type [-1]: Revealed type for `b` is `typing_extensions.Literal[42]`."];
  assert_type_errors
    {|
      if True and (b := 42):
        reveal_type(b)
    |}
    ["Revealed type [-1]: Revealed type for `b` is `typing_extensions.Literal[42]`."];
  assert_type_errors
    {|
      if (b := 0) or True:
        reveal_type(b)
    |}
    ["Revealed type [-1]: Revealed type for `b` is `typing_extensions.Literal[0]`."];
  assert_type_errors
    {|
      if False or (b := 0):
        reveal_type(b)
    |}
    ["Revealed type [-1]: Revealed type for `b` is `typing_extensions.Literal[0]`."];
  assert_type_errors
    {|
      if (b := 3) > 4:
        pass
      reveal_type(b)
    |}
    ["Revealed type [-1]: Revealed type for `b` is `typing_extensions.Literal[3]`."];
  assert_type_errors
    {|
    from typing import Optional
    def foo() -> Optional[int]:
      ...

    if (a := foo()) is not None:
      reveal_type(a)
    |}
    ["Revealed type [-1]: Revealed type for `a` is `int`."]


let () = "walrus" >::: ["check_walrus_operator" >:: test_check_walrus_operator] |> Test.run
