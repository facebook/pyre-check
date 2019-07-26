(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open OUnit2
open IntegrationTest

let test_annotated context =
  assert_type_errors
    ~context
    {|
      def foo(annotated: typing.Annotated[int]) -> int:
        expect_int(annotated)
        reveal_type(annotated)
        return annotated
    |}
    ["Revealed type [-1]: Revealed type for `annotated` is `typing.Annotated[int]`."];
  assert_type_errors
    ~context
    {|
      def foo(input: float) -> int:
        return pyre_extensions.safe_cast(int, input)
    |}
    [ "Unsafe cast [103]: `safe_cast` is only permitted to loosen the type of `input`. `float` is \
       not a super type of `input`." ];
  assert_type_errors
    ~context
    {|
        def foo(input: int) -> float:
          return pyre_extensions.safe_cast(float, input)
    |}
    []


let () = "annotated" >::: ["annotated" >:: test_annotated] |> Test.run
