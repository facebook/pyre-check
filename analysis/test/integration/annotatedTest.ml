(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open OUnit2
open IntegrationTest

let test_annotated _ =
  assert_type_errors
    {|
      def foo(annotated: typing.Annotated[int]) -> int:
        expect_int(annotated)
        reveal_type(annotated)
        return annotated
    |}
    ["Revealed type [-1]: Revealed type for `annotated` is `typing.Annotated[int]`."]


let () = "annotated" >::: ["annotated" >:: test_annotated] |> Test.run
