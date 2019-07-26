(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open OUnit2
open IntegrationTest

let test_delete context =
  assert_type_errors
    ~context
    {|
    def foo() -> None:
        x = 10
        del x
        return x # Error
    |}
    [ "Incompatible return type [7]: Expected `None` but got `unknown`.";
      "Undefined name [18]: Global name `x` is not defined, or there is at least one control flow \
       path that doesn't define `x`." ];
  assert_type_errors
    ~context
    {|
      def foo(x: int) -> int:
        if x > 100:
          del x
        else:
          x =+ 1
        return x
    |}
    [ "Incompatible return type [7]: Expected `int` but got `typing.Union[int, typing.Undeclared]`.";
      "Undefined name [18]: Global name `x` is not defined, or there is at least one control flow \
       path that doesn't define `x`." ]


let () = "method" >::: ["check_delete" >:: test_delete] |> Test.run
