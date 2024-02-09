(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest

let test_mode context =
  assert_default_type_errors
    {|
      # pyre-strict
      # pyre-unsafe
      def foo():
        pass
    |}
    [
      "Unused local mode [51]: Mode `pyre-unsafe` is unused. This conflicts with `pyre-strict` \
       mode set on line 2.";
      "Missing return annotation [3]: Returning `None` but no return type is specified.";
    ]
    context;
  assert_default_type_errors
    {|
      # pyre-strict
      # pyre-unsafe
      def foo():
        pass
      # pyre-ignore-all-errors
    |}
    [
      "Unused local mode [51]: Mode `pyre-unsafe` is unused. This conflicts with `pyre-strict` \
       mode set on line 2.";
      "Missing return annotation [3]: Returning `None` but no return type is specified.";
      "Unused local mode [51]: Mode `pyre-ignore-all-errors` is unused. This conflicts with \
       `pyre-strict` mode set on line 2.";
    ]
    context;
  assert_strict_type_errors
    {|
      # pyre-unsafe
      # pyre-strict
      def foo():
        pass
    |}
    [
      "Unused local mode [51]: Mode `pyre-strict` is unused. This conflicts with `pyre-unsafe` \
       mode set on line 2.";
    ]
    context;
  assert_type_errors
    {|
      # pyre-unsafe
      # pyre-strict
      def foo():
        pass
    |}
    [
      "Unused local mode [51]: Mode `pyre-strict` is unused. This conflicts with `pyre-unsafe` \
       mode set on line 2.";
      "Missing return annotation [3]: Returning `None` but no return type is specified.";
    ]
    context;
  ()


let () = "mode" >::: ["mode" >:: test_mode] |> Test.run
