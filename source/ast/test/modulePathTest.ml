(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open OUnit2

let test_qualifier_from_relative_path _context =
  let qualifier_from_relative_path path expected =
    assert_equal
      ~cmp:Reference.equal
      ~printer:Reference.show
      (Reference.create expected)
      (ModulePath.qualifier_from_relative_path path)
  in
  qualifier_from_relative_path "" "";
  qualifier_from_relative_path "foo/bar/baz.py" "foo.bar.baz";
  qualifier_from_relative_path "foo-stubs/bar/baz.py" "foo.bar.baz";
  qualifier_from_relative_path "foo/bar/baz.special.py" "foo.bar.baz.special";
  qualifier_from_relative_path "foo/bar.special/baz.py" "foo.bar.special.baz";
  ()


let () =
  "modulePath"
  >::: ["qualifier_from_relative_path" >:: test_qualifier_from_relative_path]
  |> Test.run
