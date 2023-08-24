(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Analysis
open TypeInference.Data
open Test

let test_create =
  let assert_ignore ~target ~type_ expected _context =
    assert_bool_equals ~expected (Inference.TestingOnly.should_ignore ~target type_)
  in
  [
    "don't ignore int"
    >:: assert_ignore
          ~target:(Inference.Parameter { name = !&"foo.bar" })
          ~type_:(Type.Primitive "Foo")
          false;
    "ignore type containing Top"
    >:: assert_ignore
          ~target:(Inference.Parameter { name = !&"foo.bar" })
          ~type_:(Type.list Type.Top)
          true;
    "ignore type containing Bottom"
    >:: assert_ignore
          ~target:(Inference.Parameter { name = !&"foo.bar" })
          ~type_:(Type.list Type.Bottom)
          true;
    "ignore type containing Any"
    >:: assert_ignore
          ~target:(Inference.Parameter { name = !&"foo.bar" })
          ~type_:(Type.list Type.Any)
          true;
    "ignore parameter type that is None"
    >:: assert_ignore ~target:(Inference.Parameter { name = !&"foo.bar" }) ~type_:Type.none true;
    "don't ignore return type that is None"
    >:: assert_ignore ~target:Inference.Return ~type_:Type.none false;
    "ignore ReadOnly annotation"
    >:: assert_ignore
          ~target:(Inference.Parameter { name = !&"foo.bar" })
          ~type_:(Type.ReadOnly.create (Type.Primitive "Foo"))
          true;
  ]


let () = "typeInferenceDataTest" >::: [test_list test_create] |> Test.run
