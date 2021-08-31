(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Test
open Interprocedural

let test_decorators_to_skip _ =
  let normal_model =
    { Taint.Result.empty_model with modes = Taint.Result.ModeSet.empty }
    |> AnalysisResult.make_model Taint.Result.kind
  in
  let skipped_decorator_model =
    {
      Taint.Result.empty_model with
      modes = Taint.Result.ModeSet.singleton SkipDecoratorWhenInlining;
    }
    |> AnalysisResult.make_model Taint.Result.kind
  in
  assert_equal
    ~cmp:Ast.Reference.Set.equal
    (Ast.Reference.Set.of_list [!&"foo.skipped"])
    (Taint.Result.decorators_to_skip
       (Target.Map.of_alist_exn
          [
            Target.create_function !&"foo.not_skipped", normal_model;
            Target.create_function !&"foo.skipped", skipped_decorator_model;
          ]));
  ()


let () = "analyze" >::: ["decorators_to_skip" >:: test_decorators_to_skip] |> Test.run
