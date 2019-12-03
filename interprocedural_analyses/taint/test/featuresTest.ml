(* Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Analysis
open OUnit2

let test_add_type_breadcrumb context =
  let open Taint.Features in
  let assert_type_based_breadcrumbs annotation expected =
    let project = Test.ScratchProject.setup ~context [] in
    let { Test.ScratchProject.BuiltTypeEnvironment.type_environment; _ } =
      Test.ScratchProject.build_type_environment project
    in
    let resolution = TypeEnvironment.global_resolution type_environment in
    let actual =
      add_type_breadcrumb ~resolution (Some annotation) []
      |> List.map ~f:(fun { SimpleSet.element; _ } -> element)
      |> List.filter_map ~f:(function
             | Simple.Breadcrumb (Breadcrumb.Type type_name) -> Some type_name
             | _ -> None)
    in
    assert_equal ~printer:(String.concat ~sep:", ") ~cmp:(List.equal String.equal) expected actual
  in
  assert_type_based_breadcrumbs Type.integer ["scalar"];
  assert_type_based_breadcrumbs Type.bool ["bool"; "scalar"];
  assert_type_based_breadcrumbs Type.float ["scalar"];
  assert_type_based_breadcrumbs (Type.optional Type.integer) ["scalar"];
  assert_type_based_breadcrumbs (Type.optional Type.bool) ["bool"; "scalar"];
  assert_type_based_breadcrumbs Type.none [];
  assert_type_based_breadcrumbs Type.Any [];
  assert_type_based_breadcrumbs (Type.awaitable Type.integer) ["scalar"];
  assert_type_based_breadcrumbs (Type.awaitable Type.bool) ["bool"; "scalar"];
  assert_type_based_breadcrumbs (Type.awaitable (Type.Optional Type.bool)) ["bool"; "scalar"];
  assert_type_based_breadcrumbs (Type.awaitable (Type.Optional Type.integer)) ["scalar"]


let () = "features" >::: ["add_type_breadcrumb" >:: test_add_type_breadcrumb] |> Test.run
