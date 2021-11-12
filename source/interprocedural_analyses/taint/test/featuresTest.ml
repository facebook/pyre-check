(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

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
    let resolution =
      TypeEnvironment.read_only type_environment |> TypeEnvironment.ReadOnly.global_resolution
    in
    let actual =
      let open Abstract.OverUnderSetDomain in
      type_breadcrumbs ~resolution (Some annotation)
      |> BreadcrumbSet.to_approximation
      |> List.map ~f:(fun { element; _ } -> element)
      |> List.map ~f:BreadcrumbInterned.unintern
      |> List.filter_map ~f:(function
             | Breadcrumb.Type type_name -> Some type_name
             | _ -> None)
      |> List.sort ~compare:String.compare
    in
    assert_equal ~printer:(String.concat ~sep:", ") ~cmp:(List.equal String.equal) expected actual
  in
  assert_type_based_breadcrumbs Type.bool ["bool"; "scalar"];
  assert_type_based_breadcrumbs Type.enumeration ["enumeration"; "scalar"];
  assert_type_based_breadcrumbs Type.integer ["integer"; "scalar"];
  assert_type_based_breadcrumbs (Type.optional Type.bool) ["bool"; "scalar"];
  assert_type_based_breadcrumbs (Type.optional Type.enumeration) ["enumeration"; "scalar"];
  assert_type_based_breadcrumbs (Type.optional Type.integer) ["integer"; "scalar"];
  assert_type_based_breadcrumbs Type.none [];
  assert_type_based_breadcrumbs Type.Any [];
  assert_type_based_breadcrumbs (Type.awaitable Type.bool) ["bool"; "scalar"];
  assert_type_based_breadcrumbs (Type.awaitable Type.enumeration) ["enumeration"; "scalar"];
  assert_type_based_breadcrumbs (Type.awaitable Type.integer) ["integer"; "scalar"];
  assert_type_based_breadcrumbs (Type.awaitable (Type.optional Type.bool)) ["bool"; "scalar"];
  assert_type_based_breadcrumbs
    (Type.awaitable (Type.optional Type.enumeration))
    ["enumeration"; "scalar"];
  assert_type_based_breadcrumbs (Type.awaitable (Type.optional Type.integer)) ["integer"; "scalar"]


let () = "features" >::: ["add_type_breadcrumb" >:: test_add_type_breadcrumb] |> Test.run
