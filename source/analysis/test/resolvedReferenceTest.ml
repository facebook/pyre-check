(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Ast
open Analysis
open Pyre
open Test

let test_as_module_toplevel_reference context =
  let assert_as_module_toplevel expected resolved =
    assert_equal
      ~ctxt:context
      ~printer:[%show: Reference.t option]
      ~cmp:[%equal: Reference.t option]
      expected
      (ResolvedReference.as_module_toplevel_reference resolved)
  in
  assert_as_module_toplevel
    None
    (ResolvedReference.ModuleAttribute
       {
         from = !&"package.module";
         name = "name";
         export = FromModuleGetattr;
         remaining = ["remaining"];
       });
  assert_as_module_toplevel
    (Some !&"package.module.name")
    (ResolvedReference.ModuleAttribute
       { from = !&"package.module"; name = "name"; export = FromModuleGetattr; remaining = [] });
  assert_as_module_toplevel
    (Some !&"package.module.remaining.names")
    (ResolvedReference.PlaceholderStub
       { stub_module = !&"package.module"; remaining = ["remaining"; "names"] });
  assert_as_module_toplevel (Some !&"package.module") (ResolvedReference.Module !&"package.module");
  ()


let () =
  "resolved_reference"
  >::: ["as_module_toplevel_reference" >:: test_as_module_toplevel_reference]
  |> Test.run
