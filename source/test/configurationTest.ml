(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Pyre

let test_search_path _ =
  let assert_search_path ?(search_path = []) ~source_path expected =
    let search_path =
      List.map search_path ~f:Path.create_absolute |> List.map ~f:(fun root -> SearchPath.Root root)
    in
    let source_path = List.map source_path ~f:Path.create_absolute in
    let to_search_path root = SearchPath.Root root in
    let search_path =
      Configuration.Analysis.search_path
        (Configuration.Analysis.create
           ~search_path
           ~source_path:(List.map source_path ~f:to_search_path)
           ())
      |> List.map ~f:SearchPath.show
    in
    assert_equal ~printer:(List.to_string ~f:ident) expected search_path
  in
  assert_search_path ~source_path:["/a"] ["/a"];
  assert_search_path ~source_path:["/a"] ["/a"];
  assert_search_path
    ~search_path:["/other"; "/another"]
    ~source_path:["/a"]
    ["/other"; "/another"; "/a"];
  assert_search_path
    ~search_path:["/other"; "/another"]
    ~source_path:["/a"]
    ["/other"; "/another"; "/a"];
  assert_search_path
    ~search_path:["/other"; "/another"]
    ~source_path:["/a"; "/b"]
    ["/other"; "/another"; "/a"; "/b"];
  ()


let test_extensions _ =
  let assert_extensions ~extensions expected =
    let extensions = List.map ~f:Configuration.Extension.create_extension extensions in
    assert_equal
      ~cmp:(List.equal Configuration.Extension.equal)
      ~printer:(List.to_string ~f:Configuration.Extension.show)
      expected
      extensions
  in
  assert_extensions
    ~extensions:[".extension"]
    [{ Configuration.Extension.suffix = ".extension"; include_suffix_in_module_qualifier = false }];
  assert_extensions
    ~extensions:[".extension$include_suffix_in_module_qualifier"]
    [{ Configuration.Extension.suffix = ".extension"; include_suffix_in_module_qualifier = true }];
  ()


let () =
  "configuration"
  >::: ["search_path" >:: test_search_path; "extensions" >:: test_extensions]
  |> Test.run
