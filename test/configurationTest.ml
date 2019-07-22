(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Test
open Pyre

let test_equal _ =
  assert_true
    (Configuration.Analysis.equal
       (Configuration.Analysis.create ~start_time:1.0 ())
       (Configuration.Analysis.create ~start_time:2.0 ()));
  assert_false
    (Configuration.Analysis.equal
       (Configuration.Analysis.create ~infer:true ())
       (Configuration.Analysis.create ~infer:false ()));
  assert_true
    (Configuration.Analysis.equal
       (Configuration.Analysis.create ~parallel:true ())
       (Configuration.Analysis.create ~parallel:false ()));
  let root = Path.current_working_directory () in
  assert_true
    (Configuration.Analysis.equal
       (Configuration.Analysis.create ~filter_directories:[] ())
       (Configuration.Analysis.create ~filter_directories:[root] ()));
  assert_true
    (Configuration.Analysis.equal
       (Configuration.Analysis.create ~number_of_workers:42 ())
       (Configuration.Analysis.create ~number_of_workers:84 ()));
  assert_true
    (Configuration.Analysis.equal
       (Configuration.Analysis.create ~ignore_dependencies:false ())
       (Configuration.Analysis.create ()));
  assert_true
    (Configuration.Analysis.equal
       (Configuration.Analysis.create ~search_path:[] ())
       (Configuration.Analysis.create ~search_path:[SearchPath.Root root] ()));
  assert_true
    (Configuration.Analysis.equal
       (Configuration.Analysis.create ~verbose:true ())
       (Configuration.Analysis.create ~verbose:false ()));
  assert_false
    (Configuration.Analysis.equal
       (Configuration.Analysis.create ~expected_version:"a" ())
       (Configuration.Analysis.create ~expected_version:"b" ()));
  assert_false
    (Configuration.Analysis.equal
       (Configuration.Analysis.create ~strict:true ())
       (Configuration.Analysis.create ~strict:false ()));
  assert_false
    (Configuration.Analysis.equal
       (Configuration.Analysis.create ~declare:true ())
       (Configuration.Analysis.create ~declare:false ()));
  assert_false
    (Configuration.Analysis.equal
       (Configuration.Analysis.create ~debug:true ())
       (Configuration.Analysis.create ~debug:false ()))


let test_search_path _ =
  let assert_search_path ?(search_path = []) ~local_root expected =
    let search_path =
      List.map search_path ~f:(Path.create_absolute ~follow_symbolic_links:false)
      |> List.map ~f:(fun root -> SearchPath.Root root)
    in
    let local_root = Path.create_absolute ~follow_symbolic_links:false local_root in
    let search_path =
      Configuration.Analysis.search_path
        (Configuration.Analysis.create ~search_path ~local_root ())
      |> List.map ~f:SearchPath.show
    in
    assert_equal ~printer:(List.to_string ~f:ident) expected search_path
  in
  assert_search_path ~local_root:"/a" ["/a"];
  assert_search_path ~local_root:"/a" ["/a"];
  assert_search_path
    ~search_path:["/other"; "/another"]
    ~local_root:"/a"
    ["/other"; "/another"; "/a"];
  assert_search_path
    ~search_path:["/other"; "/another"]
    ~local_root:"/a"
    ["/other"; "/another"; "/a"];
  ()


let () =
  "configuration" >::: ["equal" >:: test_equal; "search_path" >:: test_search_path] |> Test.run
