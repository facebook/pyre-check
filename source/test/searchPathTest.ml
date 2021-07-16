(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Pyre

let ( ! ) = Path.create_absolute

let test_create_search_path _ =
  let root = Path.create_absolute "/root" in
  (* Create root/subdirectory. *)
  let subdirectory = Path.create_relative ~root ~relative:"subdirectory" in
  assert_equal ~cmp:SearchPath.equal (SearchPath.Root root) (SearchPath.create (Path.show root));
  assert_equal
    ~cmp:SearchPath.equal
    (SearchPath.Root subdirectory)
    (SearchPath.create (Path.show subdirectory));
  assert_equal
    ~cmp:SearchPath.equal
    (SearchPath.Subdirectory { root; subdirectory = "subdirectory" })
    (SearchPath.create (Path.show root ^ "$subdirectory"));
  assert_raises (Failure "Unable to create search path from too$many$levels") (fun () ->
      SearchPath.create "too$many$levels")


let test_show_search_path _ =
  let assert_round_trip search_path =
    let round_trip_search_path = SearchPath.show search_path |> SearchPath.create in
    assert_equal ~cmp:SearchPath.equal ~printer:SearchPath.show search_path round_trip_search_path
  in
  assert_round_trip (SearchPath.Root !"foo");
  assert_round_trip (SearchPath.Root !"/foo/bar");
  assert_round_trip (SearchPath.Subdirectory { root = !"foo"; subdirectory = "bar" });
  assert_round_trip (SearchPath.Subdirectory { root = !"/foo"; subdirectory = "bar" });
  assert_round_trip (SearchPath.Subdirectory { root = !"/foo/bar"; subdirectory = "baz" });
  assert_round_trip (SearchPath.Subdirectory { root = !"/foo"; subdirectory = "bar/baz" });
  ()


let test_normalize context =
  let good_root = bracket_tmpdir context in
  let bad_root = "nonexist/directory" in
  let good_subroot = good_root ^ "/subroot" in
  Sys_utils.mkdir_no_fail good_subroot;
  let create_input ?subdirectory root =
    let search_path =
      match subdirectory with
      | None -> SearchPath.Root !root
      | Some subdirectory -> SearchPath.Subdirectory { root = !root; subdirectory }
    in
    SearchPath.show search_path
  in
  let assert_success ~normalize ~expected input =
    let create_search_path, create_type =
      if normalize then
        SearchPath.create_normalized, "normalized"
      else
        SearchPath.create, "non-normalized"
    in
    try
      let _ = create_search_path input in
      if not expected then
        let message =
          Format.sprintf
            "Expect %s search path creation to succeed but it failed on input %s"
            create_type
            input
        in
        assert_failure message
    with
    | _ ->
        if expected then
          let message =
            Format.sprintf
              "Expect %s search path creation to fail but it succeeded on input %s"
              create_type
              input
          in
          assert_failure message
  in

  (* Non-normalized creation succeeds all the time. *)
  assert_success ~normalize:false ~expected:true (create_input good_root);
  assert_success ~normalize:false ~expected:true (create_input good_subroot);
  assert_success ~normalize:false ~expected:true (create_input bad_root);
  assert_success ~normalize:false ~expected:true (create_input ~subdirectory:"subroot" good_root);
  assert_success ~normalize:false ~expected:true (create_input ~subdirectory:"nosubroot" good_root);
  assert_success ~normalize:false ~expected:true (create_input ~subdirectory:"subroot" bad_root);

  (* Normalized creation depends on filesystem state. *)
  assert_success ~normalize:true ~expected:true (create_input good_root);
  assert_success ~normalize:true ~expected:true (create_input good_subroot);
  assert_success ~normalize:true ~expected:false (create_input bad_root);
  assert_success ~normalize:true ~expected:true (create_input ~subdirectory:"subroot" good_root);
  assert_success ~normalize:true ~expected:false (create_input ~subdirectory:"nosubroot" good_root);
  assert_success ~normalize:true ~expected:false (create_input ~subdirectory:"subroot" bad_root);

  ()


let test_search_for_path context =
  let root = bracket_tmpdir context |> Path.create_absolute in
  let assert_path ~search_paths ~path ~expected =
    assert_equal
      (Some expected)
      (SearchPath.search_for_path ~search_paths path
      >>| fun SearchPath.{ relative_path; _ } -> Path.RelativePath.relative relative_path)
  in
  let search_paths =
    [
      SearchPath.Subdirectory { root; subdirectory = "a" };
      SearchPath.Subdirectory
        { root = Path.create_relative ~root ~relative:"b"; subdirectory = "c" };
      SearchPath.Subdirectory { root; subdirectory = "b" };
    ]
  in
  assert_path
    ~search_paths
    ~path:(Path.create_relative ~root ~relative:"a/file.py")
    ~expected:"a/file.py";
  assert_path
    ~search_paths
    ~path:(Path.create_relative ~root ~relative:"b/c/file.py")
    ~expected:"c/file.py";
  assert_path
    ~search_paths
    ~path:(Path.create_relative ~root ~relative:"b/other/file.py")
    ~expected:"b/other/file.py"


let () =
  "searchPath"
  >::: [
         "create_search_path" >:: test_create_search_path;
         "show_search_path" >:: test_show_search_path;
         "normalize" >:: test_normalize;
         "search_for_path" >:: test_search_for_path;
       ]
  |> Test.run
