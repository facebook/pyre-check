(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2

let test_search_path _ =
  let assert_search_paths ?(search_paths = []) ~source_paths expected =
    let search_paths =
      List.map search_paths ~f:PyrePath.create_absolute
      |> List.map ~f:(fun root -> SearchPath.Root root)
    in
    let source_paths = List.map source_paths ~f:PyrePath.create_absolute in
    let to_search_path root = SearchPath.Root root in
    let search_paths =
      Configuration.Analysis.search_paths
        (Configuration.Analysis.create
           ~search_paths
           ~source_paths:(List.map source_paths ~f:to_search_path)
           ())
      |> List.map ~f:SearchPath.show
    in
    assert_equal ~printer:(List.to_string ~f:ident) expected search_paths
  in
  assert_search_paths ~source_paths:["/a"] ["/a"];
  assert_search_paths ~source_paths:["/a"] ["/a"];
  assert_search_paths
    ~search_paths:["/other"; "/another"]
    ~source_paths:["/a"]
    ["/other"; "/another"; "/a"];
  assert_search_paths
    ~search_paths:["/other"; "/another"]
    ~source_paths:["/a"]
    ["/other"; "/another"; "/a"];
  assert_search_paths
    ~search_paths:["/other"; "/another"]
    ~source_paths:["/a"; "/b"]
    ["/other"; "/another"; "/a"; "/b"];
  ()


let test_extensions _ =
  let assert_extensions ~extensions expected =
    let extensions = List.map ~f:Configuration.Extension.create_extension extensions in
    assert_equal
      ~cmp:(List.equal [%compare.equal: Configuration.Extension.t])
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


let test_change_indicator context =
  let open Configuration.ChangeIndicator in
  let assert_parsed ~expected text =
    match Yojson.Safe.from_string text |> of_yojson with
    | Result.Ok actual ->
        assert_equal
          ~ctxt:context
          ~cmp:[%compare.equal: t]
          ~printer:(fun wheel -> Sexp.to_string_hum ([%sexp_of: t] wheel))
          expected
          actual
    | Result.Error message -> assert_failure message
  in
  let assert_not_parsed text =
    match Yojson.Safe.from_string text |> of_yojson with
    | Result.Error _ -> ()
    | Result.Ok actual ->
        let message =
          Format.asprintf "Unexpected parsing success: %a" Sexp.pp_hum (sexp_of_t actual)
        in
        assert_failure message
  in

  assert_not_parsed "{}";
  assert_not_parsed "42";
  assert_not_parsed {| { "root": 42 } |};
  assert_not_parsed {| { "root": "/foo", "relative": [] } |};

  assert_parsed
    {| { "root": "/foo", "relative": "derp.txt" }|}
    ~expected:{ root = PyrePath.create_absolute "/foo"; relative = "derp.txt" };
  ()


let test_unwatched_files context =
  let open Configuration.UnwatchedFiles in
  let assert_parsed ~expected text =
    match Yojson.Safe.from_string text |> of_yojson with
    | Result.Ok actual ->
        assert_equal
          ~ctxt:context
          ~cmp:[%compare.equal: t]
          ~printer:(fun wheel -> Sexp.to_string_hum ([%sexp_of: t] wheel))
          expected
          actual
    | Result.Error message -> assert_failure message
  in
  let assert_not_parsed text =
    match Yojson.Safe.from_string text |> of_yojson with
    | Result.Error _ -> ()
    | Result.Ok actual ->
        let message =
          Format.asprintf "Unexpected parsing success: %a" Sexp.pp_hum (sexp_of_t actual)
        in
        assert_failure message
  in

  assert_not_parsed "{}";
  assert_not_parsed "42";
  assert_not_parsed {| { "root": 42 } |};
  assert_not_parsed {| { "root": "/foo", "checksum_path": [] } |};

  assert_parsed
    {| { "root": "/foo", "checksum_path": "derp.txt" }|}
    ~expected:{ root = PyrePath.create_absolute "/foo"; checksum_path = "derp.txt" };
  ()


let test_unwatched_dependency context =
  let open Configuration.UnwatchedDependency in
  let assert_parsed ~expected text =
    match Yojson.Safe.from_string text |> of_yojson with
    | Result.Ok actual ->
        assert_equal
          ~ctxt:context
          ~cmp:[%compare.equal: t]
          ~printer:(fun wheel -> Sexp.to_string_hum ([%sexp_of: t] wheel))
          expected
          actual
    | Result.Error message -> assert_failure message
  in
  let assert_not_parsed text =
    match Yojson.Safe.from_string text |> of_yojson with
    | Result.Error _ -> ()
    | Result.Ok actual ->
        let message =
          Format.asprintf "Unexpected parsing success: %a" Sexp.pp_hum (sexp_of_t actual)
        in
        assert_failure message
  in

  assert_not_parsed "{}";
  assert_not_parsed "42";
  assert_not_parsed {| { "change_indicator": 42 } |};
  assert_not_parsed {| { "files": [] } |};
  assert_not_parsed
    {| { "change_indicator": [], "files": { "root": "/foo", "checksum_path": "bar" } } |};
  assert_not_parsed {| { "change_indicator": { "root": "/foo", "relative": "bar" }, "files": [] } |};

  assert_parsed
    {|
       {
         "change_indicator": { "root": "/foo", "relative": "bar" },
         "files": { "root": "/baz", "checksum_path": "derp.txt" }
       }
    |}
    ~expected:
      {
        change_indicator =
          { Configuration.ChangeIndicator.root = PyrePath.create_absolute "/foo"; relative = "bar" };
        files =
          {
            Configuration.UnwatchedFiles.root = PyrePath.create_absolute "/baz";
            checksum_path = "derp.txt";
          };
      };
  ()


let () =
  "configuration"
  >::: [
         "search_path" >:: test_search_path;
         "extensions" >:: test_extensions;
         "change_indicator" >:: test_change_indicator;
         "unwatched_files" >:: test_unwatched_files;
         "unwatched_dependency" >:: test_unwatched_dependency;
       ]
  |> Test.run
