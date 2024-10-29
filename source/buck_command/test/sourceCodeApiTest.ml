(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Base
open Buck_commands.Testing

let ( ! ) = Ast.Reference.create

let test_file_loader =
  let assert_loaded ~context ~expected ~loader:{ FileLoader.load } path =
    match load path with
    | Result.Error message -> assert_failure message
    | Result.Ok actual ->
        assert_equal ~ctxt:context ~cmp:String.equal ~printer:Fn.id expected actual
  in
  let assert_not_loaded ~loader:{ FileLoader.load } path =
    match load path with
    | Result.Ok content ->
        let message =
          Stdlib.Format.sprintf "Unexpected load success on path `%s`: %s" path content
        in
        assert_failure message
    | Result.Error _ -> ()
  in
  [
    (Test.labeled_test_case Stdlib.__FUNCTION__ Stdlib.__LINE__
    @@ fun context ->
    let root =
      TestHelper.setup_scratch_directory ~context ["src/foo.py", "x = 42"; "src/bar.py", "y = 43"]
    in
    let lookup =
      Sourcedb.Lookup.create_for_testing
        ~sources:["foo.py", "src/foo.py"]
        ~dependencies:["bar.py", "src/bar.py"]
        ()
    in
    let loader = FileLoader.create_from_sourcedb_lookup ~root lookup in
    assert_loaded ~context ~loader "foo.py" ~expected:"x = 42";
    assert_loaded ~context ~loader "bar.py" ~expected:"y = 43";
    assert_not_loaded ~loader "baz.py");
    (Test.labeled_test_case Stdlib.__FUNCTION__ Stdlib.__LINE__
    @@ fun context ->
    let root =
      TestHelper.setup_scratch_directory ~context ["src/test.py", "0"; "dep/test.py", "1"]
    in
    let lookup =
      Sourcedb.Lookup.create_for_testing
        ~sources:["test.py", "src/test.py"]
        ~dependencies:["test.py", "dep/test.py"; "not_on_filesystem.py", "src/not_on_filesystem.py"]
        ()
    in
    let loader = FileLoader.create_from_sourcedb_lookup ~root lookup in
    assert_loaded ~context ~loader "test.py" ~expected:"0";
    assert_not_loaded ~loader "not_on_filesystem.py";
    assert_not_loaded ~loader "not_in_lookup.py");
  ]


let test_source_code_api =
  let controls =
    (* Just a dummy value *)
    Configuration.Analysis.create ~source_paths:[] () |> Analysis.EnvironmentControls.create
  in
  let create loader listing = BuckBasedSourceCodeApi.create ~controls ~loader ~listing () in
  let assert_lookup_relative_path ~context ~api expects =
    let f (qualifier, expected) =
      let actual =
        let source_code_api = BuckBasedSourceCodeApi.get_source_code_api api in
        Analysis.SourceCodeApi.module_path_of_qualifier source_code_api qualifier
        |> Option.map ~f:Ast.ModulePath.relative
      in
      assert_equal
        ~ctxt:context
        ~cmp:[%compare.equal: string option]
        ~printer:(fun p -> [%sexp_of: string option] p |> Sexp.to_string_hum)
        expected
        actual
    in
    List.iter expects ~f
  in
  let assert_type_check_qualifiers ~context ~api expected =
    let expected = List.sort expected ~compare:Ast.Reference.compare in
    let actual = BuckBasedSourceCodeApi.get_type_check_qualifiers api in
    let actual = List.sort actual ~compare:Ast.Reference.compare in
    assert_equal
      ~ctxt:context
      ~cmp:[%compare.equal: Ast.Reference.t list]
      ~printer:(fun q -> [%sexp_of: Ast.Reference.t list] q |> Sexp.to_string_hum)
      expected
      actual
  in
  let assert_module_tracked ~context ~api ~expected qualifier =
    let actual =
      let source_code_api = BuckBasedSourceCodeApi.get_source_code_api api in
      Analysis.SourceCodeApi.is_qualifier_tracked source_code_api qualifier
    in
    assert_equal ~ctxt:context ~cmp:Bool.equal ~printer:Bool.to_string expected actual
  in
  [
    (Test.labeled_test_case Stdlib.__FUNCTION__ Stdlib.__LINE__ ~name:"empty"
    @@ fun context ->
    let api = create (FileLoader.create ()) (Sourcedb.Listing.create ()) in
    assert_lookup_relative_path ~context ~api [];
    assert_type_check_qualifiers ~context ~api []);
    (Test.labeled_test_case Stdlib.__FUNCTION__ Stdlib.__LINE__ ~name:"simple"
    @@ fun context ->
    let api =
      let loader = FileLoader.create_for_testing ["foo.py", ""; "bar.py", ""] in
      let listing =
        Sourcedb.Listing.create_for_testing ~sources:["foo.py"] ~dependencies:["bar.py"] ()
      in
      create loader listing
    in
    assert_lookup_relative_path
      ~context
      ~api
      [!"foo", Some "foo.py"; !"bar", Some "bar.py"; !"baz", None];
    assert_type_check_qualifiers ~context ~api [!"foo"];
    assert_module_tracked ~context ~api !"foo" ~expected:true;
    assert_module_tracked ~context ~api !"bar" ~expected:true;
    assert_module_tracked ~context ~api !"baz" ~expected:false;
    ());
    (Test.labeled_test_case Stdlib.__FUNCTION__ Stdlib.__LINE__ ~name:"conflict mappings in source"
    @@ fun context ->
    let api =
      let loader = FileLoader.create_for_testing ["foo.py", ""; "foo.pyi", ""] in
      let listing =
        Sourcedb.Listing.create_for_testing ~sources:["foo.py"; "foo.pyi"] ~dependencies:[] ()
      in
      create loader listing
    in
    assert_lookup_relative_path ~context ~api [!"foo", Some "foo.pyi"];
    assert_type_check_qualifiers ~context ~api [!"foo"];
    assert_module_tracked ~context ~api !"foo" ~expected:true;
    ());
    (Test.labeled_test_case Stdlib.__FUNCTION__ Stdlib.__LINE__ ~name:"source_overwrites_dep"
    @@ fun context ->
    let api =
      let loader = FileLoader.create_for_testing ["foo.py", ""; "foo.pyi", ""] in
      let listing =
        Sourcedb.Listing.create_for_testing ~sources:["foo.py"] ~dependencies:["foo.pyi"] ()
      in
      create loader listing
    in
    assert_lookup_relative_path ~context ~api [!"foo", Some "foo.py"];
    assert_type_check_qualifiers ~context ~api [!"foo"];
    assert_module_tracked ~context ~api !"foo" ~expected:true;
    ());
    (Test.labeled_test_case Stdlib.__FUNCTION__ Stdlib.__LINE__ ~name:"implicit modules"
    @@ fun context ->
    let api =
      let loader = FileLoader.create_for_testing ["a/b/c.py", ""; "a/d/e.pyi", ""] in
      let listing =
        Sourcedb.Listing.create_for_testing ~sources:["a/b/c.py"] ~dependencies:["a/d/e.pyi"] ()
      in
      create loader listing
    in
    assert_lookup_relative_path
      ~context
      ~api
      [
        !"a.b.c", Some "a/b/c.py"; !"a.b", None; !"a.d.e", Some "a/d/e.pyi"; !"a.d", None; !"a", None;
      ];
    assert_type_check_qualifiers ~context ~api [!"a.b.c"];
    assert_module_tracked ~context ~api !"a" ~expected:true;
    assert_module_tracked ~context ~api !"a.b" ~expected:true;
    assert_module_tracked ~context ~api !"a.b.c" ~expected:true;
    assert_module_tracked ~context ~api !"a.d" ~expected:true;
    assert_module_tracked ~context ~api !"a.d.e" ~expected:true;
    assert_module_tracked ~context ~api !"a.x" ~expected:false;
    ());
    (Test.labeled_test_case Stdlib.__FUNCTION__ Stdlib.__LINE__ ~name:"do not shadow builtins.pyi"
    @@ fun context ->
    let api =
      let loader = FileLoader.create_for_testing ["__init__.py", ""; "builtins.pyi", ""] in
      let listing =
        Sourcedb.Listing.create_for_testing
          ~sources:["__init__.py"]
          ~dependencies:["builtins.pyi"]
          ()
      in
      create loader listing
    in
    assert_lookup_relative_path ~context ~api [!"", Some "__init__.py"];
    ());
  ]


let () =
  "source_code_api" >::: [test_list test_file_loader; test_list test_source_code_api] |> Test.run
