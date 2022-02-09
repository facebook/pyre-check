(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base
open OUnit2

(* Create aliases to private modules so we could test their internal APIs. *)
module BuildMap = Buck__BuildMap
module Builder = Buck__Builder

let assert_mapping_equal ~context ~expected actual =
  assert_equal
    ~ctxt:context
    ~cmp:[%compare.equal: (string * string) list]
    ~printer:(fun items -> [%sexp_of: (string * string) list] items |> Sexp.to_string_hum)
    (expected |> List.sort ~compare:[%compare: string * string])
    (actual |> List.sort ~compare:[%compare: string * string])


let test_lookup_source context =
  let assert_lookup ~source_root ~artifact_root ~build_map ~expected path =
    let index = BuildMap.Partial.of_alist_exn build_map |> BuildMap.create |> BuildMap.index in
    let actual =
      Builder.do_lookup_source
        ~index
        ~source_root:(PyrePath.create_absolute source_root)
        ~artifact_root:(PyrePath.create_absolute artifact_root)
        (PyrePath.create_absolute path)
      |> Option.map ~f:PyrePath.absolute
    in
    assert_equal
      ~ctxt:context
      ~cmp:[%compare.equal: string option]
      ~printer:(fun result -> Sexp.to_string_hum ([%sexp_of: string option] result))
      expected
      actual
  in

  assert_lookup
    ~source_root:"/source"
    ~artifact_root:"/artifact"
    ~build_map:["a.py", "a.py"]
    "/artifact/a.py"
    ~expected:(Some "/source/a.py");
  assert_lookup
    ~source_root:"/source"
    ~artifact_root:"/artifact"
    ~build_map:["a.py", "b.py"]
    "/artifact/a.py"
    ~expected:(Some "/source/b.py");
  assert_lookup
    ~source_root:"/source"
    ~artifact_root:"/artifact"
    ~build_map:["a.py", "a.py"]
    "/artifact/b.py"
    ~expected:None;
  assert_lookup
    ~source_root:"/source"
    ~artifact_root:"/artifact"
    ~build_map:["a.py", "a.py"]
    "/source/a.py"
    ~expected:None;
  assert_lookup
    ~source_root:"/source"
    ~artifact_root:"/artifact"
    ~build_map:["foo/a.py", "a.py"; "bar/b.py", "b.py"]
    "/artifact/foo/b.py"
    ~expected:None;
  assert_lookup
    ~source_root:"/source"
    ~artifact_root:"/artifact"
    ~build_map:["foo/a.py", "a.py"; "bar/b.py", "b.py"]
    "/artifact/bar/b.py"
    ~expected:(Some "/source/b.py");
  assert_lookup
    ~source_root:"/source"
    ~artifact_root:"/artifact"
    ~build_map:["a.py", "foo/a.py"; "b.py", "bar/b.py"]
    "/artifact/a.py"
    ~expected:(Some "/source/foo/a.py");
  assert_lookup
    ~source_root:"/source"
    ~artifact_root:"/artifact"
    ~build_map:["a.py", "a.py"; "b.py", "a.py"]
    "/artifact/a.py"
    ~expected:(Some "/source/a.py");
  assert_lookup
    ~source_root:"/source"
    ~artifact_root:"/artifact"
    ~build_map:["a.py", "a.py"; "b.py", "a.py"]
    "/artifact/b.py"
    ~expected:(Some "/source/a.py");
  ()


let test_lookup_artifact context =
  let assert_lookup ~source_root ~artifact_root ~build_map ~expected path =
    let index = BuildMap.Partial.of_alist_exn build_map |> BuildMap.create |> BuildMap.index in
    let actual =
      Builder.do_lookup_artifact
        ~index
        ~source_root:(PyrePath.create_absolute source_root)
        ~artifact_root:(PyrePath.create_absolute artifact_root)
        (PyrePath.create_absolute path)
      |> List.map ~f:PyrePath.absolute
    in
    assert_equal
      ~ctxt:context
      ~cmp:[%compare.equal: string list]
      ~printer:(fun result -> Sexp.to_string_hum ([%sexp_of: string list] result))
      (List.sort ~compare:String.compare expected)
      (List.sort ~compare:String.compare actual)
  in

  assert_lookup
    ~source_root:"/source"
    ~artifact_root:"/artifact"
    ~build_map:["a.py", "a.py"]
    "/source/a.py"
    ~expected:["/artifact/a.py"];
  assert_lookup
    ~source_root:"/source"
    ~artifact_root:"/artifact"
    ~build_map:["a.py", "b.py"]
    "/source/b.py"
    ~expected:["/artifact/a.py"];
  assert_lookup
    ~source_root:"/source"
    ~artifact_root:"/artifact"
    ~build_map:["a.py", "a.py"]
    "/source/b.py"
    ~expected:[];
  assert_lookup
    ~source_root:"/source"
    ~artifact_root:"/artifact"
    ~build_map:["a.py", "a.py"]
    "/artifact/a.py"
    ~expected:[];
  assert_lookup
    ~source_root:"/source"
    ~artifact_root:"/artifact"
    ~build_map:["foo/a.py", "a.py"; "bar/b.py", "b.py"]
    "/source/b.py"
    ~expected:["/artifact/bar/b.py"];
  assert_lookup
    ~source_root:"/source"
    ~artifact_root:"/artifact"
    ~build_map:["a.py", "foo/a.py"; "b.py", "bar/b.py"]
    "/source/foo/a.py"
    ~expected:["/artifact/a.py"];
  assert_lookup
    ~source_root:"/source"
    ~artifact_root:"/artifact"
    ~build_map:["a.py", "foo/a.py"; "b.py", "bar/b.py"]
    "/source/foo/b.py"
    ~expected:[];
  assert_lookup
    ~source_root:"/source"
    ~artifact_root:"/artifact"
    ~build_map:["a.py", "a.py"; "b.py", "a.py"]
    "/source/a.py"
    ~expected:["/artifact/a.py"; "/artifact/b.py"];
  assert_lookup
    ~source_root:"/source"
    ~artifact_root:"/artifact"
    ~build_map:["foo/a.py", "baz/a.py"; "bar/b.py", "baz/a.py"]
    "/source/baz/a.py"
    ~expected:["/artifact/foo/a.py"; "/artifact/bar/b.py"];
  ()


let assert_difference_equal ~context ~expected actual =
  let compare = [%compare: string * BuildMap.Difference.Kind.t] in
  assert_equal
    ~ctxt:context
    ~cmp:[%compare.equal: (string * BuildMap.Difference.Kind.t) list]
    ~printer:(fun result ->
      [%sexp_of: (string * BuildMap.Difference.Kind.t) list] result |> Sexp.to_string_hum)
    (List.sort ~compare expected)
    (List.sort ~compare actual)


let test_difference_from_removed_relative_paths context =
  let assert_difference ~expected ~paths build_map =
    let build_map_index =
      BuildMap.Partial.of_alist_exn build_map |> BuildMap.create |> BuildMap.index
    in
    let actual =
      Builder.compute_difference_from_removed_relative_paths ~build_map_index paths
      |> BuildMap.Difference.to_alist
    in
    assert_difference_equal ~context ~expected actual
  in

  assert_difference [] ~paths:["source/foo.py"] ~expected:[];
  assert_difference
    ["foo.py", "source/foo.py"]
    ~paths:["source/foo.py"]
    ~expected:["foo.py", Deleted];
  assert_difference
    ["foo.py", "source/foo.py"; "foo2.py", "source/foo2.py"]
    ~paths:["source/foo.py"]
    ~expected:["foo.py", Deleted];
  assert_difference
    ["foo.py", "source/foo.py"; "foo2.py", "source/foo.py"]
    ~paths:["source/foo.py"]
    ~expected:["foo.py", Deleted; "foo2.py", Deleted];
  assert_difference
    ["foo.py", "source/foo.py"; "foo2.py", "source/foo2.py"]
    ~paths:["source/foo.py"; "source/foo2.py"]
    ~expected:["foo.py", Deleted; "foo2.py", Deleted];
  ()


let test_difference_from_changed_relative_paths context =
  let assert_difference ~expected ~paths build_map =
    let build_map_index =
      BuildMap.Partial.of_alist_exn build_map |> BuildMap.create |> BuildMap.index
    in
    let actual =
      Builder.compute_difference_from_changed_relative_paths ~build_map_index paths
      |> BuildMap.Difference.to_alist
    in
    assert_difference_equal ~context ~expected actual
  in

  assert_difference [] ~paths:["source/foo.py"] ~expected:[];
  assert_difference
    ["foo.py", "source/foo.py"]
    ~paths:["source/foo.py"]
    ~expected:["foo.py", Changed "source/foo.py"];
  assert_difference
    ["foo.py", "source/foo.py"; "foo2.py", "source/foo2.py"]
    ~paths:["source/foo.py"]
    ~expected:["foo.py", Changed "source/foo.py"];
  assert_difference
    ["foo.py", "source/foo.py"; "foo2.py", "source/foo.py"]
    ~paths:["source/foo.py"]
    ~expected:["foo.py", Changed "source/foo.py"; "foo2.py", Changed "source/foo.py"];
  assert_difference
    ["foo.py", "source/foo.py"; "foo2.py", "source/foo2.py"]
    ~paths:["source/foo.py"; "source/foo2.py"]
    ~expected:["foo.py", Changed "source/foo.py"; "foo2.py", Changed "source/foo2.py"];
  ()


let () =
  "builder_test"
  >::: [
         "lookup_source" >:: test_lookup_source;
         "lookup_artifact" >:: test_lookup_artifact;
         "difference_from_removed_relative_paths" >:: test_difference_from_removed_relative_paths;
         "difference_from_changed_relative_paths" >:: test_difference_from_changed_relative_paths;
       ]
  |> Test.run
