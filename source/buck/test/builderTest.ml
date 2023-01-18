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
module Interface = Buck__Interface

let assert_mapping_equal ~context ~expected actual =
  assert_equal
    ~ctxt:context
    ~cmp:[%compare.equal: (string * string) list]
    ~printer:(fun items -> [%sexp_of: (string * string) list] items |> Sexp.to_string_hum)
    (expected |> List.sort ~compare:[%compare: string * string])
    (actual |> List.sort ~compare:[%compare: string * string])


let assert_artifact_event_equal ~context ~expected actual =
  assert_equal
    ~ctxt:context
    ~cmp:[%compare.equal: ArtifactPath.Event.t list]
    ~printer:(fun items -> [%sexp_of: ArtifactPath.Event.t list] items |> Sexp.to_string_hum)
    (expected |> List.sort ~compare:[%compare: ArtifactPath.Event.t])
    (actual |> List.sort ~compare:[%compare: ArtifactPath.Event.t])


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
      Builder.Classic.V1.compute_difference_from_removed_relative_paths ~build_map_index paths
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
      Builder.Classic.V1.compute_difference_from_changed_relative_paths ~build_map_index paths
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


let test_lazy_build context =
  let source_root = bracket_tmpdir context |> PyrePath.create_absolute in
  let artifact_root = bracket_tmpdir context |> PyrePath.create_absolute in

  let source_path0 = PyrePath.create_relative ~root:source_root ~relative:"a.py" in
  let source_path1 = PyrePath.create_relative ~root:source_root ~relative:"b.py" in
  File.create source_path0 ~content:"" |> File.write;
  File.create source_path1 ~content:"" |> File.write;
  let source_path0 = SourcePath.create source_path0 in
  let source_path1 = SourcePath.create source_path1 in
  let artifact_path0 =
    PyrePath.create_relative ~root:artifact_root ~relative:"foo/a.py" |> ArtifactPath.create
  in
  let artifact_path1 =
    PyrePath.create_relative ~root:artifact_root ~relative:"bar/b.py" |> ArtifactPath.create
  in

  let mock_interface =
    let construct_build_map paths =
      (* Create a simple build map containing source_path0 or source_path1, depending on which ones
         are specified. *)
      let contains = List.mem ~equal:String.equal in
      let result = [] in
      let result = if contains paths "a.py" then ("foo/a.py", "a.py") :: result else result in
      let result = if contains paths "b.py" then ("bar/b.py", "b.py") :: result else result in
      BuildMap.Partial.of_alist_exn result |> BuildMap.create |> Lwt.return
    in
    Interface.Lazy.create_for_testing ~construct_build_map ()
  in
  let builder = Builder.Lazy.create ~source_root ~artifact_root mock_interface in
  let empty_build_map = BuildMap.(Partial.empty |> create) in

  let open Lwt.Infix in
  (* Building for empty file set results in empty build map *)
  Builder.Lazy.incremental_build builder ~old_build_map:empty_build_map ~source_paths:[]
  >>= fun { Builder.Lazy.IncrementalBuildResult.build_map; changed_artifacts } ->
  assert_bool "build map should be empty" (BuildMap.to_alist build_map |> List.is_empty);
  assert_bool "changed artifacts be empty" (List.is_empty changed_artifacts);

  (* Building for one file results in 1-element build map *)
  Builder.Lazy.incremental_build builder ~old_build_map:build_map ~source_paths:[source_path0]
  >>= fun { Builder.Lazy.IncrementalBuildResult.build_map; changed_artifacts } ->
  assert_mapping_equal ~context ~expected:["foo/a.py", "a.py"] (BuildMap.to_alist build_map);
  assert_artifact_event_equal
    ~context
    ~expected:[ArtifactPath.Event.(create ~kind:Kind.CreatedOrChanged artifact_path0)]
    changed_artifacts;

  (* Incrementally add 1 file to build map *)
  Builder.Lazy.incremental_build
    builder
    ~old_build_map:build_map
    ~source_paths:[source_path0; source_path1]
  >>= fun { Builder.Lazy.IncrementalBuildResult.build_map; changed_artifacts } ->
  assert_mapping_equal
    ~context
    ~expected:["foo/a.py", "a.py"; "bar/b.py", "b.py"]
    (BuildMap.to_alist build_map);
  assert_artifact_event_equal
    ~context
    ~expected:[ArtifactPath.Event.(create ~kind:Kind.CreatedOrChanged artifact_path1)]
    changed_artifacts;

  (* Incrementally remove 1 file from build map *)
  Builder.Lazy.incremental_build builder ~old_build_map:build_map ~source_paths:[source_path1]
  >>= fun { Builder.Lazy.IncrementalBuildResult.build_map; changed_artifacts } ->
  assert_mapping_equal ~context ~expected:["bar/b.py", "b.py"] (BuildMap.to_alist build_map);
  assert_artifact_event_equal
    ~context
    ~expected:[ArtifactPath.Event.(create ~kind:Kind.Deleted artifact_path0)]
    changed_artifacts;
  Lwt.return_unit


let () =
  "builder_test"
  >::: [
         "lookup_source" >:: test_lookup_source;
         "lookup_artifact" >:: test_lookup_artifact;
         "difference_from_removed_relative_paths" >:: test_difference_from_removed_relative_paths;
         "difference_from_changed_relative_paths" >:: test_difference_from_changed_relative_paths;
         "lazy_build" >:: OUnitLwt.lwt_wrapper test_lazy_build;
       ]
  |> Test.run
