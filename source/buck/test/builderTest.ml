(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base
open OUnit2
module Path = Pyre.Path

(* Create aliases to private modules so we could test their internal APIs. *)
module BuildMap = Buck__BuildMap
module Builder = Buck__Builder
module Target = Buck__Target

let test_parse_buck_normalized_targets_query_output context =
  let assert_parsed ~expected output =
    let actual = Builder.parse_buck_normalized_targets_query_output output in
    assert_equal
      ~ctxt:context
      ~cmp:[%compare.equal: string list]
      ~printer:(fun targets -> Sexp.to_string_hum ([%sexp_of: string list] targets))
      expected
      actual
  in
  let assert_not_parsed output =
    try
      let _ = Builder.parse_buck_normalized_targets_query_output output in
      let message = Format.sprintf "Unexpected parsing success: %s" output in
      assert_failure message
    with
    | Builder.JsonError _ -> ()
  in

  assert_parsed "{}" ~expected:[];
  assert_parsed {|
     {"//foo:bar":[]}
  |} ~expected:[];
  assert_parsed {|
     {"//foo:bar":["//foo:bar"]}
  |} ~expected:["//foo:bar"];
  assert_parsed
    {|
      {
        "//foo:bar":["//foo:bar"],
        "//foo:baz":["//foo:baz"]
      }
    |}
    ~expected:["//foo:bar"; "//foo:baz"];
  assert_parsed
    {|
      {
        "//foo:bar":["//foo:bar", "//foo:qux"],
        "//foo:baz":["//foo:baz", "//foo:bar"]
      }
    |}
    ~expected:["//foo:bar"; "//foo:baz"; "//foo:qux"];
  assert_parsed
    {|
      {
        "//foo:bar":["//foo:bar", "//foo:baz-mypy_ini", "foo:qux-testmodules-lib"]
      }
    |}
    ~expected:["//foo:bar"];

  assert_not_parsed "42";
  assert_not_parsed "derp";
  assert_not_parsed {|"abc"|};
  assert_not_parsed "[]";
  assert_not_parsed {| { foo: 42 } |};
  assert_not_parsed {| { "foo": 42 } |};
  assert_not_parsed {| { "foo": { "bar": 42 } } |};
  assert_not_parsed {| { "foo": [], "bar": 42 } |};
  assert_not_parsed {| { "foo": [ 42 ] } |};
  assert_not_parsed {| { "foo": [ { "bar": 42 } ] } |};
  ()


let test_parse_buck_changed_targets_query_output context =
  let assert_parsed ~expected output =
    let actual = Builder.parse_buck_changed_targets_query_output output in
    assert_equal
      ~ctxt:context
      ~cmp:[%compare.equal: Builder.BuckChangedTargetsQueryOutput.t list]
      ~printer:(fun results ->
        Sexp.to_string_hum ([%sexp_of: Builder.BuckChangedTargetsQueryOutput.t list] results))
      expected
      actual
  in
  let assert_not_parsed output =
    try
      let _ = Builder.parse_buck_changed_targets_query_output output in
      let message = Format.sprintf "Unexpected parsing success: %s" output in
      assert_failure message
    with
    | Builder.JsonError _ -> ()
  in
  let module Output = Builder.BuckChangedTargetsQueryOutput in
  assert_parsed "{}" ~expected:[];
  assert_parsed
    {|
        {
          "//foo:bar": {
            "srcs": { "a.py": "b.py" },
            "buck.base_path" : "foo/bar"
          }
        }
      |}
    ~expected:
      [
        {
          Output.source_base_path = "foo/bar";
          artifact_base_path = "foo/bar";
          artifacts_to_sources = ["a.py", "b.py"];
        };
      ];
  assert_parsed
    {|
        {
          "//foo:bar": {
            "srcs": { "a.py": "b.py" },
            "buck.base_path": "foo/bar",
            "buck.base_module": "foo.baz"
          }
        }
      |}
    ~expected:
      [
        {
          Output.source_base_path = "foo/bar";
          artifact_base_path = "foo/baz";
          artifacts_to_sources = ["a.py", "b.py"];
        };
      ];
  assert_parsed
    {|
        {
          "//foo:bar": {
            "srcs": { "a.py": "b.py", "c.py": "d.py" },
            "buck.base_path": "foo/bar",
            "base_module": "foo.baz"
          }
        }
      |}
    ~expected:
      [
        {
          Output.source_base_path = "foo/bar";
          artifact_base_path = "foo/baz";
          artifacts_to_sources = ["a.py", "b.py"; "c.py", "d.py"];
        };
      ];
  assert_parsed
    {|
        {
          "//foo:bar": {
            "srcs": { "a.py": "b.py", "c.py": "//derp:generate-version=version.py" },
            "buck.base_path": "foo/bar"
          }
        }
      |}
    ~expected:
      [
        {
          Output.source_base_path = "foo/bar";
          artifact_base_path = "foo/bar";
          artifacts_to_sources = ["a.py", "b.py"];
        };
      ];
  assert_parsed
    {|
        {
          "//foo:bar": {
            "srcs": { "a.py": "b.py" },
            "buck.base_path": "foo/bar"
          },
          "//foo:baz": {
            "srcs": { "c.py": "d.py" },
            "buck.base_path": "foo/baz"
          }
        }
      |}
    ~expected:
      [
        {
          Output.source_base_path = "foo/bar";
          artifact_base_path = "foo/bar";
          artifacts_to_sources = ["a.py", "b.py"];
        };
        {
          Output.source_base_path = "foo/baz";
          artifact_base_path = "foo/baz";
          artifacts_to_sources = ["c.py", "d.py"];
        };
      ];

  assert_not_parsed "42";
  assert_not_parsed "derp";
  assert_not_parsed {|"abc"|};
  assert_not_parsed "[]";
  assert_not_parsed {| { foo: 42 } |};
  assert_not_parsed {| { "foo": 42 } |};
  assert_not_parsed {| { "foo": { "bar": 42 } } |};
  assert_not_parsed {| {"//foo:bar":[]} |};
  assert_not_parsed {| { "foo": [], "bar": 42 } |};
  assert_not_parsed {| { "foo": [ 42 ] } |};
  assert_not_parsed {| { "foo": [ { "bar": 42 } ] } |};
  assert_not_parsed {| { "foo": [ { "srcs": 42 } ] } |};
  assert_not_parsed {| { "foo": [ { "srcs": { "a": "b" } } ] } |};
  assert_not_parsed {| { "foo": [ { "srcs": { "a": "b" }, "buck.base_path": 42 } ] } |};
  ()


let assert_mapping_equal ~context ~expected actual =
  assert_equal
    ~ctxt:context
    ~cmp:[%compare.equal: (string * string) list]
    ~printer:(fun items -> [%sexp_of: (string * string) list] items |> Sexp.to_string_hum)
    (expected |> List.sort ~compare:[%compare: string * string])
    (actual |> List.sort ~compare:[%compare: string * string])


let test_parse_buck_build_output context =
  let assert_parsed ~expected output =
    Builder.parse_buck_build_output output |> assert_mapping_equal ~context ~expected
  in
  let assert_not_parsed output =
    try
      let _ = Builder.parse_buck_build_output output in
      let message = Format.sprintf "Unexpected parsing success: %s" output in
      assert_failure message
    with
    | Builder.JsonError _ -> ()
  in

  assert_parsed "{}" ~expected:[];
  assert_parsed
    {|
     {"//foo:bar": "/path/to/sourcedb.json"}
  |}
    ~expected:["//foo:bar", "/path/to/sourcedb.json"];
  assert_parsed
    {|
      {
        "//foo:bar":"/path/to/bar.json",
        "//foo:baz":"/path/to/baz.json"
      }
    |}
    ~expected:["//foo:bar", "/path/to/bar.json"; "//foo:baz", "/path/to/baz.json"];

  assert_not_parsed "42";
  assert_not_parsed "derp";
  assert_not_parsed {|"abc"|};
  assert_not_parsed "[]";
  assert_not_parsed {| { foo: 42 } |};
  assert_not_parsed {| { "foo": 42 } |};
  assert_not_parsed {| { "foo": { "bar": 42 } } |};
  assert_not_parsed {| { "foo": "derp", "bar": 42 } |};
  assert_not_parsed {| { "foo": [ "bar" ] } |};
  ()


let test_load_partial_build_map context =
  let assert_loaded ~expected input =
    Yojson.Safe.from_string input
    |> Builder.load_partial_build_map_from_json
    |> BuildMap.Partial.to_alist
    |> assert_mapping_equal ~context ~expected
  in
  assert_loaded
    {| {
      "sources": {
        "foo.py": "source/foo.py"
      },
      "dependencies": {
        "bar.py": "source/bar.py"
      }
  }|}
    ~expected:["foo.py", "source/foo.py"; "bar.py", "source/bar.py"];
  (* Special-cased entries are ignored. *)
  assert_loaded
    {| {
      "sources": {
        "foo.py": "source/foo.py",
        "__manifest__.py": "generated/__manifest__.py"
      },
      "dependencies": {
      }
  }|}
    ~expected:["foo.py", "source/foo.py"];
  assert_loaded
    {| {
      "sources": {
        "foo.py": "source/foo.py",
        "__test_main__.py": "generated/__test_main__.py"
      },
      "dependencies": {
      }
  }|}
    ~expected:["foo.py", "source/foo.py"];
  assert_loaded
    {| {
      "sources": {
        "foo.py": "source/foo.py",
        "__test_modules__.py": "generated/__test_modules__.py"
      },
      "dependencies": {
        "__test_modules__.py": "generated/__test_modules__.py"
      }
  }|}
    ~expected:["foo.py", "source/foo.py"];
  ()


let test_merge_build_map context =
  let assert_loaded ~targets ~build_map target_and_build_maps =
    let actual_targets, actual_build_map =
      List.map target_and_build_maps ~f:(fun (target, build_map) ->
          Target.of_string target, BuildMap.Partial.of_alist_exn build_map)
      |> Builder.merge_build_maps
    in
    assert_equal
      ~ctxt:context
      ~cmp:[%compare.equal: string list]
      ~printer:(fun targets -> Sexp.to_string_hum ([%sexp_of: string list] targets))
      targets
      (List.map actual_targets ~f:Target.show);
    let actual_build_map = BuildMap.to_alist actual_build_map in
    let compare = [%compare: string * string] in
    assert_equal
      ~ctxt:context
      ~cmp:[%compare.equal: (string * string) list]
      ~printer:(fun mappings -> Sexp.to_string_hum ([%sexp_of: (string * string) list] mappings))
      (List.sort ~compare build_map)
      (List.sort ~compare actual_build_map)
  in

  assert_loaded [] ~targets:[] ~build_map:[];
  assert_loaded
    ["//foo:bar", ["a.py", "source/a.py"]]
    ~targets:["//foo:bar"]
    ~build_map:["a.py", "source/a.py"];
  assert_loaded
    ["//foo:bar", ["a.py", "source/a.py"; "b.py", "source/b.py"]]
    ~targets:["//foo:bar"]
    ~build_map:["a.py", "source/a.py"; "b.py", "source/b.py"];
  assert_loaded
    ["//foo:bar", ["a.py", "source/a.py"]; "//foo:baz", ["b.py", "source/b.py"]]
    ~targets:["//foo:bar"; "//foo:baz"]
    ~build_map:["a.py", "source/a.py"; "b.py", "source/b.py"];
  assert_loaded
    [
      "//foo:bar", ["a.py", "source/a.py"; "b.py", "source/b.py"];
      "//foo:baz", ["b.py", "source/b.py"];
    ]
    ~targets:["//foo:bar"; "//foo:baz"]
    ~build_map:["a.py", "source/a.py"; "b.py", "source/b.py"];
  assert_loaded
    [
      "//foo:bar", ["a.py", "source/a.py"; "x.py", "source/b.py"];
      (* Conflict on `x.py` *)
      "//foo:baz", ["d.py", "source/d.py"; "x.py", "source/c.py"];
      "//foo:qux", ["e.py", "source/e.py"];
    ]
    ~targets:["//foo:bar"; "//foo:qux"]
    ~build_map:["a.py", "source/a.py"; "x.py", "source/b.py"; "e.py", "source/e.py"];
  assert_loaded
    [
      "//foo:bar", ["a.py", "source/a.py"];
      "//foo:baz", ["b.py", "source/b.py"; "x.py", "source/c.py"];
      (* Conflict on `x.py` *)
      "//foo:qux", ["e.py", "source/e.py"; "x.py", "source/d.py"];
    ]
    ~targets:["//foo:bar"; "//foo:baz"]
    ~build_map:["a.py", "source/a.py"; "b.py", "source/b.py"; "x.py", "source/c.py"];
  assert_loaded
    [
      "//foo:bar", ["a.py", "source/a.py"; "x.py", "source/b.py"];
      (* Conflict on `x.py` *)
      "//foo:baz", ["d.py", "source/d.py"; "x.py", "source/c.py"];
      (* Conflict on `x.py` *)
      "//foo:qux", ["e.py", "source/e.py"; "x.py", "source/f.py"];
    ]
    ~targets:["//foo:bar"]
    ~build_map:["a.py", "source/a.py"; "x.py", "source/b.py"];
  ()


let test_lookup_source context =
  let assert_lookup ~source_root ~artifact_root ~build_map ~expected path =
    let index = BuildMap.Partial.of_alist_exn build_map |> BuildMap.create |> BuildMap.index in
    let actual =
      Builder.do_lookup_source
        ~index
        ~source_root:(Path.create_absolute source_root)
        ~artifact_root:(Path.create_absolute artifact_root)
        (Path.create_absolute path)
      |> Option.map ~f:Path.absolute
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
        ~source_root:(Path.create_absolute source_root)
        ~artifact_root:(Path.create_absolute artifact_root)
        (Path.create_absolute path)
      |> List.map ~f:Path.absolute
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


let test_buck_changed_targets_to_build_map context =
  let assert_build_map ~expected changed_targets =
    let actual =
      Builder.BuckChangedTargetsQueryOutput.to_build_map_batch changed_targets
      |> Result.ok_or_failwith
      |> BuildMap.to_alist
    in
    assert_mapping_equal ~context ~expected actual
  in
  let assert_no_build_map changed_targets =
    match Builder.BuckChangedTargetsQueryOutput.to_build_map_batch changed_targets with
    | Result.Error _ -> ()
    | Result.Ok _ ->
        let message =
          Format.asprintf
            "Unexpected parsing success: %a"
            Sexp.pp
            ([%sexp_of: Builder.BuckChangedTargetsQueryOutput.t list] changed_targets)
        in
        assert_failure message
  in
  let module Output = Builder.BuckChangedTargetsQueryOutput in
  assert_build_map
    [
      {
        Output.source_base_path = "foo";
        Output.artifact_base_path = "bar";
        artifacts_to_sources = ["a.py", "b.py"];
      };
    ]
    ~expected:["bar/a.py", "foo/b.py"];
  assert_build_map
    [
      {
        Output.source_base_path = "foo";
        Output.artifact_base_path = "bar";
        artifacts_to_sources = ["a.py", "b.py"];
      };
      {
        Output.source_base_path = "foo";
        Output.artifact_base_path = "baz";
        artifacts_to_sources = ["c.py", "d.py"];
      };
    ]
    ~expected:["bar/a.py", "foo/b.py"; "baz/c.py", "foo/d.py"];

  (* Single partial build map conflicting on `bar/a.py`. *)
  assert_no_build_map
    [
      {
        Output.source_base_path = "foo";
        Output.artifact_base_path = "bar";
        artifacts_to_sources = ["a.py", "b.py"; "a.py", "c.py"];
      };
    ];
  (* Different partial build maps conflicting on `bar/a.py`. *)
  assert_no_build_map
    [
      {
        Output.source_base_path = "foo";
        Output.artifact_base_path = "bar";
        artifacts_to_sources = ["a.py", "b.py"];
      };
      {
        Output.source_base_path = "foo";
        Output.artifact_base_path = "bar";
        artifacts_to_sources = ["a.py", "c.py"];
      };
    ];
  ()


let () =
  "builder_test"
  >::: [
         "parse_buck_normalized_targets_query_output"
         >:: test_parse_buck_normalized_targets_query_output;
         "parse_buck_changed_targets_query_output" >:: test_parse_buck_changed_targets_query_output;
         "parse_buck_build_output" >:: test_parse_buck_build_output;
         "load_parital_build_map" >:: test_load_partial_build_map;
         "merge_build_map" >:: test_merge_build_map;
         "lookup_source" >:: test_lookup_source;
         "lookup_artifact" >:: test_lookup_artifact;
         "difference_from_removed_relative_paths" >:: test_difference_from_removed_relative_paths;
         "difference_from_changed_relative_paths" >:: test_difference_from_changed_relative_paths;
         "buck_changed_targets_to_build_map" >:: test_buck_changed_targets_to_build_map;
       ]
  |> Test.run
