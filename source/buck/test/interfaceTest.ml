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
module Interface = Buck__Interface
module Target = Buck__Target

let test_parse_buck_normalized_targets_query_output context =
  let assert_parsed ~expected output =
    let actual = Interface.parse_buck_normalized_targets_query_output output in
    assert_equal
      ~ctxt:context
      ~cmp:[%compare.equal: string list]
      ~printer:(fun targets -> Sexp.to_string_hum ([%sexp_of: string list] targets))
      expected
      actual
  in
  let assert_not_parsed output =
    try
      let _ = Interface.parse_buck_normalized_targets_query_output output in
      let message = Format.sprintf "Unexpected parsing success: %s" output in
      assert_failure message
    with
    | Interface.JsonError _ -> ()
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
    let actual = Interface.parse_buck_changed_targets_query_output output in
    assert_equal
      ~ctxt:context
      ~cmp:[%compare.equal: Interface.BuckChangedTargetsQueryOutput.t list]
      ~printer:(fun results ->
        Sexp.to_string_hum ([%sexp_of: Interface.BuckChangedTargetsQueryOutput.t list] results))
      expected
      actual
  in
  let assert_not_parsed output =
    try
      let _ = Interface.parse_buck_changed_targets_query_output output in
      let message = Format.sprintf "Unexpected parsing success: %s" output in
      assert_failure message
    with
    | Interface.JsonError _ -> ()
  in
  let module Output = Interface.BuckChangedTargetsQueryOutput in
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
    Interface.parse_buck_build_output output |> assert_mapping_equal ~context ~expected
  in
  let assert_not_parsed output =
    try
      let _ = Interface.parse_buck_build_output output in
      let message = Format.sprintf "Unexpected parsing success: %s" output in
      assert_failure message
    with
    | Interface.JsonError _ -> ()
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


let assert_targets_equal ~context ~expected actual =
  assert_equal
    ~ctxt:context
    ~cmp:[%compare.equal: Target.t list]
    ~printer:(fun items -> [%sexp_of: Target.t list] items |> Sexp.to_string_hum)
    (expected |> List.sort ~compare:[%compare: Target.t])
    (actual |> List.sort ~compare:[%compare: Target.t])


let assert_conflicts_equal ~context ~expected actual =
  let compare = [%compare: string * Interface.V2.BuckBxlBuilderOutput.Conflict.t] in
  assert_equal
    ~ctxt:context
    ~cmp:[%compare.equal: (string * Interface.V2.BuckBxlBuilderOutput.Conflict.t) list]
    ~printer:(fun items ->
      [%sexp_of: (string * Interface.V2.BuckBxlBuilderOutput.Conflict.t) list] items
      |> Sexp.to_string_hum)
    (expected |> List.sort ~compare)
    (actual |> List.sort ~compare)


let test_parse_merged_sourcedb context =
  let assert_parsed ~expected_build_map ~expected_targets ~expected_conflicts output =
    let {
      Interface.V2.BuckBxlBuilderOutput.build_map = actual_build_map;
      targets = actual_targets;
      conflicts = actual_conflicts;
    }
      =
      Yojson.Safe.from_string output |> Interface.V2.parse_merged_sourcedb
    in
    assert_mapping_equal ~context ~expected:expected_build_map (BuildMap.to_alist actual_build_map);
    assert_targets_equal
      ~context
      ~expected:(List.map expected_targets ~f:Target.of_string)
      actual_targets;
    assert_conflicts_equal
      ~context
      ~expected:expected_conflicts
      (List.map actual_conflicts ~f:(fun (target, conflict) -> Target.show target, conflict))
  in
  let assert_not_parsed output =
    try
      let _ = Interface.parse_buck_build_output output in
      let message = Format.sprintf "Unexpected parsing success: %s" output in
      assert_failure message
    with
    | Interface.JsonError _ -> ()
  in
  assert_not_parsed "42";
  assert_not_parsed "derp";
  assert_not_parsed {|"abc"|};
  assert_not_parsed "[]";
  assert_not_parsed {| { "foo": 42 } |};
  assert_not_parsed {| { "build_map": {} } |};

  assert_parsed
    {| { "build_map": {}, "built_targets": [], "dropped_targets": {} } |}
    ~expected_build_map:[]
    ~expected_targets:[]
    ~expected_conflicts:[];
  assert_parsed
    {| {
         "build_map": {
           "a.py": "source/a.py",
           "b.py": "source/b.py"
         },
         "built_targets": ["//targets:a", "//targets:b"],
         "dropped_targets": {
           "//targets:c": {
             "conflict_with": "//targets:a",
             "artifact_path": "a.py",
             "preserved_source_path": "source/a.py",
             "dropped_source_path": "source/c.py"
           }
         }
    } |}
    ~expected_build_map:["a.py", "source/a.py"; "b.py", "source/b.py"]
    ~expected_targets:["//targets:a"; "//targets:b"]
    ~expected_conflicts:
      [
        ( "//targets:c",
          {
            Interface.V2.BuckBxlBuilderOutput.Conflict.conflict_with = "//targets:a";
            artifact_path = "a.py";
            preserved_source_path = "source/a.py";
            dropped_source_path = "source/c.py";
          } );
      ];
  ()


let test_parse_buck_bxl_output context =
  let assert_parsed output = Interface.V2.parse_bxl_output output |> ignore in
  let assert_not_parsed output =
    try
      let _ = Interface.V2.parse_bxl_output output in
      let message = Format.sprintf "Unexpected parsing success: %s" output in
      assert_failure message
    with
    | Interface.JsonError _ -> ()
  in
  let create_db_output path =
    `Assoc ["db", `String (PyrePath.absolute path)] |> Yojson.Safe.to_string
  in

  let root = bracket_tmpdir context |> PyrePath.create_absolute in
  let well_formed_path = PyrePath.create_relative ~root ~relative:"well_formed.json" in
  let malformed_path = PyrePath.create_relative ~root ~relative:"malformed.json" in
  File.create
    well_formed_path
    ~content:{| { "build_map": {}, "built_targets": [], "dropped_targets": {} } |}
  |> File.write;
  File.create malformed_path ~content:{| { "derp": 42 } |} |> File.write;
  let nonexistent_path = PyrePath.create_relative ~root ~relative:"nonexistent.json" in

  assert_not_parsed "{}";
  assert_not_parsed {| { "db": 42 }|};
  assert_not_parsed (create_db_output nonexistent_path);
  assert_not_parsed (create_db_output malformed_path);

  assert_parsed (create_db_output well_formed_path);
  ()


let test_load_partial_build_map context =
  let assert_loaded ~expected input =
    Yojson.Safe.from_string input
    |> Interface.load_partial_build_map_from_json
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


let test_merge_build_map_by_name context =
  let assert_loaded ~targets ~build_map target_and_build_maps =
    let actual_targets, actual_build_map =
      List.map target_and_build_maps ~f:(fun (target, build_map) ->
          Target.of_string target, BuildMap.Partial.of_alist_exn build_map)
      |> Interface.(merge_build_maps ~resolve_conflict:resolve_merge_conflict_by_name)
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


let test_merge_build_map_by_name_and_content context =
  let assert_loaded ~targets ~included target_and_file_map =
    (* Some explanation on the test setup: we expect each test to specify a [target_and_file_map],
       which is an alist from targets to another alist of relative path -> file content mapping. The
       test function will attempt to create a bunch of partial build maps out of the mappings, merge
       them with the name+content conflict handler, and assert on how many targets get kept and what
       keys are remained in the merged build map.

       For example, if [target_and_file_map] looks like this:
     * { "//foo": { "a.py": "a = 1", "b.py": "b = 2" }, "//bar": { "c.py": "c = 3"} }

       Then we are going to lay out the source dir as follows:
     *  Source root is a temporary dir `root/`.
     *  `root/target0_a.py` has content "a = 1"
     *  `root/target0_b.py` has content "b = 2"
     *  `root/target1_c.py` has content "c = 3"

     * Partial build map of `//foo` will be { "a.py": "root/target0_a.py", "b.py": "root/target0_b.py" }
     * Partial build map of `//bar` will be { "c.py": "root/target1_c.py" }

       The basic idea here is to lay out all specified files&contents under the source root,
       with some name mangling on source path side to avoid overwriting. This way, we could fine-tune
       the content of each source file, and observe the build map merger's behavior when a conflict
       occurs on the artifact path side. *)
    let source_root = bracket_tmpdir context |> PyrePath.create_absolute in
    let actual_targets, actual_build_map =
      let get_mangled_source_path index original_path =
        Format.sprintf "target%d_%s" index original_path
      in
      List.mapi target_and_file_map ~f:(fun index (target, file_map) ->
          List.iter file_map ~f:(fun (path, content) ->
              let source_path =
                PyrePath.create_relative
                  ~root:source_root
                  ~relative:(get_mangled_source_path index path)
              in
              File.create source_path ~content |> File.write);
          let build_map =
            (* Using a simple identity build map *)
            List.map file_map ~f:(fun (path, _) -> path, get_mangled_source_path index path)
            |> BuildMap.Partial.of_alist_exn
          in
          Target.of_string target, build_map)
      |> Interface.(
           merge_build_maps
             ~resolve_conflict:(resolve_merge_conflict_by_name_and_content ~source_root))
    in
    assert_equal
      ~ctxt:context
      ~cmp:[%compare.equal: string list]
      ~printer:(fun targets -> Sexp.to_string_hum ([%sexp_of: string list] targets))
      targets
      (List.map actual_targets ~f:Target.show);
    let actual_included = BuildMap.to_alist actual_build_map |> List.map ~f:fst in
    assert_equal
      ~ctxt:context
      ~cmp:[%compare.equal: string list]
      ~printer:(fun paths -> Sexp.to_string_hum ([%sexp_of: string list] paths))
      (List.sort ~compare:String.compare included)
      (List.sort ~compare:String.compare actual_included)
  in

  assert_loaded [] ~targets:[] ~included:[];
  assert_loaded ["//foo:bar", ["a.py", "a = 1"]] ~targets:["//foo:bar"] ~included:["a.py"];
  assert_loaded
    ["//foo:bar", ["a.py", "a = 1"; "b.py", "b = 2"]]
    ~targets:["//foo:bar"]
    ~included:["a.py"; "b.py"];
  assert_loaded
    ["//foo:bar", ["a.py", "a = 1"]; "//foo:baz", ["b.py", "b = 2"]]
    ~targets:["//foo:bar"; "//foo:baz"]
    ~included:["a.py"; "b.py"];

  (* Name conflict on b.py but content matches *)
  assert_loaded
    ["//foo:bar", ["a.py", "a = 1"; "b.py", "b = 2"]; "//foo:baz", ["b.py", "b = 2"]]
    ~targets:["//foo:bar"; "//foo:baz"]
    ~included:["a.py"; "b.py"];
  assert_loaded
    ["//foo:bar", ["b.py", "b = 2"]; "//foo:baz", ["a.py", "a = 1"; "b.py", "b = 2"]]
    ~targets:["//foo:bar"; "//foo:baz"]
    ~included:["a.py"; "b.py"];
  assert_loaded
    [
      "//foo:bar", ["a.py", "a = 1"; "b.py", "b = 2"];
      "//foo:baz", ["a.py", "a = 1"; "b.py", "b = 2"];
    ]
    ~targets:["//foo:bar"; "//foo:baz"]
    ~included:["a.py"; "b.py"];

  (* Name and content conflict on b.py *)
  assert_loaded
    ["//foo:bar", ["a.py", "a = 1"; "b.py", "b = 2"]; "//foo:baz", ["b.py", "b = 3"]]
    ~targets:["//foo:bar"]
    ~included:["a.py"; "b.py"];
  assert_loaded
    ["//foo:bar", ["b.py", "b = 2"]; "//foo:baz", ["a.py", "a = 1"; "b.py", "b = 3"]]
    ~targets:["//foo:bar"]
    ~included:["b.py"];
  assert_loaded
    [
      "//foo:bar", ["a.py", "a = 1"; "b.py", "b = 2"];
      "//foo:baz", ["a.py", "a = 1"; "b.py", "b = 3"];
    ]
    ~targets:["//foo:bar"]
    ~included:["a.py"; "b.py"];

  assert_loaded
    [
      "//foo:bar", ["b.py", "b = 2"];
      "//foo:baz", ["a.py", "a = 1"; "b.py", "b = 2"];
      "//foo:qux", ["b.py", "b = 3"; "c.py", "c = 3"];
    ]
    ~targets:["//foo:bar"; "//foo:baz"]
    ~included:["a.py"; "b.py"];
  ()


let test_buck_changed_targets_to_build_map context =
  let assert_build_map ~expected changed_targets =
    let actual =
      Interface.BuckChangedTargetsQueryOutput.to_build_map_batch changed_targets
      |> Result.ok_or_failwith
      |> BuildMap.to_alist
    in
    assert_mapping_equal ~context ~expected actual
  in
  let assert_no_build_map changed_targets =
    match Interface.BuckChangedTargetsQueryOutput.to_build_map_batch changed_targets with
    | Result.Error _ -> ()
    | Result.Ok _ ->
        let message =
          Format.asprintf
            "Unexpected parsing success: %a"
            Sexp.pp
            ([%sexp_of: Interface.BuckChangedTargetsQueryOutput.t list] changed_targets)
        in
        assert_failure message
  in
  let module Output = Interface.BuckChangedTargetsQueryOutput in
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
         "parse_merged_sourcedb" >:: test_parse_merged_sourcedb;
         "parse_buck_bxl_output" >:: test_parse_buck_bxl_output;
         "load_parital_build_map" >:: test_load_partial_build_map;
         "merge_build_map_by_name" >:: test_merge_build_map_by_name;
         "merge_build_map_by_name_and_content" >:: test_merge_build_map_by_name_and_content;
         "buck_changed_targets_to_build_map" >:: test_buck_changed_targets_to_build_map;
       ]
  |> Test.run
