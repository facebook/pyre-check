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


let test_merge_build_map context =
  let assert_loaded ~targets ~build_map target_and_build_maps =
    let actual_targets, actual_build_map =
      List.map target_and_build_maps ~f:(fun (target, build_map) ->
          Target.of_string target, BuildMap.Partial.of_alist_exn build_map)
      |> Interface.merge_build_maps
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
         "load_parital_build_map" >:: test_load_partial_build_map;
         "merge_build_map" >:: test_merge_build_map;
         "buck_changed_targets_to_build_map" >:: test_buck_changed_targets_to_build_map;
       ]
  |> Test.run
