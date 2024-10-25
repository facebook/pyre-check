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
module Target = Buck__BuckTarget

let assert_mapping_equal ~context ~expected actual =
  assert_equal
    ~ctxt:context
    ~cmp:[%compare.equal: (string * string) list]
    ~printer:(fun items -> [%sexp_of: (string * string) list] items |> Sexp.to_string_hum)
    (expected |> List.sort ~compare:[%compare: string * string])
    (actual |> List.sort ~compare:[%compare: string * string])


let assert_conflicts_equal ~context ~expected actual =
  let compare = [%compare: string * Interface.Eager.BuckBxlBuilderOutput.Conflict.t] in
  assert_equal
    ~ctxt:context
    ~cmp:[%compare.equal: (string * Interface.Eager.BuckBxlBuilderOutput.Conflict.t) list]
    ~printer:(fun items ->
      [%sexp_of: (string * Interface.Eager.BuckBxlBuilderOutput.Conflict.t) list] items
      |> Sexp.to_string_hum)
    (expected |> List.sort ~compare)
    (actual |> List.sort ~compare)


let test_parse_merged_sourcedb_v2 context =
  let assert_parsed ~expected_build_map ~expected_target_count ~expected_conflicts output =
    let {
      Interface.Eager.BuckBxlBuilderOutput.build_map = actual_build_map;
      target_count = actual_target_count;
      conflicts = actual_conflicts;
    }
      =
      Yojson.Safe.from_string output |> Interface.Eager.parse_merged_sourcedb
    in
    assert_mapping_equal ~context ~expected:expected_build_map (BuildMap.to_alist actual_build_map);
    assert_equal
      ~ctxt:context
      ~cmp:Int.equal
      ~printer:Int.to_string
      expected_target_count
      actual_target_count;
    assert_conflicts_equal
      ~context
      ~expected:expected_conflicts
      (List.map actual_conflicts ~f:(fun (target, conflict) -> Target.show target, conflict))
  in
  let assert_not_parsed output =
    try
      let _ = Yojson.Safe.from_string output |> Interface.Eager.parse_merged_sourcedb in
      let message = Stdlib.Format.sprintf "Unexpected parsing success: %s" output in
      assert_failure message
    with
    | Interface.JsonError _ -> ()
  in
  assert_not_parsed "42";
  assert_not_parsed {|"abc"|};
  assert_not_parsed "[]";
  assert_not_parsed {| { "foo": 42 } |};
  assert_not_parsed {| { "build_map": {} } |};

  assert_parsed
    {| { "build_map": {}, "built_targets_count": 0, "dropped_targets": {} } |}
    ~expected_build_map:[]
    ~expected_target_count:0
    ~expected_conflicts:[];
  assert_parsed
    {| {
         "build_map": {
           "a.py": "source/a.py",
           "b.py": "source/b.py"
         },
         "built_targets_count": 2,
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
    ~expected_target_count:2
    ~expected_conflicts:
      [
        ( "//targets:c",
          {
            Interface.Eager.BuckBxlBuilderOutput.Conflict.conflict_with = "//targets:a";
            artifact_path = "a.py";
            preserved_source_path = "source/a.py";
            dropped_source_path = "source/c.py";
          } );
      ];
  ()


let test_parse_merged_sourcedb_lazy context =
  let assert_parsed ~expected output =
    let actual = Yojson.Safe.from_string output |> Interface.Lazy.parse_merged_sourcedb in
    assert_mapping_equal ~context ~expected (BuildMap.to_alist actual)
  in
  let assert_not_parsed output =
    try
      let _ = Yojson.Safe.from_string output |> Interface.Lazy.parse_merged_sourcedb in
      let message = Stdlib.Format.sprintf "Unexpected parsing success: %s" output in
      assert_failure message
    with
    | Interface.JsonError _ -> ()
  in
  assert_not_parsed "42";
  assert_not_parsed {|"abc"|};
  assert_not_parsed "[]";
  assert_not_parsed {| { "foo": 42 } |};
  assert_not_parsed {| { "build_map": {} } |};

  assert_parsed "{}" ~expected:[];
  assert_parsed {| {
         "a.py": "source/a.py"
    } |} ~expected:["a.py", "source/a.py"];
  assert_parsed
    {| {
         "a.py": "source/a.py",
         "b.py": "source/b.py"
    } |}
    ~expected:["a.py", "source/a.py"; "b.py", "source/b.py"];
  ()


let test_parse_buck_bxl_output_v2 context =
  let assert_parsed output = Interface.Eager.parse_bxl_output output |> ignore in
  let assert_not_parsed output =
    try
      let _ = Interface.Eager.parse_bxl_output output in
      let message = Stdlib.Format.sprintf "Unexpected parsing success: %s" output in
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
    ~content:{| { "build_map": {}, "built_targets_count": 0, "dropped_targets": {} } |}
  |> File.write;
  File.create malformed_path ~content:{| { "derp": 42 } |} |> File.write;
  let nonexistent_path = PyrePath.create_relative ~root ~relative:"nonexistent.json" in

  assert_not_parsed "{}";
  assert_not_parsed {| { "db": 42 }|};
  assert_not_parsed (create_db_output nonexistent_path);
  assert_not_parsed (create_db_output malformed_path);

  assert_parsed (create_db_output well_formed_path);
  ()


let test_parse_buck_bxl_output_lazy context =
  let assert_parsed output = Interface.Lazy.parse_bxl_output output |> ignore in
  let assert_not_parsed output =
    try
      let _ = Interface.Lazy.parse_bxl_output output in
      let message = Stdlib.Format.sprintf "Unexpected parsing success: %s" output in
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
  File.create well_formed_path ~content:{| { "a.py": "source/a.py", "b.py": "source/b.py" } |}
  |> File.write;
  File.create malformed_path ~content:{| { "derp": 42 } |} |> File.write;
  let nonexistent_path = PyrePath.create_relative ~root ~relative:"nonexistent.json" in

  assert_not_parsed "{}";
  assert_not_parsed {| { "db": 42 }|};
  assert_not_parsed (create_db_output nonexistent_path);
  assert_not_parsed (create_db_output malformed_path);

  assert_parsed (create_db_output well_formed_path);
  ()


let () =
  "builder_test"
  >::: [
         "parse_merged_sourcedb_v2" >:: test_parse_merged_sourcedb_v2;
         "parse_merged_sourcedb_lazy" >:: test_parse_merged_sourcedb_lazy;
         "parse_buck_bxl_output_v2" >:: test_parse_buck_bxl_output_v2;
         "parse_buck_bxl_output_lazy" >:: test_parse_buck_bxl_output_lazy;
       ]
  |> Test.run
