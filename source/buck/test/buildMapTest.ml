(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base
open OUnit2
open Buck

let assert_mapping_equal ~context ~expected actual =
  assert_equal
    ~ctxt:context
    ~cmp:[%compare.equal: (string * string) list]
    ~printer:(fun elements -> [%sexp_of: (string * string) list] elements |> Sexp.to_string_hum)
    (expected |> List.sort ~compare:[%compare: string * string])
    (actual |> List.sort ~compare:[%compare: string * string])


let test_partial_build_map_from_json context =
  let assert_parsed ~expected input =
    Yojson.Safe.from_string input
    |> BuildMap.Partial.of_json_exn
    |> BuildMap.Partial.to_alist
    |> assert_mapping_equal ~context ~expected
  in
  let assert_not_parsed input =
    try
      let _ = Yojson.Safe.from_string input |> BuildMap.Partial.of_json_exn in
      assert_failure "Expected JSON parsing to fail but it unexpectedly succeeded"
    with
    | _ -> ()
  in
  assert_not_parsed "42";
  assert_not_parsed "{}";
  assert_not_parsed {| { "derp": 42 } |};
  assert_parsed {| { "sources": {}, "dependencies": {} } |} ~expected:[];
  assert_parsed
    {| {
      "sources": {
        "foo.py": "source/foo.py"
      },
      "dependencies": {
        "bar.py": "source/bar.py"
      }
  }|}
    ~expected:["foo.py", "source/foo.py"; "bar.py", "source/bar.py"];
  (* Non-Python files are dropped in the mapping. *)
  assert_parsed
    {| {
      "sources": {
        "foo.py": "source/foo.py",
        "README.txt": "source/README.txt"
      },
      "dependencies": {
        "bar.py": "source/bar.py",
        "cert.pem": "source/security/cert.pem"
      }
  }|}
    ~expected:["foo.py", "source/foo.py"; "bar.py", "source/bar.py"];
  ()


let test_partial_build_map_merge context =
  let assert_merged ~left ~right ~expected () =
    match BuildMap.Partial.(merge (of_alist_exn left) (of_alist_exn right)) with
    | BuildMap.Partial.MergeResult.Ok merged ->
        assert_mapping_equal ~context ~expected (BuildMap.Partial.to_alist merged)
    | _ -> assert_failure "Expected partial map merging to succeed but it unexpectedly failed."
  in
  let assert_not_merged ~left ~right ~key ~left_value ~right_value () =
    match BuildMap.Partial.(merge (of_alist_exn left) (of_alist_exn right)) with
    | BuildMap.Partial.MergeResult.Incompatible actual ->
        assert_equal
          ~ctxt:context
          ~cmp:[%compare.equal: BuildMap.Partial.MergeResult.IncompatibleItem.t]
          ~printer:(fun item ->
            [%sexp_of: BuildMap.Partial.MergeResult.IncompatibleItem.t] item |> Sexp.to_string_hum)
          { BuildMap.Partial.MergeResult.IncompatibleItem.key; left_value; right_value }
          actual
    | _ -> assert_failure "Expected partial map merging to succeed but it unexpectedly failed."
  in
  assert_merged
    ~left:["foo.py", "source/foo.py"]
    ~right:["bar.py", "source/bar.py"]
    ~expected:["foo.py", "source/foo.py"; "bar.py", "source/bar.py"]
    ();
  assert_merged
    ~left:["foo.py", "source/foo.py"]
    ~right:["foo.py", "source/foo.py"; "bar.py", "source/bar.py"]
    ~expected:["foo.py", "source/foo.py"; "bar.py", "source/bar.py"]
    ();
  assert_merged
    ~left:["foo.py", "source/foo.py"; "bar.py", "source/bar.py"]
    ~right:["foo.py", "source/foo.py"]
    ~expected:["foo.py", "source/foo.py"; "bar.py", "source/bar.py"]
    ();
  assert_not_merged
    ~left:["foo.py", "source/foo0.py"; "bar.py", "source/bar.py"]
    ~right:["foo.py", "source/foo1.py"; "bar.py", "source/bar.py"]
    ~key:"foo.py"
    ~left_value:"source/foo0.py"
    ~right_value:"source/foo1.py"
    ();
  ()


let test_build_map_lookup context =
  let index_from_mappings mappings =
    BuildMap.Partial.of_alist_exn mappings |> BuildMap.create |> BuildMap.index
  in
  let assert_lookup_source ~mappings ~expected key =
    let actual = BuildMap.Indexed.lookup_source (index_from_mappings mappings) key in
    assert_equal
      ~ctxt:context
      ~cmp:[%compare.equal: string option]
      ~printer:(fun element -> [%sexp_of: string option] element |> Sexp.to_string_hum)
      expected
      actual
  in
  let assert_lookup_artifact ~mappings ~expected key =
    let actual = BuildMap.Indexed.lookup_artifact (index_from_mappings mappings) key in
    assert_equal
      ~ctxt:context
      ~cmp:[%compare.equal: string list]
      ~printer:(fun elements -> [%sexp_of: string list] elements |> Sexp.to_string_hum)
      (List.sort ~compare:String.compare expected)
      (List.sort ~compare:String.compare actual)
  in

  assert_lookup_source
    "foo.py"
    ~mappings:["foo.py", "source/foo.py"]
    ~expected:(Some "source/foo.py");
  assert_lookup_source
    "bar.py"
    ~mappings:["foo.py", "source/foo.py"; "bar.py", "source/bar.py"]
    ~expected:(Some "source/bar.py");
  assert_lookup_source
    "bar.py"
    ~mappings:["foo.py", "source/foo.py"; "bar.py", "source/foo.py"]
    ~expected:(Some "source/foo.py");
  assert_lookup_source "bar.py" ~mappings:["foo.py", "source/foo.py"] ~expected:None;

  assert_lookup_artifact "source/foo.py" ~mappings:["foo.py", "source/foo.py"] ~expected:["foo.py"];
  assert_lookup_artifact
    "source/bar.py"
    ~mappings:["foo.py", "source/foo.py"; "bar.py", "source/bar.py"]
    ~expected:["bar.py"];
  assert_lookup_artifact
    "source/foo.py"
    ~mappings:["foo.py", "source/foo.py"; "bar.py", "source/foo.py"]
    ~expected:["foo.py"; "bar.py"];
  assert_lookup_artifact
    "source/baz.py"
    ~mappings:["foo.py", "source/foo.py"; "bar.py", "source/foo.py"; "baz.py", "source/baz.py"]
    ~expected:["baz.py"];
  assert_lookup_artifact
    "source/baz.py"
    ~mappings:["foo.py", "source/foo.py"; "bar.py", "source/bar.py"]
    ~expected:[];
  ()


let () =
  "build_map_test"
  >::: [
         "partial_build_map_from_json" >:: test_partial_build_map_from_json;
         "partial_build_map_merge" >:: test_partial_build_map_merge;
         "build_map_lookup" >:: test_build_map_lookup;
       ]
  |> Test.run
