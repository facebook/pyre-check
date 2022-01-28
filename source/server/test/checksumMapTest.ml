(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base
open OUnit2
open Server

let assert_checksum_map_equal ~context ~expected actual =
  let compare = [%compare: string * string] in
  assert_equal
    ~ctxt:context
    ~cmp:[%compare.equal: (string * string) list]
    ~printer:(fun items -> Sexp.to_string_hum ([%sexp_of: (string * string) list] items))
    (List.sort ~compare expected)
    (ChecksumMap.to_alist actual |> List.sort ~compare)


let test_load_from_string context =
  let assert_loaded ~expected text =
    match ChecksumMap.load_from_string text with
    | Result.Ok actual -> assert_checksum_map_equal ~context ~expected actual
    | Result.Error message -> assert_failure message
  in
  let assert_not_loaded text =
    match ChecksumMap.load_from_string text with
    | Result.Error _ -> ()
    | Result.Ok _ ->
        let message = Format.sprintf "Unexpected load success on %s" text in
        assert_failure message
  in

  assert_not_loaded "";
  assert_not_loaded "42";
  assert_not_loaded "[]";
  assert_not_loaded {| { "bfg": 9000 } |};
  assert_not_loaded {| { "doom": [] } |};
  assert_not_loaded {| { "doom": "4", "doom", "2016" } |};

  assert_loaded "{}" ~expected:[];
  assert_loaded {| { "doom": "eternal" } |} ~expected:["doom", "eternal"];
  assert_loaded
    {| { "samuel": "hayden", "olivia": "pierce" } |}
    ~expected:["samuel", "hayden"; "olivia", "pierce"];
  ()


let test_load_from_file context =
  let root = bracket_tmpdir context |> PyrePath.create_absolute in
  let test_path = PyrePath.create_relative ~root ~relative:"test.json" in
  File.create test_path ~content:{| { "bfg": "9000" }|} |> File.write;

  ChecksumMap.load { Configuration.UnwatchedFiles.root; checksum_path = "test.json" }
  |> Result.ok_or_failwith
  |> assert_checksum_map_equal ~context ~expected:["bfg", "9000"];

  ChecksumMap.load { Configuration.UnwatchedFiles.root; checksum_path = "nonexist.json" }
  |> Result.is_error
  |> assert_bool "loading nonexist file should be an error";

  let () =
    try
      let _ =
        ChecksumMap.load_exn { Configuration.UnwatchedFiles.root; checksum_path = "nonexist.json" }
      in
      assert_failure "unexpected loading success"
    with
    | ChecksumMap.LoadError _ -> ()
  in
  ()


let test_difference context =
  let assert_difference ~expected ~original ~current () =
    let actual =
      ChecksumMap.difference
        ~original:(ChecksumMap.of_alist_exn original)
        (ChecksumMap.of_alist_exn current)
    in
    let compare = [%compare: ChecksumMap.Difference.t] in
    assert_equal
      ~ctxt:context
      ~cmp:[%compare.equal: ChecksumMap.Difference.t list]
      ~printer:(fun items -> Sexp.to_string_hum ([%sexp_of: ChecksumMap.Difference.t list] items))
      (List.sort ~compare expected)
      (List.sort ~compare actual)
  in

  let open ChecksumMap.Difference in
  assert_difference ~original:[] ~current:[] ~expected:[] ();
  assert_difference
    ~original:["foo", "a"]
    ~current:[]
    ~expected:[{ kind = Kind.Deleted; path = "foo" }]
    ();
  assert_difference
    ~original:[]
    ~current:["foo", "a"]
    ~expected:[{ kind = Kind.New; path = "foo" }]
    ();
  assert_difference ~original:["foo", "a"] ~current:["foo", "a"] ~expected:[] ();
  assert_difference
    ~original:["foo", "a"]
    ~current:["foo", "b"]
    ~expected:[{ kind = Kind.Changed; path = "foo" }]
    ();

  assert_difference
    ~original:["foo", "a"]
    ~current:["foo", "a"; "bar", "b"]
    ~expected:[{ kind = Kind.New; path = "bar" }]
    ();
  assert_difference
    ~original:["foo", "a"; "bar", "b"]
    ~current:["foo", "a"]
    ~expected:[{ kind = Kind.Deleted; path = "bar" }]
    ();
  assert_difference
    ~original:["foo", "a"; "bar", "b"]
    ~current:["foo", "a"; "bar", "c"]
    ~expected:[{ kind = Kind.Changed; path = "bar" }]
    ();
  assert_difference
    ~original:["foo", "a"; "bar", "b"]
    ~current:["foo", "c"; "bar", "d"]
    ~expected:[{ kind = Kind.Changed; path = "foo" }; { kind = Kind.Changed; path = "bar" }]
    ();
  assert_difference
    ~original:["foo", "a"; "bar", "b"]
    ~current:["baz", "c"; "bar", "b"]
    ~expected:[{ kind = Kind.Deleted; path = "foo" }; { kind = Kind.New; path = "baz" }]
    ();

  ()


let () =
  "checksum_map_test"
  >::: [
         "load_from_string" >:: test_load_from_string;
         "load_from_file" >:: test_load_from_file;
         "difference" >:: test_difference;
       ]
  |> Test.run
