(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2

let should_analyze_file ~paths_to_modify path =
  PyrePath.create_absolute path
  |> Service.Infer.should_analyze_file
       ~paths_to_modify:(List.map paths_to_modify ~f:PyrePath.create_absolute)


let test_should_analyze_file _context =
  let assert_should_analyze ~expected ~paths_to_modify ~path =
    let actual = should_analyze_file ~paths_to_modify path in
    assert_equal
      actual
      expected
      ~cmp:Bool.equal
      ~msg:
        (Format.asprintf
           "Unexpected result actual: %B != expected: %B from should_analyze_file %s %s"
           actual
           expected
           ([%show: string list] paths_to_modify)
           path)
  in
  assert_should_analyze ~paths_to_modify:["/a/b"; "/a/c.py"] ~path:"/a/c.py" ~expected:true;
  assert_should_analyze ~paths_to_modify:["/a/b"; "/a/c.py"] ~path:"/a/b/d.py" ~expected:true;
  assert_should_analyze ~paths_to_modify:["/a/b"; "/a/c.py"] ~path:"/a/c/d.py" ~expected:false;
  assert_should_analyze ~paths_to_modify:["/a/b"; "/a/c.py"] ~path:"/a/other.py" ~expected:false;
  assert_should_analyze
    ~paths_to_modify:["/a/b"; "/a/c.py"]
    ~path:"relative_path.py"
    ~expected:false;
  ()


let () = "inferTest" >::: ["should_analyze_file" >:: test_should_analyze_file] |> Test.run
