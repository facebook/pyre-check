(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base
open OUnit2
module Path = Pyre.Path
open Buck

let setup_source_directory ~root contents =
  let add_file (relative, content) =
    File.create ~content (Path.create_relative ~root ~relative) |> File.write
  in
  List.iter contents ~f:add_file


let assert_content ~context ~expected path =
  let actual = File.create path |> File.content in
  assert_equal
    ~ctxt:context
    ~cmp:[%compare.equal: string option]
    ~printer:(fun content -> Sexp.to_string_hum ([%sexp_of: string option] content))
    (Some expected)
    actual


let assert_populate ~context ~build_map ~expected sources =
  let source_root = bracket_tmpdir context |> Path.create_absolute in
  let artifact_root = bracket_tmpdir context |> Path.create_absolute in
  let () = setup_source_directory ~root:source_root sources in
  let build_map = BuildMap.Partial.of_alist_exn build_map |> BuildMap.create in
  let open Lwt.Infix in
  Artifacts.populate ~source_root ~artifact_root build_map
  >>= function
  | Result.Error message -> assert_failure message
  | Result.Ok () ->
      List.iter expected ~f:(fun (relative, content) ->
          Path.create_relative ~root:artifact_root ~relative
          |> assert_content ~context ~expected:content);
      Lwt.return_unit


let assert_fails f =
  let open Lwt.Infix in
  f ()
  >>= function
  | Result.Ok () -> assert_failure "Expected `f` to fail but it succeeded."
  | Result.Error _ -> Lwt.return_unit


let test_artifacts_populate_ok context =
  let open Lwt.Infix in
  assert_populate
    ~context
    ["a.py", "a"; "b.py", "b"]
    ~build_map:["foo/a.py", "a.py"; "foo/b.py", "b.py"]
    ~expected:["foo/a.py", "a"; "foo/b.py", "b"]
  >>= fun () ->
  assert_populate
    ~context
    ["foo/a.py", "a"; "foo/b.py", "b"]
    ~build_map:["a.py", "foo/a.py"; "bar/b.py", "foo/b.py"]
    ~expected:["a.py", "a"; "bar/b.py", "b"]
  >>= fun () -> Lwt.return_unit


let test_artifacts_populate_bad_directory context =
  let root = bracket_tmpdir context |> Path.create_absolute in
  let build_map = BuildMap.Partial.of_alist_exn [] |> BuildMap.create in
  let nonexistent_root = Path.create_relative ~root ~relative:"nonexistent" in
  let existent_non_directory = Path.create_relative ~root ~relative:"not_a_directory" in
  File.create existent_non_directory ~content:"" |> File.write;

  let open Lwt.Infix in
  assert_fails (fun () ->
      Artifacts.populate ~source_root:nonexistent_root ~artifact_root:root build_map)
  >>= fun () ->
  assert_fails (fun () ->
      Artifacts.populate ~source_root:existent_non_directory ~artifact_root:root build_map)
  >>= fun () ->
  assert_fails (fun () ->
      Artifacts.populate ~source_root:root ~artifact_root:nonexistent_root build_map)
  >>= fun () ->
  assert_fails (fun () ->
      Artifacts.populate ~source_root:root ~artifact_root:existent_non_directory build_map)
  >>= fun () -> Lwt.return_unit


let test_artifacts_populate_unclean_artifact_directory context =
  let source_root = bracket_tmpdir context |> Path.create_absolute in
  let artifact_root = bracket_tmpdir context |> Path.create_absolute in
  let () = setup_source_directory ~root:source_root ["foo.py", "foo"] in
  let build_map = BuildMap.Partial.of_alist_exn ["foo.py", "foo.py"] |> BuildMap.create in
  Path.create_relative ~root:artifact_root ~relative:"foo.py"
  |> File.create ~content:""
  |> File.write;
  assert_fails (fun () -> Artifacts.populate ~source_root ~artifact_root build_map)


let () =
  "builder_test"
  >::: [
         "artifacts_populate_ok" >:: OUnitLwt.lwt_wrapper test_artifacts_populate_ok;
         "artifacts_populate_bad_directory"
         >:: OUnitLwt.lwt_wrapper test_artifacts_populate_bad_directory;
         "artifacts_populate_unclean_artifact_directory"
         >:: OUnitLwt.lwt_wrapper test_artifacts_populate_unclean_artifact_directory;
       ]
  |> Test.run
