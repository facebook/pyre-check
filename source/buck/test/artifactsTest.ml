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
module Artifacts = Buck__Artifacts

let setup_source_directory ~root contents =
  let add_file (relative, content) =
    File.create ~content (PyrePath.create_relative ~root ~relative) |> File.write
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
  let source_root = bracket_tmpdir context |> PyrePath.create_absolute in
  let artifact_root = bracket_tmpdir context |> PyrePath.create_absolute in
  let () = setup_source_directory ~root:source_root sources in
  let build_map = BuildMap.Partial.of_alist_exn build_map |> BuildMap.create in
  let open Lwt.Infix in
  Artifacts.populate ~source_root ~artifact_root build_map
  >>= function
  | Result.Error message -> assert_failure message
  | Result.Ok () ->
      List.iter expected ~f:(fun (relative, content) ->
          PyrePath.create_relative ~root:artifact_root ~relative
          |> assert_content ~context ~expected:content);
      Lwt.return_unit


let assert_update ~context ~build_map ~difference ~expected sources =
  let source_root = bracket_tmpdir context |> PyrePath.create_absolute in
  let artifact_root = bracket_tmpdir context |> PyrePath.create_absolute in
  let () = setup_source_directory ~root:source_root sources in
  let build_map = BuildMap.Partial.of_alist_exn build_map |> BuildMap.create in
  let open Lwt.Infix in
  Artifacts.populate ~source_root ~artifact_root build_map
  >>= function
  | Result.Error message -> assert_failure message
  | Result.Ok () -> (
      let difference = BuildMap.Difference.of_alist_exn difference in
      Artifacts.update ~source_root ~artifact_root difference
      >>= function
      | Result.Error message -> assert_failure message
      | Result.Ok () ->
          List.iter expected ~f:(fun (relative, content) ->
              let path = PyrePath.create_relative ~root:artifact_root ~relative in
              match content with
              | None ->
                  let message = Format.sprintf "%s is expected to be removed" relative in
                  assert_bool message (not (PyrePath.file_exists path))
              | Some content -> assert_content path ~context ~expected:content);
          Lwt.return_unit)


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
  >>= fun () ->
  (* Notice that the build map may point some artifacts to a nonexistent source file -- `populate`
     will NOT fail when that happens and we do rely on this behavior downstream. *)
  assert_populate
    ~context
    ["foo/a.py", "a"]
    ~build_map:["a.py", "foo/a.py"; "bar/b.py", "foo/b.py"]
    ~expected:["a.py", "a"]
  >>= fun () -> Lwt.return_unit


let test_artifacts_populate_bad_directory context =
  let root = bracket_tmpdir context |> PyrePath.create_absolute in
  let build_map = BuildMap.Partial.of_alist_exn [] |> BuildMap.create in
  let nonexistent_root = PyrePath.create_relative ~root ~relative:"nonexistent" in
  let existent_non_directory = PyrePath.create_relative ~root ~relative:"not_a_directory" in
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
  let source_root = bracket_tmpdir context |> PyrePath.create_absolute in
  let artifact_root = bracket_tmpdir context |> PyrePath.create_absolute in
  let () = setup_source_directory ~root:source_root ["foo.py", "foo"] in
  let build_map = BuildMap.Partial.of_alist_exn ["foo.py", "foo.py"] |> BuildMap.create in
  PyrePath.create_relative ~root:artifact_root ~relative:"foo.py"
  |> File.create ~content:""
  |> File.write;
  assert_fails (fun () -> Artifacts.populate ~source_root ~artifact_root build_map)


let test_artifact_update_ok context =
  let open Lwt.Infix in
  let open BuildMap.Difference.Kind in
  assert_update
    ~context
    ["a.py", "a"; "b.py", "b"]
    ~build_map:["foo/a.py", "a.py"]
    ~difference:["b.py", New "b.py"]
    ~expected:["foo/a.py", Some "a"; "b.py", Some "b"]
  >>= fun () ->
  assert_update
    ~context
    ["a.py", "a"; "b.py", "b"]
    ~build_map:["foo/a.py", "a.py"]
    ~difference:["bar/b.py", New "b.py"]
    ~expected:["foo/a.py", Some "a"; "bar/b.py", Some "b"]
  >>= fun () ->
  assert_update
    ~context
    ["a.py", "a"]
    ~build_map:["a.py", "a.py"]
    ~difference:["a.py", Deleted]
    ~expected:["a.py", None]
  >>= fun () ->
  assert_update
    ~context
    ["a.py", "a"]
    ~build_map:["foo/a.py", "a.py"]
    ~difference:["foo/a.py", Deleted]
    ~expected:["foo/a.py", None]
  >>= fun () ->
  assert_update
    ~context
    ["a.py", "a"; "b.py", "b"]
    ~build_map:["a.py", "a.py"]
    ~difference:["a.py", Changed "b.py"]
    ~expected:["a.py", Some "b"]
  >>= fun () ->
  assert_update
    ~context
    ["a.py", "a"; "b.py", "b"]
    ~build_map:["a.py", "a.py"]
    ~difference:["foo/a.py", New "a.py"; "foo/bar/b.py", New "b.py"]
    ~expected:["a.py", Some "a"; "foo/a.py", Some "a"; "foo/bar/b.py", Some "b"]
  >>= fun () ->
  assert_update
    ~context
    ["a.py", "a"]
    ~build_map:["foo/a.py", "a.py"]
    ~difference:["foo/bar/a.py", New "a.py"; "foo/a.py", Deleted]
    ~expected:["foo/a.py", None; "foo/bar/a.py", Some "a"]
  >>= fun () ->
  assert_update
    ~context
    ["a.py", "a"; "b.py", "b"]
    ~build_map:["foo/a.py", "a.py"]
    ~difference:["foo/b.py", New "a.py"; "foo/a.py", Changed "b.py"]
    ~expected:["foo/a.py", Some "b"; "foo/b.py", Some "a"]
  >>= fun () ->
  assert_update
    ~context
    ["a.py", "a"; "b.py", "b"]
    ~build_map:["a.py", "a.py"; "b.py", "b.py"]
    ~difference:["a.py", Deleted; "b.py", Deleted]
    ~expected:["a.py", None; "b.py", None]
  >>= fun () ->
  assert_update
    ~context
    ["a.py", "a"; "b.py", "b"]
    ~build_map:["a.py", "a.py"; "b.py", "b.py"]
    ~difference:["a.py", Deleted; "b.py", Changed "a.py"]
    ~expected:["a.py", None; "b.py", Some "a"]
  >>= fun () ->
  assert_update
    ~context
    ["a.py", "a"; "b.py", "b"]
    ~build_map:["foo/a.py", "a.py"; "bar/b.py", "b.py"]
    ~difference:["foo/a.py", Changed "b.py"; "bar/b.py", Changed "a.py"]
    ~expected:["foo/a.py", Some "b"; "bar/b.py", Some "a"]
  >>= fun () ->
  assert_update
    ~context
    ["a.py", "a"; "b.py", "b"; "c.py", "c"]
    ~build_map:["foo/a.py", "a.py"; "bar/b.py", "b.py"]
    ~difference:["foo/c.py", New "c.py"; "foo/a.py", Deleted; "bar/b.py", Changed "a.py"]
    ~expected:["foo/a.py", None; "bar/b.py", Some "a"; "foo/c.py", Some "c"]
  >>= fun () ->
  (* These tests make sure that `Delete` is idempotent. *)
  assert_update ~context [] ~build_map:[] ~difference:[] ~expected:["a.py", None]
  >>= fun () ->
  assert_update ~context [] ~build_map:[] ~difference:["a.py", Deleted] ~expected:["a.py", None]
  >>= fun () ->
  (* These tests make sure that `New` and `Changed` are idempotent. *)
  assert_update
    ~context
    ["a.py", "a"]
    ~build_map:["foo/a.py", "a.py"]
    ~difference:[]
    ~expected:["foo/a.py", Some "a"]
  >>= fun () ->
  assert_update
    ~context
    ["a.py", "a"]
    ~build_map:["foo/a.py", "a.py"]
    ~difference:["foo/a.py", New "a.py"]
    ~expected:["foo/a.py", Some "a"]
  >>= fun () ->
  assert_update
    ~context
    ["a.py", "a"]
    ~build_map:["foo/a.py", "a.py"]
    ~difference:["foo/a.py", Changed "a.py"]
    ~expected:["foo/a.py", Some "a"]
  >>= fun () -> Lwt.return_unit


let test_artifact_update_bad_directory context =
  let root = bracket_tmpdir context |> PyrePath.create_absolute in
  let difference = BuildMap.Difference.of_alist_exn [] in
  let nonexistent_root = PyrePath.create_relative ~root ~relative:"nonexistent" in
  let existent_non_directory = PyrePath.create_relative ~root ~relative:"not_a_directory" in
  File.create existent_non_directory ~content:"" |> File.write;

  let open Lwt.Infix in
  assert_fails (fun () ->
      Artifacts.update ~source_root:nonexistent_root ~artifact_root:root difference)
  >>= fun () ->
  assert_fails (fun () ->
      Artifacts.update ~source_root:existent_non_directory ~artifact_root:root difference)
  >>= fun () ->
  assert_fails (fun () ->
      Artifacts.update ~source_root:root ~artifact_root:nonexistent_root difference)
  >>= fun () ->
  assert_fails (fun () ->
      Artifacts.update ~source_root:root ~artifact_root:existent_non_directory difference)
  >>= fun () -> Lwt.return_unit


let () =
  "builder_test"
  >::: [
         "artifacts_populate_ok" >:: OUnitLwt.lwt_wrapper test_artifacts_populate_ok;
         "artifacts_populate_bad_directory"
         >:: OUnitLwt.lwt_wrapper test_artifacts_populate_bad_directory;
         "artifacts_populate_unclean_artifact_directory"
         >:: OUnitLwt.lwt_wrapper test_artifacts_populate_unclean_artifact_directory;
         "artifact_update_ok" >:: OUnitLwt.lwt_wrapper test_artifact_update_ok;
         "artifact_update_bad_directory" >:: OUnitLwt.lwt_wrapper test_artifact_update_bad_directory;
       ]
  |> Test.run
