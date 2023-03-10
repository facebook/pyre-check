(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base
open OUnit2
module ClientState = CodeNavigationServer.Testing.ClientState

let ( ! ) path = PyrePath.create_absolute path |> SourcePath.create

let assert_working_set ~context ~expected actual =
  let sort = List.sort ~compare:[%compare: SourcePath.t] in
  assert_equal
    ~ctxt:context
    ~cmp:[%compare.equal: SourcePath.t list]
    ~printer:(fun paths -> Format.asprintf "%a" Sexp.pp_hum ([%sexp_of: SourcePath.t list] paths))
    (sort actual)
    (sort expected)


let test_open_files context =
  let states = ClientState.create () in
  let assert_working_set = assert_working_set ~context in
  let assert_lookup_ok ~client_id source_path =
    match ClientState.WorkingSet.lookup states ~client_id ~source_path with
    | `Ok _ -> ()
    | _ -> assert_failure "unexpected working set lookup failure"
  in
  let assert_lookup_failure ~client_id source_path =
    match ClientState.WorkingSet.lookup states ~client_id ~source_path with
    | `Ok _ -> assert_failure "unexpected working set lookup success"
    | _ -> ()
  in
  assert_working_set (ClientState.WorkingSet.to_list states) ~expected:[];

  let success = ClientState.register states "foo" in
  assert_bool "client registration should succeed the first time" success;

  let success = ClientState.register states "foo" in
  assert_bool "client registration should fail for the same id" (not success);

  let success = ClientState.dispose states "bar" in
  assert_bool "cannot dispose un-registered client" (not success);

  let _ = ClientState.register states "bar" in

  let _ = ClientState.WorkingSet.add states ~client_id:"foo" ~source_path:!"/a/b.py" in
  assert_lookup_ok ~client_id:"foo" !"/a/b.py";
  assert_lookup_failure ~client_id:"foo" !"/b/c.py";
  assert_working_set (ClientState.WorkingSet.to_list states) ~expected:[!"/a/b.py"];

  let _ = ClientState.WorkingSet.add states ~client_id:"bar" ~source_path:!"/b/c.py" in
  assert_lookup_ok ~client_id:"bar" !"/b/c.py";
  assert_lookup_failure ~client_id:"bar" !"/a/b.py";
  assert_working_set (ClientState.WorkingSet.to_list states) ~expected:[!"/a/b.py"; !"/b/c.py"];

  let _ = ClientState.WorkingSet.add states ~client_id:"bar" ~source_path:!"/a/b.py" in
  assert_lookup_ok ~client_id:"bar" !"/a/b.py";
  assert_lookup_ok ~client_id:"bar" !"/b/c.py";
  assert_working_set (ClientState.WorkingSet.to_list states) ~expected:[!"/a/b.py"; !"/b/c.py"];

  let _ = ClientState.WorkingSet.remove states ~client_id:"foo" ~source_path:!"/a/b.py" in
  assert_lookup_failure ~client_id:"foo" !"/a/b.py";
  assert_working_set (ClientState.WorkingSet.to_list states) ~expected:[!"/a/b.py"; !"/b/c.py"];

  let _ = ClientState.WorkingSet.remove states ~client_id:"nonexistent" ~source_path:!"/a/b.py" in
  assert_working_set (ClientState.WorkingSet.to_list states) ~expected:[!"/a/b.py"; !"/b/c.py"];

  let _ = ClientState.WorkingSet.remove states ~client_id:"bar" ~source_path:!"/nonexistent.py" in
  assert_working_set (ClientState.WorkingSet.to_list states) ~expected:[!"/a/b.py"; !"/b/c.py"];

  let success = ClientState.dispose states "foo" in
  assert_bool "client disposal should succeed the first time" success;

  let success = ClientState.dispose states "foo" in
  assert_bool "client disposal should fail the second time" (not success);

  let _ = ClientState.dispose states "bar" in
  assert_working_set (ClientState.WorkingSet.to_list states) ~expected:[];
  ()


let () = "state_test" >::: ["test_open_files" >:: test_open_files] |> Test.run
