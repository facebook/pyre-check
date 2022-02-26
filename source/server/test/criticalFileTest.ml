(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Server

let ( ! ) = PyrePath.create_absolute

let test_critical_files context =
  let base_name name = CriticalFile.BaseName name in
  let extension name = CriticalFile.Extension name in
  let full_path path = CriticalFile.FullPath !path in
  let assert_find ~expected ~critical_files paths =
    let actual = CriticalFile.find critical_files ~within:paths in
    assert_equal
      ~ctxt:context
      ~cmp:[%compare.equal: PyrePath.t option]
      ~printer:(fun result -> [%sexp_of: PyrePath.t option] result |> Sexp.to_string_hum)
      expected
      actual
  in
  assert_find ~critical_files:[] ~expected:None [];
  assert_find ~critical_files:[base_name "a.py"] ~expected:None [];
  assert_find ~critical_files:[base_name "a.py"] ~expected:None [!"b.py"];
  assert_find ~critical_files:[base_name "a.py"] ~expected:(Some !"a.py") [!"a.py"];
  assert_find ~critical_files:[base_name "a.py"] ~expected:(Some !"foo/a.py") [!"foo/a.py"];
  assert_find ~critical_files:[extension "derp"] ~expected:None [];
  assert_find ~critical_files:[extension "derp"] ~expected:None [!"a.py"];
  assert_find ~critical_files:[extension "derp"] ~expected:(Some !"a.derp") [!"a.derp"];
  assert_find ~critical_files:[extension "derp"] ~expected:(Some !"a.derp") [!"a.derp"; !"a.derp"];
  assert_find ~critical_files:[full_path "a.py"] ~expected:None [!"/foo/a.py"];
  assert_find ~critical_files:[full_path "/foo/a.py"] ~expected:(Some !"/foo/a.py") [!"/foo/a.py"];
  assert_find
    ~critical_files:[base_name "a.py"]
    ~expected:(Some !"/foo/bar/a.py")
    [!"/foo/bar/a.py"];
  assert_find ~critical_files:[extension "py"] ~expected:(Some !"/foo/bar/a.py") [!"/foo/bar/a.py"];
  assert_find ~critical_files:[full_path "/bar/a.py"] ~expected:None [!"/foo/bar/a.py"];
  assert_find
    ~critical_files:[full_path "/foo/bar/a.py"]
    ~expected:(Some !"/foo/bar/a.py")
    [!"/foo/bar/a.py"];
  assert_find
    ~critical_files:[base_name "b.py"]
    ~expected:(Some !"foo/b.py")
    [!"foo/a.py"; !"foo/b.py"];
  assert_find ~critical_files:[full_path "b.py"] ~expected:None [!"foo/a.py"; !"foo/b.py"];
  assert_find
    ~critical_files:[full_path "foo/b.py"]
    ~expected:(Some !"foo/b.py")
    [!"foo/a.py"; !"foo/b.py"];
  assert_find ~critical_files:[base_name "a.py"] ~expected:None [!"a/foo.py"; !"/b/a/foo.py"];
  assert_find ~critical_files:[base_name "a.py"] ~expected:(Some !"a.py") [!"a.py"; !"b.py"];
  assert_find ~critical_files:[base_name "a.py"] ~expected:(Some !"a.py") [!"b.py"; !"a.py"];
  assert_find ~critical_files:[extension "py"] ~expected:(Some !"a.py") [!"a.py"; !"b.py"];
  assert_find ~critical_files:[extension "py"] ~expected:(Some !"b.py") [!"b.py"; !"a.py"];
  assert_find ~critical_files:[extension "py"] ~expected:(Some !"a.py") [!"b.derp"; !"a.py"];
  assert_find ~critical_files:[base_name "a.py"; base_name "b.py"] ~expected:None [];
  assert_find ~critical_files:[base_name "a.py"; base_name "b.py"] ~expected:None [!"c.py"];
  assert_find
    ~critical_files:[base_name "a.py"; base_name "b.py"]
    ~expected:(Some !"b.py")
    [!"b.py"; !"c.py"];
  assert_find
    ~critical_files:[base_name "a.py"; base_name "b.py"]
    ~expected:(Some !"b.py")
    [!"c.py"; !"b.py"];
  assert_find
    ~critical_files:[base_name "a.py"; base_name "b.py"]
    ~expected:(Some !"foo/b.py")
    [!"foo/c.py"; !"d.py"; !"foo/b.py"];
  assert_find
    ~critical_files:[extension "txt"; extension "pyi"]
    ~expected:None
    [!"foo/c.py"; !"d.py"; !"foo/b.py"];
  assert_find
    ~critical_files:[extension "txt"; extension "pyi"]
    ~expected:(Some !"d.pyi")
    [!"foo/c.py"; !"d.pyi"; !"foo/b.py"];
  assert_find
    ~critical_files:[base_name "a.py"; full_path "b.py"]
    ~expected:None
    [!"foo/c.py"; !"d.py"; !"foo/b.py"];
  assert_find
    ~critical_files:[base_name "a.py"; full_path "foo/b.py"]
    ~expected:(Some !"foo/b.py")
    [!"foo/c.py"; !"d.py"; !"foo/b.py"];

  assert_find
    ~critical_files:[base_name ".pyre_configuration"]
    ~expected:(Some !"foo/.pyre_configuration")
    [!"foo/a.py"; !"foo/.pyre_configuration.local"; !"foo/.pyre_configuration"];
  assert_find
    ~critical_files:[base_name ".pyre_configuration.local"]
    ~expected:(Some !"foo/.pyre_configuration.local")
    [!"foo/a.py"; !"foo/.pyre_configuration"; !"foo/.pyre_configuration.local"];
  ()


let () = "critical_files" >::: ["critical_files" >:: test_critical_files] |> Test.run
