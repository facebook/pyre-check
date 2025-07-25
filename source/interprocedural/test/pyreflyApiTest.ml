(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Interprocedural
open Ast
module ModulePath = PyreflyApi.ModulePath
module ModuleId = PyreflyApi.ModuleId
module ModuleQualifier = PyreflyApi.ModuleQualifier

module ModuleQualifierTest = struct
  type t = {
    module_name: string;
    path: ModulePath.t;
    expected_qualifier: string;
  }
  [@@deriving show]
end

let filesystem_module_path path =
  ModulePath.Filesystem (ArtifactPath.create (PyrePath.create_absolute path))


let test_module_qualifiers _ =
  let assert_module_qualifiers test () =
    let make_pyrefly_module { ModuleQualifierTest.module_name; path; _ } =
      {
        PyreflyApi.ProjectFile.Module.module_id = ModuleId.from_int 0;
        module_name = Reference.create module_name;
        module_path = path;
        info_path = None;
      }
    in
    let to_string map =
      map
      |> Map.to_alist
      |> List.map
           ~f:(fun (module_qualifier, { PyreflyApi.ProjectFile.Module.module_name; module_path; _ })
              ->
             {
               ModuleQualifierTest.expected_qualifier = ModuleQualifier.show module_qualifier;
               module_name = Reference.show module_name;
               path = module_path;
             })
      |> [%show: ModuleQualifierTest.t list]
    in
    let inputs = List.map ~f:make_pyrefly_module test in
    let expected =
      test
      |> List.map ~f:(fun ({ ModuleQualifierTest.expected_qualifier; _ } as test_module) ->
             ( ModuleQualifier.from_reference_unchecked (Reference.create expected_qualifier),
               make_pyrefly_module test_module ))
      |> PyreflyApi.ModuleQualifier.Map.of_alist_exn
    in
    let actual = PyreflyApi.Testing.create_module_qualifiers inputs in
    assert_equal
      ~cmp:(ModuleQualifier.Map.equal PyreflyApi.ProjectFile.Module.equal)
      ~printer:to_string
      expected
      actual
  in
  assert_module_qualifiers
    [
      { module_name = "a"; path = filesystem_module_path "/root/a.py"; expected_qualifier = "a" };
      { module_name = "b"; path = filesystem_module_path "/root/b.py"; expected_qualifier = "b" };
      { module_name = "c"; path = filesystem_module_path "/root/c.py"; expected_qualifier = "c" };
    ]
    ();
  assert_module_qualifiers
    [
      {
        module_name = "a";
        path = filesystem_module_path "/root/a/__init__.py";
        expected_qualifier = "a";
      };
      {
        module_name = "a.b";
        path = filesystem_module_path "/root/a/b/__init__.py";
        expected_qualifier = "a.b";
      };
      {
        module_name = "a.b.c";
        path = filesystem_module_path "/root/a/b/c.py";
        expected_qualifier = "a.b.c";
      };
    ]
    ();
  (* Conflicting module name due to multiple roots *)
  assert_module_qualifiers
    [
      {
        module_name = "a.b";
        path = filesystem_module_path "/first_root/a/b.py";
        expected_qualifier = "first_root/a/b.py:a.b";
      };
      {
        module_name = "a.b";
        path = filesystem_module_path "/second_root/a/b.py";
        expected_qualifier = "second_root/a/b.py:a.b";
      };
    ]
    ();
  (* Conflicting module name due to stub files *)
  assert_module_qualifiers
    [
      {
        module_name = "a.b";
        path = filesystem_module_path "/root/a/b.py";
        expected_qualifier = "b.py:a.b";
      };
      {
        module_name = "a.b";
        path = filesystem_module_path "/root/a/b.pyi";
        expected_qualifier = "b.pyi:a.b";
      };
    ]
    ();
  (* Multiple modules with conflicts *)
  assert_module_qualifiers
    [
      {
        module_name = "a.b";
        path = filesystem_module_path "/root/a/b.py";
        expected_qualifier = "b.py:a.b";
      };
      {
        module_name = "a.b";
        path = filesystem_module_path "/root/a/b.pyi";
        expected_qualifier = "b.pyi:a.b";
      };
      {
        module_name = "a.c";
        path = filesystem_module_path "/root/a/c.py";
        expected_qualifier = "c.py:a.c";
      };
      {
        module_name = "a.c";
        path = filesystem_module_path "/root/a/c.pyi";
        expected_qualifier = "c.pyi:a.c";
      };
    ]
    ();
  (* __init__.py vs module.py *)
  assert_module_qualifiers
    [
      {
        module_name = "a.b";
        path = filesystem_module_path "/first_root/a/b.py";
        expected_qualifier = "b.py:a.b";
      };
      {
        module_name = "a.b";
        path = filesystem_module_path "/second_root/a/b/__init__.py";
        expected_qualifier = "__init__.py:a.b";
      };
    ]
    ();
  (* 3 conflicts *)
  assert_module_qualifiers
    [
      {
        module_name = "a.b";
        path = filesystem_module_path "/first_root/a/b.py";
        expected_qualifier = "first_root/a/b.py:a.b";
      };
      {
        module_name = "a.b";
        path = filesystem_module_path "/second_root/a/b/__init__.py";
        expected_qualifier = "a/b/__init__.py:a.b";
      };
      {
        module_name = "a.b";
        path = filesystem_module_path "/third_root/a/b.py";
        expected_qualifier = "third_root/a/b.py:a.b";
      };
    ]
    ();
  (* filesystem vs typeshed *)
  assert_module_qualifiers
    [
      {
        module_name = "typing";
        path = filesystem_module_path "/root/stdlib/typing.py";
        expected_qualifier = "stdlib/typing.py:typing";
      };
      {
        module_name = "typing";
        path = ModulePath.BundledTypeshed (PyrePath.create_absolute "typing.py");
        expected_qualifier = "typeshed://typing.py:typing";
      };
    ]
    ();
  (* filesystem vs namespace *)
  assert_module_qualifiers
    [
      {
        module_name = "a";
        path = filesystem_module_path "/root/a.py";
        expected_qualifier = "root/a.py:a";
      };
      {
        module_name = "a";
        path = ModulePath.Namespace (PyrePath.create_absolute "a.py");
        expected_qualifier = "namespace://a.py:a";
      };
    ]
    ();
  (* filesystem vs memory *)
  assert_module_qualifiers
    [
      {
        module_name = "a";
        path = filesystem_module_path "/root/a.py";
        expected_qualifier = "root/a.py:a";
      };
      {
        module_name = "a";
        path = ModulePath.Memory (PyrePath.create_absolute "a.py");
        expected_qualifier = "memory://a.py:a";
      };
    ]
    ();
  ()


let () = "pyreflyApi" >::: ["module_qualifiers" >:: test_module_qualifiers] |> Test.run
