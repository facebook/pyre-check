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
module ModuleInfoFile = PyreflyApi.ModuleInfoFile
module FullyQualifiedName = PyreflyApi.FullyQualifiedName

module ModuleQualifierInput = struct
  type t = {
    module_name: string;
    path: ModulePath.t;
    id: int;
  }
end

module ModuleQualifierExpected = struct
  type t = {
    qualifier: string;
    module_name: string;
    source_path: string option;
    id: int;
  }
  [@@deriving show]
end

let filesystem_module_path path =
  ModulePath.Filesystem (ArtifactPath.create (PyrePath.create_absolute path))


let test_module_qualifiers _ =
  let open Option.Monad_infix in
  let assert_module_qualifiers ?(add_toplevel_modules = false) ~inputs ~expected () =
    let pyrefly_directory = PyrePath.create_absolute "/pyrefly" in
    let make_pyrefly_module { ModuleQualifierInput.module_name; path; id } =
      {
        PyreflyApi.ProjectFile.Module.module_id = ModuleId.from_int id;
        module_name = Reference.create module_name;
        module_path = path;
        info_path = None;
        is_test = false;
        is_interface = false;
        is_init = false;
      }
    in
    let make_testing_module { ModuleQualifierExpected.module_name; source_path; id; qualifier = _ } =
      {
        PyreflyApi.Testing.Module.module_id = ModuleId.from_int id;
        module_name = Reference.create module_name;
        source_path = source_path >>| PyrePath.create_absolute >>| ArtifactPath.create;
        pyrefly_info_path = None;
        is_test = false;
        is_stub = false;
      }
    in
    let to_string map =
      map |> Map.to_alist |> [%show: (ModuleQualifier.t * PyreflyApi.Testing.Module.t) list]
    in
    let inputs = List.map ~f:make_pyrefly_module inputs in
    let expected =
      expected
      |> List.map ~f:(fun ({ ModuleQualifierExpected.qualifier; _ } as test_module) ->
             ( ModuleQualifier.from_reference_unchecked (Reference.create qualifier),
               make_testing_module test_module ))
      |> PyreflyApi.ModuleQualifier.Map.of_alist_exn
    in
    let actual =
      PyreflyApi.Testing.create_module_qualifiers ~pyrefly_directory ~add_toplevel_modules inputs
    in
    assert_equal
      ~cmp:(ModuleQualifier.Map.equal PyreflyApi.Testing.Module.equal)
      ~printer:to_string
      expected
      actual
  in
  assert_module_qualifiers
    ~inputs:
      [
        {
          ModuleQualifierInput.module_name = "a";
          path = filesystem_module_path "/root/a.py";
          id = 0;
        };
        {
          ModuleQualifierInput.module_name = "b";
          path = filesystem_module_path "/root/b.py";
          id = 1;
        };
        {
          ModuleQualifierInput.module_name = "c";
          path = filesystem_module_path "/root/c.py";
          id = 2;
        };
      ]
    ~expected:
      [
        {
          ModuleQualifierExpected.qualifier = "a";
          module_name = "a";
          source_path = Some "/root/a.py";
          id = 0;
        };
        {
          ModuleQualifierExpected.qualifier = "b";
          module_name = "b";
          source_path = Some "/root/b.py";
          id = 1;
        };
        {
          ModuleQualifierExpected.qualifier = "c";
          module_name = "c";
          source_path = Some "/root/c.py";
          id = 2;
        };
      ]
    ();
  assert_module_qualifiers
    ~inputs:
      [
        {
          ModuleQualifierInput.module_name = "a";
          path = filesystem_module_path "/root/a/__init__.py";
          id = 0;
        };
        {
          ModuleQualifierInput.module_name = "a.b";
          path = filesystem_module_path "/root/a/b/__init__.py";
          id = 1;
        };
        {
          ModuleQualifierInput.module_name = "a.b.c";
          path = filesystem_module_path "/root/a/b/c.py";
          id = 2;
        };
      ]
    ~expected:
      [
        {
          ModuleQualifierExpected.qualifier = "a";
          module_name = "a";
          source_path = Some "/root/a/__init__.py";
          id = 0;
        };
        {
          ModuleQualifierExpected.qualifier = "a.b";
          module_name = "a.b";
          source_path = Some "/root/a/b/__init__.py";
          id = 1;
        };
        {
          ModuleQualifierExpected.qualifier = "a.b.c";
          module_name = "a.b.c";
          source_path = Some "/root/a/b/c.py";
          id = 2;
        };
      ]
    ();
  (* Conflicting module name due to multiple roots *)
  assert_module_qualifiers
    ~inputs:
      [
        {
          ModuleQualifierInput.module_name = "a.b";
          path = filesystem_module_path "/first_root/a/b.py";
          id = 0;
        };
        {
          ModuleQualifierInput.module_name = "a.b";
          path = filesystem_module_path "/second_root/a/b.py";
          id = 1;
        };
      ]
    ~expected:
      [
        {
          ModuleQualifierExpected.qualifier = "first_root/a/b.py:a.b";
          module_name = "a.b";
          source_path = Some "/first_root/a/b.py";
          id = 0;
        };
        {
          ModuleQualifierExpected.qualifier = "second_root/a/b.py:a.b";
          module_name = "a.b";
          source_path = Some "/second_root/a/b.py";
          id = 1;
        };
      ]
    ();
  (* Conflicting module name due to stub files *)
  assert_module_qualifiers
    ~inputs:
      [
        {
          ModuleQualifierInput.module_name = "a.b";
          path = filesystem_module_path "/root/a/b.py";
          id = 0;
        };
        {
          ModuleQualifierInput.module_name = "a.b";
          path = filesystem_module_path "/root/a/b.pyi";
          id = 1;
        };
      ]
    ~expected:
      [
        {
          ModuleQualifierExpected.qualifier = "b.py:a.b";
          module_name = "a.b";
          source_path = Some "/root/a/b.py";
          id = 0;
        };
        {
          ModuleQualifierExpected.qualifier = "b.pyi:a.b";
          module_name = "a.b";
          source_path = Some "/root/a/b.pyi";
          id = 1;
        };
      ]
    ();
  (* Multiple modules with conflicts *)
  assert_module_qualifiers
    ~inputs:
      [
        {
          ModuleQualifierInput.module_name = "a.b";
          path = filesystem_module_path "/root/a/b.py";
          id = 0;
        };
        {
          ModuleQualifierInput.module_name = "a.b";
          path = filesystem_module_path "/root/a/b.pyi";
          id = 1;
        };
        {
          ModuleQualifierInput.module_name = "a.c";
          path = filesystem_module_path "/root/a/c.py";
          id = 2;
        };
        {
          ModuleQualifierInput.module_name = "a.c";
          path = filesystem_module_path "/root/a/c.pyi";
          id = 3;
        };
      ]
    ~expected:
      [
        {
          ModuleQualifierExpected.qualifier = "b.py:a.b";
          module_name = "a.b";
          source_path = Some "/root/a/b.py";
          id = 0;
        };
        {
          ModuleQualifierExpected.qualifier = "b.pyi:a.b";
          module_name = "a.b";
          source_path = Some "/root/a/b.pyi";
          id = 1;
        };
        {
          ModuleQualifierExpected.qualifier = "c.py:a.c";
          module_name = "a.c";
          source_path = Some "/root/a/c.py";
          id = 2;
        };
        {
          ModuleQualifierExpected.qualifier = "c.pyi:a.c";
          module_name = "a.c";
          source_path = Some "/root/a/c.pyi";
          id = 3;
        };
      ]
    ();
  (* __init__.py vs module.py *)
  assert_module_qualifiers
    ~inputs:
      [
        {
          ModuleQualifierInput.module_name = "a.b";
          path = filesystem_module_path "/first_root/a/b.py";
          id = 0;
        };
        {
          ModuleQualifierInput.module_name = "a.b";
          path = filesystem_module_path "/second_root/a/b/__init__.py";
          id = 1;
        };
      ]
    ~expected:
      [
        {
          ModuleQualifierExpected.qualifier = "b.py:a.b";
          module_name = "a.b";
          source_path = Some "/first_root/a/b.py";
          id = 0;
        };
        {
          ModuleQualifierExpected.qualifier = "__init__.py:a.b";
          module_name = "a.b";
          source_path = Some "/second_root/a/b/__init__.py";
          id = 1;
        };
      ]
    ();
  (* 3 conflicts *)
  assert_module_qualifiers
    ~inputs:
      [
        {
          ModuleQualifierInput.module_name = "a.b";
          path = filesystem_module_path "/first_root/a/b.py";
          id = 0;
        };
        {
          ModuleQualifierInput.module_name = "a.b";
          path = filesystem_module_path "/second_root/a/b/__init__.py";
          id = 1;
        };
        {
          ModuleQualifierInput.module_name = "a.b";
          path = filesystem_module_path "/third_root/a/b.py";
          id = 2;
        };
      ]
    ~expected:
      [
        {
          ModuleQualifierExpected.qualifier = "first_root/a/b.py:a.b";
          module_name = "a.b";
          source_path = Some "/first_root/a/b.py";
          id = 0;
        };
        {
          ModuleQualifierExpected.qualifier = "a/b/__init__.py:a.b";
          module_name = "a.b";
          source_path = Some "/second_root/a/b/__init__.py";
          id = 1;
        };
        {
          ModuleQualifierExpected.qualifier = "third_root/a/b.py:a.b";
          module_name = "a.b";
          source_path = Some "/third_root/a/b.py";
          id = 2;
        };
      ]
    ();
  (* filesystem vs typeshed *)
  assert_module_qualifiers
    ~inputs:
      [
        {
          ModuleQualifierInput.module_name = "typing";
          path = filesystem_module_path "/root/stdlib/typing.py";
          id = 0;
        };
        {
          ModuleQualifierInput.module_name = "typing";
          path = ModulePath.BundledTypeshed (PyrePath.create_absolute "typing.py");
          id = 1;
        };
      ]
    ~expected:
      [
        {
          ModuleQualifierExpected.qualifier = "stdlib/typing.py:typing";
          module_name = "typing";
          source_path = Some "/root/stdlib/typing.py";
          id = 0;
        };
        {
          ModuleQualifierExpected.qualifier = "typeshed://typing.py:typing";
          module_name = "typing";
          source_path = Some "/pyrefly/typeshed/typing.py";
          id = 1;
        };
      ]
    ();
  (* filesystem vs namespace *)
  assert_module_qualifiers
    ~inputs:
      [
        {
          ModuleQualifierInput.module_name = "a";
          path = filesystem_module_path "/root/a.py";
          id = 0;
        };
        {
          ModuleQualifierInput.module_name = "a";
          path = ModulePath.Namespace (PyrePath.create_absolute "a.py");
          id = 1;
        };
      ]
    ~expected:
      [
        {
          ModuleQualifierExpected.qualifier = "root/a.py:a";
          module_name = "a";
          source_path = Some "/root/a.py";
          id = 0;
        };
        {
          ModuleQualifierExpected.qualifier = "namespace://a.py:a";
          module_name = "a";
          source_path = None;
          id = 1;
        };
      ]
    ();
  (* filesystem vs memory *)
  assert_module_qualifiers
    ~inputs:
      [
        {
          ModuleQualifierInput.module_name = "a";
          path = filesystem_module_path "/root/a.py";
          id = 0;
        };
        {
          ModuleQualifierInput.module_name = "a";
          path = ModulePath.Memory (PyrePath.create_absolute "a.py");
          id = 1;
        };
      ]
    ~expected:
      [
        {
          ModuleQualifierExpected.qualifier = "root/a.py:a";
          module_name = "a";
          source_path = Some "/root/a.py";
          id = 0;
        };
        {
          ModuleQualifierExpected.qualifier = "memory://a.py:a";
          module_name = "a";
          source_path = None;
          id = 1;
        };
      ]
    ();
  (* add implicit toplevel modules *)
  assert_module_qualifiers
    ~add_toplevel_modules:true
    ~inputs:
      [
        {
          ModuleQualifierInput.module_name = "a.b";
          path = filesystem_module_path "/root/a/b.py";
          id = 0;
        };
      ]
    ~expected:
      [
        { ModuleQualifierExpected.qualifier = "a"; module_name = "a"; source_path = None; id = 1 };
        {
          ModuleQualifierExpected.qualifier = "a.b";
          module_name = "a.b";
          source_path = Some "/root/a/b.py";
          id = 0;
        };
      ]
    ();
  ()


let test_fully_qualified_names _ =
  let location_at_line line =
    let position = { Ast.Location.line; column = 0 } in
    { Ast.Location.start = position; stop = position }
  in
  let assert_fully_qualified_names ?(module_name = "test") ?(modules = []) ~definitions ~expected ()
    =
    let module_qualifier =
      module_name |> Reference.create |> ModuleQualifier.from_reference_unchecked
    in
    let existing_modules = List.map modules ~f:Reference.create |> Reference.Set.of_list in
    let class_definitions =
      definitions
      |> List.filter_map ~f:(fun (line, definition) ->
             match definition with
             | PyreflyApi.Testing.Definition.Class class_definition ->
                 Some (location_at_line line, class_definition)
             | _ -> None)
      |> Ast.Location.Map.of_alist_exn
    in
    let function_definitions =
      definitions
      |> List.filter_map ~f:(fun (line, definition) ->
             match definition with
             | PyreflyApi.Testing.Definition.Function function_definition ->
                 Some (location_at_line line, function_definition)
             | _ -> None)
      |> Ast.Location.Map.of_alist_exn
    in
    let actual =
      PyreflyApi.Testing.create_fully_qualified_names
        ~module_qualifier
        ~module_exists:(fun qualifier ->
          Set.mem existing_modules (ModuleQualifier.to_reference qualifier))
        ~class_definitions
        ~function_definitions
      |> List.map ~f:(fun { PyreflyApi.Testing.QualifiedDefinition.qualified_name; _ } ->
             qualified_name |> FullyQualifiedName.to_reference |> Reference.show)
    in
    assert_equal ~cmp:[%compare.equal: string list] ~printer:[%show: string list] expected actual
  in
  let create_function
      ?(parent = ModuleInfoFile.ParentScope.TopLevel)
      ?(is_overload = false)
      ?(is_property_getter = false)
      ?(is_property_setter = false)
      name
    =
    PyreflyApi.Testing.Definition.Function
      {
        ModuleInfoFile.FunctionDefinition.name;
        parent;
        is_overload;
        undecorated_signatures = [];
        is_staticmethod = false;
        is_classmethod = false;
        is_property_getter;
        is_property_setter;
        is_stub = false;
        is_toplevel = false;
        is_class_toplevel = false;
      }
  in
  let create_class ?(parent = ModuleInfoFile.ParentScope.TopLevel) name =
    PyreflyApi.Testing.Definition.Class
      {
        ModuleInfoFile.ClassDefinition.name;
        parent;
        local_class_id = PyreflyApi.LocalClassId.from_int 0;
        bases = [];
        is_synthesized = false;
        fields = [];
      }
  in
  let class_parent ~line = ModuleInfoFile.ParentScope.Class (location_at_line line) in
  let function_parent ~line = ModuleInfoFile.ParentScope.Function (location_at_line line) in
  assert_fully_qualified_names
    ~definitions:
      [
        1, create_function "foo";
        2, create_function "bar";
        3, create_class "MyClass";
        4, create_function ~parent:(class_parent ~line:3) "__init__";
        5, create_function ~parent:(class_parent ~line:3) "method";
      ]
    ~expected:
      [
        "test.$toplevel";
        "test.foo";
        "test.bar";
        "test.MyClass";
        "test.MyClass.$class_toplevel";
        "test.MyClass.__init__";
        "test.MyClass.method";
      ]
    ();
  (* Multiple definitions with the same name *)
  assert_fully_qualified_names
    ~definitions:[1, create_function "foo"; 2, create_function "foo"; 3, create_function "foo"]
    ~expected:["test.$toplevel"; "test.foo"; "test.foo$2"; "test.foo$3"]
    ();
  (* Nested definitions *)
  assert_fully_qualified_names
    ~definitions:
      [
        1, create_function "decorator";
        2, create_function ~parent:(function_parent ~line:1) "inner";
        3, create_function ~parent:(function_parent ~line:1) "wrapper";
      ]
    ~expected:["test.$toplevel"; "test.decorator"; "test.decorator.inner"; "test.decorator.wrapper"]
    ();
  (* Nested definitions with the same name *)
  assert_fully_qualified_names
    ~definitions:
      [
        1, create_function "decorator";
        2, create_function ~parent:(function_parent ~line:1) "inner";
        3, create_function ~parent:(function_parent ~line:1) "inner";
      ]
    ~expected:["test.$toplevel"; "test.decorator"; "test.decorator.inner"; "test.decorator.inner$2"]
    ();
  assert_fully_qualified_names
    ~definitions:
      [
        1, create_function "decorator";
        2, create_function ~parent:(function_parent ~line:1) "inner";
        3, create_function ~parent:(function_parent ~line:2) "inner";
        4, create_function ~parent:(function_parent ~line:2) "inner";
        5, create_function ~parent:(function_parent ~line:1) "inner";
        6, create_function ~parent:(function_parent ~line:5) "inner";
        7, create_function ~parent:(function_parent ~line:5) "inner";
      ]
    ~expected:
      [
        "test.$toplevel";
        "test.decorator";
        "test.decorator.inner";
        "test.decorator.inner.inner";
        "test.decorator.inner.inner$2";
        "test.decorator.inner$2";
        "test.decorator.inner$2.inner";
        "test.decorator.inner$2.inner$2";
      ]
    ();
  (* Nested classes *)
  assert_fully_qualified_names
    ~definitions:
      [
        1, create_class "a";
        2, create_class ~parent:(class_parent ~line:1) "b";
        3, create_function ~parent:(class_parent ~line:2) "__init__";
        4, create_function ~parent:(class_parent ~line:1) "__init__";
      ]
    ~expected:
      [
        "test.$toplevel";
        "test.a";
        "test.a.$class_toplevel";
        "test.a.b";
        "test.a.b.$class_toplevel";
        "test.a.b.__init__";
        "test.a.__init__";
      ]
    ();
  (* Nested classes with the same name *)
  assert_fully_qualified_names
    ~definitions:
      [
        1, create_class "a";
        2, create_class ~parent:(class_parent ~line:1) "b";
        3, create_class ~parent:(class_parent ~line:1) "b";
      ]
    ~expected:
      [
        "test.$toplevel";
        "test.a";
        "test.a.$class_toplevel";
        "test.a.b";
        "test.a.b.$class_toplevel";
        "test.a.b$2";
        "test.a.b$2.$class_toplevel";
      ]
    ();
  (* Nested classes that conflicts with another module *)
  assert_fully_qualified_names
    ~definitions:
      [
        1, create_class "conflict";
        2, create_function ~parent:(class_parent ~line:1) "foo";
        3, create_class "no_conflict";
        4, create_function ~parent:(class_parent ~line:3) "foo";
      ]
    ~modules:["test.conflict"]
    ~expected:
      [
        "test.$toplevel";
        "test#conflict";
        "test#conflict.$class_toplevel";
        "test#conflict.foo";
        "test.no_conflict";
        "test.no_conflict.$class_toplevel";
        "test.no_conflict.foo";
      ]
    ();
  assert_fully_qualified_names
    ~definitions:
      [
        1, create_class "a";
        2, create_function ~parent:(class_parent ~line:1) "b";
        3, create_function ~parent:(function_parent ~line:2) "c";
      ]
    ~modules:["test.a.b"]
    ~expected:["test.$toplevel"; "test.a"; "test.a.$class_toplevel"; "test#a.b"; "test#a.b.c"]
    ();
  (* Conflict between class and function name *)
  assert_fully_qualified_names
    ~definitions:
      [
        1, create_class "a";
        2, create_function ~parent:(class_parent ~line:1) "__init__";
        3, create_function "a";
      ]
    ~expected:["test.$toplevel"; "test.a"; "test.a.$class_toplevel"; "test.a.__init__"; "test.a$2"]
    ();
  (* Type overloads *)
  (* Note: pyrefly only exports one definition per overload set, so this test isn't relevant
     anymore. *)
  assert_fully_qualified_names
    ~definitions:
      [
        1, create_function "a";
        2, create_function ~is_overload:true "a";
        3, create_function ~is_overload:true "a";
      ]
    ~expected:["test.$toplevel"; "test.a"; "test.a$2"; "test.a$3"]
    ();
  (* Property getter and setter *)
  assert_fully_qualified_names
    ~definitions:
      [
        1, create_class "A";
        2, create_function ~parent:(class_parent ~line:1) ~is_property_getter:true "x";
        3, create_function ~parent:(class_parent ~line:1) ~is_property_setter:true "x";
      ]
    ~expected:["test.$toplevel"; "test.A"; "test.A.$class_toplevel"; "test.A.x"; "test.A.x@setter"]
    ();
  ()


let () =
  "pyreflyApi"
  >::: [
         "module_qualifiers" >:: test_module_qualifiers;
         "fully_qualified_names" >:: test_fully_qualified_names;
       ]
  |> Test.run
