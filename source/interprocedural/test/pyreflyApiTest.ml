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
module ModuleDefinitionsFile = PyreflyApi.ModuleDefinitionsFile
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

  let _ = pp
end

let filesystem_module_path path =
  ModulePath.Filesystem (ArtifactPath.create (PyrePath.create_absolute path))


let test_module_qualifiers _ =
  let open Option.Monad_infix in
  let assert_module_qualifiers ?(add_toplevel_modules = false) ~inputs ~expected () =
    let pyrefly_directory = PyrePath.create_absolute "/pyrefly" in
    let make_module_definitions { ModuleQualifierInput.module_name; path; id } =
      {
        PyreflyApi.ProjectFile.Module.module_id = ModuleId.from_int id;
        module_name = Reference.create module_name;
        absolute_source_path = path;
        relative_source_path = None;
        info_filename = None;
        is_test = false;
        is_interface = false;
        is_init = false;
        is_internal = false;
      }
    in
    let make_testing_module { ModuleQualifierExpected.module_name; source_path; id; qualifier = _ } =
      {
        PyreflyApi.Testing.Module.module_id = ModuleId.from_int id;
        module_name = Reference.create module_name;
        absolute_source_path = source_path >>| PyrePath.create_absolute >>| ArtifactPath.create;
        relative_source_path = None;
        pyrefly_info_filename = None;
        is_test = false;
        is_stub = false;
        is_internal = false;
      }
    in
    let to_string map =
      map |> Map.to_alist |> [%show: (ModuleQualifier.t * PyreflyApi.Testing.Module.t) list]
    in
    let inputs = List.map ~f:make_module_definitions inputs in
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
      |> List.filter_map ~f:(fun definition ->
             match definition with
             | PyreflyApi.Testing.Definition.Class
                 ({ ModuleDefinitionsFile.ClassDefinition.name_location; _ } as class_definition) ->
                 Some (name_location, class_definition)
             | _ -> None)
      |> Ast.Location.Map.of_alist_exn
    in
    let function_definitions =
      definitions
      |> List.filter_map ~f:(fun definition ->
             match definition with
             | PyreflyApi.Testing.Definition.Function
                 ({ ModuleDefinitionsFile.FunctionDefinition.local_function_id; _ } as
                 function_definition) ->
                 Some (local_function_id, function_definition)
             | _ -> None)
      |> PyreflyApi.LocalFunctionId.Map.of_alist_exn
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
      ?(parent = ModuleDefinitionsFile.ParentScope.TopLevel)
      ?(is_overload = false)
      ?(is_property_getter = false)
      ?(is_property_setter = false)
      ~line
      name
    =
    PyreflyApi.Testing.Definition.Function
      {
        ModuleDefinitionsFile.FunctionDefinition.name;
        local_function_id = PyreflyApi.LocalFunctionId.create_function (location_at_line line);
        parent;
        is_overload;
        undecorated_signatures = [];
        captured_variables = [];
        is_staticmethod = false;
        is_classmethod = false;
        is_property_getter;
        is_property_setter;
        is_stub = false;
        is_def_statement = true;
        is_toplevel = false;
        is_class_toplevel = false;
        overridden_base_method = None;
        defining_class = None;
        decorator_callees = Location.SerializableMap.empty;
      }
  in
  let create_class ?(parent = ModuleDefinitionsFile.ParentScope.TopLevel) ~line name =
    PyreflyApi.Testing.Definition.Class
      {
        ModuleDefinitionsFile.ClassDefinition.name;
        name_location = location_at_line line;
        parent;
        local_class_id = PyreflyApi.LocalClassId.from_int 0;
        bases = [];
        mro = ModuleDefinitionsFile.ClassMro.Resolved [];
        is_synthesized = false;
        is_dataclass = false;
        is_named_tuple = false;
        is_typed_dict = false;
        fields = [];
        decorator_callees = Location.SerializableMap.empty;
      }
  in
  let class_parent ~line = ModuleDefinitionsFile.ParentScope.Class (location_at_line line) in
  let function_parent ~line = ModuleDefinitionsFile.ParentScope.Function (location_at_line line) in
  assert_fully_qualified_names
    ~definitions:
      [
        create_function ~line:1 "foo";
        create_function ~line:2 "bar";
        create_class ~line:3 "MyClass";
        create_function ~line:4 ~parent:(class_parent ~line:3) "__init__";
        create_function ~line:5 ~parent:(class_parent ~line:3) "method";
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
    ~definitions:
      [create_function ~line:1 "foo"; create_function ~line:2 "foo"; create_function ~line:3 "foo"]
    ~expected:["test.$toplevel"; "test.foo"; "test.foo$2"; "test.foo$3"]
    ();
  (* Nested definitions *)
  assert_fully_qualified_names
    ~definitions:
      [
        create_function ~line:1 "decorator";
        create_function ~line:2 ~parent:(function_parent ~line:1) "inner";
        create_function ~line:3 ~parent:(function_parent ~line:1) "wrapper";
      ]
    ~expected:["test.$toplevel"; "test.decorator"; "test.decorator.inner"; "test.decorator.wrapper"]
    ();
  (* Nested definitions with the same name *)
  assert_fully_qualified_names
    ~definitions:
      [
        create_function ~line:1 "decorator";
        create_function ~line:2 ~parent:(function_parent ~line:1) "inner";
        create_function ~line:3 ~parent:(function_parent ~line:1) "inner";
      ]
    ~expected:["test.$toplevel"; "test.decorator"; "test.decorator.inner"; "test.decorator.inner$2"]
    ();
  assert_fully_qualified_names
    ~definitions:
      [
        create_function ~line:1 "decorator";
        create_function ~line:2 ~parent:(function_parent ~line:1) "inner";
        create_function ~line:3 ~parent:(function_parent ~line:2) "inner";
        create_function ~line:4 ~parent:(function_parent ~line:2) "inner";
        create_function ~line:5 ~parent:(function_parent ~line:1) "inner";
        create_function ~line:6 ~parent:(function_parent ~line:5) "inner";
        create_function ~line:7 ~parent:(function_parent ~line:5) "inner";
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
        create_class ~line:1 "a";
        create_class ~line:2 ~parent:(class_parent ~line:1) "b";
        create_function ~line:3 ~parent:(class_parent ~line:2) "__init__";
        create_function ~line:4 ~parent:(class_parent ~line:1) "__init__";
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
        create_class ~line:1 "a";
        create_class ~line:2 ~parent:(class_parent ~line:1) "b";
        create_class ~line:3 ~parent:(class_parent ~line:1) "b";
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
        create_class ~line:1 "conflict";
        create_function ~line:2 ~parent:(class_parent ~line:1) "foo";
        create_class ~line:3 "no_conflict";
        create_function ~line:4 ~parent:(class_parent ~line:3) "foo";
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
        create_class ~line:1 "a";
        create_function ~line:2 ~parent:(class_parent ~line:1) "b";
        create_function ~line:3 ~parent:(function_parent ~line:2) "c";
      ]
    ~modules:["test.a.b"]
    ~expected:["test.$toplevel"; "test.a"; "test.a.$class_toplevel"; "test#a.b"; "test#a.b.c"]
    ();
  (* Conflict between class and function name *)
  assert_fully_qualified_names
    ~definitions:
      [
        create_class ~line:1 "a";
        create_function ~line:2 ~parent:(class_parent ~line:1) "__init__";
        create_function ~line:3 "a";
      ]
    ~expected:["test.$toplevel"; "test.a"; "test.a.$class_toplevel"; "test.a.__init__"; "test.a$2"]
    ();
  (* Type overloads *)
  (* Note: pyrefly only exports one definition per overload set, so this test isn't relevant
     anymore. *)
  assert_fully_qualified_names
    ~definitions:
      [
        create_function ~line:1 "a";
        create_function ~line:2 ~is_overload:true "a";
        create_function ~line:3 ~is_overload:true "a";
      ]
    ~expected:["test.$toplevel"; "test.a"; "test.a$2"; "test.a$3"]
    ();
  (* Property getter and setter *)
  assert_fully_qualified_names
    ~definitions:
      [
        create_class ~line:1 "A";
        create_function ~line:2 ~parent:(class_parent ~line:1) ~is_property_getter:true "x";
        create_function ~line:3 ~parent:(class_parent ~line:1) ~is_property_setter:true "x";
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
