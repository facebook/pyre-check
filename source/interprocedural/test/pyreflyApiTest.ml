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
module FullyQualifiedName = PyreflyApi.FullyQualifiedName

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


let test_fully_qualified_names _ =
  let assert_fully_qualified_names ?(module_name = "test") ?(modules = []) ~source ~expected () =
    let source = Test.parse source in
    let module_qualifier =
      module_name |> Reference.create |> ModuleQualifier.from_reference_unchecked
    in
    let existing_modules = List.map modules ~f:Reference.create |> Reference.Set.of_list in
    let actual =
      PyreflyApi.Testing.create_fully_qualified_names
        ~module_qualifier
        ~module_exists:(fun qualifier ->
          Set.mem existing_modules (ModuleQualifier.to_reference qualifier))
        source
      |> List.map ~f:(fun { PyreflyApi.Testing.QualifiedDefinition.qualified_name; _ } ->
             qualified_name |> FullyQualifiedName.to_reference |> Reference.show)
    in
    assert_equal ~cmp:[%compare.equal: string list] ~printer:[%show: string list] expected actual
  in
  assert_fully_qualified_names
    ~source:
      {|
  def foo():
    return 1

  def bar():
    return 2

  class MyClass:
    def __init__(self):
      return

    def method(self):
      return
  |}
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
    ~source:{|
  def foo():
    return 1

  def foo():
    return 2

  def foo():
    return 3
  |}
    ~expected:["test.$toplevel"; "test.foo"; "test.foo$2"; "test.foo$3"]
    ();
  (* Nested definitions *)
  assert_fully_qualified_names
    ~source:
      {|
  def decorator():
    def inner():
      return

    def wrapper():
      return
      
    return
  |}
    ~expected:["test.$toplevel"; "test.decorator"; "test.decorator.inner"; "test.decorator.wrapper"]
    ();
  (* Nested definitions with the same name *)
  assert_fully_qualified_names
    ~source:
      {|
  def decorator():
    def inner():
      return

    def inner():
      return
      
    return
  |}
    ~expected:["test.$toplevel"; "test.decorator"; "test.decorator.inner"; "test.decorator.inner$2"]
    ();
  assert_fully_qualified_names
    ~source:
      {|
  def decorator():
    def inner():
      def inner():
        return

      def inner():
        return

      return

    def inner():
      def inner():
        return

      def inner():
        return

      return
      
    return
  |}
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
    ~source:
      {|
  class a:
    class b:
      def __init__(self):
        return

    def __init__(self):
      return
  |}
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
    ~source:{|
  class a:
    class b:
      pass
    class b:
      pass
  |}
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
    ~source:
      {|
  class conflict:
    def foo(self):
      return
  class no_conflict:
    def foo(self):
      return
  |}
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
    ~source:{|
  class a:
    def b():
      def c():
        return
  |}
    ~modules:["test.a.b"]
    ~expected:["test.$toplevel"; "test.a"; "test.a.$class_toplevel"; "test#a.b"; "test#a.b.c"]
    ();
  (* Conflict between class and function name *)
  assert_fully_qualified_names
    ~source:{|
  class a:
    def __init__(self):
      return

  def a():
    return
  |}
    ~expected:["test.$toplevel"; "test.a"; "test.a.$class_toplevel"; "test.a.__init__"; "test.a$2"]
    ();
  (* Type overloads *)
  assert_fully_qualified_names
    ~source:
      {|
  from typing import overload
  def a(x: int | str) -> int | str:
    return
  @overload
  def a(x: int) -> int:
    return
  @overload
  def a(x: str) -> str:
    return
  |}
    ~expected:["test.$toplevel"; "test.a"; "test.a@overload"; "test.a@overload$2"]
    ();
  (* Property getter and setter *)
  assert_fully_qualified_names
    ~source:
      {|
  class A:
    @property
    def x(self):
      return 0
    @x.setter
    def x(self, value):
      return
  |}
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
