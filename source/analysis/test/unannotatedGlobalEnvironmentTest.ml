(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Ast
open Analysis
open Pyre
open Test

let location (start_line, start_column) (stop_line, stop_column) =
  {
    Location.start = { Location.line = start_line; column = start_column };
    stop = { Location.line = stop_line; column = stop_column };
  }


let create_with_location value start end_ = Node.create value ~location:(location start end_)

let test_global_registration context =
  let assert_registers ?(expected = true) source name =
    let project = ScratchProject.setup ["test.py", source] ~context in
    let ast_environment = ScratchProject.build_ast_environment project in
    let update_result =
      UnannotatedGlobalEnvironment.create ast_environment
      |> fun unannotated_global_environment ->
      UnannotatedGlobalEnvironment.update_this_and_all_preceding_environments
        unannotated_global_environment
        ~scheduler:(mock_scheduler ())
        ColdStart
    in
    let read_only = UnannotatedGlobalEnvironment.UpdateResult.read_only update_result in
    assert_equal (UnannotatedGlobalEnvironment.ReadOnly.class_exists read_only name) expected
  in
  assert_registers {|
   class Bar:
     pass
  |} "test.Bar";
  assert_registers ~expected:false {|
   class Foo:
     pass
  |} "test.Bar";
  ()


let test_define_registration context =
  let assert_registers ~expected source =
    let project = ScratchProject.setup ["test.py", source] ~context in
    let ast_environment = ScratchProject.build_ast_environment project in
    let unannotated_global_environment = UnannotatedGlobalEnvironment.create ast_environment in
    let update_result =
      UnannotatedGlobalEnvironment.update_this_and_all_preceding_environments
        unannotated_global_environment
        ~scheduler:(mock_scheduler ())
        ColdStart
    in
    let read_only = UnannotatedGlobalEnvironment.UpdateResult.read_only update_result in
    let actual = UnannotatedGlobalEnvironment.ReadOnly.all_defines_in_module read_only !&"test" in
    let expected = List.sort expected ~compare:Reference.compare in
    assert_equal
      ~cmp:(List.equal Reference.equal)
      ~printer:(List.to_string ~f:Reference.show)
      expected
      actual
  in
  assert_registers {|
    def foo():
      pass
  |} ~expected:[!&"test.$toplevel"; !&"test.foo"];
  assert_registers
    {|
    def bar(): ...
    def foo():
      return bar()
  |}
    ~expected:[!&"test.$toplevel"; !&"test.foo"; !&"test.bar"];
  assert_registers
    {|
     from typing import overload

     @overload
     def foo(x: int) -> int: ...
     @overload
     def foo(x: str) -> str: ...

     def foo(x):
       return x
  |}
    ~expected:[!&"test.$toplevel"; !&"test.foo"];
  assert_registers
    {|
     class Foo:
       pass
    |}
    ~expected:[!&"test.$toplevel"; !&"test.Foo.$class_toplevel"];
  assert_registers
    {|
     class Foo:
       x: int
     class Foo:
       y: str
    |}
    ~expected:[!&"test.$toplevel"; !&"test.Foo.$class_toplevel"];
  assert_registers
    {|
     class Foo:
       def foo(self): ...
    |}
    ~expected:[!&"test.$toplevel"; !&"test.Foo.$class_toplevel"; !&"test.Foo.foo"];
  assert_registers
    {|
     class Foo:
       @property
       def foo(self): ...
       @foo.setter
       def foo(self, value): ...
    |}
    ~expected:[!&"test.$toplevel"; !&"test.Foo.$class_toplevel"; !&"test.Foo.foo"];
  assert_registers
    {|
    def foo():
      def bar():
        ...
      return bar()
  |}
    ~expected:[!&"test.$toplevel"; !&"test.foo"; !&"$local_test?foo$bar"];
  assert_registers
    {|
     def foo():
       def bar():
         pass
       bar()
       def baz():
         pass
       baz()
    |}
    ~expected:[!&"test.$toplevel"; !&"test.foo"; !&"$local_test?foo$bar"; !&"$local_test?foo$baz"];
  assert_registers
    {|
     def foo():
       def bar():
         def baz():
           pass
       bar(x)
    |}
    ~expected:
      [!&"test.$toplevel"; !&"test.foo"; !&"$local_test?foo$bar"; !&"$local_test?foo?bar$baz"];
  assert_registers
    {|
     def foo(flag):
       if flag:
         def bar():
           pass
         return bar()
       else:
         def baz():
           pass
         return baz()
    |}
    ~expected:[!&"test.$toplevel"; !&"test.foo"; !&"$local_test?foo$bar"; !&"$local_test?foo$baz"];
  assert_registers
    {|
     def foo():
       for x in range(3):
         def bar():
           def baz():
             pass
       return bar(x)
    |}
    ~expected:
      [!&"test.$toplevel"; !&"test.foo"; !&"$local_test?foo$bar"; !&"$local_test?foo?bar$baz"];
  assert_registers
    {|
     def foo():
       with open("something") as f:
         def bar():
           def baz():
             pass
         bar()
    |}
    ~expected:
      [!&"test.$toplevel"; !&"test.foo"; !&"$local_test?foo$bar"; !&"$local_test?foo?bar$baz"];
  assert_registers
    {|
     def foo():
       try:
         def bar():
           pass
         bar()
       except:
         def baz():
           pass
         baz()
       finally:
         def quix():
           pass
         return quix()
    |}
    ~expected:
      [
        !&"test.$toplevel";
        !&"test.foo";
        !&"$local_test?foo$bar";
        !&"$local_test?foo$baz";
        !&"$local_test?foo$quix";
      ];

  (* Do not look into bodies of classes that are not at top-level *)
  assert_registers
    {|
     def foo():
       class C:
         x: int
         def bar(self): ...
    |}
    ~expected:[!&"test.$toplevel"; !&"test.foo"];
  assert_registers
    {|
     def foo():
       class C:
         x: int
         def bar(self):
           class D:
             def baz(self): ...
    |}
    ~expected:[!&"test.$toplevel"; !&"test.foo"];

  (* Nested class at module toplevel should still work *)
  assert_registers
    {|
     class C:
       def foo(self): ...
       class D:
         def bar(self): ...
  |}
    ~expected:
      [
        !&"test.$toplevel";
        !&"test.C.$class_toplevel";
        !&"test.C.foo";
        !&"test.C.D.$class_toplevel";
        !&"test.C.D.bar";
      ];
  ()


let test_simple_global_registration context =
  let assert_registers source name expected =
    let project = ScratchProject.setup ["test.py", source] ~context in
    let ast_environment = ScratchProject.build_ast_environment project in
    let unannotated_global_environment = UnannotatedGlobalEnvironment.create ast_environment in
    let update_result =
      UnannotatedGlobalEnvironment.update_this_and_all_preceding_environments
        unannotated_global_environment
        ~scheduler:(mock_scheduler ())
        ColdStart
    in
    let read_only = UnannotatedGlobalEnvironment.UpdateResult.read_only update_result in
    let printer global =
      global >>| UnannotatedGlobal.sexp_of_t >>| Sexp.to_string_hum |> Option.value ~default:"None"
    in
    let location_insensitive_compare left right =
      Option.compare UnannotatedGlobal.compare left right = 0
    in
    assert_equal
      ~cmp:location_insensitive_compare
      ~printer
      expected
      (UnannotatedGlobalEnvironment.ReadOnly.get_unannotated_global
         read_only
         (Reference.create name))
  in
  let target_location =
    { Location.start = { line = 2; column = 0 }; stop = { line = 2; column = 3 } }
    |> Location.with_module ~module_reference:(Reference.create "test")
  in
  let value_location =
    { Location.start = { line = 2; column = 6 }; stop = { line = 2; column = 7 } }
  in
  let value =
    let value = parse_single_expression "8" in
    { value with location = value_location }
  in
  assert_registers
    {|
    bar = 8
  |}
    "test.bar"
    (Some (SimpleAssign { explicit_annotation = None; value; target_location }));
  assert_registers {|
    other.bar = 8
  |} "test.other.bar" None;
  assert_registers {|
    other.bar = 8
  |} "other.bar" None;
  assert_registers
    {|
    try:
      baz = 8
    except:
      pass
  |}
    "test.baz"
    (Some
       (SimpleAssign
          {
            explicit_annotation = None;
            value =
              create_with_location
                (Expression.Expression.Constant (Expression.Constant.Integer 8))
                (3, 8)
                (3, 9);
            target_location =
              { Location.start = { line = 3; column = 2 }; stop = { line = 3; column = 5 } }
              |> Location.with_module ~module_reference:(Reference.create "test");
          }));
  let parse_define define =
    match parse_single_statement define ~preprocess:true ~handle:"test.py" with
    | { Node.value = Statement.Statement.Define { signature; _ }; location } ->
        {
          UnannotatedGlobal.UnannotatedDefine.define = signature;
          location = Location.with_module ~module_reference:(Reference.create "test") location;
        }
    | _ -> failwith "not define"
  in
  assert_registers
    {|
      def foo(x: int) -> str:
        pass
      def foo(x: float) -> bool:
        pass
    |}
    "test.foo"
    (Some
       (Define
          [
            parse_define
              {|
                def foo(x: int) -> str:
                  pass
              |};
            parse_define
              {|
                # spacer
                # spacer
                def foo(x: float) -> bool:
                  pass
              |};
          ]));
  ()


let test_builtin_modules context =
  let ast_environment =
    let ast_environment =
      let sources = ["builtins.py", "foo: int = 42"] in
      let project =
        ScratchProject.setup
          ~context
          ~include_typeshed_stubs:false
          ~include_helper_builtins:false
          sources
      in
      ScratchProject.build_ast_environment project
    in
    let update_result =
      let unannotated_global_environment = UnannotatedGlobalEnvironment.create ast_environment in
      UnannotatedGlobalEnvironment.update_this_and_all_preceding_environments
        unannotated_global_environment
        ~scheduler:(mock_scheduler ())
        ColdStart
    in
    UnannotatedGlobalEnvironment.UpdateResult.read_only update_result
  in
  assert_bool
    "empty qualifier module exists"
    (UnannotatedGlobalEnvironment.ReadOnly.module_exists ast_environment Reference.empty);
  assert_bool
    "random qualifier doesn't exist"
    (not (UnannotatedGlobalEnvironment.ReadOnly.module_exists ast_environment !&"derp"));
  assert_bool
    "`builtins` exists"
    (UnannotatedGlobalEnvironment.ReadOnly.module_exists ast_environment !&"builtins");
  assert_bool
    "`future.builtins` exists"
    (UnannotatedGlobalEnvironment.ReadOnly.module_exists ast_environment !&"future.builtins");

  let assert_nonempty qualifier =
    match UnannotatedGlobalEnvironment.ReadOnly.get_module_metadata ast_environment qualifier with
    | None -> assert_failure "Module does not exist"
    | Some metadata ->
        assert_bool "empty stub not expected" (not (Module.empty_stub metadata));
        assert_bool "implicit module not expected" (not (Module.is_implicit metadata));
        assert_equal
          ~ctxt:context
          ~cmp:[%compare.equal: Module.Export.t option]
          ~printer:(fun export -> Sexp.to_string_hum [%message (export : Module.Export.t option)])
          (Some Module.Export.(Name GlobalVariable))
          (Module.get_export metadata "foo")
  in
  assert_nonempty Reference.empty;
  assert_nonempty !&"builtins";
  assert_nonempty !&"future.builtins";
  ()


let test_resolve_exports context =
  let open UnannotatedGlobalEnvironment in
  let assert_resolved ?(include_typeshed = false) ~expected ?from ~reference sources =
    Memory.reset_shared_memory ();
    let ast_environment =
      let project =
        if include_typeshed then
          ScratchProject.setup ~context sources
        else
          ScratchProject.setup
            ~context
            ~include_typeshed_stubs:false
            ~include_helper_builtins:false
            ~external_sources:["builtins.py", ""]
            sources
      in
      ScratchProject.build_ast_environment project
    in
    let update_result =
      let unannotated_global_environment = UnannotatedGlobalEnvironment.create ast_environment in
      UnannotatedGlobalEnvironment.update_this_and_all_preceding_environments
        unannotated_global_environment
        ~scheduler:(mock_scheduler ())
        ColdStart
    in
    let unannotated_global_environment = UpdateResult.read_only update_result in
    let actual = ReadOnly.resolve_exports unannotated_global_environment ?from reference in
    assert_equal
      ~ctxt:context
      ~cmp:[%compare.equal: ResolvedReference.t option]
      ~printer:(fun result -> Sexp.to_string_hum [%message (result : ResolvedReference.t option)])
      expected
      actual
  in
  let resolved_module name = ResolvedReference.Module name in
  let resolved_attribute ?(remaining = []) ?export from name =
    let export =
      match export with
      | None -> ResolvedReference.FromModuleGetattr
      | Some export -> ResolvedReference.Exported export
    in
    ResolvedReference.ModuleAttribute { from; name; export; remaining }
  in
  let resolved_placeholder_stub ?(remaining = []) stub_module =
    ResolvedReference.PlaceholderStub { stub_module; remaining }
  in

  let open Module in
  assert_resolved [] ~reference:!&"derp" ~expected:None;
  assert_resolved ["derp.py", ""] ~reference:!&"derp" ~expected:(Some (resolved_module !&"derp"));
  assert_resolved ["derp.py", ""] ~reference:!&"derp.foo" ~expected:None;
  assert_resolved
    ["derp.pyi", "# pyre-placeholder-stub"]
    ~reference:!&"derp.foo"
    ~expected:(Some (resolved_placeholder_stub !&"derp" ~remaining:["foo"]));
  assert_resolved
    ["derp/foo.pyi", "# pyre-placeholder-stub"]
    ~reference:!&"derp.foo"
    ~expected:(Some (resolved_placeholder_stub !&"derp.foo"));
  assert_resolved
    ["derp/foo.py", ""]
    ~reference:!&"derp"
    ~expected:(Some (resolved_module !&"derp"));
  assert_resolved
    ["derp/foo.py", ""]
    ~reference:!&"derp.foo"
    ~expected:(Some (resolved_module !&"derp.foo"));
  assert_resolved ["derp/foo.py", ""] ~reference:!&"derp.foo.bar" ~expected:None;
  assert_resolved
    ["derp/__init__.py", ""; "derp/foo.py", ""]
    ~reference:!&"derp.foo"
    ~expected:(Some (resolved_module !&"derp.foo"));
  assert_resolved
    ["derp/__init__.py", "foo = 1"]
    ~reference:!&"derp.foo"
    ~expected:(Some (resolved_attribute !&"derp" "foo" ~export:Export.Name.GlobalVariable));
  assert_resolved
    ["derp.py", "foo = 1"]
    ~reference:!&"derp.foo"
    ~expected:(Some (resolved_attribute !&"derp" "foo" ~export:Export.Name.GlobalVariable));
  assert_resolved
    ["derp.py", "def foo(): pass"]
    ~reference:!&"derp.foo"
    ~expected:
      (Some
         (resolved_attribute !&"derp" "foo" ~export:(Export.Name.Define { is_getattr_any = false })));
  assert_resolved
    ["derp.py", "class foo: pass"]
    ~reference:!&"derp.foo"
    ~expected:(Some (resolved_attribute !&"derp" "foo" ~export:Export.Name.Class));
  assert_resolved
    ["derp.py", "class foo: pass"]
    ~reference:!&"derp.foo.bar.baz"
    ~expected:
      (Some (resolved_attribute !&"derp" "foo" ~export:Export.Name.Class ~remaining:["bar"; "baz"]));

  assert_resolved ["a.py", "import b"] ~reference:!&"a.b" ~expected:None;
  assert_resolved
    ["a.py", "import b"; "b.py", ""]
    ~reference:!&"a.b"
    ~expected:(Some (resolved_module !&"b"));
  assert_resolved
    ["a.py", "import b"; "b/c.py", ""]
    ~reference:!&"a.b"
    ~expected:(Some (resolved_module !&"b"));
  assert_resolved
    ["a.py", "import b"; "b/c.py", ""]
    ~reference:!&"a.b.c"
    ~expected:(Some (resolved_module !&"b.c"));
  assert_resolved
    ["a.py", "import b as c"; "b.py", ""]
    ~reference:!&"a.c"
    ~expected:(Some (resolved_module !&"b"));
  assert_resolved ["a.py", "from b import c"] ~reference:!&"a.c" ~expected:None;
  assert_resolved ["a.py", "from b import c"; "b.py", ""] ~reference:!&"a.c" ~expected:None;
  assert_resolved
    ["a.py", "from b import c"; "b.py", "import c"; "c.py", ""]
    ~reference:!&"a.c"
    ~expected:(Some (resolved_module !&"c"));
  assert_resolved
    ["a.py", "from b import c"; "b.py", "import c"; "c.py", ""]
    ~reference:!&"a.c.d"
    ~expected:None;
  assert_resolved
    ["a.py", "from b import c"; "b.py", "c = 42"]
    ~reference:!&"a.c"
    ~expected:(Some (resolved_attribute !&"b" "c" ~export:Export.Name.GlobalVariable));
  assert_resolved
    ["a.py", "from b import c as d"; "b.py", "c = 42"]
    ~reference:!&"a.d"
    ~expected:(Some (resolved_attribute !&"b" "c" ~export:Export.Name.GlobalVariable));
  assert_resolved
    ["a.py", "from b import c as d"; "b.py", "c = 42"]
    ~reference:!&"a.d.e"
    ~expected:
      (Some (resolved_attribute !&"b" "c" ~export:Export.Name.GlobalVariable ~remaining:["e"]));

  (* Transitive import tests *)
  assert_resolved
    ["a.py", "from b import c"; "b.py", "from d import c"; "d.py", "c = 42"]
    ~reference:!&"a.c"
    ~expected:(Some (resolved_attribute !&"d" "c" ~export:Export.Name.GlobalVariable));
  assert_resolved
    [
      "a.py", "from b import c";
      "b.py", "from d import c";
      "d.py", "from e import f as c";
      "e.py", "f = 42";
    ]
    ~reference:!&"a.c"
    ~expected:(Some (resolved_attribute !&"e" "f" ~export:Export.Name.GlobalVariable));
  assert_resolved
    [
      "a.py", "from b import foo";
      "b.py", "from c import bar as foo";
      "c.py", "from d import cow as bar";
      "d.py", "cow = 1";
    ]
    ~reference:!&"a.foo"
    ~expected:(Some (resolved_attribute !&"d" "cow" ~export:Export.Name.GlobalVariable));

  (* Getattr-any tests *)
  assert_resolved
    ["a.py", "from typing import Any\nb = 42\ndef __getattr__(name) -> Any: ..."]
    ~reference:!&"a.b"
    ~expected:(Some (resolved_attribute !&"a" "b" ~export:Export.Name.GlobalVariable));
  assert_resolved
    ["a.py", "from typing import Any\nb = 42\ndef __getattr__(name) -> Any: ..."]
    ~reference:!&"a.c"
    ~expected:(Some (resolved_attribute !&"a" "c"));
  assert_resolved
    ["a.py", "from typing import Any\nb = 42\ndef __getattr__(name) -> Any: ..."]
    ~reference:!&"a.c.d"
    ~expected:(Some (resolved_attribute !&"a" "c" ~remaining:["d"]));
  assert_resolved
    [
      "a.py", "from b import c";
      "b.py", "from typing import Any\nc = 42\ndef __getattr__(name) -> Any: ...";
    ]
    ~reference:!&"a.c"
    ~expected:(Some (resolved_attribute !&"b" "c" ~export:Export.Name.GlobalVariable));
  assert_resolved
    [
      "a.py", "from b import d";
      "b.py", "from typing import Any\nc = 42\ndef __getattr__(name) -> Any: ...";
    ]
    ~reference:!&"a.d"
    ~expected:(Some (resolved_attribute !&"b" "d"));
  assert_resolved
    [
      "a.py", "from b import d";
      "b.py", "from typing import Any\nc = 42\ndef __getattr__(name) -> Any: ...";
    ]
    ~reference:!&"a.d.e"
    ~expected:(Some (resolved_attribute !&"b" "d" ~remaining:["e"]));

  (* Access into placeholder stub *)
  assert_resolved
    ["a.py", "from b import c"; "b.pyi", "# pyre-placeholder-stub"]
    ~reference:!&"a.c.d"
    ~expected:(Some (resolved_placeholder_stub !&"b" ~remaining:["c"; "d"]));
  (* Cyclic imports *)
  assert_resolved ["a.py", "from a import b"] ~reference:!&"a.b" ~expected:None;
  assert_resolved
    ["a.py", "from b import c"; "b.py", "from a import c"]
    ~reference:!&"a.c"
    ~expected:None;
  assert_resolved
    ["a.py", "from b import c"; "b.py", "from d import c"; "d.py", "from b import c"]
    ~reference:!&"a.c"
    ~expected:None;
  (* Self-cycle is OK in certain circumstances. *)
  assert_resolved ["a/__init__.py", "from a import b"] ~reference:!&"a.b" ~expected:None;
  assert_resolved
    ["a/__init__.py", "from a import b"; "a/b.py", ""]
    ~reference:!&"a.b"
    ~expected:(Some (resolved_module !&"a.b"));
  assert_resolved
    ["a/__init__.py", "from a import b as c"; "a/b.py", ""]
    ~reference:!&"a.c"
    ~expected:(Some (resolved_module !&"a.b"));
  (* TODO: We might want to ban this in the future since it is technically not OK at runtime. *)
  assert_resolved
    ["a.py", "from a import b"; "a/b.py", ""]
    ~reference:!&"a.b"
    ~expected:(Some (resolved_module !&"a.b"));
  (* This is technically OK at runtime but we don't want to support it. *)
  assert_resolved ["a.py", "b = 1\nfrom a import b"] ~reference:!&"a.b" ~expected:None;
  (* Runtime does not allow `from X import ...` when `X` is itself a module alias. *)
  assert_resolved
    ["a.py", "from b.c import d"; "b.py", "import c"; "c.py", "d = 42"]
    ~reference:!&"a.d"
    ~expected:None;
  (* ... but we do pretend placeholder stubs exist. *)
  assert_resolved
    ["a.py", "from b.nonexistent import foo"; "b.pyi", "# pyre-placeholder-stub"]
    ~reference:!&"a.foo"
    ~expected:(Some (resolved_placeholder_stub !&"b" ~remaining:["nonexistent"; "foo"]));
  (* Package takes precedence over module of the same name. *)
  assert_resolved
    [
      "qualifier.py", "from qualifier.foo import foo";
      "qualifier/__init__.py", "";
      "qualifier/foo/__init__.py", "foo = 1";
    ]
    ~reference:!&"qualifier.foo.foo"
    ~expected:(Some (resolved_attribute !&"qualifier.foo" "foo" ~export:Export.Name.GlobalVariable));

  (* Illustrate why the `from` argument matters. *)
  assert_resolved
    [
      "qualifier/__init__.py", "from qualifier.a import bar as a";
      "qualifier/a.py", "foo = 1\nbar = 1";
    ]
    ~reference:!&"qualifier.a"
    ~expected:(Some (resolved_attribute !&"qualifier.a" "bar" ~export:Export.Name.GlobalVariable));
  assert_resolved
    [
      "qualifier/__init__.py", "from qualifier.a import bar as a";
      "qualifier/a.py", "foo = 1\nbar = 1";
    ]
    ~reference:!&"qualifier.a.foo"
    ~expected:
      (Some
         (resolved_attribute
            !&"qualifier.a"
            "bar"
            ~export:Export.Name.GlobalVariable
            ~remaining:["foo"]));
  assert_resolved
    [
      "qualifier/__init__.py", "from qualifier.a import bar as a";
      "qualifier/a.py", "foo = 1\nbar = 1";
    ]
    ~from:!&"qualifier.a"
    ~reference:!&"foo"
    ~expected:(Some (resolved_attribute !&"qualifier.a" "foo" ~export:Export.Name.GlobalVariable));

  (* Special attribute tests. *)
  assert_resolved
    ["foo.py", ""]
    ~reference:!&"foo.__doc__"
    ~expected:(Some (resolved_attribute !&"foo" "__doc__" ~export:GlobalVariable));
  assert_resolved
    ["foo.py", ""]
    ~reference:!&"foo.__file__"
    ~expected:(Some (resolved_attribute !&"foo" "__file__" ~export:GlobalVariable));
  assert_resolved
    ["foo.py", ""]
    ~reference:!&"foo.__name__"
    ~expected:(Some (resolved_attribute !&"foo" "__name__" ~export:GlobalVariable));
  assert_resolved
    ["foo.py", ""]
    ~reference:!&"foo.__package__"
    ~expected:(Some (resolved_attribute !&"foo" "__package__" ~export:GlobalVariable));
  (* FIXME(grievejia): This is not 100% correct. `__path__` only exists if the containing module is
     `__init__`. *)
  assert_resolved
    ["foo.py", ""]
    ~reference:!&"foo.__path__"
    ~expected:(Some (resolved_attribute !&"foo" "__path__" ~export:GlobalVariable));
  assert_resolved
    ["foo.py", ""]
    ~reference:!&"foo.__dict__"
    ~expected:(Some (resolved_attribute !&"foo" "__dict__" ~export:GlobalVariable));
  (* Implicit modules also contain speical attributes. *)
  assert_resolved
    ["foo/bar.py", ""]
    ~reference:!&"foo.__name__"
    ~expected:(Some (resolved_attribute !&"foo" "__name__" ~export:GlobalVariable));
  (* Explicitly defined special attributes take precedence over the default ones. *)
  assert_resolved
    ["foo.py", "from bar import __name__"; "bar.py", ""]
    ~reference:!&"foo.__name__"
    ~expected:(Some (resolved_attribute !&"bar" "__name__" ~export:GlobalVariable));
  ()


let assert_updates
    ~context
    ?original_source
    ?new_source
    ~middle_actions
    ~expected_triggers
    ?post_actions
    ()
  =
  Memory.reset_shared_memory ();
  let sources = original_source >>| (fun source -> "test.py", source) |> Option.to_list in
  let project =
    ScratchProject.setup
      ~include_typeshed_stubs:false
      ~incremental_style:FineGrained
      ~in_memory:false
      sources
      ~context
  in
  let ast_environment = ScratchProject.build_ast_environment project in
  let unannotated_global_environment = UnannotatedGlobalEnvironment.create ast_environment in
  let configuration = ScratchProject.configuration_of project in
  let update_result =
    UnannotatedGlobalEnvironment.update_this_and_all_preceding_environments
      unannotated_global_environment
      ~scheduler:(mock_scheduler ())
      ColdStart
  in
  let read_only = UnannotatedGlobalEnvironment.UpdateResult.read_only update_result in
  let execute_action = function
    | `Get (class_name, dependency, expected_number_of_statements) ->
        let printer number =
          number >>| Format.sprintf "number of attributes: %d" |> Option.value ~default:"No class"
        in
        UnannotatedGlobalEnvironment.ReadOnly.get_class_summary read_only ~dependency class_name
        >>| Node.value
        >>| ClassSummary.attributes
        >>| Identifier.SerializableMap.bindings
        >>| List.length
        |> assert_equal ~printer expected_number_of_statements
    | `Mem (class_name, dependency, expectation) ->
        UnannotatedGlobalEnvironment.ReadOnly.class_exists read_only ~dependency class_name
        |> assert_equal expectation
    | `AllClasses expectation ->
        UnannotatedGlobalEnvironment.ReadOnly.all_classes read_only
        |> assert_equal ~printer:(List.to_string ~f:Fn.id) expectation
    | `Global (global_name, dependency, expectation) ->
        let printer optional =
          optional
          >>| UnannotatedGlobal.sexp_of_t
          >>| Sexp.to_string_hum
          |> Option.value ~default:"none"
        in
        let cmp left right = Option.compare UnannotatedGlobal.compare left right = 0 in
        let remove_target_location = function
          | UnannotatedGlobal.SimpleAssign assign ->
              UnannotatedGlobal.SimpleAssign
                { assign with target_location = Location.WithModule.any }
          | UnannotatedGlobal.TupleAssign assign ->
              UnannotatedGlobal.TupleAssign
                { assign with target_location = Location.WithModule.any }
          | global -> global
        in
        UnannotatedGlobalEnvironment.ReadOnly.get_unannotated_global
          read_only
          global_name
          ~dependency
        >>| remove_target_location
        |> assert_equal ~cmp ~printer expectation
    | `Define (define_name, dependency, expectation) ->
        let actual =
          UnannotatedGlobalEnvironment.ReadOnly.get_function_definition
            read_only
            define_name
            ~dependency
        in
        let cmp left right = Int.equal 0 (Option.compare FunctionDefinition.compare left right) in
        let print format definition =
          Format.fprintf
            format
            "%s"
            (Sexp.to_string_hum [%message (definition : FunctionDefinition.t option)])
        in
        assert_equal
          ~cmp
          ~printer:(fun definition ->
            Sexp.to_string_hum [%message (definition : FunctionDefinition.t option)])
          ~pp_diff:(diff ~print)
          expectation
          actual
    | `DefineBody (define_name, dependency, expectation) ->
        let actual =
          UnannotatedGlobalEnvironment.ReadOnly.get_define_body read_only define_name ~dependency
        in
        let cmp = [%compare.equal: Statement.Define.t Node.t option] in
        assert_equal
          ~cmp
          ~printer:(fun bodies ->
            Sexp.to_string_hum [%message (bodies : Statement.Define.t Node.t option)])
          expectation
          actual
  in
  List.iter middle_actions ~f:execute_action;
  let add_file
      { ScratchProject.configuration = { Configuration.Analysis.local_root; _ }; _ }
      content
      ~relative
    =
    let content = trim_extra_indentation content in
    let file = File.create ~content (PyrePath.create_relative ~root:local_root ~relative) in
    File.write file
  in
  let delete_file
      { ScratchProject.configuration = { Configuration.Analysis.local_root; _ }; _ }
      relative
    =
    PyrePath.create_relative ~root:local_root ~relative |> PyrePath.absolute |> Core.Unix.remove
  in
  if Option.is_some original_source then
    delete_file project "test.py";
  new_source >>| add_file project ~relative:"test.py" |> Option.value ~default:();
  let { ScratchProject.module_tracker; _ } = project in
  let { Configuration.Analysis.local_root; _ } = configuration in
  let path = PyrePath.create_relative ~root:local_root ~relative:"test.py" in
  let update_result =
    ModuleTracker.update ~paths:[path] module_tracker
    |> (fun updates -> AstEnvironment.Update updates)
    |> UnannotatedGlobalEnvironment.update_this_and_all_preceding_environments
         unannotated_global_environment
         ~scheduler:(mock_scheduler ())
  in
  let printer set =
    SharedMemoryKeys.DependencyKey.RegisteredSet.elements set
    |> List.map ~f:SharedMemoryKeys.DependencyKey.get_key
    |> List.to_string ~f:SharedMemoryKeys.show_dependency
  in
  let expected_triggers = SharedMemoryKeys.DependencyKey.RegisteredSet.of_list expected_triggers in
  post_actions >>| List.iter ~f:execute_action |> Option.value ~default:();
  assert_equal
    ~cmp:SharedMemoryKeys.DependencyKey.RegisteredSet.equal
    ~printer
    expected_triggers
    (UnannotatedGlobalEnvironment.UpdateResult.locally_triggered_dependencies update_result)


let type_check_dependency =
  SharedMemoryKeys.DependencyKey.Registry.register (TypeCheckDefine (Reference.create "test"))


let alias_dependency =
  SharedMemoryKeys.DependencyKey.Registry.register (TypeCheckDefine (Reference.create "dep"))


let test_get_class_summary context =
  let assert_updates = assert_updates ~context in
  let dependency = type_check_dependency in
  assert_updates
    ~original_source:{|
      class Foo:
        x: int
    |}
    ~new_source:{|
      class Foo:
        x: str
    |}
    ~middle_actions:[`Get ("test.Foo", dependency, Some 1)]
    ~expected_triggers:[dependency]
    ();
  assert_updates
    ~original_source:{|
      class Foo:
        x: int
    |}
    ~new_source:{|
      class Foo:
        x: str
    |}
    ~middle_actions:[`Get ("test.Missing", dependency, None)]
    ~expected_triggers:[]
    ();
  assert_updates
    ~original_source:{|
      class Foo:
        x: int
    |}
    ~new_source:{|
      class Unrelated:
        x: int
      class Foo:
        x: int
    |}
    ~middle_actions:[`Get ("test.Foo", dependency, Some 1)]
    ~expected_triggers:[dependency]
    ();

  (* Last class definition wins *)
  assert_updates
    ~original_source:
      {|
      class Foo:
        x: int
      class Foo:
        x: int
        y: int
    |}
    ~new_source:{|
      class Unrelated:
        x: int
      class Foo:
        x: int
    |}
    ~middle_actions:[`Get ("test.Foo", dependency, Some 2)]
    ~expected_triggers:[dependency]
    ~post_actions:[`Get ("test.Foo", dependency, Some 1)]
    ();
  assert_updates
    ~original_source:
      {|
      class Foo:
        def method(self) -> None:
         print("hello")
    |}
    ~new_source:{|
      class Foo:
        def method(self) -> int:
          return 1
    |}
    ~middle_actions:[`Get ("test.Foo", dependency, Some 1)]
    ~expected_triggers:[dependency]
    ~post_actions:[`Get ("test.Foo", dependency, Some 1)]
    ();
  assert_updates
    ~original_source:
      {|
      class Foo:
        def method(self) -> None:
         print("hellobo")
    |}
    ~new_source:
      {|
      class Foo:
        def method(self) -> None:
         print("goodbye")
    |}
    ~middle_actions:[`Get ("test.Foo", dependency, Some 1)]
    ~expected_triggers:[]
    ~post_actions:[`Get ("test.Foo", dependency, Some 1)]
    ();
  ()


let test_class_exists_and_all_classes context =
  let assert_updates = assert_updates ~context in
  let dependency = type_check_dependency in

  (* class exists *)
  assert_updates
    ~new_source:{|
      class Foo:
        x: int
    |}
    ~middle_actions:[`Mem ("test.Foo", dependency, false)]
    ~expected_triggers:[dependency]
    ();
  assert_updates
    ~original_source:{|
      class Foo:
        x: int
    |}
    ~middle_actions:[`Mem ("test.Foo", dependency, true)]
    ~expected_triggers:[dependency]
    ();
  assert_updates
    ~original_source:{|
      class Foo:
        x: int
    |}
    ~new_source:{|
      class Foo:
        x: int
    |}
    ~middle_actions:[`Mem ("test.Foo", dependency, true)]
    ~expected_triggers:[]
    ();

  assert_updates
    ~original_source:{|
      class Foo:
        x: int
    |}
    ~new_source:{|
      class Foo:
        x: str
    |}
    ~middle_actions:[`Mem ("test.Foo", dependency, true)]
    ~expected_triggers:[]
    ();

  (* all_classes *)
  assert_updates
    ~original_source:{|
      class Foo:
        x: int
      class Bar:
        y: str
    |}
    ~new_source:{|
      class Foo:
        x: str
    |}
    ~middle_actions:[`AllClasses ["test.Bar"; "test.Foo"]]
    ~expected_triggers:[]
    ~post_actions:[`AllClasses ["test.Foo"]]
    ();
  ()


let test_get_unannotated_global context =
  let assert_updates = assert_updates ~context in
  let dependency = alias_dependency in

  assert_updates
    ~original_source:{|
      x: int = 7
    |}
    ~new_source:{|
      x: int = 9
    |}
    ~middle_actions:
      [
        `Global
          ( Reference.create "test.x",
            dependency,
            Some
              (UnannotatedGlobal.SimpleAssign
                 {
                   explicit_annotation =
                     Some { (parse_single_expression "int") with location = location (2, 3) (2, 6) };
                   value = { (parse_single_expression "7") with location = location (2, 9) (2, 10) };
                   target_location = Location.WithModule.any;
                 }) );
      ]
    ~expected_triggers:[dependency]
    ~post_actions:
      [
        `Global
          ( Reference.create "test.x",
            dependency,
            Some
              (UnannotatedGlobal.SimpleAssign
                 {
                   explicit_annotation =
                     Some { (parse_single_expression "int") with location = location (2, 3) (2, 6) };
                   value = { (parse_single_expression "9") with location = location (2, 9) (2, 10) };
                   target_location = Location.WithModule.any;
                 }) );
      ]
    ();
  assert_updates
    ~original_source:{|
      import target.member as alias
    |}
    ~new_source:{|
      import target.member as new_alias
    |}
    ~middle_actions:
      [
        `Global
          ( Reference.create "test.alias",
            dependency,
            Some
              (UnannotatedGlobal.Imported
                 (UnannotatedGlobal.ImportEntry.Module
                    { target = !&"target.member"; implicit_alias = false })) );
      ]
    ~expected_triggers:[dependency]
    ~post_actions:[`Global (Reference.create "test.alias", dependency, None)]
    ();
  assert_updates
    ~original_source:{|
      from target import member, other_member
    |}
    ~new_source:{|
      from target import other_member, member
    |}
    ~middle_actions:
      [
        `Global
          ( Reference.create "test.member",
            dependency,
            Some
              (UnannotatedGlobal.Imported
                 (UnannotatedGlobal.ImportEntry.Name
                    { from = !&"target"; target = "member"; implicit_alias = true })) );
        `Global
          ( Reference.create "test.other_member",
            dependency,
            Some
              (UnannotatedGlobal.Imported
                 (UnannotatedGlobal.ImportEntry.Name
                    { from = !&"target"; target = "other_member"; implicit_alias = true })) );
      ]
      (* Location insensitive *)
    ~expected_triggers:[]
    ~post_actions:
      [
        `Global
          ( Reference.create "test.member",
            dependency,
            Some
              (UnannotatedGlobal.Imported
                 (UnannotatedGlobal.ImportEntry.Name
                    { from = !&"target"; target = "member"; implicit_alias = true })) );
        `Global
          ( Reference.create "test.other_member",
            dependency,
            Some
              (UnannotatedGlobal.Imported
                 (UnannotatedGlobal.ImportEntry.Name
                    { from = !&"target"; target = "other_member"; implicit_alias = true })) );
      ]
    ();

  (* Don't infer * as a real thing *)
  assert_updates
    ~original_source:{|
      from target import *
    |}
    ~middle_actions:[`Global (Reference.create "test.*", dependency, None)]
    ~expected_triggers:[]
    ();

  let open Statement in
  let open Expression in
  let tuple_expression =
    node
      ~start:(2, 10)
      ~stop:(2, 24)
      (Expression.Tuple
         [
           node ~start:(2, 10) ~stop:(2, 13) (Expression.Name (Name.Identifier "int"));
           node ~start:(2, 15) ~stop:(2, 18) (Expression.Name (Name.Identifier "str"));
           node ~start:(2, 20) ~stop:(2, 24) (Expression.Name (Name.Identifier "bool"));
         ])
  in
  assert_updates
    ~original_source:{|
      X, Y, Z = int, str, bool
    |}
    ~middle_actions:
      [
        `Global
          ( Reference.create "test.X",
            dependency,
            Some
              (UnannotatedGlobal.TupleAssign
                 {
                   value = tuple_expression;
                   index = 0;
                   target_location = Location.WithModule.any;
                   total_length = 3;
                 }) );
        `Global
          ( Reference.create "test.Y",
            dependency,
            Some
              (UnannotatedGlobal.TupleAssign
                 {
                   value = tuple_expression;
                   index = 1;
                   target_location = Location.WithModule.any;
                   total_length = 3;
                 }) );
        `Global
          ( Reference.create "test.Z",
            dependency,
            Some
              (UnannotatedGlobal.TupleAssign
                 {
                   value = tuple_expression;
                   index = 2;
                   target_location = Location.WithModule.any;
                   total_length = 3;
                 }) );
      ]
    ~expected_triggers:[dependency]
    ();

  (* First global wins. *)
  assert_updates
    ~original_source:{|
      X = int
      X = str
    |}
    ~new_source:{|
      X = int
      X = str
    |}
    ~middle_actions:
      [
        `Global
          ( Reference.create "test.X",
            dependency,
            Some
              (UnannotatedGlobal.SimpleAssign
                 {
                   explicit_annotation = None;
                   value =
                     { (parse_single_expression "int") with location = location (2, 4) (2, 7) };
                   target_location = Location.WithModule.any;
                 }) );
      ]
    ~expected_triggers:[]
    ();

  (* Only recurse into ifs *)
  assert_updates
    ~original_source:{|
      if condition:
        X = int
      else:
        X = str
    |}
    ~new_source:{|
      if condition:
        X = int
      else:
        X = str
    |}
    ~middle_actions:
      [
        `Global
          ( Reference.create "test.X",
            dependency,
            Some
              (UnannotatedGlobal.SimpleAssign
                 {
                   explicit_annotation = None;
                   value =
                     { (parse_single_expression "int") with location = location (3, 6) (3, 9) };
                   target_location = Location.WithModule.any;
                 }) );
      ]
    ~expected_triggers:[]
    ();

  (* get defines *)
  let create_simple_signature
      ~start:(start_line, start_column)
      ~stop:(stop_line, stop_column)
      name
      return_annotation
    =
    {
      UnannotatedGlobal.UnannotatedDefine.define =
        {
          Define.Signature.name;
          parameters = [];
          decorators = [];
          return_annotation;
          async = false;
          generator = false;
          parent = None;
          nesting_define = None;
        };
      location =
        {
          Location.WithModule.start = { Location.line = start_line; column = start_column };
          stop = { Location.line = stop_line; column = stop_column };
          module_reference = Reference.create "test";
        };
    }
  in

  assert_updates
    ~original_source:{|
      def foo() -> None:
       print("hellobo")
    |}
    ~new_source:{|
      def foo() -> None:
       print("goodbye")
    |}
    ~middle_actions:
      [
        `Global
          ( Reference.create "test.foo",
            dependency,
            Some
              (UnannotatedGlobal.Define
                 [
                   create_simple_signature
                     ~start:(2, 0)
                     ~stop:(3, 17)
                     !&"test.foo"
                     (Some
                        (node
                           ~start:(2, 13)
                           ~stop:(2, 17)
                           (Expression.Constant Constant.NoneLiteral)));
                 ]) );
      ]
    ~expected_triggers:[]
    ~post_actions:
      [
        `Global
          ( Reference.create "test.foo",
            dependency,
            Some
              (UnannotatedGlobal.Define
                 [
                   create_simple_signature
                     ~start:(2, 0)
                     ~stop:(3, 17)
                     !&"test.foo"
                     (Some
                        (node
                           ~start:(2, 13)
                           ~stop:(2, 17)
                           (Expression.Constant Constant.NoneLiteral)));
                 ]) );
      ]
    ();
  ()


let test_dependencies_and_new_values context =
  let assert_updates = assert_updates ~context in

  (* we should be able to keep different dependencies straight *)
  assert_updates
    ~original_source:{|
      class Foo:
        x: int
    |}
    ~new_source:{|
      class Foo:2376
        x: str
    |}
    ~middle_actions:
      [
        `Get ("test.Foo", alias_dependency, Some 1); `Get ("test.Foo", type_check_dependency, Some 1);
      ]
    ~expected_triggers:[alias_dependency; type_check_dependency]
    ();

  (* Addition should add values when previously they were missing *)
  assert_updates
    ~original_source:{|
    |}
    ~new_source:{|
      x: int = 9
    |}
    ~middle_actions:[`Global (Reference.create "test.x", alias_dependency, None)]
    ~expected_triggers:[alias_dependency]
    ~post_actions:
      [
        `Global
          ( Reference.create "test.x",
            alias_dependency,
            Some
              (UnannotatedGlobal.SimpleAssign
                 {
                   explicit_annotation =
                     Some { (parse_single_expression "int") with location = location (2, 3) (2, 6) };
                   value = { (parse_single_expression "9") with location = location (2, 9) (2, 10) };
                   target_location = Location.WithModule.any;
                 }) );
      ]
    ();
  ()


let test_get_define_body context =
  let assert_updates = assert_updates ~context in
  let dependency = type_check_dependency in
  let open Statement in
  let open Expression in
  let create_simple_return ~start ~stop expression =
    node
      ~start
      ~stop
      (Statement.Return { Return.is_implicit = false; expression = Some expression })
  in
  let create_simple_define ~start ~stop name body =
    node
      ~start
      ~stop
      {
        Define.signature =
          {
            Define.Signature.name;
            parameters = [];
            decorators = [];
            return_annotation = None;
            async = false;
            generator = false;
            parent = None;
            nesting_define = None;
          };
        captures = [];
        unbound_names = [];
        body;
      }
  in
  (* Body doesn't change *)
  assert_updates
    ~original_source:{|
      def foo():
        return 1
    |}
    ~new_source:{|
      def foo():
        return 1
    |}
    ~middle_actions:
      [
        `DefineBody
          ( !&"test.foo",
            dependency,
            Some
              (create_simple_define
                 ~start:(2, 0)
                 ~stop:(3, 10)
                 !&"test.foo"
                 [
                   create_simple_return
                     ~start:(3, 2)
                     ~stop:(3, 10)
                     (node ~start:(3, 9) ~stop:(3, 10) (Expression.Constant (Constant.Integer 1)));
                 ]) );
      ]
    ~expected_triggers:[]
    ~post_actions:
      [
        `DefineBody
          ( !&"test.foo",
            dependency,
            Some
              (create_simple_define
                 ~start:(2, 0)
                 ~stop:(3, 10)
                 !&"test.foo"
                 [
                   create_simple_return
                     ~start:(3, 2)
                     ~stop:(3, 10)
                     (node ~start:(3, 9) ~stop:(3, 10) (Expression.Constant (Constant.Integer 1)));
                 ]) );
      ]
    ();

  (* Body changes *)
  assert_updates
    ~original_source:{|
      def foo():
        return 1
    |}
    ~new_source:{|
      def foo():
        return 2
    |}
    ~middle_actions:
      [
        `DefineBody
          ( !&"test.foo",
            dependency,
            Some
              (create_simple_define
                 ~start:(2, 0)
                 ~stop:(3, 10)
                 !&"test.foo"
                 [
                   create_simple_return
                     ~start:(3, 2)
                     ~stop:(3, 10)
                     (node ~start:(3, 9) ~stop:(3, 10) (Expression.Constant (Constant.Integer 1)));
                 ]) );
      ]
    ~expected_triggers:[dependency]
    ~post_actions:
      [
        `DefineBody
          ( !&"test.foo",
            dependency,
            Some
              (create_simple_define
                 ~start:(2, 0)
                 ~stop:(3, 10)
                 !&"test.foo"
                 [
                   create_simple_return
                     ~start:(3, 2)
                     ~stop:(3, 10)
                     (node ~start:(3, 9) ~stop:(3, 10) (Expression.Constant (Constant.Integer 2)));
                 ]) );
      ]
    ();

  assert_updates
    ~original_source:{|
      def foo():
        return 1
    |}
    ~new_source:{|
      def foo():
        return 2
      def foo():
        return 3
    |}
    ~middle_actions:
      [
        `DefineBody
          ( !&"test.foo",
            dependency,
            Some
              (create_simple_define
                 ~start:(2, 0)
                 ~stop:(3, 10)
                 !&"test.foo"
                 [
                   create_simple_return
                     ~start:(3, 2)
                     ~stop:(3, 10)
                     (node ~start:(3, 9) ~stop:(3, 10) (Expression.Constant (Constant.Integer 1)));
                 ]) );
      ]
    ~expected_triggers:[dependency]
    ~post_actions:
      [
        (* Last define wins *)
        `DefineBody
          ( !&"test.foo",
            dependency,
            Some
              (create_simple_define
                 ~start:(4, 0)
                 ~stop:(5, 10)
                 !&"test.foo"
                 [
                   create_simple_return
                     ~start:(5, 2)
                     ~stop:(5, 10)
                     (node ~start:(5, 9) ~stop:(5, 10) (Expression.Constant (Constant.Integer 3)));
                 ]) );
      ]
    ();
  assert_updates
    ~original_source:{|
      def foo():
        return 1
      def foo():
        return 2
    |}
    ~new_source:{|
      def foo():
        return 3
    |}
    ~middle_actions:
      [
        (* Last define wins *)
        `DefineBody
          ( !&"test.foo",
            dependency,
            Some
              (create_simple_define
                 ~start:(4, 0)
                 ~stop:(5, 10)
                 !&"test.foo"
                 [
                   create_simple_return
                     ~start:(5, 2)
                     ~stop:(5, 10)
                     (node ~start:(5, 9) ~stop:(5, 10) (Expression.Constant (Constant.Integer 2)));
                 ]) );
      ]
    ~expected_triggers:[dependency]
    ~post_actions:
      [
        `DefineBody
          ( !&"test.foo",
            dependency,
            Some
              (create_simple_define
                 ~start:(2, 0)
                 ~stop:(3, 10)
                 !&"test.foo"
                 [
                   create_simple_return
                     ~start:(3, 2)
                     ~stop:(3, 10)
                     (node ~start:(3, 9) ~stop:(3, 10) (Expression.Constant (Constant.Integer 3)));
                 ]) );
      ]
    ();

  assert_updates
    ~original_source:
      {|
      from typing import overload
      @overload
      def foo(x: int) -> int: ...
      @overload
      def foo(x: str) -> str: ...
      def foo(x):
        return x
    |}
    ~new_source:
      {|
      from typing import overload
      @overload
      def foo(x: str) -> str: ...
      def foo(x):
        return x
      @overload
      def foo(x: int) -> int: ...
    |}
    ~middle_actions:
      [
        (let body =
           node
             ~start:(7, 0)
             ~stop:(8, 10)
             {
               Define.signature =
                 {
                   Define.Signature.name = !&"test.foo";
                   parameters =
                     [
                       node
                         ~start:(7, 8)
                         ~stop:(7, 9)
                         { Parameter.name = "$parameter$x"; value = None; annotation = None };
                     ];
                   decorators = [];
                   return_annotation = None;
                   async = false;
                   generator = false;
                   parent = None;
                   nesting_define = None;
                 };
               captures = [];
               unbound_names = [];
               body =
                 [
                   create_simple_return
                     ~start:(8, 2)
                     ~stop:(8, 10)
                     (node
                        ~start:(8, 9)
                        ~stop:(8, 10)
                        (Expression.Name (Name.Identifier "$parameter$x")));
                 ];
             }
         in
         `DefineBody (!&"test.foo", dependency, Some body));
      ]
    ~expected_triggers:[dependency]
    ~post_actions:
      [
        (let body =
           node
             ~start:(5, 0)
             ~stop:(6, 10)
             {
               Define.signature =
                 {
                   Define.Signature.name = !&"test.foo";
                   parameters =
                     [
                       node
                         ~start:(5, 8)
                         ~stop:(5, 9)
                         { Parameter.name = "$parameter$x"; value = None; annotation = None };
                     ];
                   decorators = [];
                   return_annotation = None;
                   async = false;
                   generator = false;
                   parent = None;
                   nesting_define = None;
                 };
               captures = [];
               unbound_names = [];
               body =
                 [
                   create_simple_return
                     ~start:(6, 2)
                     ~stop:(6, 10)
                     (node
                        ~start:(6, 9)
                        ~stop:(6, 10)
                        (Expression.Name (Name.Identifier "$parameter$x")));
                 ];
             }
         in
         `DefineBody (!&"test.foo", dependency, Some body));
      ]
    ();
  let () =
    let body =
      node
        ~start:(5, 0)
        ~stop:(6, 10)
        {
          Define.signature =
            {
              Define.Signature.name = !&"test.foo";
              parameters =
                [
                  node
                    ~start:(5, 8)
                    ~stop:(5, 9)
                    { Parameter.name = "$parameter$x"; value = None; annotation = None };
                ];
              decorators = [];
              return_annotation = None;
              async = false;
              generator = false;
              parent = None;
              nesting_define = None;
            };
          captures = [];
          unbound_names = [];
          body =
            [
              create_simple_return
                ~start:(6, 2)
                ~stop:(6, 10)
                (node
                   ~start:(6, 9)
                   ~stop:(6, 10)
                   (Expression.Name (Name.Identifier "$parameter$x")));
            ];
        }
    in
    let first_overload =
      node
        ~start:(4, 0)
        ~stop:(4, 27)
        {
          Define.signature =
            {
              Define.Signature.name = !&"test.foo";
              parameters =
                [
                  node
                    ~start:(4, 8)
                    ~stop:(4, 14)
                    {
                      Parameter.name = "$parameter$x";
                      value = None;
                      annotation =
                        Some
                          (node
                             ~start:(4, 11)
                             ~stop:(4, 14)
                             (Expression.Name (Name.Identifier "int")));
                    };
                ];
              decorators =
                [
                  node
                    ~start:(3, 1)
                    ~stop:(3, 9)
                    (Expression.Name
                       (Name.Attribute
                          {
                            Name.Attribute.base =
                              node
                                ~start:(3, 1)
                                ~stop:(3, 9)
                                (Expression.Name (Name.Identifier "typing"));
                            attribute = "overload";
                            special = false;
                          }));
                ];
              return_annotation =
                Some (node ~start:(4, 19) ~stop:(4, 22) (Expression.Name (Name.Identifier "int")));
              async = false;
              generator = false;
              parent = None;
              nesting_define = None;
            };
          captures = [];
          unbound_names = [];
          body =
            [
              node
                ~start:(4, 24)
                ~stop:(4, 27)
                (Statement.Expression
                   (node ~start:(4, 24) ~stop:(4, 27) (Expression.Constant Constant.Ellipsis)));
            ];
        }
    in
    let second_overload =
      node
        ~start:(8, 0)
        ~stop:(8, 27)
        {
          Define.signature =
            {
              Define.Signature.name = !&"test.foo";
              parameters =
                [
                  node
                    ~start:(8, 8)
                    ~stop:(8, 14)
                    {
                      Parameter.name = "$parameter$x";
                      value = None;
                      annotation =
                        Some
                          (node
                             ~start:(8, 11)
                             ~stop:(8, 14)
                             (Expression.Name (Name.Identifier "str")));
                    };
                ];
              decorators =
                [
                  node
                    ~start:(7, 1)
                    ~stop:(7, 9)
                    (Expression.Name
                       (Name.Attribute
                          {
                            Name.Attribute.base =
                              node
                                ~start:(7, 1)
                                ~stop:(7, 9)
                                (Expression.Name (Name.Identifier "typing"));
                            attribute = "overload";
                            special = false;
                          }));
                ];
              return_annotation =
                Some (node ~start:(8, 19) ~stop:(8, 22) (Expression.Name (Name.Identifier "str")));
              async = false;
              generator = false;
              parent = None;
              nesting_define = None;
            };
          captures = [];
          unbound_names = [];
          body =
            [
              node
                ~start:(8, 24)
                ~stop:(8, 27)
                (Statement.Expression
                   (node ~start:(8, 24) ~stop:(8, 27) (Expression.Constant Constant.Ellipsis)));
            ];
        }
    in
    assert_updates
      ~original_source:
        {|
      from typing import overload
      @overload
      def foo(x: int) -> int: ...
      def foo(x):
        return x
    |}
      ~new_source:
        {|
      from typing import overload
      @overload
      def foo(x: int) -> int: ...
      def foo(x):
        return x
      @overload
      def foo(x: str) -> str: ...
    |}
      ~middle_actions:
        [
          (let definition =
             let open FunctionDefinition in
             let siblings = [{ Sibling.kind = Sibling.Kind.Overload; body = first_overload }] in
             { body = Some body; siblings; qualifier = !&"test" }
           in
           `Define (!&"test.foo", dependency, Some definition));
        ]
      ~expected_triggers:[dependency]
      ~post_actions:
        [
          (let definition =
             let open FunctionDefinition in
             let siblings =
               [
                 { Sibling.kind = Sibling.Kind.Overload; body = first_overload };
                 { Sibling.kind = Sibling.Kind.Overload; body = second_overload };
               ]
             in
             { body = Some body; siblings; qualifier = !&"test" }
           in
           `Define (!&"test.foo", dependency, Some definition));
        ]
      ()
  in

  assert_updates
    ~original_source:{|
      class A:
        foo: int
    |}
    ~new_source:
      {|
      class A:
        @property
        def foo(self) -> int: ...
        @foo.setter
        def foo(self, value: int) -> None: ...
    |}
    ~middle_actions:[`Define (!&"test.A.foo", dependency, None)]
    ~expected_triggers:[dependency]
    ~post_actions:
      [
        (let definition =
           let open FunctionDefinition in
           let create_elipsis ~start ~stop () =
             node
               ~start
               ~stop
               (Statement.Expression (node ~start ~stop (Expression.Constant Constant.Ellipsis)))
           in
           let body =
             node
               ~start:(4, 2)
               ~stop:(4, 27)
               {
                 Define.signature =
                   {
                     Define.Signature.name = !&"test.A.foo";
                     parameters =
                       [
                         node
                           ~start:(4, 10)
                           ~stop:(4, 14)
                           { Parameter.name = "$parameter$self"; value = None; annotation = None };
                       ];
                     decorators =
                       [
                         node
                           ~start:(3, 3)
                           ~stop:(3, 11)
                           (Expression.Name (Name.Identifier "property"));
                       ];
                     return_annotation =
                       Some
                         (node
                            ~start:(4, 19)
                            ~stop:(4, 22)
                            (Expression.Name (Name.Identifier "int")));
                     async = false;
                     generator = false;
                     parent = Some !&"test.A";
                     nesting_define = None;
                   };
                 captures = [];
                 unbound_names = [];
                 body = [create_elipsis ~start:(4, 24) ~stop:(4, 27) ()];
               }
           in
           let siblings =
             [
               (let body =
                  node
                    ~start:(6, 2)
                    ~stop:(6, 40)
                    {
                      Define.signature =
                        {
                          Define.Signature.name = !&"test.A.foo";
                          parameters =
                            [
                              node
                                ~start:(6, 10)
                                ~stop:(6, 14)
                                {
                                  Parameter.name = "$parameter$self";
                                  value = None;
                                  annotation = None;
                                };
                              node
                                ~start:(6, 16)
                                ~stop:(6, 26)
                                {
                                  Parameter.name = "$parameter$value";
                                  value = None;
                                  annotation =
                                    Some
                                      (node
                                         ~start:(6, 23)
                                         ~stop:(6, 26)
                                         (Expression.Name (Name.Identifier "int")));
                                };
                            ];
                          decorators =
                            [
                              node
                                ~start:(5, 3)
                                ~stop:(5, 13)
                                (Expression.Name
                                   (Name.Attribute
                                      {
                                        Name.Attribute.base =
                                          node
                                            ~start:(5, 3)
                                            ~stop:(5, 6)
                                            (Expression.Name (Name.Identifier "foo"));
                                        attribute = "setter";
                                        special = false;
                                      }));
                            ];
                          return_annotation =
                            Some
                              (node
                                 ~start:(6, 31)
                                 ~stop:(6, 35)
                                 (Expression.Constant Constant.NoneLiteral));
                          async = false;
                          generator = false;
                          parent = Some !&"test.A";
                          nesting_define = None;
                        };
                      captures = [];
                      unbound_names = [];
                      body = [create_elipsis ~start:(6, 37) ~stop:(6, 40) ()];
                    }
                in
                { Sibling.kind = Sibling.Kind.PropertySetter; body });
             ]
           in
           { body = Some body; siblings; qualifier = !&"test" }
         in
         `Define (!&"test.A.foo", dependency, Some definition));
      ]
    ();

  (* Location-only change *)
  assert_updates
    ~original_source:{|
      def foo():
        return 1
    |}
    ~new_source:
      {|
      # The truth is, the game was rigged from the start.
      def foo():
          return 1
    |}
    ~middle_actions:
      [
        `DefineBody
          ( !&"test.foo",
            dependency,
            Some
              (create_simple_define
                 ~start:(2, 0)
                 ~stop:(3, 10)
                 !&"test.foo"
                 [
                   create_simple_return
                     ~start:(3, 2)
                     ~stop:(3, 10)
                     (node ~start:(3, 9) ~stop:(3, 10) (Expression.Constant (Constant.Integer 1)));
                 ]) );
      ]
    ~expected_triggers:[dependency]
    ~post_actions:
      [
        `DefineBody
          ( !&"test.foo",
            dependency,
            Some
              (create_simple_define
                 ~start:(3, 0)
                 ~stop:(4, 12)
                 !&"test.foo"
                 [
                   create_simple_return
                     ~start:(4, 4)
                     ~stop:(4, 12)
                     (node ~start:(4, 11) ~stop:(4, 12) (Expression.Constant (Constant.Integer 1)));
                 ]) );
      ]
    ();

  (* Added define *)
  assert_updates
    ~original_source:{|
    |}
    ~new_source:{|
      def foo():
        return 2
    |}
    ~middle_actions:[`DefineBody (!&"test.foo", dependency, None)]
    ~expected_triggers:[dependency]
    ~post_actions:
      [
        `DefineBody
          ( !&"test.foo",
            dependency,
            Some
              (create_simple_define
                 ~start:(2, 0)
                 ~stop:(3, 10)
                 !&"test.foo"
                 [
                   create_simple_return
                     ~start:(3, 2)
                     ~stop:(3, 10)
                     (node ~start:(3, 9) ~stop:(3, 10) (Expression.Constant (Constant.Integer 2)));
                 ]) );
      ]
    ();

  (* Removed define *)
  assert_updates
    ~original_source:{|
      def foo():
        return 1
    |}
    ~new_source:{|
    |}
    ~middle_actions:
      [
        `DefineBody
          ( !&"test.foo",
            dependency,
            Some
              (create_simple_define
                 ~start:(2, 0)
                 ~stop:(3, 10)
                 !&"test.foo"
                 [
                   create_simple_return
                     ~start:(3, 2)
                     ~stop:(3, 10)
                     (node ~start:(3, 9) ~stop:(3, 10) (Expression.Constant (Constant.Integer 1)));
                 ]) );
      ]
    ~expected_triggers:[dependency]
    ~post_actions:[`DefineBody (!&"test.foo", dependency, None)]
    ();
  ()


let () =
  "environment"
  >::: [
         (* Simple tests *)
         "global_registration" >:: test_global_registration;
         "define_registration" >:: test_define_registration;
         "simple_globals" >:: test_simple_global_registration;
         "builtins" >:: test_builtin_modules;
         "resolve_exports" >:: test_resolve_exports;
         (* Update tests *)
         "get_class_summary" >:: test_get_class_summary;
         "class_exists_and_all_classes" >:: test_class_exists_and_all_classes;
         "get_unannotated_global" >:: test_get_unannotated_global;
         "dependencies_and_new_values" >:: test_dependencies_and_new_values;
         "get_define_body" >:: test_get_define_body;
       ]
  |> Test.run
