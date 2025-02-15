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
    let read_only =
      ScratchProject.errors_environment project
      |> ErrorsEnvironment.Testing.ReadOnly.unannotated_global_environment
    in
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


let test_simple_global_registration context =
  let assert_registers source name expected =
    let project = ScratchProject.setup ["test.py", source] ~context in
    let read_only =
      ScratchProject.errors_environment project
      |> ErrorsEnvironment.Testing.ReadOnly.unannotated_global_environment
    in
    let printer global =
      global
      >>| Module.UnannotatedGlobal.sexp_of_t
      >>| Sexp.to_string_hum
      |> Option.value ~default:"None"
    in
    let location_insensitive_compare left right =
      Option.compare Module.UnannotatedGlobal.compare left right = 0
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
    (Some (SimpleAssign { explicit_annotation = None; value = Some value; target_location }));
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
              Some
                (create_with_location
                   (Expression.Expression.Constant (Expression.Constant.Integer 8))
                   (3, 8)
                   (3, 9));
            target_location =
              { Location.start = { line = 3; column = 2 }; stop = { line = 3; column = 5 } }
              |> Location.with_module ~module_reference:(Reference.create "test");
          }));
  let parse_define define =
    match parse_single_statement define ~preprocess:true ~handle:"test.py" with
    | { Node.value = Statement.Statement.Define { signature; _ }; location } ->
        {
          Module.UnannotatedGlobal.signature;
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
  let read_only =
    let sources = ["builtins.py", "foo: int = 42"] in
    let project =
      ScratchProject.setup
        ~context
        ~include_typeshed_stubs:false
        ~include_helper_builtins:false
        sources
    in
    ScratchProject.errors_environment project
    |> ErrorsEnvironment.Testing.ReadOnly.unannotated_global_environment
  in
  assert_bool
    "empty qualifier module exists"
    (UnannotatedGlobalEnvironment.ReadOnly.module_exists read_only Reference.empty);
  assert_bool
    "random qualifier doesn't exist"
    (not (UnannotatedGlobalEnvironment.ReadOnly.module_exists read_only !&"derp"));
  assert_bool
    "`builtins` exists"
    (UnannotatedGlobalEnvironment.ReadOnly.module_exists read_only !&"builtins");
  assert_bool
    "`future.builtins` exists"
    (UnannotatedGlobalEnvironment.ReadOnly.module_exists read_only !&"future.builtins");

  let assert_nonempty qualifier =
    match UnannotatedGlobalEnvironment.ReadOnly.get_module_metadata read_only qualifier with
    | None -> assert_failure "Module does not exist"
    | Some metadata ->
        assert_bool "implicit module not expected" (not (Module.Metadata.is_implicit metadata));
        assert_equal
          ~ctxt:context
          ~cmp:[%compare.equal: Module.Export.t option]
          ~printer:(fun export -> Sexp.to_string_hum [%message (export : Module.Export.t option)])
          (Some Module.Export.(Name GlobalVariable))
          (Module.Metadata.get_export metadata "foo")
  in
  assert_nonempty Reference.empty;
  assert_nonempty !&"builtins";
  assert_nonempty !&"future.builtins";
  ()


let test_resolve_exports context =
  let open UnannotatedGlobalEnvironment in
  let assert_resolved ?(include_typeshed = false) ~expected ?from ~reference sources =
    Memory.reset_shared_memory ();
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
    let read_only =
      ScratchProject.errors_environment project
      |> ErrorsEnvironment.Testing.ReadOnly.unannotated_global_environment
    in
    let actual = ReadOnly.resolve_exports read_only ?from reference in
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

  let open Module in
  assert_resolved [] ~reference:!&"derp" ~expected:None;
  assert_resolved ["derp.py", ""] ~reference:!&"derp" ~expected:(Some (resolved_module !&"derp"));
  assert_resolved ["derp.py", ""] ~reference:!&"derp.foo" ~expected:None;
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


(* Create a convenient marker type for sources in update tests *)
module AssertUpdateSource = struct
  type t =
    | Local of string * string
    | External of string * string

  (* This is just here to suppress an unused constructor error *)
  let _ = External ("", "")
end

let assert_update
    ~original_sources
    ~new_sources
    ~middle_queries_with_expectations
    ~expected_triggers_except_uge_and_fde
    ?post_queries_with_expectations
    context
  =
  Memory.reset_shared_memory ();
  let project =
    let original_local_sources, original_external_sources =
      let partition = function
        | AssertUpdateSource.Local (relative, code) -> Either.First (relative, code)
        | AssertUpdateSource.External (relative, code) -> Either.Second (relative, code)
      in
      List.partition_map ~f:partition original_sources
    in
    ScratchProject.setup
      ~include_typeshed_stubs:false
      ~track_dependencies:true
      ~in_memory:false
      original_local_sources
      ~external_sources:original_external_sources
      ~context
  in
  let global_module_paths_api = ScratchProject.global_module_paths_api project in
  let read_only =
    ScratchProject.errors_environment project
    |> ErrorsEnvironment.Testing.ReadOnly.unannotated_global_environment
  in
  let execute_action = function
    | `ModuleMetadata (qualifier, dependency, expected) ->
        let expected =
          match expected with
          | `None -> "None"
          | `Explicit -> "Explicit"
          | `Implicit -> "Implicit"
        in
        let actual =
          match
            UnannotatedGlobalEnvironment.ReadOnly.get_module_metadata
              read_only
              ~dependency
              qualifier
          with
          | None -> "None"
          | Some module_metadata ->
              if Module.Metadata.is_implicit module_metadata then "Implicit" else "Explicit"
        in
        assert_equal ~ctxt:context actual expected ~printer:Fn.id
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
        let _force_lazy_loading_of_module_ =
          UnannotatedGlobalEnvironment.ReadOnly.get_module_metadata
            read_only
            (Reference.create "test")
        in
        UnannotatedGlobalEnvironment.ReadOnly.GlobalApis.all_classes
          read_only
          ~scheduler:(Test.mock_scheduler ())
          ~global_module_paths_api
        |> assert_equal ~printer:(List.to_string ~f:Fn.id) expectation
    | `Global (global_name, dependency, expectation) ->
        let printer optional =
          optional
          >>| Module.UnannotatedGlobal.sexp_of_t
          >>| Sexp.to_string_hum
          |> Option.value ~default:"none"
        in
        let cmp left right = Option.compare Module.UnannotatedGlobal.compare left right = 0 in
        let remove_target_location = function
          | Module.UnannotatedGlobal.SimpleAssign assign ->
              Module.UnannotatedGlobal.SimpleAssign
                { assign with target_location = Location.WithModule.any }
          | Module.UnannotatedGlobal.TupleAssign assign ->
              Module.UnannotatedGlobal.TupleAssign
                { assign with target_location = Location.WithModule.any }
          | global -> global
        in
        UnannotatedGlobalEnvironment.ReadOnly.get_unannotated_global
          read_only
          global_name
          ~dependency
        >>| remove_target_location
        |> assert_equal ~cmp ~printer expectation
    | `GetRawSource (qualifier, dependency) ->
        SourceCodeApi.parse_result_of_qualifier
          (UnannotatedGlobalEnvironment.ReadOnly.get_tracked_source_code_api read_only ~dependency)
          qualifier
        |> ignore
  in
  List.iter middle_queries_with_expectations ~f:execute_action;
  let remove_source = function
    | AssertUpdateSource.Local (relative, _) ->
        ScratchProject.delete_from_local_root project ~relative
    | AssertUpdateSource.External (relative, _) ->
        ScratchProject.delete_from_external_root project ~relative
  in
  List.iter original_sources ~f:remove_source;
  let add_source = function
    | AssertUpdateSource.Local (relative, code) ->
        ScratchProject.add_to_local_root project ~relative code
    | AssertUpdateSource.External (relative, code) ->
        ScratchProject.add_to_external_root project ~relative code
  in
  List.iter new_sources ~f:add_source;
  let events =
    let local_root = ScratchProject.local_root_of project in
    let external_root = ScratchProject.external_root_of project in
    let to_event assert_update_source =
      let root, relative =
        match assert_update_source with
        | AssertUpdateSource.Local (relative, _) -> local_root, relative
        | AssertUpdateSource.External (relative, _) -> external_root, relative
      in
      Test.relative_artifact_path ~root ~relative |> ArtifactPath.Event.(create ~kind:Kind.Unknown)
    in
    List.concat [original_sources; new_sources] |> List.map ~f:to_event
  in
  let update_result =
    ScratchProject.update_environment project events
    |> ErrorsEnvironment.Testing.UpdateResult.unannotated_global_environment
  in
  let printer set =
    SharedMemoryKeys.DependencyKey.RegisteredSet.elements set
    |> List.map ~f:SharedMemoryKeys.DependencyKey.get_key
    |> List.to_string ~f:SharedMemoryKeys.show_dependency
  in
  let expected_trigger_set_except_uge_and_fde =
    SharedMemoryKeys.DependencyKey.RegisteredSet.of_list expected_triggers_except_uge_and_fde
  in
  let actual_trigger_set_except_uge_and_fde =
    let fold_trigger_set sofar triggered =
      let fold_one registered sofar =
        match SharedMemoryKeys.DependencyKey.get_key registered with
        | ComputeModuleComponents _
        | FunctionDefinitions _ ->
            sofar
        | _ -> SharedMemoryKeys.DependencyKey.RegisteredSet.add registered sofar
      in
      SharedMemoryKeys.DependencyKey.RegisteredSet.fold fold_one triggered sofar
    in
    List.fold
      ~init:SharedMemoryKeys.DependencyKey.RegisteredSet.empty
      ~f:fold_trigger_set
      (UnannotatedGlobalEnvironment.UpdateResult.all_triggered_dependencies update_result)
  in
  post_queries_with_expectations >>| List.iter ~f:execute_action |> Option.value ~default:();
  assert_equal
    ~cmp:SharedMemoryKeys.DependencyKey.RegisteredSet.equal
    ~printer
    expected_trigger_set_except_uge_and_fde
    actual_trigger_set_except_uge_and_fde


let type_check_dependency =
  SharedMemoryKeys.DependencyKey.Registry.register (TypeCheckDefine (Reference.create "test"))


let alias_dependency =
  SharedMemoryKeys.DependencyKey.Registry.register (TypeCheckDefine (Reference.create "dep"))


let test_get_explicit_module_metadata =
  let dependency = type_check_dependency in
  (* No actual change *)
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_update
           ~original_sources:[AssertUpdateSource.Local ("a.py", "x: int = 1")]
           ~middle_queries_with_expectations:[`ModuleMetadata (!&"a", dependency, `Explicit)]
           ~new_sources:[AssertUpdateSource.Local ("a.py", "x: int = 1")]
           ~expected_triggers_except_uge_and_fde:[]
           ~post_queries_with_expectations:[`ModuleMetadata (!&"a", dependency, `Explicit)];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_update
           ~original_sources:[AssertUpdateSource.External ("a.py", "x: int = 1")]
           ~middle_queries_with_expectations:[`ModuleMetadata (!&"a", dependency, `Explicit)]
           ~new_sources:[AssertUpdateSource.External ("a.py", "x: int = 1")]
           ~expected_triggers_except_uge_and_fde:[]
           ~post_queries_with_expectations:[`ModuleMetadata (!&"a", dependency, `Explicit)];
      (* Updated code *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_update
           ~original_sources:[AssertUpdateSource.Local ("a.py", "x: int = 1")]
           ~middle_queries_with_expectations:[`ModuleMetadata (!&"a", dependency, `Explicit)]
           ~new_sources:[AssertUpdateSource.Local ("a.py", "a: int = 2")]
           ~expected_triggers_except_uge_and_fde:[dependency]
           ~post_queries_with_expectations:[`ModuleMetadata (!&"a", dependency, `Explicit)];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_update
           ~original_sources:[AssertUpdateSource.External ("a.py", "x: int = 1")]
           ~middle_queries_with_expectations:[`ModuleMetadata (!&"a", dependency, `Explicit)]
           ~new_sources:[AssertUpdateSource.External ("a.py", "a: int = 2")]
           ~expected_triggers_except_uge_and_fde:[dependency]
           ~post_queries_with_expectations:[`ModuleMetadata (!&"a", dependency, `Explicit)];
      (* Deleted code *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_update
           ~original_sources:[AssertUpdateSource.Local ("a.py", "x: int = 1")]
           ~middle_queries_with_expectations:[`ModuleMetadata (!&"a", dependency, `Explicit)]
           ~new_sources:[]
           ~expected_triggers_except_uge_and_fde:[dependency]
           ~post_queries_with_expectations:[`ModuleMetadata (!&"a", dependency, `None)];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_update
           ~original_sources:[AssertUpdateSource.External ("a.py", "x: int = 1")]
           ~middle_queries_with_expectations:[`ModuleMetadata (!&"a", dependency, `Explicit)]
           ~new_sources:[]
           ~expected_triggers_except_uge_and_fde:[dependency]
           ~post_queries_with_expectations:[`ModuleMetadata (!&"a", dependency, `None)];
      (* Added code *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_update
           ~original_sources:[]
           ~middle_queries_with_expectations:[`ModuleMetadata (!&"a", dependency, `None)]
           ~new_sources:[AssertUpdateSource.Local ("a.py", "x: int = 1")]
           ~expected_triggers_except_uge_and_fde:[dependency]
           ~post_queries_with_expectations:[`ModuleMetadata (!&"a", dependency, `Explicit)];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_update
           ~original_sources:[]
           ~middle_queries_with_expectations:[`ModuleMetadata (!&"a", dependency, `None)]
           ~new_sources:[AssertUpdateSource.External ("a.py", "x: int = 1")]
           ~expected_triggers_except_uge_and_fde:[dependency]
           ~post_queries_with_expectations:[`ModuleMetadata (!&"a", dependency, `Explicit)];
    ]


let test_get_implicit_module_metadata =
  let dependency = type_check_dependency in
  test_list
    [
      (* Changed nested module -> no change to implicit *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_update
           ~original_sources:[AssertUpdateSource.Local ("a/b.py", "x: int = 1")]
           ~new_sources:[AssertUpdateSource.Local ("a/b.py", "x: int = 2")]
           ~middle_queries_with_expectations:[`ModuleMetadata (!&"a", dependency, `Implicit)]
           ~expected_triggers_except_uge_and_fde:[]
           ~post_queries_with_expectations:[`ModuleMetadata (!&"a", dependency, `Implicit)];
      (* Deleted nested module -> deleted implicit *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_update
           ~original_sources:[AssertUpdateSource.Local ("a/b.py", "x: int = 1")]
           ~new_sources:[]
           ~middle_queries_with_expectations:[`ModuleMetadata (!&"a", dependency, `Implicit)]
           ~expected_triggers_except_uge_and_fde:[dependency]
           ~post_queries_with_expectations:[`ModuleMetadata (!&"a", dependency, `None)];
      (* Added nested module -> added implicit *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_update
           ~original_sources:[]
           ~new_sources:[AssertUpdateSource.Local ("a/b.py", "x: int = 1")]
           ~middle_queries_with_expectations:[`ModuleMetadata (!&"a", dependency, `None)]
           ~expected_triggers_except_uge_and_fde:[dependency]
           ~post_queries_with_expectations:[`ModuleMetadata (!&"a", dependency, `Implicit)];
    ]


let test_get_class_summary =
  let dependency = type_check_dependency in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_update
           ~original_sources:
             [AssertUpdateSource.Local ("test.py", {|
        class Foo:
          x: int
      |})]
           ~new_sources:
             [AssertUpdateSource.Local ("test.py", {|
        class Foo:
          x: str
      |})]
           ~middle_queries_with_expectations:[`Get ("test.Foo", dependency, Some 1)]
           ~expected_triggers_except_uge_and_fde:[dependency];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_update
           ~original_sources:
             [AssertUpdateSource.Local ("test.py", {|
        class Foo:
          x: int
      |})]
           ~new_sources:
             [AssertUpdateSource.Local ("test.py", {|
        class Foo:
          x: str
      |})]
           ~middle_queries_with_expectations:[`Get ("test.Missing", dependency, None)]
           ~expected_triggers_except_uge_and_fde:[dependency];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_update
           ~original_sources:
             [AssertUpdateSource.Local ("test.py", {|
        class Foo:
          x: int
      |})]
           ~new_sources:
             [
               AssertUpdateSource.Local
                 ( "test.py",
                   {|
              class Unrelated:
                x: int
              class Foo:
                x: int
            |}
                 );
             ]
           ~middle_queries_with_expectations:[`Get ("test.Foo", dependency, Some 1)]
           ~expected_triggers_except_uge_and_fde:[dependency];
      (* Last class definition wins *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_update
           ~original_sources:
             [
               AssertUpdateSource.Local
                 ( "test.py",
                   {|
              class Foo:
                x: int
              class Foo:
                x: int
                y: int
            |}
                 );
             ]
           ~new_sources:
             [
               AssertUpdateSource.Local
                 ( "test.py",
                   {|
              class Unrelated:
                x: int
              class Foo:
                x: int
            |}
                 );
             ]
           ~middle_queries_with_expectations:[`Get ("test.Foo", dependency, Some 2)]
           ~expected_triggers_except_uge_and_fde:[dependency]
           ~post_queries_with_expectations:[`Get ("test.Foo", dependency, Some 1)];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_update
           ~original_sources:
             [
               AssertUpdateSource.Local
                 ( "test.py",
                   {|
              class Foo:
                def method(self) -> None:
                  print("hello")
            |}
                 );
             ]
           ~new_sources:
             [
               AssertUpdateSource.Local
                 ( "test.py",
                   {|
              class Foo:
                def method(self) -> int:
                  return 1
            |}
                 );
             ]
           ~middle_queries_with_expectations:[`Get ("test.Foo", dependency, Some 1)]
           ~expected_triggers_except_uge_and_fde:[dependency]
           ~post_queries_with_expectations:[`Get ("test.Foo", dependency, Some 1)];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_update
           ~original_sources:
             [
               AssertUpdateSource.Local
                 ( "test.py",
                   {|
              class Foo:
                def method(self) -> None:
                  print("hellobo")
            |}
                 );
             ]
           ~new_sources:
             [
               AssertUpdateSource.Local
                 ( "test.py",
                   {|
              class Foo:
                def method(self) -> None:
                  print("goodbye")
            |}
                 );
             ]
           ~middle_queries_with_expectations:[`Get ("test.Foo", dependency, Some 1)]
           ~expected_triggers_except_uge_and_fde:[]
           ~post_queries_with_expectations:[`Get ("test.Foo", dependency, Some 1)];
    ]


let test_class_exists_and_all_classes =
  let dependency = type_check_dependency in
  test_list
    [
      (* class exists *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_update
           ~original_sources:[]
           ~middle_queries_with_expectations:[`Mem ("test.Foo", dependency, false)]
           ~new_sources:
             [AssertUpdateSource.Local ("test.py", {|
        class Foo:
          x: int
      |})]
           ~expected_triggers_except_uge_and_fde:[dependency];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_update
           ~original_sources:
             [AssertUpdateSource.Local ("test.py", {|
        class Foo:
          x: int
      |})]
           ~middle_queries_with_expectations:[`Mem ("test.Foo", dependency, true)]
           ~new_sources:[]
           ~expected_triggers_except_uge_and_fde:[dependency];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_update
           ~original_sources:
             [AssertUpdateSource.Local ("test.py", {|
        class Foo:
          x: int
      |})]
           ~new_sources:
             [AssertUpdateSource.Local ("test.py", {|
        class Foo:
          x: int
      |})]
           ~middle_queries_with_expectations:[`Mem ("test.Foo", dependency, true)]
           ~expected_triggers_except_uge_and_fde:[];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_update
           ~original_sources:
             [AssertUpdateSource.Local ("test.py", {|
        class Foo:
          x: int
      |})]
           ~new_sources:
             [AssertUpdateSource.Local ("test.py", {|
        class Foo:
          x: str
      |})]
           ~middle_queries_with_expectations:[`Mem ("test.Foo", dependency, true)]
           ~expected_triggers_except_uge_and_fde:[dependency];
      (* all_classes *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_update
           ~original_sources:
             [
               AssertUpdateSource.Local
                 ( "test.py",
                   {|
            class Foo:
              x: int
            class Bar:
              y: str
          |}
                 );
             ]
           ~new_sources:
             [AssertUpdateSource.Local ("test.py", {|
      class Foo:
        x: str
    |})]
           ~middle_queries_with_expectations:[`AllClasses ["test.Bar"; "test.Foo"]]
           ~expected_triggers_except_uge_and_fde:[]
           ~post_queries_with_expectations:[`AllClasses ["test.Foo"]];
    ]


let test_get_unannotated_global =
  let dependency = alias_dependency in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_update
           ~original_sources:[AssertUpdateSource.Local ("test.py", {|
        x: int = 7
      |})]
           ~new_sources:[AssertUpdateSource.Local ("test.py", {|
        x: int = 9
      |})]
           ~middle_queries_with_expectations:
             [
               `Global
                 ( Reference.create "test.x",
                   dependency,
                   Some
                     (Module.UnannotatedGlobal.SimpleAssign
                        {
                          explicit_annotation =
                            Some
                              {
                                (parse_single_expression "int") with
                                location = location (2, 3) (2, 6);
                              };
                          value =
                            Some
                              {
                                (parse_single_expression "7") with
                                location = location (2, 9) (2, 10);
                              };
                          target_location = Location.WithModule.any;
                        }) );
             ]
           ~expected_triggers_except_uge_and_fde:[dependency]
           ~post_queries_with_expectations:
             [
               `Global
                 ( Reference.create "test.x",
                   dependency,
                   Some
                     (Module.UnannotatedGlobal.SimpleAssign
                        {
                          explicit_annotation =
                            Some
                              {
                                (parse_single_expression "int") with
                                location = location (2, 3) (2, 6);
                              };
                          value =
                            Some
                              {
                                (parse_single_expression "9") with
                                location = location (2, 9) (2, 10);
                              };
                          target_location = Location.WithModule.any;
                        }) );
             ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_update
           ~original_sources:
             [
               AssertUpdateSource.Local
                 ("test.py", {|
        import target.member as alias
      |});
             ]
           ~new_sources:
             [
               AssertUpdateSource.Local
                 ("test.py", {|
        import target.member as new_alias
      |});
             ]
           ~middle_queries_with_expectations:
             [
               `Global
                 ( Reference.create "test.alias",
                   dependency,
                   Some
                     (Module.UnannotatedGlobal.Imported
                        (Module.UnannotatedGlobal.ImportModule
                           { target = !&"target.member"; implicit_alias = false })) );
             ]
           ~expected_triggers_except_uge_and_fde:[dependency]
           ~post_queries_with_expectations:
             [`Global (Reference.create "test.alias", dependency, None)];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_update
           ~original_sources:
             [
               AssertUpdateSource.Local
                 ("test.py", {|
        from target import member, other_member
      |});
             ]
           ~new_sources:
             [
               AssertUpdateSource.Local
                 ("test.py", {|
        from target import other_member, member
      |});
             ]
           ~middle_queries_with_expectations:
             [
               `Global
                 ( Reference.create "test.member",
                   dependency,
                   Some
                     (Module.UnannotatedGlobal.Imported
                        (Module.UnannotatedGlobal.ImportFrom
                           { from = !&"target"; target = "member"; implicit_alias = true })) );
               `Global
                 ( Reference.create "test.other_member",
                   dependency,
                   Some
                     (Module.UnannotatedGlobal.Imported
                        (Module.UnannotatedGlobal.ImportFrom
                           { from = !&"target"; target = "other_member"; implicit_alias = true }))
                 );
             ]
             (* Location insensitive *)
           ~expected_triggers_except_uge_and_fde:[]
           ~post_queries_with_expectations:
             [
               `Global
                 ( Reference.create "test.member",
                   dependency,
                   Some
                     (Module.UnannotatedGlobal.Imported
                        (Module.UnannotatedGlobal.ImportFrom
                           { from = !&"target"; target = "member"; implicit_alias = true })) );
               `Global
                 ( Reference.create "test.other_member",
                   dependency,
                   Some
                     (Module.UnannotatedGlobal.Imported
                        (Module.UnannotatedGlobal.ImportFrom
                           { from = !&"target"; target = "other_member"; implicit_alias = true }))
                 );
             ];
      (* Removing a source should trigger lookup dependencies *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_update
           ~original_sources:
             [AssertUpdateSource.Local ("test.py", {|
        from target import member
      |})]
           ~middle_queries_with_expectations:
             [
               `Global
                 ( Reference.create "test.member",
                   dependency,
                   Some
                     (Module.UnannotatedGlobal.Imported
                        (Module.UnannotatedGlobal.ImportFrom
                           { from = !&"target"; target = "member"; implicit_alias = true })) );
             ]
           ~new_sources:[]
           ~expected_triggers_except_uge_and_fde:[dependency]
           ~post_queries_with_expectations:
             [`Global (Reference.create "test.member", dependency, None)];
      (* Adding a source should trigger lookup dependencies *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_update
           ~original_sources:[]
           ~new_sources:
             [AssertUpdateSource.Local ("test.py", {|
        from target import member
      |})]
           ~middle_queries_with_expectations:
             [`Global (Reference.create "test.member", dependency, None)]
           ~expected_triggers_except_uge_and_fde:[dependency]
           ~post_queries_with_expectations:
             [
               `Global
                 ( Reference.create "test.member",
                   dependency,
                   Some
                     (Module.UnannotatedGlobal.Imported
                        (Module.UnannotatedGlobal.ImportFrom
                           { from = !&"target"; target = "member"; implicit_alias = true })) );
             ];
      (* Don't infer * as a real thing *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_update
           ~original_sources:
             [AssertUpdateSource.Local ("test.py", {|
      from target import *
    |})]
           ~middle_queries_with_expectations:[`Global (Reference.create "test.*", dependency, None)]
           ~new_sources:[]
           ~expected_triggers_except_uge_and_fde:[dependency];
      (let open Expression in
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_update
           ~original_sources:
             [AssertUpdateSource.Local ("test.py", {|
        X, Y, Z = int, str, bool
      |})]
           ~middle_queries_with_expectations:
             [
               `Global
                 ( Reference.create "test.X",
                   dependency,
                   Some
                     (Module.UnannotatedGlobal.TupleAssign
                        {
                          value = Some tuple_expression;
                          index = 0;
                          target_location = Location.WithModule.any;
                          total_length = 3;
                        }) );
               `Global
                 ( Reference.create "test.Y",
                   dependency,
                   Some
                     (Module.UnannotatedGlobal.TupleAssign
                        {
                          value = Some tuple_expression;
                          index = 1;
                          target_location = Location.WithModule.any;
                          total_length = 3;
                        }) );
               `Global
                 ( Reference.create "test.Z",
                   dependency,
                   Some
                     (Module.UnannotatedGlobal.TupleAssign
                        {
                          value = Some tuple_expression;
                          index = 2;
                          target_location = Location.WithModule.any;
                          total_length = 3;
                        }) );
             ]
           ~new_sources:[]
           ~expected_triggers_except_uge_and_fde:[dependency]);
      (* First global wins. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_update
           ~original_sources:
             [AssertUpdateSource.Local ("test.py", {|
        X = int
        X = str
      |})]
           ~new_sources:
             [AssertUpdateSource.Local ("test.py", {|
        X = int
        X = str
      |})]
           ~middle_queries_with_expectations:
             [
               `Global
                 ( Reference.create "test.X",
                   dependency,
                   Some
                     (Module.UnannotatedGlobal.SimpleAssign
                        {
                          explicit_annotation = None;
                          value =
                            Some
                              {
                                (parse_single_expression "int") with
                                location = location (2, 4) (2, 7);
                              };
                          target_location = Location.WithModule.any;
                        }) );
             ]
           ~expected_triggers_except_uge_and_fde:[];
      (* Only recurse into ifs *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_update
           ~original_sources:
             [
               AssertUpdateSource.Local
                 ( "test.py",
                   {|
            if condition:
              X = int
            else:
              X = str
          |}
                 );
             ]
           ~new_sources:
             [
               AssertUpdateSource.Local
                 ( "test.py",
                   {|
            if condition:
              X = int
            else:
              X = str
          |}
                 );
             ]
           ~middle_queries_with_expectations:
             [
               `Global
                 ( Reference.create "test.X",
                   dependency,
                   Some
                     (Module.UnannotatedGlobal.SimpleAssign
                        {
                          explicit_annotation = None;
                          value =
                            Some
                              {
                                (parse_single_expression "int") with
                                location = location (3, 6) (3, 9);
                              };
                          target_location = Location.WithModule.any;
                        }) );
             ]
           ~expected_triggers_except_uge_and_fde:[];
      (* get defines *)
      (let open Expression in
      let open Statement in
      let create_simple_signature
          ~start:(start_line, start_column)
          ~stop:(stop_line, stop_column)
          name
          return_annotation
        =
        {
          Module.UnannotatedGlobal.signature =
            {
              Define.Signature.name;
              parameters = [];
              decorators = [];
              return_annotation;
              async = false;
              generator = false;
              parent = NestingContext.create_toplevel ();
              legacy_parent = None;
              type_params = [];
            };
          location =
            {
              Location.WithModule.start = { Location.line = start_line; column = start_column };
              stop = { Location.line = stop_line; column = stop_column };
              module_reference = Reference.create "test";
            };
        }
      in

      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_update
           ~original_sources:
             [
               AssertUpdateSource.Local
                 ( "test.py",
                   {|
            def foo() -> None:
              print("hellobo")
          |} );
             ]
           ~new_sources:
             [
               AssertUpdateSource.Local
                 ( "test.py",
                   {|
            def foo() -> None:
              print("goodbye")
          |} );
             ]
           ~middle_queries_with_expectations:
             [
               `Global
                 ( Reference.create "test.foo",
                   dependency,
                   Some
                     (Module.UnannotatedGlobal.Define
                        [
                          create_simple_signature
                            ~start:(2, 0)
                            ~stop:(3, 18)
                            !&"test.foo"
                            (Some
                               (node
                                  ~start:(2, 13)
                                  ~stop:(2, 17)
                                  (Expression.Constant Constant.NoneLiteral)));
                        ]) );
             ]
           ~expected_triggers_except_uge_and_fde:[]
           ~post_queries_with_expectations:
             [
               `Global
                 ( Reference.create "test.foo",
                   dependency,
                   Some
                     (Module.UnannotatedGlobal.Define
                        [
                          create_simple_signature
                            ~start:(2, 0)
                            ~stop:(3, 18)
                            !&"test.foo"
                            (Some
                               (node
                                  ~start:(2, 13)
                                  ~stop:(2, 17)
                                  (Expression.Constant Constant.NoneLiteral)));
                        ]) );
             ]);
    ]


let test_dependencies_and_new_values =
  test_list
    [
      (* we should be able to keep different dependencies straight *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_update
           ~original_sources:
             [AssertUpdateSource.Local ("test.py", {|
        class Foo:
          x: int
      |})]
           ~new_sources:
             [
               AssertUpdateSource.Local
                 ("test.py", {|
        class Foo:2376
          x: str
      |});
             ]
           ~middle_queries_with_expectations:
             [
               `Get ("test.Foo", alias_dependency, Some 1);
               `Get ("test.Foo", type_check_dependency, Some 1);
             ]
           ~expected_triggers_except_uge_and_fde:[alias_dependency; type_check_dependency];
      (* Addition should add values when previously they were missing *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_update
           ~original_sources:[AssertUpdateSource.Local ("test.py", {|
    |})]
           ~new_sources:[AssertUpdateSource.Local ("test.py", {|
      x: int = 9
    |})]
           ~middle_queries_with_expectations:
             [`Global (Reference.create "test.x", alias_dependency, None)]
           ~expected_triggers_except_uge_and_fde:[alias_dependency]
           ~post_queries_with_expectations:
             [
               `Global
                 ( Reference.create "test.x",
                   alias_dependency,
                   Some
                     (Module.UnannotatedGlobal.SimpleAssign
                        {
                          explicit_annotation =
                            Some
                              {
                                (parse_single_expression "int") with
                                location = location (2, 3) (2, 6);
                              };
                          value =
                            Some
                              {
                                (parse_single_expression "9") with
                                location = location (2, 9) (2, 10);
                              };
                          target_location = Location.WithModule.any;
                        }) );
             ];
      (* We should propagate triggered dependencies from AstEnvironment *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_update
           ~original_sources:[AssertUpdateSource.Local ("test.py", {||})]
           ~new_sources:[AssertUpdateSource.Local ("test.py", {|
        x: int = 9
      |})]
           ~middle_queries_with_expectations:
             [`GetRawSource (Reference.create "test", alias_dependency)]
           ~expected_triggers_except_uge_and_fde:[alias_dependency];
    ]


let equal compare value0 value1 = Int.equal (compare value0 value1) 0

let create_overlay_test_data ~context ?(updatable = false) sources =
  let project =
    ScratchProject.setup
      ~include_typeshed_stubs:false
      ~track_dependencies:true
      ~in_memory:(not updatable)
      sources
      ~context
  in
  let read_write_parent =
    ScratchProject.ReadWrite.errors_environment project
    |> ErrorsEnvironment.AssumeDownstreamNeverNeedsUpdates.unannotated_global_environment
  in
  let environment = UnannotatedGlobalEnvironment.overlay read_write_parent in
  project, UnannotatedGlobalEnvironment.read_only read_write_parent, environment


let create_overlay_test_functions ~project ~parent ~environment =
  let read_only = UnannotatedGlobalEnvironment.Overlay.read_only environment in
  let from_parent_and_overlay getter ?dependency key =
    getter parent ?dependency key, getter read_only ?dependency key
  in
  let assert_values_are_overlaid ~qualifier ~class_name =
    let assert_is_overlaid ~cmp ?printer getter key =
      let from_parent, from_overlay = from_parent_and_overlay getter key in
      if not (cmp from_parent from_overlay) then
        assert_failure
          ("Expected overlaid value"
          ^
          match printer with
          | Some printer -> ", got: " ^ printer from_overlay
          | None -> "")
    in
    assert_is_overlaid
      ~cmp:[%equal: Module.Metadata.t option]
      UnannotatedGlobalEnvironment.ReadOnly.get_module_metadata
      qualifier;
    assert_is_overlaid
      ~cmp:(equal [%compare: ClassSummary.t Node.t option])
      UnannotatedGlobalEnvironment.ReadOnly.get_class_summary
      class_name;
    ()
  in
  let assert_values_not_overlaid ~qualifier ~class_name =
    let assert_not_overlaid ~cmp ?printer getter key =
      let from_parent, from_overlay = from_parent_and_overlay getter key in
      assert_equal ~cmp ?printer from_parent from_overlay
    in
    assert_not_overlaid
      ~cmp:[%equal: Module.Metadata.t option]
      UnannotatedGlobalEnvironment.ReadOnly.get_module_metadata
      qualifier;
    assert_not_overlaid
      ~cmp:(equal [%compare: ClassSummary.t Node.t option])
      UnannotatedGlobalEnvironment.ReadOnly.get_class_summary
      class_name;
    ()
  in
  let update_and_assert_invalidated_modules ~expected code_updates =
    let code_updates =
      let relative_to_artifact_path (relative, code) =
        let local_root = ScratchProject.local_root_of project in
        Test.relative_artifact_path ~root:local_root ~relative, code
      in
      List.map code_updates ~f:relative_to_artifact_path
    in
    let invalidated_modules =
      UnannotatedGlobalEnvironment.Overlay.update_overlaid_code environment ~code_updates
      |> UnannotatedGlobalEnvironment.UpdateResult.invalidated_modules
    in
    assert_equal
      ~printer:[%show: Reference.t list]
      ~cmp:[%equal: Reference.t list]
      expected
      invalidated_modules
  in
  assert_values_are_overlaid, assert_values_not_overlaid, update_and_assert_invalidated_modules


let test_overlay_basic context =
  let source_on_disk =
    trim_extra_indentation
      {|
      global_variable: bytes = bytes("defined_on_disk")

      def foo() -> bytes
        return by"defined_on_disk"

      class Bar:
        x: bytes = "defined_on_disk"
      |}
  in
  let source_in_overlay =
    trim_extra_indentation
      {|
      global_variable: str = "defined_in_overlay"

      def foo() -> str
        return "defined_in_overlay"

      class Bar:
        x: str = "defined_in_overlay"
      |}
  in
  let sources =
    [
      "in_overlay.py", source_on_disk;
      "not_in_overlay.py", source_on_disk;
      "shadowed_by_stub.pyi", source_on_disk;
    ]
  in
  let project, parent, environment = create_overlay_test_data ~context sources in
  let assert_values_are_overlaid, assert_values_not_overlaid, update_and_assert_invalidated_modules =
    create_overlay_test_functions ~project ~parent ~environment
  in

  (* Validate behavior prior to update *)
  assert_values_not_overlaid ~qualifier:!&"in_overlay" ~class_name:"in_overlay.Bar";
  assert_values_not_overlaid ~qualifier:!&"not_in_overlay" ~class_name:"not_in_overlay.Bar";
  assert_values_not_overlaid ~qualifier:!&"shadowed_by_stub" ~class_name:"shadowed_by_stub.Bar";
  (* Run the update *)
  update_and_assert_invalidated_modules
    [
      "in_overlay.py", SourceCodeIncrementalApi.Overlay.CodeUpdate.NewCode source_in_overlay;
      "shadowed_by_stub.py", SourceCodeIncrementalApi.Overlay.CodeUpdate.NewCode source_in_overlay;
    ]
    ~expected:[!&"in_overlay"; !&"shadowed_by_stub"];
  (* Validate behavior after to update *)
  assert_values_are_overlaid ~qualifier:!&"in_overlay" ~class_name:"in_overlay.Bar";
  assert_values_not_overlaid ~qualifier:!&"not_in_overlay" ~class_name:"not_in_overlay.Bar";
  assert_values_not_overlaid ~qualifier:!&"shadowed_by_stub" ~class_name:"shadowed_by_stub.Bar";
  ()


let test_overlay_update_filters context =
  let sources =
    [
      "in_overlay_and_imported.py", "x: int = 10";
      "in_overlay_dependent.py", "from in_overlay_and_imported import *";
      "not_in_overlay.py", "from in_overlay_and_imported import *";
    ]
  in
  let project, parent, environment = create_overlay_test_data ~context sources in
  let read_only = UnannotatedGlobalEnvironment.Overlay.read_only environment in
  let _, _, update_and_assert_invalidated_modules =
    create_overlay_test_functions ~project ~parent ~environment
  in
  let assert_global ~exists name =
    match UnannotatedGlobalEnvironment.ReadOnly.get_unannotated_global read_only name with
    | Some _ ->
        if not exists then
          assert_failure ("Global " ^ Reference.show name ^ " exists, but was expected not to")
    | None ->
        if exists then
          assert_failure ("Global " ^ Reference.show name ^ " does not exists, but was expected to")
  in
  (* Run the initial update *)
  update_and_assert_invalidated_modules
    [
      ( "in_overlay_and_imported.py",
        SourceCodeIncrementalApi.Overlay.CodeUpdate.NewCode "y: float = 10.0" );
      ( "in_overlay_dependent.py",
        SourceCodeIncrementalApi.Overlay.CodeUpdate.NewCode "from in_overlay_and_imported import *"
      );
    ]
    ~expected:[!&"in_overlay_and_imported"; !&"in_overlay_dependent"];
  (* Validate behavior of wildcard import after the first update
   * - in_overlay_dependent should see the overlaid state of in_overlay_and_imported, with y defined
   * - not_in_overlay should see the parent state of in_overlay_and_imported, with x defined
   *)
  assert_global ~exists:false !&"in_overlay_dependent.x";
  assert_global ~exists:true !&"in_overlay_dependent.y";
  assert_global ~exists:false !&"in_overlay_dependent.z";
  assert_global ~exists:true !&"not_in_overlay.x";
  assert_global ~exists:false !&"not_in_overlay.y";
  assert_global ~exists:false !&"not_in_overlay.z";
  (* Run the second update *)
  update_and_assert_invalidated_modules
    [
      ( "in_overlay_and_imported.py",
        SourceCodeIncrementalApi.Overlay.CodeUpdate.NewCode "z: float = 10.0" );
    ]
    ~expected:[!&"in_overlay_and_imported"; !&"in_overlay_dependent"];
  (* Validate behavior after update *)
  assert_global ~exists:false !&"in_overlay_dependent.x";
  assert_global ~exists:false !&"in_overlay_dependent.y";
  assert_global ~exists:true !&"in_overlay_dependent.z";
  assert_global ~exists:true !&"not_in_overlay.x";
  assert_global ~exists:false !&"not_in_overlay.y";
  assert_global ~exists:false !&"not_in_overlay.z";
  ()


let test_overlay_propagation context =
  let sources =
    [
      "on_filesystem.py", {|
        on_filesystem_old = 5
      |};
      "in_overlay.py", {|
        from on_filesystem import *

        in_overlay_old = 5
      |};
    ]
  in
  let project, parent, environment = create_overlay_test_data ~context ~updatable:true sources in
  let read_only = UnannotatedGlobalEnvironment.Overlay.read_only environment in
  let _, _, update_and_assert_invalidated_modules =
    create_overlay_test_functions ~project ~parent ~environment
  in
  let assert_global ~exists name =
    match UnannotatedGlobalEnvironment.ReadOnly.get_unannotated_global read_only name with
    | Some _ ->
        if not exists then
          assert_failure ("Global " ^ Reference.show name ^ " exists, but was expected not to")
    | None ->
        if exists then
          assert_failure ("Global " ^ Reference.show name ^ " does not exists, but was expected to")
  in
  (* Initially all x variables exist, no y variables exist *)
  assert_global ~exists:true !&"on_filesystem.on_filesystem_old";
  assert_global ~exists:false !&"on_filesystem.on_filesystem_new";
  assert_global ~exists:true !&"in_overlay.in_overlay_old";
  assert_global ~exists:false !&"in_overlay.in_overlay_new";
  assert_global ~exists:true !&"in_overlay.on_filesystem_old";
  assert_global ~exists:false !&"in_overlay.on_filesystem_new";
  (* Run the initial update. Only the in_overlay variables should change *)
  update_and_assert_invalidated_modules
    [
      ( "in_overlay.py",
        SourceCodeIncrementalApi.Overlay.CodeUpdate.NewCode
          (trim_extra_indentation
             {|
               from on_filesystem import *

               in_overlay_new = 5
             |})
      );
    ]
    ~expected:[!&"in_overlay"];
  assert_global ~exists:true !&"on_filesystem.on_filesystem_old";
  assert_global ~exists:false !&"on_filesystem.on_filesystem_new";
  assert_global ~exists:false !&"in_overlay.in_overlay_old";
  assert_global ~exists:true !&"in_overlay.in_overlay_new";
  assert_global ~exists:true !&"in_overlay.on_filesystem_old";
  assert_global ~exists:false !&"in_overlay.on_filesystem_new";
  (* Run an update on the parent, but do not propagate yet. The overlay-owned parts of the overlay
     environment should see old data about the on_filesystem module, while the parent-owned parts
     should see the updated information (this is a grey-box test, updating only the parent is an
     illegal operation with undefined behavior in production!) *)
  let update_code relative new_code =
    ScratchProject.delete_from_local_root project ~relative;
    ScratchProject.add_to_local_root project ~relative new_code;
    ()
  in
  update_code "on_filesystem.py" {|
    on_filesystem_new = 5
  |};
  let local_root = ScratchProject.local_root_of project in
  let parent_update_result =
    ScratchProject.update_environment
      project
      [
        (Test.relative_artifact_path ~root:local_root ~relative:"on_filesystem.py"
        |> ArtifactPath.Event.(create ~kind:Kind.CreatedOrChanged));
      ]
    |> ErrorsEnvironment.Testing.UpdateResult.unannotated_global_environment
  in
  assert_global ~exists:false !&"on_filesystem.on_filesystem_old";
  assert_global ~exists:true !&"on_filesystem.on_filesystem_new";
  assert_global ~exists:false !&"in_overlay.in_overlay_old";
  assert_global ~exists:true !&"in_overlay.in_overlay_new";
  assert_global ~exists:true !&"in_overlay.on_filesystem_old";
  assert_global ~exists:false !&"in_overlay.on_filesystem_new";
  (* Propagate the update to the overlay. Now the overlay-owned parts of the overlay environment
     should reflect changes from the filesystem *)
  UnannotatedGlobalEnvironment.Overlay.propagate_parent_update environment parent_update_result
  |> ignore;
  assert_global ~exists:false !&"on_filesystem.on_filesystem_old";
  assert_global ~exists:true !&"on_filesystem.on_filesystem_new";
  assert_global ~exists:false !&"in_overlay.in_overlay_old";
  assert_global ~exists:true !&"in_overlay.in_overlay_new";
  assert_global ~exists:false !&"in_overlay.on_filesystem_old";
  assert_global ~exists:true !&"in_overlay.on_filesystem_new";
  ()


let () =
  Test.sanitized_module_name __MODULE__
  >::: [
         (* Simple tests *)
         "global_registration" >:: test_global_registration;
         "simple_globals" >:: test_simple_global_registration;
         "builtins" >:: test_builtin_modules;
         "resolve_exports" >:: test_resolve_exports;
         (* Tests of dependency tracking across an update *)
         test_get_explicit_module_metadata;
         test_get_implicit_module_metadata;
         test_get_class_summary;
         test_class_exists_and_all_classes;
         test_get_unannotated_global;
         test_dependencies_and_new_values;
         "overlay_basic" >:: test_overlay_basic;
         "overlay_update_filters" >:: test_overlay_update_filters;
         "overlay_propagation" >:: test_overlay_propagation;
       ]
  |> Test.run
