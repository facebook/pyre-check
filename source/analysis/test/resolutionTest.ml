(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Analysis
open Ast
open Pyre
open Statement
open Test

let test_new_and_refine context =
  let assert_local ~name ~expected resolution =
    assert_equal
      ~cmp:(Option.equal Type.equal)
      (expected >>| parse_single_expression >>| Type.create ~aliases:Type.empty_aliases)
      (Resolution.get_local ~reference:!&name resolution >>| Annotation.annotation)
  in
  let assert_local_with_attributes
      ?(global_fallback = true)
      ~name
      ~attribute_path
      ~expected
      resolution
    =
    assert_equal
      ~cmp:(Option.equal Type.equal)
      (expected >>| parse_single_expression >>| Type.create ~aliases:Type.empty_aliases)
      (Resolution.get_local_with_attributes
         ~global_fallback
         ~name:!&name
         ~attribute_path:!&attribute_path
         resolution
      >>| Annotation.annotation)
  in
  let resolution = ScratchProject.setup ~context [] |> ScratchProject.build_resolution in
  (* nothing to start out with *)
  assert_local ~name:"local" ~expected:None resolution;
  (* create a local `local` and make sure the type is right *)
  let resolution =
    Resolution.new_local
      resolution
      ~reference:!&"local"
      ~annotation:(Annotation.create_mutable Type.object_primitive)
  in
  assert_local ~name:"local" ~expected:(Some "object") resolution;
  (* create an attribute `local.x.y` and make sure the type is right, also refine it *)
  assert_local_with_attributes ~name:"local" ~attribute_path:"x.y" ~expected:None resolution;
  let resolution =
    Resolution.new_local_with_attributes
      resolution
      ~name:!&"local"
      ~attribute_path:!&"x.y"
      ~base_annotation:None
      ~annotation:(Annotation.create_mutable Type.object_primitive)
  in
  assert_local_with_attributes
    ~name:"local"
    ~attribute_path:"x.y"
    ~expected:(Some "object")
    resolution;
  (* Make sure we can refine `local.x.y` *)
  let resolution =
    Resolution.refine_local_with_attributes
      resolution
      ~name:!&"local"
      ~attribute_path:!&"x.y"
      ~base_annotation:None
      ~annotation:(Annotation.create_mutable Type.integer)
  in
  assert_local_with_attributes ~name:"local" ~attribute_path:"x.y" ~expected:(Some "int") resolution;
  (* refine `local.x` and make sure it refines, and doesn't destroy `local.x.y` *)
  let resolution =
    Resolution.refine_local_with_attributes
      resolution
      ~name:!&"local"
      ~attribute_path:!&"x"
      ~base_annotation:None
      ~annotation:(Annotation.create_mutable Type.float)
  in
  assert_local_with_attributes ~name:"local" ~attribute_path:"x" ~expected:(Some "float") resolution;
  assert_local_with_attributes ~name:"local" ~attribute_path:"x.y" ~expected:(Some "int") resolution;
  (* bind a new type to `local.x`. This should destroy `local.x.y` *)
  let resolution =
    Resolution.new_local_with_attributes
      resolution
      ~name:!&"local"
      ~attribute_path:!&"x"
      ~base_annotation:None
      ~annotation:(Annotation.create_mutable Type.integer)
  in
  assert_local_with_attributes ~name:"local" ~attribute_path:"x" ~expected:(Some "int") resolution;
  assert_local_with_attributes ~name:"local" ~attribute_path:"x.y" ~expected:None resolution;
  (* refine `local`. This should not destroy `local.x`. *)
  let resolution =
    Resolution.refine_local
      resolution
      ~reference:!&"local"
      ~annotation:(Annotation.create_mutable Type.float)
  in
  assert_local ~name:"local" ~expected:(Some "float") resolution;
  assert_local_with_attributes ~name:"local" ~attribute_path:"x" ~expected:(Some "int") resolution;
  (* bind a new type to `local`. This should destroy `local.x`. *)
  let resolution =
    Resolution.new_local
      resolution
      ~reference:!&"local"
      ~annotation:(Annotation.create_mutable Type.integer)
  in
  assert_local ~name:"local" ~expected:(Some "int") resolution;
  assert_local_with_attributes ~name:"local" ~attribute_path:"x" ~expected:None resolution;
  ()


let test_parse_annotation context =
  let assert_parse_annotation ~validation ~resolution ~expected expression =
    assert_equal
      ~cmp:Type.equal
      ~printer:Type.show
      (parse_single_expression expected |> Type.create ~aliases:Type.empty_aliases)
      (GlobalResolution.parse_annotation ~validation resolution expression)
  in
  let resolution =
    let resolution =
      ScratchProject.setup
        ~context
        ["empty.pyi", "class Empty: ... "; "empty/stub.pyi", "# pyre-placeholder-stub"]
      |> ScratchProject.build_resolution
    in
    Resolution.global_resolution resolution
  in
  assert_parse_annotation
    ~validation:ValidatePrimitivesAndTypeParameters
    ~resolution
    ~expected:"int"
    !"int";
  assert_parse_annotation
    ~validation:NoValidation
    ~resolution
    ~expected:"qualifier.int"
    !"$local_qualifier$int";
  assert_parse_annotation
    ~validation:ValidatePrimitivesAndTypeParameters
    ~resolution
    ~expected:"typing.Any"
    !"empty.stub.Annotation";
  assert_parse_annotation
    ~validation:ValidatePrimitivesAndTypeParameters
    ~resolution
    ~expected:"typing.Dict[str, typing.Any]"
    (parse_single_expression "typing.Dict[str, empty.stub.Annotation]")


(** The purpose of this test is to test environments in which the value for the global variable
    no_validation_on_class_lookup_failure is set to true. If both
    no_validation_on_class_lookup_failure is true and the validation parameter of the
    parse_annotation function is unset, we will NOT validate the annotation. **)
let test_parse_annotation_for_no_validation_on_class_lookup_failure_environment context =
  let assert_parse_annotation ?validation ~resolution ~expected expression =
    assert_equal
      ~cmp:Type.equal
      ~printer:Type.show
      expected
      (GlobalResolution.parse_annotation ?validation resolution expression)
  in
  let resolution =
    ScratchProject.setup ~context ~no_validation_on_class_lookup_failure:true []
    |> ScratchProject.build_global_environment
    |> (fun { ScratchProject.BuiltGlobalEnvironment.global_environment; _ } -> global_environment)
    |> GlobalResolution.create
  in
  assert_parse_annotation
    ~validation:NoValidation
    ~resolution
    ~expected:(Type.Primitive "qualifier.int")
    !"$local_qualifier$int";
  assert_parse_annotation
    ~resolution
    ~expected:(Type.Primitive "qualifier.int")
    !"$local_qualifier$int";
  assert_parse_annotation
    ~validation:ValidatePrimitivesAndTypeParameters
    ~resolution
    ~expected:Type.Top
    !"$local_qualifier$int";
  assert_parse_annotation
    ~validation:NoValidation
    ~resolution
    ~expected:(Type.dictionary ~key:Type.string ~value:(Type.Primitive "Empty"))
    (parse_single_expression "typing.Dict[str, Empty]");
  assert_parse_annotation
    ~resolution
    ~expected:(Type.dictionary ~key:Type.string ~value:(Type.Primitive "Empty"))
    (parse_single_expression "typing.Dict[str, Empty]");
  assert_parse_annotation
    ~validation:ValidatePrimitivesAndTypeParameters
    ~resolution
    ~expected:Type.Top
    (parse_single_expression "typing.Dict[str, Empty]");
  ()


let make_resolution ~context source =
  ScratchProject.setup ~context ["test.py", source] |> ScratchProject.build_resolution


let test_parse_reference context =
  let resolution =
    make_resolution ~context {|
      import typing
      class Foo: ...
      MyType = int
    |}
    |> Resolution.global_resolution
  in
  let assert_parse_reference reference expected =
    assert_equal
      ~printer:Type.show
      expected
      (GlobalResolution.parse_reference resolution !&reference)
  in
  assert_parse_reference "undefined" Type.Top;
  assert_parse_reference "test.MyType" Type.integer;
  assert_parse_reference "test.Foo" (Type.Primitive "test.Foo");
  assert_parse_reference "typing.List" (Type.Primitive "list")


let test_resolve_literal context =
  let resolution =
    make_resolution
      ~context
      {|
      class C:
        def __init__(self) -> None:
          pass
      T = typing.TypeVar("T")
      class G(typing.Generic[T]):
        def __init__(self, x: T) -> None:
          pass
      def foo()->int:
        ...
      i = 1
      j = foo()
      s = 'asdf'
      t = 1, 1.0
      none = None
      awaitable: typing.Awaitable[int]
    |}
    |> Resolution.global_resolution
  in
  let assert_resolve_literal source expected =
    let expression =
      match parse_single_statement source with
      | { Node.value = Statement.Expression expression; _ } -> expression
      | _ -> failwith "No Assign to parse"
    in
    assert_equal
      ~printer:Type.show
      expected
      (GlobalResolution.resolve_literal resolution expression)
  in
  assert_resolve_literal "i" Type.Any;
  assert_resolve_literal "await i" Type.Any;
  assert_resolve_literal "await awaitable" Type.Any;
  assert_resolve_literal "\"\"" Type.string;
  assert_resolve_literal "1" Type.integer;
  assert_resolve_literal "1+1" Type.Any;
  assert_resolve_literal "j" Type.Any;
  assert_resolve_literal "foo()" Type.Any;
  assert_resolve_literal "C()" (Type.Primitive "C");
  assert_resolve_literal "G(7)" Type.Any;
  assert_resolve_literal "C" (Type.meta (Type.Primitive "C"));
  assert_resolve_literal "G" Type.Any;

  (* None *)
  assert_resolve_literal "None" Type.Any;
  assert_resolve_literal "[None]" (Type.list Type.Any);

  (* Dictionary *)
  assert_resolve_literal "{'a': 1}" (Type.dictionary ~key:Type.string ~value:Type.integer);
  assert_resolve_literal "{'a': i}" (Type.dictionary ~key:Type.string ~value:Type.Any);
  assert_resolve_literal "{'a': [], 'b': [1]}" (Type.dictionary ~key:Type.string ~value:Type.Any);
  assert_resolve_literal "{**foo}" (Type.dictionary ~key:Type.Any ~value:Type.Any);
  assert_resolve_literal "{'a': 1, **foo}" (Type.dictionary ~key:Type.Any ~value:Type.Any);

  (* Boolean Operator *)
  assert_resolve_literal "1 or 2" Type.integer;
  assert_resolve_literal "True or 1" (Type.union [Type.bool; Type.integer]);
  assert_resolve_literal "True or i" Type.Any;

  (* List *)
  assert_resolve_literal "[1]" (Type.list Type.integer);
  assert_resolve_literal "[1, 'string']" (Type.list (Type.Union [Type.integer; Type.string]));
  assert_resolve_literal "[1, i]" (Type.list Type.Any);

  (* Set *)
  assert_resolve_literal "{1}" (Type.set Type.integer);
  assert_resolve_literal "{1, 'string'}" (Type.set (Type.Union [Type.integer; Type.string]));
  assert_resolve_literal "{1, i}" (Type.set Type.Any);

  (* Tuple *)
  assert_resolve_literal "(1,)" (Type.Tuple (Concrete [Type.integer]));
  assert_resolve_literal "(1, 'string')" (Type.Tuple (Concrete [Type.integer; Type.string]));
  assert_resolve_literal "(1, i)" (Type.Tuple (Concrete [Type.integer; Type.Any]));

  (* Ternary *)
  assert_resolve_literal "1 if x else 2" Type.integer;
  assert_resolve_literal "'hi' if x else 1" (Type.union [Type.string; Type.integer]);
  assert_resolve_literal "1 if i else i" Type.Any


let test_resolve_exports context =
  let assert_resolve ~sources name expected =
    let resolution =
      ScratchProject.setup ~context sources |> ScratchProject.build_global_resolution
    in
    let reference = GlobalResolution.legacy_resolve_exports resolution (Reference.create name) in
    assert_equal ~printer:Reference.show ~cmp:Reference.equal (Reference.create expected) reference
  in
  assert_resolve ~sources:[] "a.b" "a.b";
  assert_resolve ~sources:["a.py", "from b import foo"; "b.py", "foo = 1"] "a.foo" "b.foo";
  assert_resolve
    ~sources:
      [
        "a.py", "from b import foo";
        "b.py", "from c import bar as foo";
        "c.py", "from d import cow as bar";
        "d.py", "cow = 1";
      ]
    "a.foo"
    "d.cow";
  assert_resolve
    ~sources:
      ["qualifier.py", "from qualifier.foo import foo"; "qualifier/foo/__init__.py", "foo = 1"]
    "qualifier.foo.foo"
    "qualifier.foo.foo";
  assert_resolve
    ~sources:
      [
        "placeholder.py", "# pyre-placeholder-stub";
        "a.py", "from placeholder.nonexistent import foo";
      ]
    "a.foo"
    "placeholder.nonexistent.foo";
  assert_resolve
    ~sources:
      [
        "qualifier/__init__.py", "from qualifier.a import bar as a";
        "qualifier/a.py", "foo = 1\nbar = 1";
      ]
    "qualifier.a.foo"
    "qualifier.a.foo"


let test_get_typed_dictionary context =
  let resolution =
    make_resolution
      ~context
      {|
      import mypy_extensions
      class Movie(mypy_extensions.TypedDict):
        name: str
        year: int

      class NonTotalMovie(mypy_extensions.TypedDict, total=False):
        name: str
        year: int

      class Child(Movie):
        rating: int

      class RegularClass:
        def __init__(self, x: int) -> None:
          self.x = x
    |}
  in
  let assert_typed_dictionary ~annotation expected_type =
    assert_equal
      ~printer:[%show: Type.t Type.Record.TypedDictionary.record option]
      expected_type
      (GlobalResolution.get_typed_dictionary
         ~resolution:(Resolution.global_resolution resolution)
         annotation)
  in
  assert_typed_dictionary ~annotation:(Type.Primitive "test.RegularClass") None;
  assert_typed_dictionary
    ~annotation:(Type.Primitive "test.Movie")
    (Some
       {
         name = "test.Movie";
         fields =
           [
             { Type.Record.TypedDictionary.name = "name"; annotation = Type.string; required = true };
             {
               Type.Record.TypedDictionary.name = "year";
               annotation = Type.integer;
               required = true;
             };
           ];
       });
  assert_typed_dictionary
    ~annotation:(Type.Primitive "test.Child")
    (Some
       {
         name = "test.Child";
         fields =
           [
             { Type.Record.TypedDictionary.name = "name"; annotation = Type.string; required = true };
             {
               Type.Record.TypedDictionary.name = "rating";
               annotation = Type.integer;
               required = true;
             };
             {
               Type.Record.TypedDictionary.name = "year";
               annotation = Type.integer;
               required = true;
             };
           ];
       });
  assert_typed_dictionary
    ~annotation:(Type.Primitive "test.NonTotalMovie")
    (Some
       {
         name = "test.NonTotalMovie";
         fields =
           [
             {
               Type.Record.TypedDictionary.name = "name";
               annotation = Type.string;
               required = false;
             };
             {
               Type.Record.TypedDictionary.name = "year";
               annotation = Type.integer;
               required = false;
             };
           ];
       });
  ()


let test_function_definitions context =
  let assert_functions sources function_name expected =
    let project = ScratchProject.setup ~context sources in
    let resolution = ScratchProject.build_resolution project in
    let resolution = Resolution.global_resolution resolution in
    let functions =
      GlobalResolution.function_definitions resolution !&function_name
      >>| List.map ~f:(fun { Node.value = { Define.signature = { name; _ }; _ }; _ } ->
              Reference.show name)
      |> Option.value ~default:[]
    in
    assert_equal ~printer:(String.concat ~sep:", ") expected functions
  in
  assert_functions ["foo.py", "def foo(): pass\n"] "foo.foo" ["foo.foo"];
  assert_functions
    [
      ( "bar.py",
        {|
        @overload
        def bar(a: int) -> str: ...
        def bar(a: str) -> int: ...
      |}
      );
    ]
    "bar.bar"
    ["bar.bar"; "bar.bar"];
  assert_functions
    ["baz.py", {|
        def foo(a: int) -> str: ...
        def bar(a: str) -> int: ...
      |}]
    "baz.foo"
    ["baz.foo"];
  assert_functions [] "undefined.undefined" [];
  assert_functions
    ["yarp.py", {|
        def foo():
          def nested(): pass
      |}]
    "yarp.foo.nested"
    [];
  ()


(* We don't reverse order when returning the classes. *)

let test_source_is_unit_test context =
  let assert_is_unit_test ?(expected = true) ?(extra_sources = []) source =
    let { ScratchProject.BuiltGlobalEnvironment.global_environment; _ } =
      ScratchProject.setup ~context (["test.py", source] @ extra_sources)
      |> ScratchProject.build_global_environment
    in
    let resolution = GlobalResolution.create global_environment in
    let source =
      AstEnvironment.ReadOnly.get_processed_source
        (AnnotatedGlobalEnvironment.ReadOnly.ast_environment global_environment)
        (Reference.create "test")
      |> fun option -> Option.value_exn option
    in
    assert_equal expected (GlobalResolution.source_is_unit_test resolution ~source)
  in
  let assert_not_unit_test = assert_is_unit_test ~expected:false in
  assert_is_unit_test "class C(unittest.case.TestCase): ...";
  assert_not_unit_test {|
    from unittest import TestCase
    class C: pass
  |};
  assert_is_unit_test
    {|
    class C:
      def foo():
        class Nested(unittest.case.TestCase): ...
  |};
  assert_not_unit_test
    ~extra_sources:["placeholder.py", "# pyre-placeholder-stub"]
    {|
    import placeholder
    class C(placeholder.Missing):
      ...
  |}


let test_fallback_attribute context =
  let assert_fallback_attribute ?(instantiated = None) ~name source annotation =
    let { ScratchProject.BuiltGlobalEnvironment.global_environment; _ } =
      ScratchProject.setup ~context ["test.py", source] |> ScratchProject.build_global_environment
    in
    let global_resolution = GlobalResolution.create global_environment in
    let resolution = TypeCheck.resolution global_resolution (module TypeCheck.DummyContext) in

    let attribute =
      let qualifier = Reference.create "test" in
      let source =
        AstEnvironment.ReadOnly.get_processed_source
          (AnnotatedGlobalEnvironment.ReadOnly.ast_environment global_environment)
          qualifier
      in
      let last_statement_exn = function
        | { Source.statements; _ } when List.length statements > 0 -> List.last_exn statements
        | _ -> failwith "Could not parse last statement"
      in

      let source = Option.value_exn source in
      last_statement_exn source
      |> Node.value
      |> (function
           | Statement.Class definition -> ClassSummary.create ~qualifier definition
           | _ -> failwith "Last statement was not a class")
      |> ClassSummary.name
      |> Reference.show
      |> Resolution.fallback_attribute ~instantiated ~resolution ~name
    in
    let printer optional_type = optional_type >>| Type.show |> Option.value ~default:"None" in
    assert_equal
      ~cmp:(Option.equal Type.equal)
      ~printer
      annotation
      (attribute >>| AnnotatedAttribute.annotation >>| Annotation.annotation)
  in
  assert_fallback_attribute ~name:"attribute" {|
      class Foo:
        pass
    |} None;
  assert_fallback_attribute
    ~name:"attribute"
    {|
      class Foo:
        def __getattr__(self, attribute: str) -> int:
          return 1
    |}
    (Some Type.integer);
  assert_fallback_attribute
    ~name:"attribute"
    {|
      class Foo:
        def __getattr__(self, attribute: str) -> int: ...
    |}
    (Some Type.integer);
  assert_fallback_attribute
    ~name:"attribute"
    {|
      class Foo:
        def __getattr__(self, attribute: str) -> int: ...
      class Bar(Foo):
        pass
    |}
    (Some Type.integer);
  assert_fallback_attribute
    ~name:"__iadd__"
    {|
      class Foo:
        def __add__(self, other: Foo) -> int:
          pass
    |}
    (Some
       (Parametric
          {
            name = "BoundMethod";
            parameters =
              [
                Single
                  (parse_callable
                     "typing.Callable('test.Foo.__add__')[[Named(self, test.Foo), Named(other, \
                      test.Foo)], int]");
                Single (Primitive "test.Foo");
              ];
          }));
  assert_fallback_attribute
    ~name:"__iadd__"
    ~instantiated:(Some (Type.parametric "test.Foo" [Single Type.integer]))
    {|
      from typing import Generic, TypeVar
      T = TypeVar("T")
      class Foo(Generic[T]):
        def __add__(self, other: Foo[T]) -> Foo[T]:
          pass
    |}
    (Some
       (Parametric
          {
            name = "BoundMethod";
            parameters =
              [
                Single
                  (parse_callable
                     "typing.Callable(test.Foo.__add__)[[Named(self, test.Foo[int]), Named(other, \
                      test.Foo[int])], test.Foo[int]]");
                Single (Type.parametric "test.Foo" [Single Type.integer]);
              ];
          }));
  assert_fallback_attribute ~name:"__iadd__" {|
      class Foo:
        pass
    |} None;
  assert_fallback_attribute
    ~name:"__iadd__"
    {|
      class Foo:
        def __getattr__(self, attribute) -> int: ...
    |}
    (Some Type.integer);
  assert_fallback_attribute
    ~name:"foo"
    {|
      from typing import overload
      import typing_extensions
      class Foo:
        @overload
        def __getattr__(self, attribute: typing_extensions.Literal['foo']) -> int: ...
        @overload
        def __getattr__(self, attribute: typing_extensions.Literal['bar']) -> str: ...
        @overload
        def __getattr__(self, attribute: str) -> None: ...
    |}
    (Some Type.integer);
  assert_fallback_attribute
    ~name:"bar"
    {|
      from typing import overload
      import typing_extensions
      class Foo:
        @overload
        def __getattr__(self, attribute: typing_extensions.Literal['foo']) -> int: ...
        @overload
        def __getattr__(self, attribute: typing_extensions.Literal['bar']) -> str: ...
        @overload
        def __getattr__(self, attribute: str) -> None: ...
    |}
    (Some Type.string);
  assert_fallback_attribute
    ~name:"baz"
    {|
      from typing import overload
      import typing_extensions
      class Foo:
        @overload
        def __getattr__(self, attribute: typing_extensions.Literal['foo']) -> int: ...
        @overload
        def __getattr__(self, attribute: typing_extensions.Literal['bar']) -> str: ...
        @overload
        def __getattr__(self, attribute: str) -> None: ...
    |}
    (Some Type.none);
  assert_fallback_attribute
    ~name:"baz"
    {|
      from typing import overload
      import typing_extensions
      class Foo:
        @overload
        def __getattr__(self: Foo, attribute: str) -> int: ...
    |}
    (Some Type.integer);
  (* Callables on the instance do not get picked up by the runtime. Who knew? *)
  assert_fallback_attribute
    ~name:"baz"
    {|
      class Foo:
        __getattr__: typing.Callable[[str], int]
    |}
    None;
  assert_fallback_attribute
    ~name:"baz"
    {|
      class Foo:
        __getattr__: BoundMethod[typing.Callable[[Foo, str], int], Foo]
    |}
    (Some Type.integer);
  ()


let () =
  "resolution"
  >::: [
         "new_and_refine" >:: test_new_and_refine;
         "parse_annotation" >:: test_parse_annotation;
         "parse_annotation_no_validation_on_class_lookup_failure"
         >:: test_parse_annotation_for_no_validation_on_class_lookup_failure_environment;
         "parse_reference" >:: test_parse_reference;
         "resolve_literal" >:: test_resolve_literal;
         "resolve_exports" >:: test_resolve_exports;
         "get_typed_dictionary " >:: test_get_typed_dictionary;
         "function_definitions" >:: test_function_definitions;
         "source_is_unit_test" >:: test_source_is_unit_test;
         "fallback_attribute" >:: test_fallback_attribute;
       ]
  |> Test.run
