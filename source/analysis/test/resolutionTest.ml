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

let variable_aliases _ = None

let test_new_and_refine context =
  let assert_local ~name ~expected resolution =
    assert_equal
      ~cmp:(Option.equal Type.equal)
      (expected
      >>| parse_single_expression
      >>| Type.create ~variables:variable_aliases ~aliases:Type.resolved_empty_aliases)
      (Resolution.get_local ~reference:!&name resolution >>| TypeInfo.Unit.annotation)
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
      (expected
      >>| parse_single_expression
      >>| Type.create ~variables:variable_aliases ~aliases:Type.resolved_empty_aliases)
      (Resolution.get_local_with_attributes
         ~global_fallback
         ~name:!&name
         ~attribute_path:!&attribute_path
         resolution
      >>| TypeInfo.Unit.annotation)
  in
  let resolution = ScratchProject.setup ~context [] |> ScratchProject.build_resolution in
  (* nothing to start out with *)
  assert_local ~name:"local" ~expected:None resolution;
  (* create a local `local` and make sure the type is right *)
  let resolution =
    Resolution.new_local
      resolution
      ~reference:!&"local"
      ~type_info:(TypeInfo.Unit.create_mutable Type.object_primitive)
  in
  assert_local ~name:"local" ~expected:(Some "object") resolution;
  (* create an attribute `local.x.y` and make sure the type is right, also refine it *)
  assert_local_with_attributes ~name:"local" ~attribute_path:"x.y" ~expected:None resolution;
  let resolution =
    Resolution.new_local_with_attributes
      resolution
      ~name:!&"local"
      ~attribute_path:!&"x.y"
      ~base_type_info:None
      ~type_info:(TypeInfo.Unit.create_mutable Type.object_primitive)
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
      ~base_type_info:None
      ~type_info:(TypeInfo.Unit.create_mutable Type.integer)
  in
  assert_local_with_attributes ~name:"local" ~attribute_path:"x.y" ~expected:(Some "int") resolution;
  (* refine `local.x` and make sure it refines, and doesn't destroy `local.x.y` *)
  let resolution =
    Resolution.refine_local_with_attributes
      resolution
      ~name:!&"local"
      ~attribute_path:!&"x"
      ~base_type_info:None
      ~type_info:(TypeInfo.Unit.create_mutable Type.float)
  in
  assert_local_with_attributes ~name:"local" ~attribute_path:"x" ~expected:(Some "float") resolution;
  assert_local_with_attributes ~name:"local" ~attribute_path:"x.y" ~expected:(Some "int") resolution;
  (* bind a new type to `local.x`. This should destroy `local.x.y` *)
  let resolution =
    Resolution.new_local_with_attributes
      resolution
      ~name:!&"local"
      ~attribute_path:!&"x"
      ~base_type_info:None
      ~type_info:(TypeInfo.Unit.create_mutable Type.integer)
  in
  assert_local_with_attributes ~name:"local" ~attribute_path:"x" ~expected:(Some "int") resolution;
  assert_local_with_attributes ~name:"local" ~attribute_path:"x.y" ~expected:None resolution;
  (* refine `local`. This should not destroy `local.x`. *)
  let resolution =
    Resolution.refine_local
      resolution
      ~reference:!&"local"
      ~type_info:(TypeInfo.Unit.create_mutable Type.float)
  in
  assert_local ~name:"local" ~expected:(Some "float") resolution;
  assert_local_with_attributes ~name:"local" ~attribute_path:"x" ~expected:(Some "int") resolution;
  (* bind a new type to `local`. This should destroy `local.x`. *)
  let resolution =
    Resolution.new_local
      resolution
      ~reference:!&"local"
      ~type_info:(TypeInfo.Unit.create_mutable Type.integer)
  in
  assert_local ~name:"local" ~expected:(Some "int") resolution;
  assert_local_with_attributes ~name:"local" ~attribute_path:"x" ~expected:None resolution;
  ()


let test_parse_annotation context =
  let assert_parse_annotation ~validation ~resolution ~expected expression =
    assert_equal
      ~cmp:Type.equal
      ~printer:Type.show
      (parse_single_expression expected
      |> Type.create ~variables:variable_aliases ~aliases:Type.resolved_empty_aliases)
      (GlobalResolution.parse_annotation ~validation resolution expression)
  in
  let resolution =
    let resolution =
      ScratchProject.setup ~context ["empty.pyi", "class Empty: ... "]
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
  ()


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
      ~printer:[%show: Type.TypedDictionary.t option]
      expected_type
      (GlobalResolution.get_typed_dictionary (Resolution.global_resolution resolution) annotation)
  in
  assert_typed_dictionary ~annotation:(Type.Primitive "test.RegularClass") None;
  assert_typed_dictionary
    ~annotation:(Type.Primitive "test.Movie")
    (Some
       {
         name = "test.Movie";
         fields =
           [
             {
               Type.TypedDictionary.name = "name";
               annotation = Type.string;
               required = true;
               readonly = false;
             };
             {
               Type.TypedDictionary.name = "year";
               annotation = Type.integer;
               required = true;
               readonly = false;
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
             {
               Type.TypedDictionary.name = "name";
               annotation = Type.string;
               required = true;
               readonly = false;
             };
             {
               Type.TypedDictionary.name = "rating";
               annotation = Type.integer;
               required = true;
               readonly = false;
             };
             {
               Type.TypedDictionary.name = "year";
               annotation = Type.integer;
               required = true;
               readonly = false;
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
               Type.TypedDictionary.name = "name";
               annotation = Type.string;
               required = false;
               readonly = false;
             };
             {
               Type.TypedDictionary.name = "year";
               annotation = Type.integer;
               required = false;
               readonly = false;
             };
           ];
       });
  ()


(* We don't reverse order when returning the classes. *)

let test_fallback_attribute =
  let assert_fallback_attribute ?(instantiated = None) ~name source annotation context =
    let project = ScratchProject.setup ~context ["test.py", source] in
    let global_resolution = Test.ScratchProject.build_global_resolution project in
    let resolution = TypeCheck.resolution global_resolution (module TypeCheck.DummyContext) in

    let attribute =
      let qualifier = Reference.create "test" in
      let source =
        SourceCodeApi.source_of_qualifier
          (Test.ScratchProject.get_untracked_source_code_api project)
          qualifier
      in
      let last_statement_exn = function
        | { Source.statements; _ } when not (List.is_empty statements) -> List.last_exn statements
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
      (attribute >>| AnnotatedAttribute.annotation >>| TypeInfo.Unit.annotation)
  in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_fallback_attribute
           ~name:"attribute"
           {|
              class Foo:
                pass
            |}
           None;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_fallback_attribute
           ~name:"attribute"
           {|
              class Foo:
                def __getattr__(self, attribute: str) -> int:
                  return 1
            |}
           (Some Type.integer);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_fallback_attribute
           ~name:"attribute"
           {|
              class Foo:
                def __getattr__(self, attribute: str) -> int: ...
            |}
           (Some Type.integer);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_fallback_attribute
           ~name:"attribute"
           {|
              class Foo:
                def __getattr__(self, attribute: str) -> int: ...
              class Bar(Foo):
                pass
            |}
           (Some Type.integer);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_fallback_attribute
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
                   arguments =
                     [
                       Single
                         (parse_callable
                            "typing.Callable('test.Foo.__add__')[[Named(self, test.Foo), \
                             Named(other, test.Foo)], int]");
                       Single (Primitive "test.Foo");
                     ];
                 }));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_fallback_attribute
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
                   arguments =
                     [
                       Single
                         (parse_callable
                            "typing.Callable(test.Foo.__add__)[[Named(self, test.Foo[int]), \
                             Named(other, test.Foo[int])], test.Foo[int]]");
                       Single (Type.parametric "test.Foo" [Single Type.integer]);
                     ];
                 }));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_fallback_attribute
           ~name:"__iadd__"
           {|
              class Foo:
                pass
            |}
           None;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_fallback_attribute
           ~name:"__iadd__"
           {|
              class Foo:
                def __getattr__(self, attribute) -> int: ...
            |}
           (Some Type.integer);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_fallback_attribute
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_fallback_attribute
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_fallback_attribute
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_fallback_attribute
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_fallback_attribute
           ~name:"baz"
           {|
              class Foo:
                __getattr__: typing.Callable[[str], int]
            |}
           None;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_fallback_attribute
           ~name:"baz"
           {|
              class Foo:
                __getattr__: BoundMethod[typing.Callable[[Foo, str], int], Foo]
            |}
           (Some Type.integer);
    ]


let () =
  "resolution"
  >::: [
         "new_and_refine" >:: test_new_and_refine;
         "parse_annotation" >:: test_parse_annotation;
         "parse_annotation_no_validation_on_class_lookup_failure"
         >:: test_parse_annotation_for_no_validation_on_class_lookup_failure_environment;
         "parse_reference" >:: test_parse_reference;
         "get_typed_dictionary " >:: test_get_typed_dictionary;
         test_fallback_attribute;
       ]
  |> Test.run
