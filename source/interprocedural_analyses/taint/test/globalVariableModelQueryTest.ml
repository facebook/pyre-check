(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Core
open Ast
open Test
open Analysis
open Pyre

let test_find_globals context =
  let assert_found_globals ~source ~expected =
    let uninteresting_globals_prefix =
      [
        !&"_T";
        !&"_T_co";
        !&"_S";
        !&"_KT";
        !&"_VT";
        !&"Ellipsis";
        !&"N1";
        !&"N2";
        !&"_T1";
        !&"_T2";
        !&"_T3";
        !&"_T4";
        !&"_T5";
        !&"_Self";
        !&"_SupportsNextT";
        !&"NotImplemented";
        !&"...";
        !&"__debug__";
        !&"abc.";
        !&"typing.";
        !&"unittest.";
      ]
    in
    let project =
      ScratchProject.setup
        ~context
        ["test.py", source]
        ~include_helper_builtins:false
        ~include_typeshed_stubs:true
    in
    let { ScratchProject.BuiltTypeEnvironment.type_environment; _ } =
      ScratchProject.build_type_environment project
    in
    let global_resolution = TypeEnvironment.ReadOnly.global_resolution type_environment in
    let is_uninteresting_global { Taint.ModelQueryExecution.VariableMetadata.name = global_name; _ }
      =
      not
        (List.exists uninteresting_globals_prefix ~f:(fun exclude_prefix ->
             Reference.is_prefix ~prefix:exclude_prefix global_name))
    in
    let actual =
      Taint.ModelQueryExecution.get_globals_and_annotations ~resolution:global_resolution
      |> List.filter ~f:is_uninteresting_global
    in
    let expected =
      List.map
        ~f:(fun (reference, annotation) ->
          {
            Taint.ModelQueryExecution.VariableMetadata.name = reference;
            type_annotation = annotation >>| Type.expression;
          })
        expected
    in
    let variable_metadata_location_insensitive_equal left right =
      Reference.equal
        left.Taint.ModelQueryExecution.VariableMetadata.name
        right.Taint.ModelQueryExecution.VariableMetadata.name
      && Option.compare
           Expression.Expression.location_insensitive_compare
           left.Taint.ModelQueryExecution.VariableMetadata.type_annotation
           right.Taint.ModelQueryExecution.VariableMetadata.type_annotation
         = 0
    in
    assert_equal
      ~cmp:(List.equal variable_metadata_location_insensitive_equal)
      ~printer:[%show: Taint.ModelQueryExecution.VariableMetadata.t list]
      expected
      actual
  in
  assert_found_globals
    ~source:{|
      foo = []
      bar: typing.List[typing.Any] = []
    |}
    ~expected:[!&"test.foo", None; !&"test.bar", Some (Type.list Type.Any)];
  (* Note that functions are not selected *)
  assert_found_globals ~source:{|
      def foo():
        pass
    |} ~expected:[];
  assert_found_globals
    ~source:
      {|
      foo = []
      bar = {}

      baz: typing.List[typing.Any] = []
      abc: typing.Dict[typing.Any, typing.Any] = {}
    |}
    ~expected:
      [
        !&"test.foo", None;
        !&"test.bar", None;
        !&"test.baz", Some (Type.list Type.Any);
        !&"test.abc", Some (Type.dictionary ~key:Type.Any ~value:Type.Any);
      ];
  (* TODO(T132423781): Classes are not recognized as globals *)
  assert_found_globals
    ~source:{|
      class C:
        def f():
          pass
      C.bar = 1
    |}
    ~expected:[];
  assert_found_globals
    ~source:
      {|
      class C:
        def f():
          pass
      c = C()
      annotated_c: C = C()
    |}
    ~expected:[!&"test.c", None; !&"test.annotated_c", Some (Type.Primitive "test.C")];
  assert_found_globals
    ~source:
      {|
      x, y = [], {}

      annotated_x: typing.List[typing.Any]
      annotated_y: typing.Dict[typing.Any, typing.Any]
      annotated_x, annotated_y = [], {}
    |}
    ~expected:
      [
        !&"test.x", None;
        !&"test.y", None;
        !&"test.annotated_x", Some (Type.list Type.Any);
        !&"test.annotated_y", Some (Type.dictionary ~key:Type.Any ~value:Type.Any);
        !&"test.annotated_x", Some (Type.list Type.Any);
        !&"test.annotated_y", Some (Type.dictionary ~key:Type.Any ~value:Type.Any);
      ];
  assert_found_globals
    ~source:
      {|
      def setup() -> int:
        return 5

      global_1: typing.Dict[str, int] = setup()
      global_2 = setup()
    |}
    ~expected:
      [
        !&"test.global_1", Some (Type.dictionary ~key:Type.string ~value:Type.integer);
        !&"test.global_2", None;
      ];
  assert_found_globals
    ~source:
      {|
    from typing import List, Callable

    x: int
    y: List[bool]
    z: Callable[[], str]
    |}
    ~expected:
      [
        !&"test.x", Some Type.integer;
        !&"test.y", Some (Type.list Type.bool);
        !&"test.z", Some (Type.lambda ~parameters:[] ~return_annotation:Type.string);
      ];
  assert_found_globals
    ~source:
      {|
      x = lambda x, y: x + int(y)

      def fun(x: int, y: str) -> int:
        return x + int(y)

      y = fun

      a = fun(1, "2")
      b: int = fun(1, "2")
      |}
    ~expected:[!&"test.x", None; !&"test.y", None; !&"test.a", None; !&"test.b", Some Type.integer];
  assert_found_globals
    ~source:{|
    x = 1
    x = "abc"

    y: int = 1
    y = "abc"
    y: str = "abc"
    |}
    ~expected:
      [
        !&"test.x", None;
        !&"test.x", None;
        !&"test.y", Some Type.integer;
        !&"test.y", Some Type.integer;
        !&"test.y", Some Type.integer;
      ];
  ()


let () = "globalVarQuery" >::: ["find_globals" >:: test_find_globals] |> Test.run
