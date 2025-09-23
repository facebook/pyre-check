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
open Interprocedural
open Taint

module VariableWithType = struct
  type t = {
    name: Reference.t;
    type_annotation: string option;
  }
  [@@deriving show, compare]

  let location_insensitive_equal left right =
    Reference.equal left.name right.name
    && Option.equal String.equal left.type_annotation right.type_annotation
end

let test_find_globals =
  let assert_found_globals ~source ~expected context =
    let uninteresting_globals_prefix =
      [
        !&"_T";
        !&"_T_co";
        !&"_R_co";
        !&"_P";
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
        !&"typing_extensions.";
        !&"unittest.";
        !&"_Self_complex__";
        !&"_Self_float__";
        !&"_Self_int__";
        !&"_Self_list__";
        !&"_Self_set__";
        !&"_BaseExceptionT_co";
      ]
    in
    let project =
      ScratchProject.setup
        ~context
        ["test.py", source]
        ~include_helper_builtins:false
          (* Without this, we'll pick up all the globals in the test typeshed. *)
        ~include_typeshed_stubs:false
    in
    let pyre_api =
      project |> ScratchProject.pyre_pysa_read_only_api |> PyrePysaApi.ReadOnly.from_pyre1_api
    in
    let is_uninteresting_global name =
      not
        (List.exists uninteresting_globals_prefix ~f:(fun exclude_prefix ->
             Reference.is_prefix ~prefix:exclude_prefix name))
    in
    let add_type_annotation name =
      {
        VariableWithType.name;
        type_annotation =
          (ModelParseResult.Modelable.create_global ~pyre_api (Target.create_object name)
          |> ModelParseResult.Modelable.type_annotation
          |> ModelParseResult.TypeAnnotation.explicit_annotation
          |> function
          | ModelParseResult.TypeAnnotation.ExplicitAnnotation.Found annotation -> Some annotation
          | _ -> None);
      }
    in
    let actual =
      ModelQueryExecution.GlobalVariableQueryExecutor.get_globals
        ~scheduler:(Test.mock_scheduler ())
        ~pyre_api
      |> List.map ~f:Target.object_name
      |> List.filter ~f:is_uninteresting_global
      |> List.map ~f:add_type_annotation
    in
    let expected =
      List.map
        ~f:(fun (reference, annotation) ->
          {
            VariableWithType.name = reference;
            type_annotation = annotation >>| Type.expression >>| Expression.show;
          })
        expected
    in
    assert_equal
      ~cmp:(List.equal VariableWithType.location_insensitive_equal)
      ~printer:[%show: VariableWithType.t list]
      expected
      actual
  in

  test_list
    [
      labeled_test_case __FILE__ __LINE__
      @@ assert_found_globals
           ~source:{|
      foo = []
      bar: typing.List[typing.Any] = []
    |}
           ~expected:[!&"test.bar", Some (Type.list Type.Any); !&"test.foo", None];
      (* Note that functions are not selected *)
      labeled_test_case __FILE__ __LINE__
      @@ assert_found_globals ~source:{|
      def foo():
        pass
    |} ~expected:[];
      labeled_test_case __FILE__ __LINE__
      @@ assert_found_globals
           ~source:
             {|
      foo = []
      bar = {}

      baz: typing.List[typing.Any] = []
      abc: typing.Dict[typing.Any, typing.Any] = {}
    |}
           ~expected:
             [
               !&"test.abc", Some (Type.dictionary ~key:Type.Any ~value:Type.Any);
               !&"test.bar", None;
               !&"test.baz", Some (Type.list Type.Any);
               !&"test.foo", None;
             ];
      (* TODO(T132423781): Classes are not recognized as globals *)
      labeled_test_case __FILE__ __LINE__
      @@ assert_found_globals
           ~source:{|
      class C:
        def f():
          pass
      C.bar = 1
    |}
           ~expected:[];
      labeled_test_case __FILE__ __LINE__
      @@ assert_found_globals
           ~source:
             {|
      class C:
        def f():
          pass
      c = C()
      annotated_c: C = C()
    |}
           ~expected:[!&"test.annotated_c", Some (Type.Primitive "test.C"); !&"test.c", None];
      labeled_test_case __FILE__ __LINE__
      @@ assert_found_globals
           ~source:
             {|
      x, y = [], {}

      annotated_x: typing.List[typing.Any]
      annotated_y: typing.Dict[typing.Any, typing.Any]
      annotated_x, annotated_y = [], {}
    |}
           ~expected:
             [
               !&"test.annotated_x", Some (Type.list Type.Any);
               !&"test.annotated_y", Some (Type.dictionary ~key:Type.Any ~value:Type.Any);
               !&"test.x", None;
               !&"test.y", None;
             ];
      labeled_test_case __FILE__ __LINE__
      @@ assert_found_globals
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
      labeled_test_case __FILE__ __LINE__
      @@ assert_found_globals
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
      labeled_test_case __FILE__ __LINE__
      @@ assert_found_globals
           ~source:
             {|
      x = lambda x, y: x + int(y)

      def fun(x: int, y: str) -> int:
        return x + int(y)

      y = fun

      a = fun(1, "2")
      b: int = fun(1, "2")
      |}
           ~expected:
             [!&"test.a", None; !&"test.b", Some Type.integer; !&"test.x", None; !&"test.y", None];
      labeled_test_case __FILE__ __LINE__
      @@ assert_found_globals
           ~source:
             {|
    x = 1
    x = "abc"

    y: int = 1
    y = "abc"
    y: str = "abc"
    |}
           ~expected:[!&"test.x", None; !&"test.y", Some Type.integer];
    ]


let () = "globalVariableQuery" >::: [test_find_globals] |> Test.run
