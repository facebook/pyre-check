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
    let is_uninteresting_global { TaintModelQuery.ModelQuery.name = global_name; _ } =
      not
        (List.exists uninteresting_globals_prefix ~f:(fun exclude_prefix ->
             Reference.is_prefix ~prefix:exclude_prefix global_name))
    in
    let actual =
      TaintModelQuery.ModelQuery.GlobalVariableQueries.get_globals_and_annotations
        ~environment:type_environment
      |> List.filter ~f:is_uninteresting_global
    in
    let expected =
      List.map
        ~f:(fun (reference, annotation) ->
          {
            TaintModelQuery.ModelQuery.name = reference;
            type_annotation = annotation >>| Type.expression;
          })
        expected
    in
    assert_equal
      ~cmp:[%compare.equal: TaintModelQuery.ModelQuery.variable_metadata list]
      ~printer:[%show: TaintModelQuery.ModelQuery.variable_metadata list]
      expected
      actual
  in
  assert_found_globals
    ~source:{|
      foo = []
    |}
    ~expected:[!&"test.foo", Some (Type.list Type.Any)];
  (* Note that functions are not selected *)
  assert_found_globals ~source:{|
      def foo():
        pass
    |} ~expected:[];
  assert_found_globals
    ~source:{|
      foo = []
      bar = {}
    |}
    ~expected:
      [
        !&"test.foo", Some (Type.list Type.Any);
        !&"test.bar", Some (Type.dictionary ~key:Type.Any ~value:Type.Any);
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
    ~source:{|
      class C:
        def f():
          pass
      c = C()
    |}
    ~expected:[!&"test.c", Some (Type.Primitive "test.C")];
  assert_found_globals
    ~source:{|
      x, y = [], {}
    |}
    ~expected:
      [
        !&"test.x", Some (Type.list Type.Any);
        !&"test.y", Some (Type.dictionary ~key:Type.Any ~value:Type.Any);
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
        !&"test.global_2", Some Type.Any;
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

      z = fun(1, "2")
      a: int = fun(1, "2")
      |}
    ~expected:
      [
        !&"test.x", Some Type.Any;
        ( !&"test.y",
          Some
            (Type.Callable.create
               ~name:!&"test.fun"
               ~parameters:
                 (Type.Record.Callable.Defined
                    (Type.Callable.Parameter.create
                       [
                         { name = "$parameter$x"; annotation = Type.integer; default = false };
                         { name = "$parameter$y"; annotation = Type.string; default = false };
                       ]))
               ~annotation:Type.integer
               ()) );
        !&"test.z", Some Type.Any;
        !&"test.a", Some Type.integer;
      ];
  assert_found_globals
    ~source:{|
    x = 1
    x = "abc"
    |}
    ~expected:[!&"test.x", Some Type.integer; !&"test.x", Some Type.integer];
  ()


let () = "globalVarQuery" >::: ["find_globals" >:: test_find_globals] |> Test.run
