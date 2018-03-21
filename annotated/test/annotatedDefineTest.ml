(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast
open Analysis
open Expression
open Pyre
open Statement

open Test
open AnnotatedTest

module Class = Annotated.Class
module Define = Annotated.Define


let test_return_annotation _ =
  let assert_return_annotation return_annotation async expected =
    let return_annotation =
      let environment =
        populate {|
          class foo():
            def bar(): pass
        |}
      in
      {
        Statement.Define.name = Access.create "derp";
        parameters = [];
        body = [+Pass];
        decorators = [];
        docstring = None;
        return_annotation;
        async;
        generated = false;
        parent = None;
      }
      |> Define.create
      |> Define.return_annotation ~resolution:(resolution environment)
    in
    assert_equal ~cmp:Type.equal expected return_annotation
  in
  assert_return_annotation (Some (Type.expression Type.integer)) false Type.integer;
  assert_return_annotation (Some (Type.expression Type.integer)) true (Type.awaitable Type.integer)


let test_callable _ =
  let assert_callable source expected =
    let resolution = populate source |> resolution in
    let callable =
      parse_single_define source
      |> Define.create
      |> Define.callable ~resolution
    in
    assert_equal ~printer:Type.show expected callable
  in
  assert_callable
    "def foo() -> int: ..."
    (Type.callable ~name:(Access.create "foo") ~annotation:Type.integer ());
  assert_callable
    "async def foo() -> int: ..."
    (Type.callable ~name:(Access.create "foo") ~annotation:(Type.awaitable Type.integer) ())


let test_parent_definition _ =
  let parent_class_definition environment name parent =
    {
      Statement.Define.name = Access.create name;
      parameters = [];
      body = [+Pass];
      decorators = [];
      docstring = None;
      return_annotation = None;
      async = false;
      generated = false;
      parent = parent >>| Access.create;
    }
    |> Define.create
    |> Define.parent_definition ~resolution:(resolution environment)
  in

  let environment =
    populate {|
      class foo():
        def bar(): pass
    |} in
  let parent =
    parent_class_definition environment "bar" (Some "foo")
    |> value
  in
  assert_equal (Class.name parent) (Access.create "foo");

  let environment =
    populate {|
      def bar(): pass
    |} in
  let parent =
    parent_class_definition environment "bar" (Some "foo")
  in
  assert_is_none parent;

  let environment =
    populate {|
      class superfoo(): ...
      class foo(superfoo):
        def bar(): pass
    |} in
  let parent =
    parent_class_definition environment "bar" (Some "foo")
    |> value
  in
  let base_type =
    match (List.hd (Class.bases parent)) with
    | Some {Argument.value; _ } ->
        resolution environment
        |> (fun resolution -> Resolution.parse_annotation resolution value)
    | _ -> Type.Top
  in
  assert_equal (Class.name parent) (Access.create "foo");
  assert_equal base_type (Type.Primitive ~~"superfoo")


let test_method_definition _ =
  let parent_class_definition environment name parent =
    {
      Statement.Define.name = Access.create name;
      parameters = [];
      body = [+Pass];
      decorators = [];
      docstring = None;
      return_annotation = None;
      async = false;
      generated = false;
      parent = parent >>| Access.create;
    }
    |> Define.create
    |> Define.method_definition ~resolution:(resolution environment)
  in
  assert_is_some
    (parent_class_definition
       (populate {|
        class Foo():
          def far(): pass
       |})
       "foo"
       (Some "Foo"));
  assert_is_none (parent_class_definition (populate "") "foo" None)


let test_parameter_annotations _ =
  let resolution =
    populate {|
      class foo():
        def bar(): pass
    |}
    |> resolution
  in
  let define parameters =
    {
      Statement.Define.name = Access.create "";
      parameters;
      body = [+Pass];
      decorators = [];
      docstring = None;
      return_annotation = None;
      async = false;
      generated = false;
      parent = None;
    }
    |> Annotated.Define.create
  in
  let create_parameter ?annotation name = +{
    Parameter.name = Identifier.create name;
    value = None;
    annotation;
  }
  in
  let definition = define [
      create_parameter ~annotation:(Type.expression Type.integer) "a";
      create_parameter "b";
    ]
  in
  let parameter_map = Annotated.Define.parameter_annotations definition ~resolution in
  assert_equal (Map.find_exn parameter_map ~~"a") Type.integer;
  assert_equal (Map.find_exn parameter_map ~~"b") Type.Top;
  assert_equal (Map.find parameter_map ~~"c") None


let test_infer_argument_name _ =
  let create_parameter ?annotation name = +{
    Parameter.name = ~~name;
    value = None;
    annotation;
  }
  in
  let parameters = [
    create_parameter ~annotation:(Type.expression Type.integer) "a";
    create_parameter "b";
    create_parameter ~annotation:(Type.expression Type.string) "*c";
  ]
  in
  let create_integer name = {
    Argument.name;
    value = +(Integer 2);
  }
  in
  let define parameters =
    {
      Statement.Define.name = Access.create "";
      parameters;
      body = [+Pass];
      decorators = [];
      docstring = None;
      return_annotation = None;
      async = false;
      generated = false;
      parent = None;
    }
    |> Annotated.Define.create
  in
  let infer_name = Annotated.Define.infer_argument_name (define parameters) in
  let unnamed = create_integer None in
  let named = create_integer (Some ~~"z") in
  assert_equal (infer_name ~index:0 ~argument:unnamed) (Some ~~"a");
  assert_equal (infer_name ~index:1 ~argument:unnamed) (Some ~~"b");
  assert_equal (infer_name ~index:2 ~argument:(create_integer None)) (Some ~~"*c");
  assert_equal (infer_name ~index:3 ~argument:(create_integer None)) (Some ~~"*c");

  assert_equal (infer_name ~index:0 ~argument:named) (Some ~~"z");
  assert_equal (infer_name ~index:1 ~argument:named) (Some ~~"z");
  assert_equal (infer_name ~index:2 ~argument:named) (Some ~~"z");
  assert_equal (infer_name ~index:3 ~argument:named) (Some ~~"z");

  let parameters = [ create_parameter ~annotation:(Type.expression Type.integer) "a" ] in
  let infer_name = Annotated.Define.infer_argument_name (define parameters) in
  assert_equal (infer_name ~index:0 ~argument:unnamed) (Some ~~"a");
  assert_equal (infer_name ~index:1 ~argument:unnamed) None;
  assert_equal (infer_name ~index:0 ~argument:named) (Some ~~"z");
  assert_equal (infer_name ~index:1 ~argument:named) (Some ~~"z")


let () =
  "define">:::[
    "return_annotation">::test_return_annotation;
    "callable">::test_callable;
    "parent_definition">::test_parent_definition;
    "method_definition">::test_method_definition;
    "infer_argument_name">::test_infer_argument_name;
    "parameter_annotations">::test_parameter_annotations;
  ]
  |> run_test_tt_main;
