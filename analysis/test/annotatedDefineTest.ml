(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast
open Analysis
open Pyre
open Statement

open Test
open AnnotatedTest

module Class = Annotated.Class
module Define = Annotated.Define


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
    |> Define.parent_definition ~resolution:(Environment.resolution environment ())
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
  assert_equal ~cmp:Access.equal ~printer:Access.show (Class.name parent) (Access.create "foo");

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
        Environment.resolution environment ()
        |> (fun resolution -> Resolution.parse_annotation resolution value)
    | _ -> Type.Top
  in
  assert_equal ~cmp:Access.equal ~printer:Access.show (Class.name parent) (Access.create "foo");
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
    |> Define.method_definition ~resolution:(Environment.resolution environment ())
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


let () =
  "define">:::[
    "parent_definition">::test_parent_definition;
    "method_definition">::test_method_definition;
  ]
  |> run_test_tt_main;
