(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

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
      Statement.Define.signature =
        {
          name = !&name;
          parameters = [];
          decorators = [];
          docstring = None;
          return_annotation = None;
          async = false;
          parent = parent >>| Reference.create;
        };
      body = [+Pass];
    }
    |> Define.create
    |> Define.parent_definition ~resolution:(Environment.resolution environment ())
  in
  let environment = populate {|
      class foo():
        def bar(): pass
    |} in
  let parent = parent_class_definition environment "bar" (Some "foo") |> value in
  assert_equal ~cmp:Reference.equal ~printer:Reference.show (Class.name parent) !&"foo";
  let environment = populate {|
      def bar(): pass
    |} in
  let parent = parent_class_definition environment "bar" (Some "foo") in
  assert_is_none parent;
  let environment =
    populate
      {|
      class superfoo(): ...
      class foo(superfoo):
        def bar(): pass
    |}
  in
  let parent = parent_class_definition environment "bar" (Some "foo") |> value in
  let base_type =
    match List.hd (Class.bases parent) with
    | Some { Expression.Call.Argument.value; _ } ->
        Environment.resolution environment ()
        |> fun resolution -> GlobalResolution.parse_annotation resolution value
    | _ -> Type.Top
  in
  assert_equal ~cmp:Reference.equal ~printer:Reference.show (Class.name parent) !&"foo";
  assert_equal base_type (Type.Primitive "superfoo")


let test_decorate _ =
  let open Statement.Define in
  let resolution = Environment.resolution (Test.environment ()) () in
  let assert_decorated source ~expected =
    let take_define = function
      | [{ Node.value = Statement.Define define; _ }] -> define
      | _ -> failwith "Expected a define"
    in
    let define =
      Test.parse source
      |> Source.statements
      |> take_define
      |> Annotated.Define.create
      |> Annotated.Define.decorate ~resolution
      |> Annotated.Define.define
    in
    let expected = Test.parse_single_define expected in
    assert_equal
      ~printer:(List.to_string ~f:(Parameter.show Expression.pp))
      ~cmp:(List.equal (Parameter.equal Expression.equal))
      define.signature.parameters
      expected.signature.parameters;
    assert_equal
      ~printer:(fun x -> x >>| Expression.show |> Option.value ~default:"No anno")
      ~cmp:(Option.equal Expression.equal)
      define.signature.return_annotation
      expected.signature.return_annotation
  in
  assert_decorated
    {|
      @click.command()
      def foo(x: int) -> None:
        ...
    |}
    ~expected:{|
      def foo(*args: typing.Any, **kwargs: typing.Any) -> None:
        ...
    |}


let () =
  "define"
  >::: ["parent_definition" >:: test_parent_definition; "decorate" >:: test_decorate]
  |> Test.run
