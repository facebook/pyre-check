(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Ast
open Analysis
open Pyre
open Statement
open Test
module StatementDefine = Define
module Define = Annotated.Define

let test_parent_definition context =
  let parent_class_definition environment name parent =
    {
      StatementDefine.signature =
        {
          name = + !&name;
          parameters = [];
          decorators = [];
          return_annotation = None;
          async = false;
          generator = false;
          parent = Some (Reference.create parent);
          nesting_define = None;
        };
      captures = [];
      unbound_names = [];
      body = [+Statement.Pass];
    }
    |> Node.create_with_default_location
    |> Define.create
    |> Define.parent_definition ~resolution:(GlobalResolution.create environment)
  in
  let assert_parent ~expected ~source ~name ~parent =
    let { ScratchProject.BuiltGlobalEnvironment.global_environment; _ } =
      ScratchProject.setup ~context ["test.py", source] |> ScratchProject.build_global_environment
    in
    let actual =
      parent_class_definition (AnnotatedGlobalEnvironment.read_only global_environment) name parent
      >>| Node.value
      >>| ClassSummary.name
    in
    let cmp = Option.equal Reference.equal in
    let printer = function
      | None -> "None"
      | Some name -> Format.asprintf "Some %a" Reference.pp name
    in
    assert_equal ~cmp ~printer expected actual
  in
  assert_parent
    ~source:{|
      class foo():
        def bar(): pass
    |}
    ~name:"test.bar"
    ~parent:"test.foo"
    ~expected:(Some !&"test.foo");
  assert_parent
    ~source:{|
      def bar(): pass
    |}
    ~name:"test.bar"
    ~parent:"test.foo"
    ~expected:None;
  assert_parent
    ~source:{|
      class superfoo(): ...
      class foo(superfoo):
        def bar(): pass
    |}
    ~name:"test.bar"
    ~parent:"test.foo"
    ~expected:(Some !&"test.foo")


let test_decorate context =
  let assert_decorated source ~expected =
    let source, environment =
      let { ScratchProject.BuiltGlobalEnvironment.global_environment; _ } =
        ScratchProject.setup ~context ["test.py", source] |> ScratchProject.build_global_environment
      in
      ( Option.value_exn
          (AstEnvironment.ReadOnly.get_processed_source
             (AnnotatedGlobalEnvironment.ast_environment global_environment
             |> AstEnvironment.read_only)
             (Reference.create "test")),
        global_environment )
    in
    let resolution = AnnotatedGlobalEnvironment.read_only environment |> GlobalResolution.create in
    let take_define = function
      | [{ Node.value = Statement.Define define; location }] -> Node.create define ~location
      | _ -> failwith "Expected a define"
    in
    let define =
      Source.statements source
      |> take_define
      |> Annotated.Define.create
      |> Annotated.Define.decorate ~resolution
      |> Annotated.Define.define
      |> Node.value
    in
    let expected = Test.parse_single_define expected in
    assert_equal
      ~printer:(List.to_string ~f:Expression.Parameter.show)
      ~cmp:
        (List.equal (fun left right ->
             Expression.Parameter.location_insensitive_compare left right = 0))
      define.signature.parameters
      expected.signature.parameters;
    assert_equal
      ~printer:(fun x -> x >>| Expression.show |> Option.value ~default:"No anno")
      ~cmp:(Option.equal (fun left right -> Expression.location_insensitive_compare left right = 0))
      define.signature.return_annotation
      expected.signature.return_annotation
  in
  (* Syntactic decoration doesn't work for non-callable returns :( *)
  assert_decorated
    {|
      @click.command()
      def foo(x: int) -> None:
        ...
    |}
    ~expected:{|
      def foo($parameter$x: int) -> None:
        ...
    |}


let () =
  "define"
  >::: ["parent_definition" >:: test_parent_definition; "decorate" >:: test_decorate]
  |> Test.run
