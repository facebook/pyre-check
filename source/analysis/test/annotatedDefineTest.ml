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
open Statement
open Test
module StatementDefine = Define
module Define = AnnotatedDefine

let test_parent_definition context =
  let parent_class_summary environment name parent =
    let legacy_parent =
      match parent with
      | ModuleContext.Class _ -> Some (ModuleContext.to_qualifier ~module_name:!&"test" parent)
      | _ -> None
    in
    {
      StatementDefine.signature =
        {
          name = !&name;
          parameters = [];
          decorators = [];
          return_annotation = None;
          async = false;
          generator = false;
          parent;
          legacy_parent;
          nesting_define = None;
          type_params = [];
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
      parent_class_summary global_environment name parent >>| Node.value >>| ClassSummary.name
    in
    let cmp = Option.equal Reference.equal in
    let printer = function
      | None -> "None"
      | Some name -> Format.asprintf "Some %a" Reference.pp name
    in
    assert_equal ~cmp ~printer expected actual
  in
  let foo_parent = ModuleContext.(create_class ~parent:(create_toplevel ()) "foo") in
  assert_parent
    ~source:{|
      class foo():
        def bar(): pass
    |}
    ~name:"test.bar"
    ~parent:foo_parent
    ~expected:(Some !&"test.foo");
  assert_parent
    ~source:{|
      def bar(): pass
    |}
    ~name:"test.bar"
    ~parent:foo_parent
    ~expected:None;
  assert_parent
    ~source:{|
      class superfoo(): ...
      class foo(superfoo):
        def bar(): pass
    |}
    ~name:"test.bar"
    ~parent:foo_parent
    ~expected:(Some !&"test.foo")


let test_decorate context =
  let assert_decorated source ~expected_parameters ~expected_return_annotation =
    let source, environment =
      let project = ScratchProject.setup ~context ["test.py", source] in
      ( Option.value_exn
          (SourceCodeApi.source_of_qualifier
             (Test.ScratchProject.get_untracked_source_code_api project)
             (Reference.create "test")),
        Test.ScratchProject.global_environment project )
    in
    let resolution = GlobalResolution.create environment in
    let take_define = function
      | [{ Node.value = Statement.Define define; location }] -> Node.create define ~location
      | _ -> failwith "Expected a define"
    in
    let define =
      Source.statements source
      |> take_define
      |> AnnotatedDefine.create
      |> AnnotatedDefine.decorate ~resolution
      |> AnnotatedDefine.define
      |> Node.value
    in
    assert_equal
      ~printer:(List.to_string ~f:Expression.Parameter.show)
      ~cmp:
        (List.equal (fun left right ->
             Expression.Parameter.location_insensitive_compare left right = 0))
      define.signature.parameters
      expected_parameters;
    assert_equal
      ~printer:(fun x -> x >>| Expression.show |> Option.value ~default:"No anno")
      ~cmp:(Option.equal (fun left right -> Expression.location_insensitive_compare left right = 0))
      define.signature.return_annotation
      expected_return_annotation
  in
  (* Syntactic decoration doesn't work for non-callable returns :( *)
  assert_decorated
    {|
      @click.command()
      def foo(x: int) -> None:
        ...
    |}
    ~expected_parameters:
      [+{ Expression.Parameter.name = "$parameter$x"; value = None; annotation = Some !"int" }]
    ~expected_return_annotation:
      (Some (+Expression.Expression.Constant Expression.Constant.NoneLiteral))


let () =
  "define"
  >::: ["parent_definition" >:: test_parent_definition; "decorate" >:: test_decorate]
  |> Test.run
