(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Core
open Analysis
open Ast
open Interprocedural
open Test

let setup ?(additional_sources = []) ~context ~handle source =
  let project =
    let additional_sources =
      List.map additional_sources ~f:(fun { handle; source } -> handle, source)
    in
    ScratchProject.setup ~context ~external_sources:[] ([handle, source] @ additional_sources)
  in
  let { ScratchProject.BuiltTypeEnvironment.sources; type_environment; _ } =
    ScratchProject.build_type_environment project
  in
  let source =
    List.find_exn sources ~f:(fun { Source.source_path = { SourcePath.relative; _ }; _ } ->
        String.equal relative handle)
  in
  source, TypeEnvironment.read_only type_environment


let test_all_decorators context =
  let assert_decorators source expected =
    let _, environment = setup ~context ~handle:"test.py" source in
    assert_equal
      ~cmp:[%equal: Reference.t list]
      ~printer:[%show: Reference.t list]
      expected
      (DecoratorHelper.all_decorators environment |> List.sort ~compare:[%compare: Reference.t])
  in
  assert_decorators
    {|
    @decorator1
    def foo(z: str) -> None:
      print(z)

    @decorator2
    @decorator3(1, 2)
    def bar(z: str) -> None:
      print(z)
  |}
    [!&"decorator1"; !&"decorator2"; !&"decorator3"];
  assert_decorators
    {|
    def outer(z: str) -> None:
      @decorator1
      def inner(z: str) -> None:
        print(z)
  |}
    [!&"decorator1"];
  assert_decorators
    {|
    class Foo:
      @decorator1
      def some_method(self, z: str) -> None:
        print(z)
  |}
    [!&"decorator1"];
  ()


let () = "decoratorHelper" >::: ["all_decorators" >:: test_all_decorators] |> Test.run
