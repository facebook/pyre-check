(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Ast
open Analysis
open Statement
open Test
module Callable = Annotated.Callable

let test_return_annotation context =
  let assert_return_annotation return_annotation async expected =
    let return_annotation =
      let _, _, environment =
        ScratchProject.setup
          ~context
          ["test.py", {|
          class foo():
            def bar(): pass
        |}]
        |> ScratchProject.build_environment
      in
      let parser = Environment.resolution environment () |> GlobalResolution.annotation_parser in
      {
        Define.signature =
          {
            name = !&"derp";
            parameters = [];
            decorators = [];
            docstring = None;
            return_annotation;
            async;
            parent = None;
          };
        body = [+Statement.Pass];
      }
      |> fun define -> Callable.return_annotation ~define ~parser
    in
    assert_equal ~printer:Type.show ~cmp:Type.equal expected return_annotation
  in
  assert_return_annotation (Some (Type.expression Type.integer)) false Type.integer;
  assert_return_annotation
    (Some (Type.expression Type.integer))
    true
    (Type.coroutine (Concrete [Type.Any; Type.Any; Type.integer]))


let test_create_overload context =
  let assert_overload source expected =
    let resolution =
      ScratchProject.setup ~context ["test.py", source] |> ScratchProject.build_resolution
    in
    let resolution = Resolution.global_resolution resolution in
    let parser = GlobalResolution.annotation_parser resolution in
    assert_equal
      ~cmp:(Type.Callable.equal_overload Type.equal)
      expected
      (source |> Test.parse_single_define |> fun define -> Callable.create_overload ~parser define)
  in
  assert_overload
    {|
      def foo(x: int) -> None:
        pass
    |}
    {
      Type.Callable.annotation = Type.none;
      parameters =
        Type.Callable.Defined
          [
            Type.Callable.Parameter.Named { name = "x"; default = false; annotation = Type.integer };
          ];
      define_location = None;
    }


let () =
  "define"
  >::: ["return_annotation" >:: test_return_annotation; "create_ovelroad" >:: test_create_overload]
  |> Test.run
