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
open Statement
open Test
module Callable = Annotated.Callable
open Pyre

let test_return_annotation context =
  let assert_return_annotation expected ~return_annotation ~async ~generator =
    let return_annotation =
      let parser =
        ScratchProject.setup
          ~context
          ["test.py", {|
          class foo():
            def bar(): pass
        |}]
        |> ScratchProject.build_global_resolution
        |> GlobalResolution.annotation_parser
      in
      let signature =
        {
          Define.Signature.name = !&"derp";
          parameters = [];
          decorators = [];
          return_annotation;
          async;
          generator;
          parent = None;
          nesting_define = None;
        }
      in
      Callable.return_annotation_without_applying_decorators ~signature ~parser
    in
    assert_equal ~printer:Type.show ~cmp:Type.equal expected return_annotation
  in
  assert_return_annotation
    ~return_annotation:(Some (Type.expression Type.integer))
    ~async:false
    ~generator:false
    Type.integer;
  assert_return_annotation
    ~return_annotation:(Some (Type.expression Type.integer))
    ~async:true
    ~generator:false
    (Type.coroutine [Single Type.Any; Single Type.Any; Single Type.integer]);
  assert_return_annotation
    ~return_annotation:(Some (Type.expression Type.integer))
    ~async:true
    ~generator:true
    Type.integer;
  assert_return_annotation
    ~return_annotation:(Some (Type.expression (Type.async_generator ~yield_type:Type.integer ())))
    ~async:true
    ~generator:true
    (Type.async_generator ~yield_type:Type.integer ());
  ()


let test_create_overload context =
  let assert_overload ?parent source expected =
    let parent = parent >>| Reference.create in
    let resolution =
      ScratchProject.setup ~context ["test.py", source] |> ScratchProject.build_resolution
    in
    let resolution = Resolution.global_resolution resolution in
    let parser = GlobalResolution.annotation_parser resolution in
    let variables = GlobalResolution.variables resolution in
    let last_statement_define { Source.statements; _ } =
      match List.last_exn statements with
      | { Node.value = Statement.Define define; _ } -> define
      | _ -> failwith "last statement not define"
    in
    assert_equal
      ~cmp:(Type.Callable.equal_overload Type.equal)
      expected
      (source
      |> Test.parse
      |> last_statement_define
      |> (fun { Define.signature; _ } -> { signature with parent })
      |> Callable.create_overload_without_applying_decorators ~parser ~variables)
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
          [Type.Callable.Parameter.Named { name = "x"; default = false; annotation = Type.integer }];
    };
  assert_overload
    {|
      def foo(x: int, /, y: str) -> None:
        pass
    |}
    {
      Type.Callable.annotation = Type.none;
      parameters =
        Type.Callable.Defined
          [
            Type.Callable.Parameter.PositionalOnly
              { index = 0; default = false; annotation = Type.integer };
            Type.Callable.Parameter.Named { name = "y"; default = false; annotation = Type.string };
          ];
    };
  assert_overload
    {|
      class C:
        pass
      def foo(x, y: str) -> None:
        pass
    |}
    ~parent:"test.C"
    {
      Type.Callable.annotation = Type.none;
      parameters =
        Type.Callable.Defined
          [
            Type.Callable.Parameter.Named
              { name = "x"; default = false; annotation = Type.Primitive "test.C" };
            Type.Callable.Parameter.Named { name = "y"; default = false; annotation = Type.string };
          ];
    };
  assert_overload
    {|
      from typing import TypeVar, Generic
      T = TypeVar("T")
      class C(Generic[T]):
        pass
      def foo(x, y: str) -> None:
        pass
    |}
    ~parent:"test.C"
    {
      Type.Callable.annotation = Type.none;
      parameters =
        Type.Callable.Defined
          [
            Type.Callable.Parameter.Named
              {
                name = "x";
                default = false;
                annotation = Type.parametric "test.C" [Single (Type.variable "test.T")];
              };
            Type.Callable.Parameter.Named { name = "y"; default = false; annotation = Type.string };
          ];
    };
  assert_overload
    {|
      class C:
        pass
      @classmethod
      def foo(x, y: str) -> None:
        pass
    |}
    ~parent:"test.C"
    {
      Type.Callable.annotation = Type.none;
      parameters =
        Type.Callable.Defined
          [
            Type.Callable.Parameter.Named
              { name = "x"; default = false; annotation = Type.meta (Primitive "test.C") };
            Type.Callable.Parameter.Named { name = "y"; default = false; annotation = Type.string };
          ];
    };
  assert_overload
    {|
      class C:
        pass
      @staticmethod
      def foo(x, y: str) -> None:
        pass
    |}
    ~parent:"test.C"
    {
      Type.Callable.annotation = Type.none;
      parameters =
        Type.Callable.Defined
          [
            Type.Callable.Parameter.Named { name = "x"; default = false; annotation = Type.Top };
            Type.Callable.Parameter.Named { name = "y"; default = false; annotation = Type.string };
          ];
    };
  ()


let () =
  "callable"
  >::: ["return_annotation" >:: test_return_annotation; "create_overload" >:: test_create_overload]
  |> Test.run
