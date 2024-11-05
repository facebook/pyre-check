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
module Callable = AnnotatedCallable
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
          parent = NestingContext.create_toplevel ();
          legacy_parent = None;
          type_params = [];
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
  (* Callables returning `AsyncIterator` should keep that return type. *)
  assert_return_annotation
    ~return_annotation:(Some (Type.expression (Type.async_iterator Type.integer)))
    ~async:true
    ~generator:true
    (Type.async_iterator Type.integer);
  (* A stub will be marked as `generator=false`, since it contains no `yield`. However, if it
     returns an `AsyncIterator[Foo]`, we should not wrap the return type in `Coroutine`. *)
  assert_return_annotation
    ~return_annotation:(Some (Type.expression (Type.async_iterator Type.integer)))
    ~async:true
    ~generator:false
    (Type.async_iterator Type.integer);
  ()


let test_create_overload context =
  let assert_overload ?legacy_parent source expected =
    let legacy_parent = legacy_parent >>| Reference.create in
    let resolution =
      ScratchProject.setup ~context ["test.py", source] |> ScratchProject.build_resolution
    in
    let resolution = Resolution.global_resolution resolution in
    let parser = GlobalResolution.annotation_parser resolution in
    let generic_parameters_as_variables =
      GlobalResolution.generic_parameters_as_variables resolution
    in
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
      |> (fun { Define.signature; _ } -> { signature with legacy_parent })
      |> Callable.create_overload_without_applying_decorators
           ~parser
           ~generic_parameters_as_variables)
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
            Type.Callable.CallableParamType.Named
              { name = "x"; default = false; annotation = Type.integer };
          ];
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
            Type.Callable.CallableParamType.PositionalOnly
              { index = 0; default = false; annotation = Type.integer };
            Type.Callable.CallableParamType.Named
              { name = "y"; default = false; annotation = Type.string };
          ];
    };
  assert_overload
    {|
      class C:
        pass
      def foo(x, y: str) -> None:
        pass
    |}
    ~legacy_parent:"test.C"
    {
      Type.Callable.annotation = Type.none;
      parameters =
        Type.Callable.Defined
          [
            Type.Callable.CallableParamType.Named
              { name = "x"; default = false; annotation = Type.Primitive "test.C" };
            Type.Callable.CallableParamType.Named
              { name = "y"; default = false; annotation = Type.string };
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
    ~legacy_parent:"test.C"
    {
      Type.Callable.annotation = Type.none;
      parameters =
        Type.Callable.Defined
          [
            Type.Callable.CallableParamType.Named
              {
                name = "x";
                default = false;
                annotation = Type.parametric "test.C" [Single (Type.variable "test.T")];
              };
            Type.Callable.CallableParamType.Named
              { name = "y"; default = false; annotation = Type.string };
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
    ~legacy_parent:"test.C"
    {
      Type.Callable.annotation = Type.none;
      parameters =
        Type.Callable.Defined
          [
            Type.Callable.CallableParamType.Named
              { name = "x"; default = false; annotation = Type.class_type (Primitive "test.C") };
            Type.Callable.CallableParamType.Named
              { name = "y"; default = false; annotation = Type.string };
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
    ~legacy_parent:"test.C"
    {
      Type.Callable.annotation = Type.none;
      parameters =
        Type.Callable.Defined
          [
            Type.Callable.CallableParamType.Named
              { name = "x"; default = false; annotation = Type.Top };
            Type.Callable.CallableParamType.Named
              { name = "y"; default = false; annotation = Type.string };
          ];
    };
  ()


let () =
  "callable"
  >::: ["return_annotation" >:: test_return_annotation; "create_overload" >:: test_create_overload]
  |> Test.run
