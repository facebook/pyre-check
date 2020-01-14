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
          Define.Signature.name = + !&"derp";
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
    ~return_annotation:(Some (Type.expression (Type.generator ~async:true Type.integer)))
    ~async:true
    ~generator:true
    (Type.generator ~async:true Type.integer);
  ()


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
      ( source
      |> Test.parse_single_define
      |> (fun { Define.signature; _ } -> signature)
      |> Node.create_with_default_location
      |> Callable.create_overload_without_applying_decorators ~parser )
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
      define_location = None;
    }


let () =
  "define"
  >::: ["return_annotation" >:: test_return_annotation; "create_ovelroad" >:: test_create_overload]
  |> Test.run
