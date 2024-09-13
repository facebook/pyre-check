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
open Expression
open Statement
open Test

let resolve_define resolution define =
  match
    GlobalResolution.resolve_define
      resolution
      ~callable_name:None
      ~implementation:(Some define)
      ~overloads:[]
      ~scoped_type_variables:None
  with
  | Ok (Callable { implementation; _ }) -> implementation
  | _ -> failwith "impossible"


let assert_resolved_return_annotation_equal resolution define expected_return_annotation =
  let resolved = resolve_define resolution define in
  assert_equal
    ~cmp:Type.equal
    ~printer:Type.show
    expected_return_annotation
    resolved.Type.Callable.annotation


let assert_resolved_parameters_equal resolution define expected_parameters =
  let resolved = resolve_define resolution define in
  assert_equal
    ~printer:Type.Callable.show_parameters
    ~cmp:Type.Callable.equal_parameters
    expected_parameters
    resolved.Type.Callable.parameters


let create_define ~decorators ~parameters ~return_annotation =
  {
    Define.Signature.name = !&"define";
    parameters;
    decorators;
    return_annotation;
    async = false;
    generator = false;
    parent = NestingContext.create_toplevel ();
    legacy_parent = None;
    type_params = [];
  }


let test_resolve_return_annotation context =
  let resolution = ScratchProject.setup ~context [] |> ScratchProject.build_global_resolution in
  assert_resolved_return_annotation_equal
    resolution
    (create_define ~decorators:[] ~parameters:[] ~return_annotation:(Some !"str"))
    Type.string


let test_resolve_contextmanager_iterator context =
  let resolution = ScratchProject.setup ~context [] |> ScratchProject.build_global_resolution in
  assert_resolved_return_annotation_equal
    resolution
    (create_define
       ~decorators:[!"contextlib.contextmanager"]
       ~parameters:[]
       ~return_annotation:
         (Some
            (+Expression.Constant (Constant.String (StringLiteral.create "typing.Iterator[str]")))))
    (Type.parametric "contextlib._GeneratorContextManager" [Single Type.string])


let test_resolve_contextmanager_generator context =
  let resolution = ScratchProject.setup ~context [] |> ScratchProject.build_global_resolution in
  assert_resolved_return_annotation_equal
    resolution
    (create_define
       ~decorators:[!"contextlib.contextmanager"]
       ~parameters:[]
       ~return_annotation:
         (Some
            (+Expression.Constant
                (Constant.String (StringLiteral.create "typing.Generator[str, None, None]")))))
    (Type.parametric "contextlib._GeneratorContextManager" [Single Type.string])


let test_strip_first_parameter context =
  let resolution = ScratchProject.setup ~context [] |> ScratchProject.build_global_resolution in
  let create_parameter ~name = Parameter.create ~location:Location.any ~name () in
  assert_resolved_parameters_equal
    resolution
    (create_define
       ~decorators:[!"_strip_first_parameter_"]
       ~parameters:[create_parameter ~name:"self"; create_parameter ~name:"other"]
       ~return_annotation:None)
    (Type.Callable.Defined
       [
         Type.Callable.CallableParamType.Named
           { name = "other"; annotation = Type.Top; default = false };
       ])


let () =
  "resolvedCallable"
  >::: [
         "resolve_return_annotation" >:: test_resolve_return_annotation;
         "resolve_contextmanager_iterator" >:: test_resolve_contextmanager_iterator;
         "resolve_contextmanager_generator" >:: test_resolve_contextmanager_generator;
         "strip_first_parameter" >:: test_strip_first_parameter;
       ]
  |> Test.run
