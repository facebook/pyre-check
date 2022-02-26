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

let test_apply_decorators context =
  let resolution = ScratchProject.setup ~context [] |> ScratchProject.build_global_resolution in
  let create_define ~decorators ~parameters ~return_annotation =
    {
      Define.Signature.name = !&"define";
      parameters;
      decorators;
      return_annotation;
      async = false;
      generator = false;
      parent = None;
      nesting_define = None;
    }
  in
  (* Contextlib related tests *)
  let assert_apply_contextlib_decorators define expected_return_annotation =
    let applied_return_annotation =
      GlobalResolution.resolve_define ~resolution ~implementation:(Some define) ~overloads:[]
      |> function
      | { decorated = Ok (Callable { implementation = { Type.Callable.annotation; _ }; _ }); _ } ->
          annotation
      | _ -> failwith "impossible"
    in
    assert_equal
      ~cmp:Type.equal
      ~printer:Type.show
      expected_return_annotation
      applied_return_annotation;

    (* Test decorators with old AST. *)
    let applied_return_annotation =
      GlobalResolution.resolve_define ~resolution ~implementation:(Some define) ~overloads:[]
      |> function
      | { decorated = Ok (Callable { implementation = { Type.Callable.annotation; _ }; _ }); _ } ->
          annotation
      | _ -> failwith "impossible"
    in
    assert_equal
      ~cmp:Type.equal
      ~printer:Type.show
      expected_return_annotation
      applied_return_annotation
  in
  assert_apply_contextlib_decorators
    (create_define ~decorators:[] ~parameters:[] ~return_annotation:(Some !"str"))
    Type.string;
  assert_apply_contextlib_decorators
    (create_define
       ~decorators:[!"contextlib.contextmanager"]
       ~parameters:[]
       ~return_annotation:
         (Some
            (+Expression.Constant (Constant.String (StringLiteral.create "typing.Iterator[str]")))))
    (Type.parametric "contextlib._GeneratorContextManager" [Single Type.string]);
  assert_apply_contextlib_decorators
    (create_define
       ~decorators:[!"contextlib.contextmanager"]
       ~parameters:[]
       ~return_annotation:
         (Some
            (+Expression.Constant
                (Constant.String (StringLiteral.create "typing.Generator[str, None, None]")))))
    (Type.parametric "contextlib._GeneratorContextManager" [Single Type.string]);

  let create_parameter ~name = Parameter.create ~location:Location.any ~name () in
  (* Custom decorators. *)
  create_define
    ~decorators:[!"_strip_first_parameter_"]
    ~parameters:[create_parameter ~name:"self"; create_parameter ~name:"other"]
    ~return_annotation:None
  |> (fun define ->
       GlobalResolution.resolve_define ~resolution ~implementation:(Some define) ~overloads:[])
  |> (function
       | { decorated = Ok (Callable { implementation = { Type.Callable.parameters; _ }; _ }); _ } ->
           parameters
       | _ -> failwith "impossible")
  |> fun parameters ->
  assert_equal
    ~printer:Type.Callable.show_parameters
    ~cmp:Type.Callable.equal_parameters
    (Type.Callable.Defined
       [Type.Callable.Parameter.Named { name = "other"; annotation = Type.Top; default = false }])
    parameters


let () = "resolvedCallable" >::: ["apply_decorators" >:: test_apply_decorators] |> Test.run
