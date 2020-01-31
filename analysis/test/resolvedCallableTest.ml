(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

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
    (let decorators = List.map ~f:parse_single_expression decorators in
     {
       Define.Signature.name = + !&"define";
       parameters;
       decorators;
       return_annotation;
       async = false;
       generator = false;
       parent = None;
       nesting_define = None;
     })
    |> Node.create_with_default_location
  in
  (* Contextlib related tests *)
  let assert_apply_contextlib_decorators define expected_return_annotation =
    let applied_return_annotation =
      GlobalResolution.create_overload ~resolution define
      |> fun { Type.Callable.annotation; _ } -> annotation
    in
    assert_equal
      ~cmp:Type.equal
      ~printer:Type.show
      expected_return_annotation
      applied_return_annotation;

    (* Test decorators with old AST. *)
    let applied_return_annotation =
      GlobalResolution.create_overload ~resolution define
      |> fun { Type.Callable.annotation; _ } -> annotation
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
       ~decorators:["contextlib.contextmanager"]
       ~parameters:[]
       ~return_annotation:(Some (+Expression.String (StringLiteral.create "typing.Iterator[str]"))))
    (Type.parametric "contextlib._GeneratorContextManager" [Single Type.string]);
  assert_apply_contextlib_decorators
    (create_define
       ~decorators:["contextlib.contextmanager"]
       ~parameters:[]
       ~return_annotation:
         (Some (+Expression.String (StringLiteral.create "typing.Generator[str, None, None]"))))
    (Type.parametric "contextlib._GeneratorContextManager" [Single Type.string]);

  (* Click related tests *)
  let assert_apply_click_decorators ~expected_count define =
    let actual_count =
      let resolution = ScratchProject.setup ~context [] |> ScratchProject.build_global_resolution in
      GlobalResolution.create_overload ~resolution define
      |> fun { Type.Callable.parameters; _ } ->
      match parameters with
      | Undefined -> 0
      | ParameterVariadicTypeVariable _ -> 0
      | Defined parameters -> List.length parameters
    in
    assert_equal ~cmp:Int.equal ~printer:Int.to_string expected_count actual_count
  in
  let create_parameter ~name = Parameter.create ~location:Location.any ~name () in
  create_define ~decorators:[] ~parameters:[create_parameter ~name:"test"] ~return_annotation:None
  |> assert_apply_click_decorators ~expected_count:1;
  create_define
    ~decorators:["click.neither_command_nor_group()"]
    ~parameters:[create_parameter ~name:"test"]
    ~return_annotation:None
  |> assert_apply_click_decorators ~expected_count:1;
  create_define
    ~decorators:["click.command()"]
    ~parameters:[create_parameter ~name:"test"]
    ~return_annotation:None
  |> assert_apply_click_decorators ~expected_count:2;
  create_define
    ~decorators:["click.group()"]
    ~parameters:[create_parameter ~name:"test"]
    ~return_annotation:None
  |> assert_apply_click_decorators ~expected_count:2;

  (* Custom decorators. *)
  create_define
    ~decorators:["$strip_first_parameter"]
    ~parameters:[create_parameter ~name:"self"; create_parameter ~name:"other"]
    ~return_annotation:None
  |> (fun define -> GlobalResolution.create_overload ~resolution define)
  |> fun { Type.Callable.parameters; _ } ->
  assert_equal
    ~printer:Type.Callable.show_parameters
    ~cmp:Type.Callable.equal_parameters
    (Type.Callable.Defined
       [Type.Callable.Parameter.Named { name = "other"; annotation = Type.Top; default = false }])
    parameters


let () = "resolvedCallable" >::: ["apply_decorators" >:: test_apply_decorators] |> Test.run
