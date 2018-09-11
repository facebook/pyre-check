(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Analysis
open Ast
open Pyre
open Statement
open Test


let test_set_local _ =
  let assert_local ~resolution ~access ~expected =
    assert_equal
      ~cmp:(Option.equal Type.equal)
      (expected >>| parse_single_expression >>| Type.create ~aliases:(fun _ -> None))
      (Resolution.get_local resolution ~access:(Access.create access) >>| Annotation.annotation)
  in

  let resolution = Test.resolution ~sources:[] () in
  assert_local ~resolution ~access:"local" ~expected:None;

  let resolution =
    Resolution.set_local
      resolution
      ~access:(Access.create "local")
      ~annotation:(Annotation.create Type.integer)
  in
  assert_local ~resolution ~access:"local" ~expected:(Some "int");

  let resolution =
    Resolution.set_local
      resolution
      ~access:(Access.create "local")
      ~annotation:(Annotation.create Type.float)
  in
  assert_local ~resolution ~access:"local" ~expected:(Some "float")


let test_parse_annotation _ =
  let assert_parse_annotation ~resolution ~expression ~expected =
    assert_equal
      ~cmp:Type.equal
      ~printer:Type.show
      (parse_single_expression expected |> Type.create ~aliases:(fun _ -> None))
      (parse_single_expression expression |> Resolution.parse_annotation resolution)
  in

  let resolution =
    Test.resolution
      ~sources:[
        parse ~qualifier:(Access.create "empty") ~handle:"empty.pyi" "class Empty: ...";
        parse
          ~qualifier:(Access.create "empty.stub")
          ~local_mode:Source.PlaceholderStub
          ~handle:"empty/stub.pyi"
          "";
      ]
      ()
  in
  assert_parse_annotation ~resolution ~expression:"int" ~expected:"int";
  assert_parse_annotation ~resolution ~expression:"$local_qualifier$int" ~expected:"qualifier.int";
  assert_parse_annotation ~resolution ~expression:"empty.stub.Annotation" ~expected:"typing.Any";
  assert_parse_annotation
    ~resolution
    ~expression:"typing.Dict[str, empty.stub.Annotation]"
    ~expected:"typing.Dict[str, typing.Any]"




let () =
  "resolution">:::[
    "set_local">::test_set_local;
    "parse_annotation">::test_parse_annotation;
  ]
  |> run_test_tt_main
