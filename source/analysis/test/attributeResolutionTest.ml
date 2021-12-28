(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Core
open OUnit2
open Analysis
open Test

let test_prepare_arguments_for_signature_selection _ =
  let open AttributeResolution in
  let assert_prepared_arguments ~self_argument arguments expected =
    let actual =
      SignatureSelection.prepare_arguments_for_signature_selection ~self_argument arguments
    in
    assert_equal
      ~printer:[%show: Argument.WithPosition.t list]
      ~cmp:[%compare.equal: Argument.WithPosition.t list]
      expected
      actual
  in
  assert_prepared_arguments ~self_argument:None [] [];
  assert_prepared_arguments
    ~self_argument:(Some (Type.parametric "Foo" []))
    [
      {
        Argument.resolved = Type.integer;
        kind = Positional;
        expression = parse_single_expression "42" |> Option.some;
      };
      {
        Argument.resolved = Type.string;
        kind = Named (Node.create_with_default_location "some_argument");
        expression = parse_single_expression "'hello'" |> Option.some;
      };
    ]
    [
      {
        Argument.WithPosition.resolved = Type.parametric "Foo" [];
        kind = Positional;
        expression = None;
        position = 0;
      };
      {
        Argument.WithPosition.resolved = Type.string;
        kind = Named (Node.create_with_default_location "some_argument");
        expression = parse_single_expression "'hello'" |> Option.some;
        position = 2;
      };
      {
        Argument.WithPosition.resolved = Type.integer;
        kind = Positional;
        expression = parse_single_expression "42" |> Option.some;
        position = 1;
      };
    ];
  assert_prepared_arguments
    ~self_argument:None
    [
      {
        Argument.resolved = Type.integer;
        kind = Positional;
        expression = parse_single_expression "42" |> Option.some;
      };
      {
        Argument.resolved = Type.tuple [Type.integer; Type.string; Type.bool];
        kind = SingleStar;
        expression = None;
      };
      {
        Argument.resolved = Type.string;
        kind = Named (Node.create_with_default_location "some_argument");
        expression = parse_single_expression "'hello'" |> Option.some;
      };
    ]
    [
      {
        Argument.WithPosition.resolved = Type.string;
        kind = Named (Node.create_with_default_location "some_argument");
        expression = parse_single_expression "'hello'" |> Option.some;
        position = 5;
      };
      {
        Argument.WithPosition.resolved = Type.integer;
        kind = Positional;
        expression = parse_single_expression "42" |> Option.some;
        position = 1;
      };
      {
        Argument.WithPosition.resolved = Type.integer;
        kind = Positional;
        expression = None;
        position = 2;
      };
      {
        Argument.WithPosition.resolved = Type.string;
        kind = Positional;
        expression = None;
        position = 3;
      };
      {
        Argument.WithPosition.resolved = Type.bool;
        kind = Positional;
        expression = None;
        position = 4;
      };
    ];
  ()


let () =
  "attributeResolution"
  >::: ["prepare_arguments" >:: test_prepare_arguments_for_signature_selection]
  |> Test.run
