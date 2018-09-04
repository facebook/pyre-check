(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Analysis
open Ast
open Statement
open Expression

open Test


let assert_environment_contains source expected =
  let (module Handler: Environment.Handler) =
    Environment.handler
      ~configuration:(Configuration.create ~infer:true ())
      (Environment.Builder.create ())
  in
  let source = Analysis.Preprocessing.preprocess (parse source) in
  Service.Environment.populate (module Handler) [source];

  let class_types =
    let get_name_if_class { Node.value; _ } =
      match value with
      | Class { Class.name; _ } ->
          Some name
      | _ ->
          None
    in
    let get_type name =
      Access name
      |> Node.create_with_default_location
      |> Type.create ~aliases:Handler.aliases
    in
    Source.statements source
    |> List.filter_map ~f:get_name_if_class
    |> List.map ~f:get_type
  in
  let assert_class_equal class_type expected =
    let { Resolution.class_definition; _ } =
      Option.value_exn (Handler.class_definition class_type)
    in
    assert_source_equal
      (Analysis.Preprocessing.preprocess (parse expected))
      (Source.create
         ~handle:(File.Handle.create "test.py")
         [+Statement.Class (Node.value class_definition)]
      )
  in
  List.iter2_exn ~f:assert_class_equal class_types expected
