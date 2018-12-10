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
  Annotated.Class.Attribute.Cache.clear ();
  let configuration = Configuration.Analysis.create ~infer:true () in
  let (module Handler: Environment.Handler) =
    Environment.handler ~configuration (Environment.Builder.create ())
  in
  let source = Analysis.Preprocessing.preprocess (parse source) in
  Service.Environment.populate ~configuration (module Handler) [source];

  let expected =
    List.map
      expected
      ~f:(fun definition -> (Analysis.Preprocessing.preprocess (parse definition)))
  in
  let class_types =
    let get_name_if_class { Node.value; _ } =
      match value with
      | Class { Class.name; _ } ->
          Some name
      | _ ->
          None
    in
    let get_type name =
      Access.expression name
      |> Type.create ~aliases:Handler.aliases
    in
    List.map ~f:Source.statements expected
    |> List.filter_map ~f:List.hd
    |> List.filter_map ~f:get_name_if_class
    |> List.map ~f:get_type
  in
  let assert_class_equal class_type expected =
    let { Resolution.class_definition; _ } =
      Option.value_exn (Handler.class_definition class_type)
    in
    assert_source_equal
      expected
      (Source.create
         ~handle:(File.Handle.create "test.py")
         [+Statement.Class (Node.value class_definition)]
      )
  in
  List.iter2_exn ~f:assert_class_equal class_types expected
