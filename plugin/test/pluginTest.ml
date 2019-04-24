(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Analysis
open Ast
open Statement

open Test


let assert_environment_contains source expected =
  Annotated.Class.AttributeCache.clear ();
  let (module Handler: Environment.Handler) =
    environment ~sources:[] ()
  in
  let source =
    parse source
    |> Preprocessing.preprocess
  in
  Test.populate
    ~configuration:(Configuration.Analysis.create ())
    (module Handler)
    (source :: typeshed_stubs ~include_helper_builtins:false ());

  let expected =
    List.map
      expected
      ~f:(fun definition -> parse definition |> Preprocessing.preprocess |> Preprocessing.convert)
  in
  let class_names =
    let get_name_if_class { Node.value; _ } =
      match value with
      | Class { Class.name; _ } ->
          Some (Reference.show name)
      | _ ->
          None
    in
    List.map ~f:Source.statements expected
    |> List.filter_map ~f:List.hd
    |> List.filter_map ~f:get_name_if_class
  in
  let assert_class_equal class_type expected =
    let class_definition = Option.value_exn (Handler.class_definition class_type) in
    assert_source_equal
      expected
      (Source.create
         ~handle:(File.Handle.create "test.py")
         [+Class (Node.value class_definition)]
      )
  in
  List.iter2_exn ~f:assert_class_equal class_names expected
