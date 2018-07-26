(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Analysis
open Ast
open Statement
open Expression

open Test


let assert_environment_expand source expected =
  let (module Handler: Environment.Handler) =
    Environment.handler
      ~configuration:(Configuration.create ~infer:true ())
      (Environment.Builder.create ())
  in
  let source = Analysis.Preprocessing.preprocess (parse source) in
  Service.Environment.populate (module Handler) [source];
  let rec get_class_name statements =
    let get_name { Node.value; _ } =
      match value with
      | Assign { Assign.target = { Node.value = Access name; _ }; _ } ->
          Some (Access.delocalize name)
      | Class { Class.name; _ } ->
          Some name
      | Define { Define.body; _ } ->
          Some (get_class_name body)
      | _ ->
          None
    in
    List.find_map_exn ~f:get_name statements
  in
  let class_type =
    Type.create
      ~aliases:Handler.aliases
      (Node.create_with_default_location (Access (get_class_name (Source.statements source))))
  in
  if not (String.equal expected "") then
    let { Environment.class_definition; _ } =
      Option.value_exn (Handler.class_definition class_type)
    in
    assert_source_equal
      (Analysis.Preprocessing.preprocess (parse expected))
      (Source.create
         ~path:"test.py"
         [+Statement.Class (Node.value class_definition)]
      )
  else
    assert_is_none (Handler.class_definition class_type)
