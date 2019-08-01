(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Analysis
open Ast
open Statement
open Test

let assert_environment_contains ~context source expected =
  Annotated.Class.AttributeCache.clear ();
  let handle = "__init__.py" in
  let _, _, environment =
    ScratchProject.setup ~context [handle, source] |> ScratchProject.build_environment
  in
  let expected =
    List.map expected ~f:(fun definition -> parse ~handle definition |> Preprocessing.preprocess)
  in
  let class_names =
    let get_name_if_class { Node.value; _ } =
      match value with
      | Class { Class.name; _ } -> Some (Reference.show name)
      | _ -> None
    in
    List.map ~f:Source.statements expected
    |> List.filter_map ~f:List.hd
    |> List.filter_map ~f:get_name_if_class
    |> List.map ~f:(fun name -> Type.Primitive name)
  in
  let assert_class_equal class_type expected =
    let global_resolution = Environment.resolution environment () in
    let class_definition =
      Option.value_exn (GlobalResolution.class_definition global_resolution class_type)
    in
    assert_source_equal
      expected
      (Source.create ~relative:handle [+Class (Node.value class_definition)])
  in
  List.iter2_exn ~f:assert_class_equal class_names expected
