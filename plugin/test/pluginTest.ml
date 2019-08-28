(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Analysis
open Ast
open Statement
open Test
open OUnit2

let assert_equivalent_attributes ~context source expected =
  let handle = "__init__.py" in
  let attributes class_type source =
    Memory.reset_shared_memory ();
    Annotated.Class.AttributeCache.clear ();
    let _, _, environment =
      ScratchProject.setup ~context [handle, source] |> ScratchProject.build_environment
    in
    let global_resolution = Environment.resolution environment () in
    let compare_by_name left right =
      String.compare (Annotated.Attribute.name left) (Annotated.Attribute.name right)
    in
    let ignore_value_location ({ Annotated.Attribute.value; _ } as attribute) =
      { attribute with value = Node.create_with_default_location value.value }
    in
    Option.value_exn (GlobalResolution.class_definition global_resolution class_type)
    |> Annotated.Class.create
    |> Annotated.Class.attributes ~transitive:false ~resolution:global_resolution
    |> List.sort ~compare:compare_by_name
    |> List.map ~f:Node.value
    |> List.map ~f:ignore_value_location
  in
  let class_names =
    let expected =
      List.map expected ~f:(fun definition -> parse ~handle definition |> Preprocessing.preprocess)
    in
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
    let pp_as_sexps format l =
      List.map l ~f:Annotated.Attribute.sexp_of_attribute
      |> List.map ~f:Sexp.to_string_hum
      |> String.concat ~sep:"\n"
      |> Format.fprintf format "%s\n"
    in
    let simple_print l =
      let simple { Annotated.Attribute.annotation; name; _ } =
        Printf.sprintf "%s, %s" name (Annotation.show annotation)
      in
      List.map l ~f:simple |> String.concat ~sep:"\n"
    in
    assert_equal
      ~printer:simple_print
      ~pp_diff:(diff ~print:pp_as_sexps)
      (attributes class_type expected)
      (attributes class_type source)
  in
  List.iter2_exn ~f:assert_class_equal class_names expected
