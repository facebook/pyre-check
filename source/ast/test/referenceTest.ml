(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Ast
open Pyre
open Test

let node = Node.create_with_default_location

let test_create _ =
  let assert_create ?prefix input =
    let expected = prefix >>| (fun prefix -> prefix ^ "." ^ input) |> Option.value ~default:input in
    let prefix = prefix >>| Reference.create in
    assert_equal expected (Reference.show (Reference.create ?prefix input))
  in
  assert_create "";
  assert_create "a";
  assert_create "a.b.c";
  assert_create ~prefix:"a.b" "c.d.e"


let test_expression _ =
  let open Expression in
  let assert_expression reference expression =
    let expected = Expression.Name expression |> Node.create_with_default_location in
    let actual = Reference.create reference |> from_reference ~location:Location.any in
    assert_equal ~printer:Expression.show expected actual
  in
  assert_expression "a" (Name.Identifier "a");
  assert_expression
    "a.b.c"
    (Name.Attribute
       {
         base =
           Expression.Name
             (Name.Attribute
                {
                  base = Expression.Name (Name.Identifier "a") |> node;
                  attribute = "b";
                  special = false;
                })
           |> node;
         attribute = "c";
         special = false;
       })


let test_name _ =
  let open Expression in
  let assert_create_name_expression reference expression =
    let expected = Expression.Name expression |> node in
    let actual = Reference.create reference |> from_reference ~location:Location.any in
    assert_equal ~printer:Expression.show expected actual
  in
  assert_create_name_expression "a" (Name.Identifier "a");
  assert_create_name_expression
    "a.b"
    (Name.Attribute
       { base = Expression.Name (Name.Identifier "a") |> node; attribute = "b"; special = false });
  assert_create_name_expression
    "a.b.c"
    (Name.Attribute
       {
         base =
           Expression.Name
             (Name.Attribute
                {
                  base = Expression.Name (Name.Identifier "a") |> node;
                  attribute = "b";
                  special = false;
                })
           |> node;
         attribute = "c";
         special = false;
       });
  let assert_create_from_name name expected =
    assert_equal
      ~printer:Reference.show
      ~cmp:Reference.equal
      (Reference.create expected)
      (name_to_reference_exn name)
  in
  assert_create_from_name (Name.Identifier "a") "a";
  assert_create_from_name
    (Name.Attribute
       { base = Expression.Name (Name.Identifier "a") |> node; attribute = "b"; special = false })
    "a.b";
  assert_create_from_name
    (Name.Attribute
       {
         base =
           Expression.Name
             (Name.Attribute
                {
                  base = Expression.Name (Name.Identifier "a") |> node;
                  attribute = "b";
                  special = false;
                })
           |> node;
         attribute = "c";
         special = false;
       })
    "a.b.c"


let test_delocalize _ =
  let assert_delocalize source expected =
    assert_equal
      ~printer:Reference.show
      ~cmp:Reference.equal
      (Reference.create expected)
      (Reference.create source |> Reference.delocalize)
  in
  assert_delocalize "constant" "constant";
  assert_delocalize "$local_qualifier$variable" "qualifier.variable";
  assert_delocalize "$local_base64$b64encode" "base64.b64encode";
  assert_delocalize "$local_module?qualifier$variable" "module.qualifier.variable";
  assert_delocalize "$local_module?hyphenated-name$variable" "module.hyphenated-name.variable";
  assert_delocalize "$local_$_T" "_T";

  (* Don't attempt to delocalize qualified expressions. *)
  assert_delocalize "qualifier.$local_qualifier$variable" "qualifier.$local_qualifier$variable"


let test_prefix _ =
  let check_prefix prefix reference =
    Reference.is_prefix ~prefix:(Reference.create prefix) (Reference.create reference)
  in
  assert_true (check_prefix "" "a");
  assert_true (check_prefix "a" "a.b");
  assert_true (check_prefix "a.b" "a.b");
  assert_false (check_prefix "a.c" "c.a");
  let check_suffix suffix reference =
    Reference.is_suffix ~suffix:(Reference.create suffix) (Reference.create reference)
  in
  assert_true (check_suffix "" "a");
  assert_true (check_suffix "a" "a");
  assert_true (check_suffix "b.c" "a.b.c");
  assert_false (check_suffix "a" "a.b");
  let check_strict prefix reference =
    Reference.is_strict_prefix ~prefix:(Reference.create prefix) (Reference.create reference)
  in
  assert_true (check_strict "a" "a.b");
  assert_true (check_strict "a.b" "a.b.c");
  assert_false (check_strict "a.b" "a.b");
  assert_false (check_strict "a.b.c" "a.b");
  assert_false (check_strict "a" "b.a");
  let assert_drop_prefix prefix reference dropped =
    let prefix = Reference.create prefix in
    let reference = Reference.create reference in
    let dropped = Reference.create dropped in
    assert_equal ~printer:Reference.show dropped (Reference.drop_prefix ~prefix reference)
  in
  assert_drop_prefix "a" "a.b" "b";
  assert_drop_prefix "a" "b.a" "b.a";
  assert_drop_prefix "a.b" "a.b.c" "c";
  assert_drop_prefix "a" "a" "a";
  let assert_prefix reference prefix =
    assert_equal
      ~printer:(fun reference -> reference >>| Reference.show |> Option.value ~default:"None")
      (prefix >>| Reference.create)
      (Reference.prefix (Reference.create reference))
  in
  assert_prefix "a" (Some "");
  assert_prefix "a.b" (Some "a");
  assert_prefix "a.b.c" (Some "a.b");
  let assert_last reference last =
    assert_equal last (Reference.last (Reference.create reference))
  in
  assert_last "a" "a";
  assert_last "a.b" "b"


let test_map_last _ =
  let assert_mapped ~f given expected =
    assert_equal
      ~cmp:Reference.equal
      ~printer:Reference.show
      (Reference.map_last ~f (Reference.create given))
      (Reference.create expected)
  in
  assert_mapped ~f:(fun s -> s ^ "_suffix") "" "";
  assert_mapped ~f:(fun s -> s ^ "_suffix") "a.b" "a.b_suffix";
  assert_mapped ~f:Fn.id "a.b" "a.b";
  ()


let test_possible_qualifiers _ =
  let assert_prefixes reference expected =
    let actual = Reference.possible_qualifiers (Reference.create reference) in
    let expected_references = List.map ~f:Reference.create expected in
    assert_equal
      ~cmp:(List.equal Reference.equal)
      ~printer:(fun references -> [%show: Reference.t list] references)
      expected_references
      actual
  in
  assert_prefixes "" [];
  assert_prefixes "a" [];
  assert_prefixes "a.b" ["a"];
  assert_prefixes "a.b.c" ["a"; "a.b"];
  ()


let () =
  "reference"
  >::: [
         "create" >:: test_create;
         "expression" >:: test_expression;
         "name" >:: test_name;
         "delocalize" >:: test_delocalize;
         "prefix" >:: test_prefix;
         "map_last" >:: test_map_last;
         "possible_qualifiers" >:: test_possible_qualifiers;
       ]
  |> Test.run
