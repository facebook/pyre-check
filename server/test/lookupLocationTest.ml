(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open OUnit2
open Core
open Ast
open Test

let test_locations _ =
  let assert_locations test_cases_json =
    let open Yojson.Basic.Util in
    let assert_test_case test_case =
      let module NodeSet = Set.Make (struct
        type t = string * int list [@@deriving compare, sexp]
      end)
      in
      let actual_nodes =
        let source =
          let source = test_case |> member "source" |> to_string in
          parse_untrimmed ~handle:"test.py" source |> Preprocessing.preprocess
        in
        let deconstruct_location
            ( expression,
              {
                Location.start = { Location.line = start_line; column = start_column };
                stop = { Location.line = end_line; column = end_column };
                _;
              } )
          =
          expression, [start_line; start_column; end_line; end_column]
        in
        source |> collect_nodes_as_strings |> List.map ~f:deconstruct_location |> NodeSet.of_list
      in
      let expected_nodes =
        let expression_locations = test_case |> member "locations" |> to_list in
        let convert_location expression_location =
          let expression = expression_location |> member "expression" |> to_string in
          let location =
            expression_location |> member "location" |> to_list |> List.map ~f:to_int
          in
          expression, location
        in
        List.map ~f:convert_location expression_locations |> NodeSet.of_list
      in
      let print_node sofar (node_string, location) =
        Format.asprintf
          "%s\n  %s -> %d:%d-%d:%d"
          sofar
          node_string
          (List.nth_exn location 0)
          (List.nth_exn location 1)
          (List.nth_exn location 2)
          (List.nth_exn location 3)
      in
      assert_equal
        ~cmp:(fun expected actual -> Set.is_subset expected ~of_:actual)
        ~printer:(fun node_list -> Set.fold ~init:"" ~f:print_node node_list)
        ~pp_diff:(fun format (expected, actual) ->
          Format.fprintf
            format
            "%s"
            (Set.fold ~init:"\nMissing nodes:" ~f:print_node (Set.diff expected actual)))
        expected_nodes
        actual_nodes
    in
    let test_cases = test_cases_json |> to_list in
    List.iter ~f:assert_test_case test_cases
  in
  let test_cases = Yojson.Basic.from_file "locationTests.json" in
  assert_locations test_cases


let () = "lookup_location" >::: ["locations" >:: test_locations] |> Test.run
